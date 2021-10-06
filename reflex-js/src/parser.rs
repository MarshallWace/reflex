// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    borrow::Cow,
    iter::{empty, once},
    path::Path,
};

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, StringValue},
    lang::{as_integer, BuiltinTerm, ValueTerm},
};
use resast::prelude::*;
use ressa::Builder;

use crate::{
    builtins::{construct, dispatch, get_builtin_field, throw, to_string},
    Env,
};

pub type ParserResult<T> = Result<T, ParserError>;
pub type ParserError = String;

fn err<T: std::fmt::Debug>(message: &str, node: T) -> String {
    format!("{}: {:?}", message, node)
}

fn err_unimplemented<T: std::fmt::Debug>(node: T) -> String {
    err("Unsupported syntax", node)
}

fn err_unreachable<T: std::fmt::Debug>(node: T) -> String {
    err("Unreachable code", node)
}

#[derive(Clone)]
struct LexicalScope<'src> {
    bindings: Vec<Option<&'src str>>,
}
impl<'src> LexicalScope<'src> {
    fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }
    fn from(identifiers: impl IntoIterator<Item = Option<&'src str>>) -> Self {
        Self {
            bindings: identifiers.into_iter().collect(),
        }
    }
    fn depth(&self) -> usize {
        self.bindings.len()
    }
    fn create_child(
        &self,
        identifiers: impl IntoIterator<Item = Option<&'src str>>,
    ) -> LexicalScope<'src> {
        LexicalScope {
            bindings: self
                .bindings
                .iter()
                .cloned()
                .chain(identifiers.into_iter())
                .collect(),
        }
    }
    fn get(&self, identifier: &'src str) -> Option<usize> {
        Some(
            self.bindings
                .iter()
                .rev()
                .enumerate()
                .find(|(_, key)| match key {
                    None => false,
                    Some(key) => *key == identifier,
                })
                .map(|(i, _)| i)?,
        )
    }
}

pub fn parse<'src, T: Expression>(
    input: &'src str,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let program = parse_ast(input)?;
    parse_script_contents(program.into_iter(), env, factory, allocator)
}

pub fn parse_module<'src, T: Expression>(
    input: &'src str,
    env: &Env<T>,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let program = parse_ast(input)?;
    parse_module_contents(program.into_iter(), env, path, loader, factory, allocator)
}

fn parse_ast(input: &str) -> ParserResult<Vec<ProgramPart>> {
    Builder::new()
        .module(true)
        .js(input)
        .build()
        .and_then(|parser| parser.collect::<Result<Vec<_>, ressa::Error>>())
        .or_else(|error| Err(format!("Parse error: {}", error)))
}

fn parse_script_contents<'src, T: Expression>(
    program: impl IntoIterator<Item = ProgramPart<'src>> + ExactSizeIterator,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let body = program
        .into_iter()
        .map(|node| match node {
            ProgramPart::Stmt(node) => match node {
                Stmt::Expr(node) => ProgramPart::Stmt(Stmt::Return(Some(node))),
                _ => ProgramPart::Stmt(node),
            },
            _ => node,
        })
        .collect::<Vec<_>>();
    match parse_block(&body, &LexicalScope::new(), &env, factory, allocator)? {
        None => Err(String::from("No expression to evaluate")),
        Some(expression) => Ok(expression),
    }
}

fn parse_module_contents<'src, T: Expression>(
    program: impl IntoIterator<Item = ProgramPart<'src>> + ExactSizeIterator,
    env: &Env<T>,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let num_statements = program.len();
    let (body, import_bindings) = program.into_iter().fold(
        Ok((Vec::with_capacity(num_statements), Vec::new())),
        |results, node| {
            let (mut body, mut import_bindings) = results?;
            match node {
                ProgramPart::Decl(node) => match node {
                    Decl::Import(node) => {
                        let bindings =
                            parse_module_import(&node, path, loader, factory, allocator)?;
                        import_bindings.extend(bindings);
                        Ok((body, import_bindings))
                    }
                    Decl::Export(node) => match *node {
                        ModExport::Default(node) => match node {
                            DefaultExportDecl::Decl(node) => Err(err_unimplemented(node)),
                            DefaultExportDecl::Expr(node) => {
                                body.push(ProgramPart::Stmt(Stmt::Return(Some(node))));
                                Ok((body, import_bindings))
                            }
                        },
                        ModExport::Named(node) => match node {
                            NamedExportDecl::Decl(node) => match node {
                                Decl::Var(_, _) => {
                                    body.push(ProgramPart::Decl(node));
                                    Ok((body, import_bindings))
                                }
                                _ => Err(err_unimplemented(node)),
                            },
                            NamedExportDecl::Specifier(_, _) => Err(err_unimplemented(node)),
                        },
                        ModExport::All(_) => Err(err_unimplemented(node)),
                    },
                    _ => {
                        body.push(ProgramPart::Decl(node));
                        Ok((body, import_bindings))
                    }
                },
                _ => {
                    body.push(node);
                    Ok((body, import_bindings))
                }
            }
        },
    )?;
    let (import_keys, import_initializers): (Vec<_>, Vec<_>) = import_bindings.into_iter().unzip();
    let scope = LexicalScope::from(import_keys.into_iter().map(Some));
    match parse_block(&body, &scope, &env, factory, allocator)? {
        None => Err(String::from("Missing default module export")),
        Some(expression) => Ok(if import_initializers.is_empty() {
            expression
        } else {
            create_declaration_block(import_initializers, expression, factory)
        }),
    }
}

fn parse_module_import<'src, T: Expression>(
    node: &ModImport<'src>,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Vec<(&'src str, T)>> {
    let module_path = match &node.source {
        Lit::String(node) => Ok(parse_string(node)),
        _ => Err(err_unimplemented(node)),
    }?;
    let module = match loader(&module_path, path)
        .unwrap_or_else(|| Err(String::from("No compatible loaders registered")))
    {
        Ok(module) => Ok(module),
        Err(error) => Err(err(
            &format!("Failed to import '{}': {}", module_path, error),
            node,
        )),
    }?;
    node.specifiers
        .iter()
        .fold(Ok(Vec::new()), |bindings, specifier| {
            let mut bindings = bindings?;
            let binding = match specifier {
                ImportSpecifier::Default(node) => {
                    let identifier = parse_identifier(node)?;
                    let value = get_static_field(module.clone(), "default", factory, allocator);
                    (identifier, value)
                }
                ImportSpecifier::Namespace(node) => {
                    let identifier = parse_identifier(node)?;
                    let value = module.clone();
                    (identifier, value)
                }
                ImportSpecifier::Normal(node) => {
                    let imported_field = parse_identifier(&node.imported)?;
                    let identifier = parse_identifier(&node.local)?;
                    let value =
                        get_static_field(module.clone(), imported_field, factory, allocator);
                    (identifier, value)
                }
            };
            bindings.push(binding);
            Ok(bindings)
        })
}

fn parse_block<'src: 'temp, 'temp, T: Expression>(
    body: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Option<T>> {
    parse_block_statements(body, None, scope, env, factory, allocator)
}

fn parse_block_statements<'src: 'temp, 'temp, T: Expression>(
    remaining: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    result: Option<T>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Option<T>> {
    let mut remaining = remaining.into_iter();
    let node = remaining.next();
    match node {
        None => Ok(result),
        Some(node) => {
            if result.is_some() {
                return Err(err_unreachable(node));
            }
            match node {
                ProgramPart::Dir(node) => {
                    let value = Expr::Lit(node.expr.clone());
                    let expression = parse_expression(&value, &scope, env, factory, allocator)?;
                    let result = Some(expression);
                    parse_block_statements(remaining, result, scope, env, factory, allocator)
                }
                ProgramPart::Decl(node) => match node {
                    Decl::Var(kind, declarators) => match kind {
                        VarKind::Const => {
                            let (initializers, child_scope) = parse_variable_declarators(
                                declarators,
                                &scope,
                                env,
                                factory,
                                allocator,
                            )?;
                            let body_scope = child_scope.as_ref().unwrap_or(scope);
                            let body = parse_block_statements(
                                remaining, result, body_scope, env, factory, allocator,
                            )?;
                            match body {
                                None => Ok(None),
                                Some(body) => {
                                    Ok(Some(create_declaration_block(initializers, body, factory)))
                                }
                            }
                        }
                        _ => Err(err_unimplemented(node)),
                    },
                    _ => Err(err_unimplemented(node)),
                },
                ProgramPart::Stmt(statement) => match statement {
                    Stmt::Expr(node) => Err(err("Unexpected expression statement", node)),
                    Stmt::Return(node) => match node {
                        None => Err(err("Missing return value", node)),
                        Some(node) => {
                            let expression =
                                parse_expression(node, &scope, env, factory, allocator)?;
                            let result = Some(expression);
                            parse_block_statements(
                                remaining, result, scope, env, factory, allocator,
                            )
                        }
                    },
                    Stmt::Throw(node) => {
                        let expression =
                            parse_throw_statement(node, &scope, env, factory, allocator)?;
                        let result = Some(expression);
                        parse_block_statements(remaining, result, scope, env, factory, allocator)
                    }
                    Stmt::If(node) => {
                        let condition =
                            parse_expression(&node.test, scope, env, factory, allocator)?;
                        let consequent =
                            parse_if_branch(&node.consequent, scope, env, factory, allocator)?;
                        match &node.alternate {
                            Some(node) => {
                                let alternate =
                                    parse_if_branch(&node, scope, env, factory, allocator)?;
                                let expression = create_if_expression(
                                    condition, consequent, alternate, factory, allocator,
                                );
                                let result = Some(expression);
                                parse_block_statements(
                                    remaining, result, scope, env, factory, allocator,
                                )
                            }
                            None => {
                                let alternate = parse_branch(
                                    &statement, remaining, scope, env, factory, allocator,
                                )?;
                                let result = create_if_expression(
                                    condition, consequent, alternate, factory, allocator,
                                );
                                Ok(Some(result))
                            }
                        }
                    }
                    Stmt::Try(_) => Err(err_unimplemented(statement)),
                    Stmt::Switch(_) => Err(err_unimplemented(statement)),
                    Stmt::Empty => {
                        parse_block_statements(remaining, result, scope, env, factory, allocator)
                    }
                    _ => Err(err_unimplemented(statement)),
                },
            }
        }
    }
}

fn create_declaration_block<T: Expression>(
    initializers: impl IntoIterator<Item = T, IntoIter = impl DoubleEndedIterator<Item = T>>,
    body: T,
    factory: &impl ExpressionFactory<T>,
) -> T {
    initializers
        .into_iter()
        .rev()
        .fold(body, |body, initializer| {
            factory.create_let_term(initializer, body)
        })
}

fn parse_variable_declarators<'src, T: Expression>(
    declarators: &[VarDecl<'src>],
    scope: &LexicalScope<'src>,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(
    impl IntoIterator<Item = T, IntoIter = impl DoubleEndedIterator<Item = T>>,
    Option<LexicalScope<'src>>,
)> {
    declarators
        .iter()
        .fold(Ok((Vec::new(), None)), |results, node| {
            let (mut results, existing_scope) = results?;
            let current_scope = existing_scope.as_ref().unwrap_or(scope);
            let (initializers, child_scope) =
                parse_variable_declarator(node, current_scope, env, factory, allocator)?;
            results.extend(initializers);
            Ok((results, child_scope.or(existing_scope)))
        })
}

fn parse_variable_declarator<'src, T: Expression>(
    node: &VarDecl<'src>,
    scope: &LexicalScope<'src>,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(impl IntoIterator<Item = T>, Option<LexicalScope<'src>>)> {
    let VarDecl { id, init } = node;
    let init = init
        .as_ref()
        .ok_or_else(|| err("Missing variable initializer", node))?;
    let value = parse_expression(init, scope, env, factory, allocator)?;
    match id {
        Pat::Ident(node) => {
            let identifier = parse_identifier(node)?;
            Ok((
                vec![value],
                Some(scope.create_child(once(Some(identifier)))),
            ))
        }
        Pat::Obj(properties) => {
            let (initializers, child_scope) = parse_object_destructuring_pattern_bindings(
                value, properties, scope, env, factory, allocator,
            )?;
            Ok((initializers.into_iter().collect::<Vec<_>>(), child_scope))
        }
        Pat::Array(accessors) => {
            let (initializers, child_scope) = parse_array_destructuring_pattern_bindings(
                value, accessors, scope, env, factory, allocator,
            )?;
            Ok((initializers.into_iter().collect::<Vec<_>>(), child_scope))
        }
        Pat::RestElement(_) => Err(err_unimplemented(node)),
        Pat::Assign(_) => Err(err_unimplemented(node)),
    }
}

fn parse_object_destructuring_pattern_bindings<'src, T: Expression>(
    target: T,
    properties: &[ObjPatPart<'src>],
    scope: &LexicalScope<'src>,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(Vec<T>, Option<LexicalScope<'src>>)> {
    let properties = properties
        .iter()
        .map(|property| match property {
            ObjPatPart::Assign(node) => {
                if node.computed {
                    Err(err_unimplemented(node))
                } else {
                    let identifier = parse_destructuring_pattern_property_identifier(node)?;
                    Ok((identifier, node))
                }
            }
            ObjPatPart::Rest(node) => Err(err_unimplemented(node)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match properties.len() {
        0 => Ok((Vec::new(), None)),
        1 => {
            let (identifier, node) = properties.into_iter().next().unwrap();
            let field_name = parse_destructuring_pattern_property_field_name(
                node, scope, env, factory, allocator,
            )?;
            let initializer = get_dynamic_field(target, field_name, factory, allocator);
            Ok((
                vec![initializer],
                Some(scope.create_child(once(Some(identifier)))),
            ))
        }
        _ => {
            let mut initializers = Vec::with_capacity(1 + properties.len());
            let initializer_scope = scope.create_child(once(None));
            initializers.push(target.clone());
            let initializer_depth = initializer_scope.depth();
            properties
                .into_iter()
                .fold(
                    Ok((initializers, scope.create_child(once(None)))),
                    |result, property| {
                        let (mut initializers, existing_scope) = result?;
                        let (identifier, node) = property;
                        let field_name = parse_destructuring_pattern_property_field_name(
                            node,
                            &existing_scope,
                            env,
                            factory,
                            allocator,
                        )?;
                        let scope_offset = existing_scope.depth() - initializer_depth;
                        let initializer = get_dynamic_field(
                            factory.create_static_variable_term(scope_offset),
                            field_name,
                            factory,
                            allocator,
                        );
                        initializers.push(initializer);
                        let next_scope = existing_scope.create_child(once(Some(identifier)));
                        Ok((initializers, next_scope))
                    },
                )
                .map(|(initializers, scope)| (initializers, Some(scope)))
        }
    }
}

fn parse_destructuring_pattern_property_field_name<'src, T: Expression>(
    node: &Prop<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match &node.key {
        PropKey::Lit(key) => match key {
            Lit::String(key) => {
                let field_name = parse_string(key);
                Ok(factory
                    .create_value_term(ValueTerm::String(allocator.create_string(field_name))))
            }
            _ => parse_literal(key, scope, env, factory, allocator),
        },
        PropKey::Pat(node) => match node {
            Pat::Ident(node) => {
                let field_name = parse_identifier(node)?;
                Ok(factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from(field_name)),
                )))
            }
            Pat::Obj(node) => Err(err_unimplemented(node)),
            Pat::Array(node) => Err(err_unimplemented(node)),
            Pat::RestElement(node) => Err(err_unimplemented(node)),
            Pat::Assign(node) => Err(err_unimplemented(node)),
        },
        PropKey::Expr(node) => Err(err_unimplemented(node)),
    }
}

fn parse_destructuring_pattern_property_identifier<'src>(
    node: &Prop<'src>,
) -> ParserResult<&'src str> {
    match &node.value {
        PropValue::Pat(node) => match node {
            Pat::Ident(node) => parse_identifier(node),
            Pat::Obj(_) => Err(err_unimplemented(node)),
            Pat::Array(_) => Err(err_unimplemented(node)),
            Pat::RestElement(_) => Err(err_unimplemented(node)),
            Pat::Assign(_) => Err(err_unimplemented(node)),
        },
        PropValue::Expr(node) => Err(err_unimplemented(node)),
        PropValue::None => match &node.key {
            PropKey::Pat(key) => match key {
                Pat::Ident(key) => parse_identifier(key),
                _ => Err(err_unimplemented(key)),
            },
            _ => Err(err_unimplemented(&node.key)),
        },
    }
}

fn parse_array_destructuring_pattern_bindings<'src, T: Expression>(
    target: T,
    accessors: &[Option<ArrayPatPart<'src>>],
    scope: &LexicalScope<'src>,
    _env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(impl IntoIterator<Item = T>, Option<LexicalScope<'src>>)> {
    let accessors = accessors
        .iter()
        .enumerate()
        .filter_map(|(index, accessor)| match accessor {
            Some(accessor) => Some((index, accessor)),
            None => None,
        })
        .map(|(index, accessor)| match accessor {
            ArrayPatPart::Pat(pattern) => match pattern {
                Pat::Ident(identifier) => {
                    let identifier = parse_identifier(identifier)?;
                    Ok((identifier, index))
                }
                Pat::RestElement(_) => Err(err_unimplemented(pattern)),
                _ => Err(err_unimplemented(pattern)),
            },
            ArrayPatPart::Expr(node) => Err(err_unimplemented(node)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match accessors.len() {
        0 => Ok((Vec::new(), None)),
        1 => {
            let (identifier, index) = accessors.into_iter().next().unwrap();
            let initializer = get_indexed_field(target, index, factory, allocator);
            Ok((
                vec![initializer],
                Some(scope.create_child(once(Some(identifier)))),
            ))
        }
        _ => {
            let mut initializers = Vec::with_capacity(1 + accessors.len());
            let initializer_scope = scope.create_child(once(None));
            initializers.push(target.clone());
            let initializer_depth = initializer_scope.depth();
            accessors
                .into_iter()
                .fold(Ok((initializers, initializer_scope)), |result, property| {
                    let (mut initializers, existing_scope) = result?;
                    let (identifier, index) = property;
                    let scope_offset = existing_scope.depth() - initializer_depth;
                    let initializer = get_indexed_field(
                        factory.create_static_variable_term(scope_offset),
                        index,
                        factory,
                        allocator,
                    );
                    initializers.push(initializer);
                    let next_scope = existing_scope.create_child(once(Some(identifier)));
                    Ok((initializers, next_scope))
                })
                .map(|(initializers, scope)| (initializers, Some(scope)))
        }
    }
}

fn parse_identifier<'src>(node: &Ident<'src>) -> ParserResult<&'src str> {
    match node.name {
        Cow::Borrowed(value) => Ok(value),
        Cow::Owned(_) => Err(err("Invalid identifier", node)),
    }
}

fn parse_throw_statement<'src, T: Expression>(
    value: &Expr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match value {
        Expr::New(constructor) => match &*constructor.callee {
            Expr::Ident(node) => match parse_identifier(&node)? {
                "Error" => {
                    if constructor.arguments.is_empty() {
                        Err(err_unimplemented(node))
                    } else {
                        let message = constructor
                            .arguments
                            .iter()
                            .map(|arg| parse_expression(arg, scope, env, factory, allocator))
                            .next()
                            .unwrap()?;
                        Ok(factory.create_application_term(
                            factory.create_native_function_term(throw()),
                            allocator.create_unit_list(message),
                        ))
                    }
                }
                _ => Err(err_unimplemented(node)),
            },
            _ => Err(err_unimplemented(value)),
        },
        _ => Err(err_unimplemented(value)),
    }
}

fn parse_if_branch<'src, T: Expression>(
    node: &Stmt<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node {
        Stmt::Block(block) => {
            let BlockStmt(body) = block;
            parse_branch(node, body, scope, env, factory, allocator)
        }
        _ => parse_branch(
            node,
            &vec![ProgramPart::Stmt(node.clone())],
            scope,
            env,
            factory,
            allocator,
        ),
    }
}

fn parse_branch<'src: 'temp, 'temp, T: Expression>(
    node: &Stmt<'src>,
    body: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let expression = parse_block(body, scope, env, factory, allocator)?;
    match expression {
        None => Err(err("Unterminated branch", node)),
        Some(expression) => Ok(expression),
    }
}

fn parse_expression<'src, T: Expression>(
    node: &Expr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node {
        Expr::Ident(node) => parse_variable_reference(node, scope, env, factory),
        Expr::Lit(node) => parse_literal(node, scope, env, factory, allocator),
        Expr::TaggedTemplate(node) => parse_tagged_template(node, scope, env, factory, allocator),
        Expr::Unary(node) => parse_unary_expression(node, scope, env, factory, allocator),
        Expr::Binary(node) => parse_binary_expression(node, scope, env, factory, allocator),
        Expr::Logical(node) => parse_logical_expression(node, scope, env, factory, allocator),
        Expr::Conditional(node) => {
            parse_conditional_expression(node, scope, env, factory, allocator)
        }
        Expr::ArrowFunc(node) => {
            parse_arrow_function_expression(node, scope, env, factory, allocator)
        }
        Expr::Member(node) => parse_member_expression(node, scope, env, factory, allocator),
        Expr::Call(node) => parse_call_expression(node, scope, env, factory, allocator),
        Expr::New(node) => parse_constructor_expression(node, scope, env, factory, allocator),
        Expr::Obj(node) => parse_object_literal(node, scope, env, factory, allocator),
        Expr::Array(node) => parse_array_literal(node, scope, env, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_expressions<'src: 'temp, 'temp, T: Expression>(
    expressions: impl IntoIterator<Item = &'temp Expr<'src>>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Vec<T>> {
    expressions
        .into_iter()
        .map(|node| parse_expression(node, scope, env, factory, allocator))
        .collect::<Result<Vec<_>, _>>()
}

fn parse_variable_reference<'src, T: Expression>(
    node: &Ident<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    let name = parse_identifier(node)?;
    let offset = scope.get(name);
    match offset {
        Some(offset) => Ok(factory.create_static_variable_term(offset)),
        None => match env.global(name) {
            Some(value) => Ok(value),
            None => Err(err(&format!("Invalid reference: '{}'", name), node)),
        },
    }
}

fn parse_literal<'src, T: Expression>(
    node: &Lit<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node {
        Lit::Null => parse_null_literal(factory),
        Lit::Boolean(node) => parse_boolean_literal(node, factory),
        Lit::Number(node) => parse_number_literal(node, factory),
        Lit::String(node) => parse_string_literal(node, factory, allocator),
        Lit::Template(node) => parse_template_literal(node, scope, env, factory, allocator),
        Lit::RegEx(_) => Err(err_unimplemented(node)),
    }
}

fn parse_null_literal<T: Expression>(factory: &impl ExpressionFactory<T>) -> ParserResult<T> {
    Ok(factory.create_value_term(ValueTerm::Null))
}

fn parse_boolean_literal<T: Expression>(
    node: &bool,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    Ok(factory.create_value_term(ValueTerm::Boolean(*node)))
}

fn parse_number_literal<T: Expression>(
    node: &Cow<str>,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    match parse_number(node) {
        Ok(value) => Ok(factory.create_value_term(ValueTerm::Float(value))),
        Err(error) => Err(error),
    }
}

fn parse_number(node: &Cow<str>) -> ParserResult<f64> {
    match node.parse::<f64>() {
        Ok(value) => Ok(value),
        Err(_) => Err(err("Invalid number literal", node)),
    }
}

fn parse_string_literal<T: Expression>(
    node: &StringLit,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let value = parse_string(node);
    Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(value))))
}

fn parse_string(node: &StringLit) -> String {
    let value = match node {
        StringLit::Double(value) => value,
        StringLit::Single(value) => value,
    };
    match value {
        Cow::Borrowed(value) => parse_escaped_string(value),
        Cow::Owned(value) => parse_escaped_string(value),
    }
}

fn parse_escaped_string(value: &str) -> String {
    value
        .chars()
        .fold(
            (String::with_capacity(value.len()), false),
            |(mut result, is_escape), current| {
                if current == '\\' && !is_escape {
                    (result, true)
                } else {
                    result.push(current);
                    (result, false)
                }
            },
        )
        .0
}

fn parse_template_literal<'src, T: Expression>(
    node: &TemplateLit<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let args = node
        .quasis
        .iter()
        .map(|quasi| match parse_template_element(quasi)? {
            "" => Ok(None),
            value => Ok(Some(factory.create_value_term(ValueTerm::String(
                allocator.create_string(value.into()),
            )))),
        })
        .zip(
            node.expressions
                .iter()
                .map(|expression| {
                    let value = parse_expression(expression, scope, env, factory, allocator)?;
                    Ok(Some(factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(value),
                    )))
                })
                .chain(once(Ok(None))),
        )
        .flat_map(|(quasi, expression)| once(quasi).chain(once(expression)))
        .filter_map(|arg| match arg {
            Err(error) => Some(Err(error)),
            Ok(Some(arg)) => Some(Ok(arg)),
            Ok(None) => None,
        })
        .collect::<ParserResult<Vec<_>>>()?;
    Ok(match args.len() {
        0 => {
            factory.create_value_term(ValueTerm::String(allocator.create_string(String::from(""))))
        }
        1 => args.into_iter().next().unwrap(),
        _ => factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Concat),
            allocator.create_list(args),
        ),
    })
}

fn parse_template_element<'src>(node: &TemplateElement<'src>) -> ParserResult<&'src str> {
    match node.cooked {
        Cow::Borrowed(value) => Ok(value),
        Cow::Owned(_) => Err(err("Invalid template string", node)),
    }
}

fn parse_tagged_template<'src, T: Expression>(
    node: &TaggedTemplateExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    parse_template_literal(&node.quasi, scope, env, factory, allocator)
}

fn parse_object_literal<'src, T: Expression>(
    node: &ObjExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    enum ObjectLiteralFields<T> {
        Properties(Vec<(String, T)>),
        Spread(T),
    }
    let elements = node
        .iter()
        .fold(Ok(Vec::with_capacity(node.len())), |results, node| {
            let mut elements = results?;
            match node {
                ObjProp::Prop(prop) => match prop.kind {
                    PropKind::Init => {
                        let key = match &prop.key {
                            PropKey::Lit(key) => match key {
                                Lit::String(key) => Ok(parse_string(key)),
                                Lit::Null => Ok(String::from("null")),
                                Lit::Boolean(value) => Ok(format!("{}", value)),
                                Lit::Number(key) => {
                                    let key = parse_number(key)?;
                                    match as_integer(key) {
                                        Some(key) => Ok(format!("{}", key)),
                                        _ => Ok(format!("{}", key)),
                                    }
                                }
                                _ => Err(err_unimplemented(key)),
                            },
                            PropKey::Expr(key) => match key {
                                Expr::Ident(key) if !prop.computed => {
                                    let field_name = parse_identifier(key)?;
                                    Ok(String::from(field_name))
                                }
                                _ => {
                                    let dynamic_key =
                                        parse_expression(key, scope, env, factory, allocator)?;
                                    match factory.match_value_term(&dynamic_key) {
                                        Some(ValueTerm::String(value)) => {
                                            Ok(String::from(value.as_str()))
                                        }
                                        Some(ValueTerm::Null) => Ok(format!("{}", dynamic_key)),
                                        Some(ValueTerm::Boolean(_)) => {
                                            Ok(format!("{}", dynamic_key))
                                        }
                                        Some(ValueTerm::Int(_)) => Ok(format!("{}", dynamic_key)),
                                        Some(ValueTerm::Float(value)) => {
                                            Ok(if let Some(value) = as_integer(*value) {
                                                format!("{}", value)
                                            } else {
                                                format!("{}", dynamic_key)
                                            })
                                        }
                                        _ => Err(err_unimplemented(dynamic_key)),
                                    }
                                }
                            },
                            PropKey::Pat(key) => match key {
                                Pat::Ident(key) if !prop.computed => {
                                    let field_name = parse_identifier(key)?;
                                    Ok(String::from(field_name))
                                }
                                _ => Err(err_unimplemented(node)),
                            },
                        }?;
                        let value = match &prop.value {
                            PropValue::Expr(node) => {
                                parse_expression(node, scope, env, factory, allocator)
                            }
                            PropValue::None => match prop.short_hand {
                                true => match &prop.key {
                                    PropKey::Pat(node) => match node {
                                        Pat::Ident(node) => {
                                            parse_variable_reference(&node, scope, env, factory)
                                        }
                                        _ => Err(err_unimplemented(node)),
                                    },
                                    _ => Err(err_unimplemented(prop)),
                                },
                                _ => Err(err_unimplemented(prop)),
                            },
                            PropValue::Pat(node) => Err(err_unimplemented(node)),
                        }?;
                        if let Some(ObjectLiteralFields::Properties(_)) = elements.last() {
                            match elements.pop() {
                                Some(ObjectLiteralFields::Properties(mut properties)) => {
                                    properties.push((key, value));
                                    elements.push(ObjectLiteralFields::Properties(properties));
                                }
                                _ => {}
                            }
                        } else {
                            elements.push(ObjectLiteralFields::Properties(vec![(key, value)]))
                        }
                        Ok(elements)
                    }
                    PropKind::Method => Err(err_unimplemented(prop)),
                    PropKind::Ctor => Err(err_unimplemented(prop)),
                    PropKind::Get => Err(err_unimplemented(prop)),
                    PropKind::Set => Err(err_unimplemented(prop)),
                },
                ObjProp::Spread(Expr::Spread(node)) => {
                    let value = parse_expression(node, scope, env, factory, allocator)?;
                    elements.push(ObjectLiteralFields::Spread(value));
                    Ok(elements)
                }
                _ => Err(err_unimplemented(node)),
            }
        })?;
    let field_sets = elements.into_iter().map(|properties| match properties {
        ObjectLiteralFields::Spread(value) => value,
        ObjectLiteralFields::Properties(properties) => {
            let (keys, values): (Vec<_>, Vec<_>) = properties.into_iter().unzip();
            factory.create_struct_term(
                allocator.create_struct_prototype(keys),
                allocator.create_list(values),
            )
        }
    });
    Ok(if field_sets.len() >= 2 {
        factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Merge),
            allocator.create_list(field_sets),
        )
    } else {
        match field_sets.into_iter().next() {
            Some(value) => value,
            None => factory.create_struct_term(
                allocator.create_struct_prototype(empty()),
                allocator.create_empty_list(),
            ),
        }
    })
}

fn parse_array_literal<'src, T: Expression>(
    node: &ArrayExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    enum ArrayLiteralFields<T> {
        Items(Vec<T>),
        Spread(T),
    }
    let elements = node
        .iter()
        .fold(Ok(Vec::with_capacity(node.len())), |results, node| {
            let mut elements = results?;
            match node {
                None => Err(err("Missing array item", node)),
                Some(node) => match node {
                    Expr::Spread(node) => {
                        let value = parse_expression(node, scope, env, factory, allocator)?;
                        elements.push(ArrayLiteralFields::Spread(value));
                        Ok(elements)
                    }
                    _ => match parse_expression(node, scope, env, factory, allocator) {
                        Err(error) => Err(error),
                        Ok(value) => {
                            if let Some(ArrayLiteralFields::Items(_)) = elements.last() {
                                match elements.pop() {
                                    Some(ArrayLiteralFields::Items(mut items)) => {
                                        items.push(value);
                                        elements.push(ArrayLiteralFields::Items(items));
                                    }
                                    _ => {}
                                }
                            } else {
                                elements.push(ArrayLiteralFields::Items(vec![value]))
                            }
                            Ok(elements)
                        }
                    },
                },
            }
        })?;
    if elements.len() == 2 {
        let left = elements.first().unwrap();
        let right = elements.last().unwrap();
        match (left, right) {
            (ArrayLiteralFields::Spread(target), ArrayLiteralFields::Items(items))
                if items.len() == 1 =>
            {
                return Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Push),
                    allocator
                        .create_pair(target.clone(), items.into_iter().next().cloned().unwrap()),
                ))
            }
            (ArrayLiteralFields::Items(items), ArrayLiteralFields::Spread(target))
                if items.len() == 1 =>
            {
                return Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::PushFront),
                    allocator
                        .create_pair(target.clone(), items.into_iter().next().cloned().unwrap()),
                ))
            }
            _ => {}
        }
    }

    let item_sets = elements.into_iter().map(|properties| match properties {
        ArrayLiteralFields::Spread(value) => value,
        ArrayLiteralFields::Items(items) => {
            factory.create_vector_term(allocator.create_list(items))
        }
    });
    Ok(if item_sets.len() >= 2 {
        factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Append),
            allocator.create_list(item_sets),
        )
    } else {
        match item_sets.into_iter().next() {
            Some(value) => value,
            None => factory.create_vector_term(allocator.create_empty_list()),
        }
    })
}

fn parse_unary_expression<'src, T: Expression>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node.operator {
        UnaryOp::Minus => parse_unary_minus_expression(node, scope, env, factory, allocator),
        UnaryOp::Plus => parse_unary_plus_expression(node, scope, env, factory, allocator),
        UnaryOp::Not => parse_unary_not_expression(node, scope, env, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_binary_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node.operator {
        BinaryOp::Plus => parse_binary_add_expression(node, scope, env, factory, allocator),
        BinaryOp::Minus => parse_binary_subtract_expression(node, scope, env, factory, allocator),
        BinaryOp::Times => parse_binary_multiply_expression(node, scope, env, factory, allocator),
        BinaryOp::Over => parse_binary_divide_expression(node, scope, env, factory, allocator),
        BinaryOp::Mod => parse_binary_remainder_expression(node, scope, env, factory, allocator),
        BinaryOp::PowerOf => parse_binary_pow_expression(node, scope, env, factory, allocator),
        BinaryOp::LessThan => parse_binary_lt_expression(node, scope, env, factory, allocator),
        BinaryOp::GreaterThan => parse_binary_gt_expression(node, scope, env, factory, allocator),
        BinaryOp::LessThanEqual => {
            parse_binary_lte_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::GreaterThanEqual => {
            parse_binary_gte_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::Equal => parse_binary_equal_expression(node, scope, env, factory, allocator),
        BinaryOp::StrictEqual => {
            parse_binary_equal_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::NotEqual => {
            parse_binary_not_equal_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::StrictNotEqual => {
            parse_binary_not_equal_expression(node, scope, env, factory, allocator)
        }
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_logical_expression<'src, T: Expression>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node.operator {
        LogicalOp::And => parse_logical_and_expression(node, scope, env, factory, allocator),
        LogicalOp::Or => parse_logical_or_expression(node, scope, env, factory, allocator),
    }
}

fn parse_unary_minus_expression<'src, T: Expression>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let operand = parse_expression(&node.argument, scope, env, factory, allocator)?;
    Ok(match factory.match_value_term(&operand) {
        Some(ValueTerm::Int(value)) => factory.create_value_term(ValueTerm::Int(-*value)),
        Some(ValueTerm::Float(value)) => factory.create_value_term(ValueTerm::Float(-*value)),
        _ => factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Subtract),
            allocator.create_pair(factory.create_value_term(ValueTerm::Float(0.0)), operand),
        ),
    })
}

fn parse_unary_plus_expression<'src, T: Expression>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let operand = parse_expression(&node.argument, scope, env, factory, allocator)?;
    Ok(match factory.match_value_term(&operand) {
        Some(ValueTerm::Int(_)) => operand,
        Some(ValueTerm::Float(_)) => operand,
        _ => factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Add),
            allocator.create_pair(factory.create_value_term(ValueTerm::Float(0.0)), operand),
        ),
    })
}

fn parse_unary_not_expression<'src, T: Expression>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let operand = parse_expression(&node.argument, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Not),
        allocator.create_unit_list(operand),
    ))
}

fn parse_binary_add_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Add),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_subtract_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Subtract),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_multiply_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Multiply),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_divide_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Divide),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_remainder_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Remainder),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_pow_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Pow),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_lt_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Lt),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_gt_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Gt),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_lte_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Lte),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_gte_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Gte),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_equal_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Eq),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_not_equal_expression<'src, T: Expression>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let expression = parse_binary_equal_expression(node, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Not),
        allocator.create_unit_list(expression),
    ))
}

fn parse_logical_and_expression<'src, T: Expression>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::And),
        allocator.create_pair(left, right),
    ))
}

fn parse_logical_or_expression<'src, T: Expression>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Or),
        allocator.create_pair(left, right),
    ))
}

fn parse_conditional_expression<'src, T: Expression>(
    node: &ConditionalExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let condition = parse_expression(&node.test, scope, env, factory, allocator)?;
    let consequent = parse_expression(&node.consequent, scope, env, factory, allocator)?;
    let alternate = parse_expression(&node.alternate, scope, env, factory, allocator)?;
    Ok(create_if_expression(
        condition, consequent, alternate, factory, allocator,
    ))
}

fn create_if_expression<T: Expression>(
    condition: T,
    consequent: T,
    alternate: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::If),
        allocator.create_triple(condition, consequent, alternate),
    )
}

fn parse_arrow_function_expression<'src, T: Expression>(
    node: &ArrowFuncExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    if node.generator || node.is_async {
        Err(err_unimplemented(node))
    } else {
        let num_args = node.params.len();
        let arg_names = node
            .params
            .iter()
            .map(|node| match node {
                FuncArg::Pat(node) => match node {
                    Pat::Ident(node) => parse_identifier(node).map(Some),
                    _ => Ok(None),
                },
                _ => Ok(None),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let inner_scope = scope.create_child(arg_names);
        let inner_depth = inner_scope.depth();
        let (initializers, body_scope) = node.params.iter().enumerate().fold(
            Ok((Vec::new(), inner_scope)),
            |result, (arg_index, node)| {
                let (mut combined_initializers, existing_scope) = result?;
                match node {
                    FuncArg::Pat(node) => match node {
                        Pat::Ident(_) => Ok((combined_initializers, existing_scope)),
                        Pat::Obj(properties) => {
                            let scope_offset = existing_scope.depth() - inner_depth;
                            let arg = factory.create_static_variable_term(
                                num_args - arg_index - 1 + scope_offset,
                            );
                            let (initializers, child_scope) =
                                parse_object_destructuring_pattern_bindings(
                                    arg,
                                    properties,
                                    &existing_scope,
                                    env,
                                    factory,
                                    allocator,
                                )?;
                            let next_scope = child_scope.unwrap_or(existing_scope);
                            combined_initializers.extend(initializers);
                            Ok((combined_initializers, next_scope))
                        }
                        Pat::Array(accessors) => {
                            let scope_offset = existing_scope.depth() - inner_depth;
                            let arg = factory.create_static_variable_term(
                                num_args - arg_index - 1 + scope_offset,
                            );
                            let (initializers, child_scope) =
                                parse_array_destructuring_pattern_bindings(
                                    arg,
                                    accessors,
                                    &existing_scope,
                                    env,
                                    factory,
                                    allocator,
                                )?;
                            let next_scope = child_scope.unwrap_or(existing_scope);
                            combined_initializers.extend(initializers);
                            Ok((combined_initializers, next_scope))
                        }
                        Pat::RestElement(_) => Err(err_unimplemented(node)),
                        Pat::Assign(_) => Err(err_unimplemented(node)),
                    },
                    _ => Err(err_unimplemented(node)),
                }
            },
        )?;
        let body = match &node.body {
            ArrowFuncBody::Expr(node) => {
                let body = &vec![ProgramPart::Stmt(Stmt::Return(Some(*node.clone())))];
                parse_block(body, &body_scope, env, factory, allocator)
            }
            ArrowFuncBody::FuncBody(node) => {
                let FuncBody(body) = node;
                parse_block(body, &body_scope, env, factory, allocator)
            }
        }?;
        match body {
            None => Err(err("Missing function return statement", node)),
            Some(body) => Ok(factory.create_lambda_term(
                num_args,
                initializers
                    .into_iter()
                    .rev()
                    .fold(body, |body, initializer| {
                        factory.create_let_term(initializer, body)
                    }),
            )),
        }
    }
}

fn parse_member_expression<'src, T: Expression>(
    node: &MemberExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let target = parse_expression(&node.object, scope, env, factory, allocator)?;
    let field_name = parse_static_member_field_name(node)?;
    match field_name {
        Some(field_name) => Ok(get_static_field(target, &field_name, factory, allocator)),
        None => {
            let field = parse_expression(&node.property, scope, env, factory, allocator)?;
            Ok(get_dynamic_field(target, field, factory, allocator))
        }
    }
}

fn parse_static_member_field_name(node: &MemberExpr) -> ParserResult<Option<String>> {
    Ok(match &*node.property {
        Expr::Ident(name) => Some(String::from(parse_identifier(&name)?)),
        Expr::Lit(name) => match name {
            Lit::String(name) => Some(parse_string(&name)),
            _ => None,
        },
        _ => None,
    })
}

fn get_static_field<T: Expression>(
    target: T,
    field: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let field = factory.create_value_term(ValueTerm::String(allocator.create_string(field.into())));
    get_dynamic_field(target, field, factory, allocator)
}

fn get_dynamic_field<T: Expression>(
    target: T,
    field: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Get),
        allocator.create_pair(target, field),
    )
}

fn get_indexed_field<T: Expression>(
    target: T,
    index: usize,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    get_dynamic_field(
        target,
        factory.create_value_term(ValueTerm::Int(index as i32)),
        factory,
        allocator,
    )
}

fn parse_call_expression<'src, T: Expression>(
    node: &CallExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let static_dispatch = match &*node.callee {
        Expr::Member(callee) => {
            let method_name = parse_static_member_field_name(callee)?;
            match method_name {
                Some(method_name) => Some(parse_static_method_call_expression(
                    &callee.object,
                    &method_name,
                    node.arguments.iter(),
                    scope,
                    env,
                    factory,
                    allocator,
                )?),
                None => None,
            }
        }
        _ => None,
    };
    match static_dispatch {
        Some(expression) => Ok(expression),
        None => {
            let callee = parse_expression(&node.callee, scope, env, factory, allocator)?;
            parse_function_application_expression(
                callee,
                node.arguments.iter(),
                scope,
                env,
                factory,
                allocator,
            )
        }
    }
}

fn parse_static_method_call_expression<'src: 'temp, 'temp, T: Expression>(
    target: &Expr<'src>,
    method_name: &'src str,
    args: impl IntoIterator<Item = &'temp Expr<'src>> + ExactSizeIterator,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let target = parse_expression(target, scope, env, factory, allocator)?;
    let is_potential_builtin_method = get_builtin_field(None, method_name, factory).is_some();
    if is_potential_builtin_method {
        let method = factory.create_value_term(ValueTerm::String(
            allocator.create_string(method_name.into()),
        ));
        let num_args = args.len();
        let args = args.into_iter().collect::<Vec<_>>();
        let method_args = parse_expressions(args.iter().cloned(), scope, env, factory, allocator)?;
        let dynamic_fallback = parse_function_application_expression(
            get_static_field(target.clone(), method_name, factory, allocator),
            args.iter().cloned(),
            scope,
            env,
            factory,
            allocator,
        )?;
        let mut combined_args = Vec::with_capacity(3 + num_args);
        combined_args.push(target);
        combined_args.push(method);
        combined_args.push(dynamic_fallback);
        combined_args.extend(method_args);
        Ok(factory.create_application_term(
            factory.create_native_function_term(dispatch()),
            allocator.create_list(combined_args),
        ))
    } else {
        parse_function_application_expression(
            get_static_field(target.clone(), method_name, factory, allocator),
            args,
            scope,
            env,
            factory,
            allocator,
        )
    }
}

fn parse_function_application_expression<'src: 'temp, 'temp, T: Expression>(
    target: T,
    args: impl IntoIterator<Item = &'temp Expr<'src>> + ExactSizeIterator,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let num_args = args.len();
    let (args, spread) = args.into_iter().fold(
        (Vec::with_capacity(num_args), None),
        |(mut args, spread), node| match node {
            Expr::Spread(node) => (args, Some(node)),
            _ => {
                args.push(node);
                (args, spread)
            }
        },
    );
    let args = parse_expressions(args, scope, env, factory, allocator)?;
    let spread = match spread {
        Some(spread) => parse_expression(spread, scope, env, factory, allocator).map(Some),
        None => Ok(None),
    }?;
    if let Some(spread) = spread {
        let target = if args.is_empty() {
            target
        } else {
            factory.create_partial_application_term(target, allocator.create_list(args))
        };
        Ok(factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Apply),
            allocator.create_pair(target, spread),
        ))
    } else {
        Ok(factory.create_application_term(target, allocator.create_list(args)))
    }
}

fn parse_constructor_expression<'src, T: Expression>(
    node: &NewExpr<'src>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let target = parse_expression(&node.callee, scope, env, factory, allocator);
    let args = node
        .arguments
        .iter()
        .map(|arg| parse_expression(arg, scope, env, factory, allocator));
    Ok(factory.create_application_term(
        factory.create_native_function_term(construct()),
        allocator.create_list(once(target).chain(args).collect::<ParserResult<Vec<_>>>()?),
    ))
}

#[cfg(test)]
mod tests {
    use std::{iter::empty, path::Path};

    use super::{parse, parse_module};
    use crate::{
        builtin_plugins,
        builtins::{dispatch, throw, to_string},
        static_module_loader,
        stdlib::builtin_imports,
        Env,
    };
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        compiler::{Compiler, CompilerMode, CompilerOptions, InstructionPointer},
        core::{
            evaluate, DependencyList, DynamicState, EvaluationResult, ExpressionFactory,
            HeapAllocator,
        },
        interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
        lang::{BuiltinTerm, TermFactory, ValueTerm},
    };

    #[test]
    fn null_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("null", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Null)),
        );
    }

    #[test]
    fn boolean_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Boolean(true))),
        );
        assert_eq!(
            parse("false", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Boolean(false))),
        );
    }

    #[test]
    fn string_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("''", &env, &factory, &allocator),
            Ok(factory
                .create_value_term(ValueTerm::String(allocator.create_string(String::from(""))))),
        );
        assert_eq!(
            parse("\"\"", &env, &factory, &allocator),
            Ok(factory
                .create_value_term(ValueTerm::String(allocator.create_string(String::from(""))))),
        );
        assert_eq!(
            parse("'foo'", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("foo"))
            ))),
        );
        assert_eq!(
            parse("\"foo\"", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("foo"))
            ))),
        );
        assert_eq!(
            parse("'\"'", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("\""))
            ))),
        );
        assert_eq!(
            parse("'\\\"'", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("\""))
            ))),
        );
        assert_eq!(
            parse("\"\\\"\"", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("\""))
            ))),
        );
    }

    #[test]
    fn numeric_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(0.0))),
        );
        assert_eq!(
            parse("3", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.0))),
        );
        assert_eq!(
            parse("0.0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(0.0))),
        );
        assert_eq!(
            parse("3.142", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.142))),
        );
        assert_eq!(
            parse("0.000", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(0.0))),
        );
        assert_eq!(
            parse("-0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(-0.0))),
        );
        assert_eq!(
            parse("-3", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(-3.0))),
        );
        assert_eq!(
            parse("-0.0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(-0.0))),
        );
        assert_eq!(
            parse("-3.142", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(-3.142))),
        );
        assert_eq!(
            parse("+0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(0.0))),
        );
        assert_eq!(
            parse("+3", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.0))),
        );
        assert_eq!(
            parse("+0.0", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(0.0))),
        );
        assert_eq!(
            parse("+3.142", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.142))),
        );
    }

    #[test]
    fn template_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("``", &env, &factory, &allocator),
            Ok(factory
                .create_value_term(ValueTerm::String(allocator.create_string(String::from(""))))),
        );
        assert_eq!(
            parse("`foo`", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("foo"))
            ))),
        );
        assert_eq!(
            parse("`\"`", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("\""))
            ))),
        );
        assert_eq!(
            parse("`\\\"`", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(String::from("\\\""))
            ))),
        );
        assert_eq!(
            parse("`${'foo'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_native_function_term(to_string()),
                allocator.create_list(allocator.create_unit_list(factory.create_value_term(
                    ValueTerm::String(allocator.create_string(String::from("foo")))
                ))),
            )),
        );
        assert_eq!(
            parse("`foo${'bar'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("bar"))
                        ))),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}bar`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        ))),
                    ),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        ))),
                    ),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("bar"))
                        ))),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`foo${'bar'}baz`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("bar"))
                        ))),
                    ),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}bar${'baz'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        ))),
                    ),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("baz"))
                        ))),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}${'baz'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        ))),
                    ),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("bar"))
                        ))),
                    ),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("baz"))
                        ))),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "`foo${'one'}bar${'two'}baz${'three'}`",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Concat),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("one"))
                        ))),
                    ),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        ))),
                    ),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                    factory.create_application_term(
                        factory.create_native_function_term(to_string()),
                        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        ))),
                    ),
                ]),
            ))
        )
    }

    #[test]
    fn object_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("({})", &env, &factory, &allocator),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(empty()),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("({ foo: 3, bar: 4, baz: 5 })", &env, &factory, &allocator),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("foo")),
                    allocator.create_string(String::from("bar")),
                    allocator.create_string(String::from("baz")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ foo: 3, \"bar\": 4, baz: 5 })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("foo")),
                    allocator.create_string(String::from("bar")),
                    allocator.create_string(String::from("baz")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ foo: 3, [\"bar\"]: 4, baz: 5 })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("foo")),
                    allocator.create_string(String::from("bar")),
                    allocator.create_string(String::from("baz")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", 4: \"bar\", 5: \"baz\" })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("3")),
                    allocator.create_string(String::from("4")),
                    allocator.create_string(String::from("5")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", [4]: \"bar\", 5: \"baz\" })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("3")),
                    allocator.create_string(String::from("4")),
                    allocator.create_string(String::from("5")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", \"4\": \"bar\", 5: \"baz\" })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("3")),
                    allocator.create_string(String::from("4")),
                    allocator.create_string(String::from("5")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", [\"4\"]: \"bar\", 5: \"baz\" })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("3")),
                    allocator.create_string(String::from("4")),
                    allocator.create_string(String::from("5")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "({ 1.1: \"foo\", [1.2]: \"bar\", 1.3: \"baz\" })",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_struct_term(
                allocator.create_struct_prototype(vec![
                    allocator.create_string(String::from("1.1")),
                    allocator.create_string(String::from("1.2")),
                    allocator.create_string(String::from("1.3")),
                ]),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("foo"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("bar"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("baz"))
                    )),
                ]),
            )),
        );
    }

    #[test]
    fn object_spread() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let expression = parse(
            "({ ...({}), ...({ first: 1, second: 2 }), third: 3, fourth: 4, ...({ fifth: 5 }), first: 6, third: 7 })",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let query = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::CollectVector),
            allocator.create_list(vec![
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("first")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("second")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("third")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("fourth")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("fifth")),
                        )),
                    ),
                ),
            ]),
        );
        let result = evaluate(
            &query,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(6.0)),
                    factory.create_value_term(ValueTerm::Float(2.0)),
                    factory.create_value_term(ValueTerm::Float(7.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ]))),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "const foo = { first: 1, second: 2 }; ({ ...foo, third: 3 })",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let query = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::CollectVector),
            allocator.create_list(vec![
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("first")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("second")),
                        )),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("third")),
                        )),
                    ),
                ),
            ]),
        );
        let result = evaluate(
            &query,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(1.0)),
                    factory.create_value_term(ValueTerm::Float(2.0)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn array_literals() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("[]", &env, &factory, &allocator),
            Ok(factory.create_vector_term(allocator.create_empty_list())),
        );
        assert_eq!(
            parse("[3, 4, 5]", &env, &factory, &allocator),
            Ok(factory.create_vector_term(allocator.create_list(vec![
                factory.create_value_term(ValueTerm::Float(3.0)),
                factory.create_value_term(ValueTerm::Float(4.0)),
                factory.create_value_term(ValueTerm::Float(5.0)),
            ]))),
        );
    }

    #[test]
    fn array_access() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let expression = parse(
            "const items = [3, 4, 5]; items[1]",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(4.0)),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn array_spread() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("[...[3, 4, 5], 6]", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Push),
                allocator.create_pair(
                    factory.create_vector_term(allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ])),
                    factory.create_value_term(ValueTerm::Float(6.0)),
                ),
            ))
        );
        assert_eq!(
            parse("[3, ...[4, 5, 6]]", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::PushFront),
                allocator.create_pair(
                    factory.create_vector_term(allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                        factory.create_value_term(ValueTerm::Float(6.0)),
                    ])),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                ),
            ))
        );
        let expression = parse(
            "[ ...[], ...[1, 2], 3, 4, ...[5], 6, 7 ]",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let query = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Collect),
            allocator.create_unit_list(expression),
        );
        let result = evaluate(
            &query,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(1.0)),
                    factory.create_value_term(ValueTerm::Float(2.0)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                    factory.create_value_term(ValueTerm::Float(6.0)),
                    factory.create_value_term(ValueTerm::Float(7.0)),
                ])),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "const foo = [1, 2]; [...foo, 3]",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let query = factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Collect),
            allocator.create_unit_list(expression),
        );
        let result = evaluate(
            &query,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(1.0)),
                    factory.create_value_term(ValueTerm::Float(2.0)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn array_methods() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse(
                "[3, 4, 5].map((value) => value * 2)",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_native_function_term(dispatch()),
                allocator.create_list(vec![
                    factory.create_vector_term(allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ])),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("map"))
                    )),
                    factory.create_application_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_vector_term(allocator.create_list(vec![
                                    factory.create_value_term(ValueTerm::Float(3.0)),
                                    factory.create_value_term(ValueTerm::Float(4.0)),
                                    factory.create_value_term(ValueTerm::Float(5.0)),
                                ])),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("map"))
                                )),
                            ),
                        ),
                        allocator.create_unit_list(factory.create_lambda_term(
                            1,
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Multiply),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::Float(2.0)),
                                ),
                            ),
                        ),),
                    ),
                    factory.create_lambda_term(
                        1,
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Multiply),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Float(2.0)),
                            ),
                        ),
                    ),
                ]),
            )),
        );
    }

    #[test]
    fn parenthesized_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("(3)", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.0))),
        );
        assert_eq!(
            parse("(((3)))", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Float(3.0))),
        );
    }

    #[test]
    fn modules() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let loader = static_module_loader(Vec::new());
        let path = Path::new("./foo.js");
        assert_eq!(
            parse_module(
                "export default 3;",
                &env,
                &path,
                &loader,
                &factory,
                &allocator
            ),
            Ok(factory.create_value_term(ValueTerm::Float(3.0))),
        );
    }

    #[test]
    fn variable_declarations() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const foo = 3; foo;", &env, &factory, &allocator),
            Ok(factory.create_let_term(
                factory.create_value_term(ValueTerm::Float(3.0)),
                factory.create_static_variable_term(0),
            )),
        );
        assert_eq!(
            parse(
                "const foo = 3; const bar = 4; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_value_term(ValueTerm::Float(3.0)),
                factory.create_let_term(
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_static_variable_term(1),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const foo = 3; const bar = 4; bar;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_value_term(ValueTerm::Float(3.0)),
                factory.create_let_term(
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_static_variable_term(0),
                ),
            )),
        );
    }

    #[test]
    fn variable_declaration_object_destructuring() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const {} = {}; true;", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Boolean(true))),
        );
        assert_eq!(
            parse(
                "const {} = { foo: 3, bar: 4, baz: 5 }; true;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_value_term(ValueTerm::Boolean(true))),
        );
        assert_eq!(
            parse(
                "const { foo } = { foo: 3, bar: 4, baz: 5 }; true;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        factory.create_struct_term(
                            allocator.create_struct_prototype(vec![
                                allocator.create_string(String::from("foo")),
                                allocator.create_string(String::from("bar")),
                                allocator.create_string(String::from("baz")),
                            ]),
                            allocator.create_list(vec![
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_value_term(ValueTerm::Float(4.0)),
                                factory.create_value_term(ValueTerm::Float(5.0)),
                            ]),
                        ),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        )),
                    ),
                ),
                factory.create_value_term(ValueTerm::Boolean(true))
            )),
        );
        assert_eq!(
            parse(
                "const { bar, foo } = { foo: 3, bar: 4, baz: 5 }; foo;",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("bar"))
                            )),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_static_variable_term(0),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const { bar, foo } = { foo: 3, bar: 4, baz: 5 }; bar;",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("bar"))
                            )),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const { bar: qux, foo } = { foo: 3, bar: 4, baz: 5 }; qux;",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("bar"))
                            )),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const { bar: foo, foo: bar } = { foo: 3, bar: 4, baz: 5 }; foo;",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                        allocator.create_string(String::from("baz")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("bar"))
                            )),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const foo = { first: 3, second: 4, third: 5 }; const { first, second } = foo; first;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("first")),
                        allocator.create_string(String::from("second")),
                        allocator.create_string(String::from("third")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_static_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("first")),
                                )),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from("second")),
                                    )),
                                ),
                            ),
                            factory.create_static_variable_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const foo = { first: 3, second: 4, third: 5 }; const bar = true; const { first, second } = foo; first;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("first")),
                        allocator.create_string(String::from("second")),
                        allocator.create_string(String::from("third")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_value_term(ValueTerm::Boolean(true)),
                    factory.create_let_term(
                        factory.create_static_variable_term(1),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from("first")),
                                    )),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(1),
                                        factory.create_value_term(ValueTerm::String(
                                            allocator.create_string(String::from("second")),
                                        )),
                                    ),
                                ),
                                factory.create_static_variable_term(1),
                            ),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const { one, two } = { one: { a: 1, b: 2 }, two: { c: 3, d: 4 }}; const { a, b } = one; const { c, d } = two; a;",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_let_term(
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("one")),
                        allocator.create_string(String::from("two")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_struct_term(
                            allocator.create_struct_prototype(vec![
                                allocator.create_string(String::from("a")),
                                allocator.create_string(String::from("b")),
                            ]),
                            allocator.create_list(vec![
                                factory.create_value_term(ValueTerm::Float(1.0)),
                                factory.create_value_term(ValueTerm::Float(2.0)),
                            ]),
                        ),
                        factory.create_struct_term(
                            allocator.create_struct_prototype(vec![
                                allocator.create_string(String::from("c")),
                                allocator.create_string(String::from("d")),
                            ]),
                            allocator.create_list(vec![
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_value_term(ValueTerm::Float(4.0)),
                            ]),
                        ),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(allocator.create_string(String::from(
                                "one"
                            )))),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::String(allocator.create_string(String::from(
                                    "two"
                                )))),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_static_variable_term(1),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("a")))),
                                    ),
                                ),
                                factory.create_let_term(
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Get),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("b")))),
                                        ),
                                    ),
                                    factory.create_let_term(
                                        factory.create_static_variable_term(3),
                                        factory.create_let_term(
                                            factory.create_application_term(
                                                factory.create_builtin_term(BuiltinTerm::Get),
                                                allocator.create_pair(
                                                    factory.create_static_variable_term(0),
                                                    factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("c")))),
                                                ),
                                            ),
                                            factory.create_let_term(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(BuiltinTerm::Get),
                                                    allocator.create_pair(
                                                        factory.create_static_variable_term(1),
                                                        factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("d")))),
                                                    ),
                                                ),
                                                factory.create_static_variable_term(4),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("((input) => { const { foo, bar } = input; return bar; })({ foo: false, bar: true });", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    factory.create_let_term(
                        factory.create_static_variable_term(0),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("foo")))),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(1),
                                        factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("bar")))),
                                    ),
                                ),
                                factory.create_static_variable_term(0),
                            ),
                        ),
                    ),
                ),
                allocator.create_unit_list(factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        allocator.create_string(String::from("foo")),
                        allocator.create_string(String::from("bar")),
                    ]),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Boolean(false)),
                        factory.create_value_term(ValueTerm::Boolean(true)),
                    ]),
                )),
            )),
        );
    }

    #[test]
    fn variable_declaration_array_destructuring() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const [] = [3, 4, 5]; true;", &env, &factory, &allocator),
            Ok(factory.create_value_term(ValueTerm::Boolean(true))),
        );
        assert_eq!(
            parse("const [foo] = [3, 4, 5]; foo;", &env, &factory, &allocator),
            Ok(factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Float(3.0)),
                            factory.create_value_term(ValueTerm::Float(4.0)),
                            factory.create_value_term(ValueTerm::Float(5.0)),
                        ])),
                        factory.create_value_term(ValueTerm::Int(0)),
                    ),
                ),
                factory.create_static_variable_term(0),
            )),
        );
        assert_eq!(
            parse(
                "const [foo, bar] = [3, 4, 5]; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ])),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::Int(0)),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::Int(1)),
                            ),
                        ),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const [, , foo] = [3, 4, 5]; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Get),
                    allocator.create_pair(
                        factory.create_vector_term(allocator.create_list(vec![
                            factory.create_value_term(ValueTerm::Float(3.0)),
                            factory.create_value_term(ValueTerm::Float(4.0)),
                            factory.create_value_term(ValueTerm::Float(5.0)),
                        ])),
                        factory.create_value_term(ValueTerm::Int(2)),
                    ),
                ),
                factory.create_static_variable_term(0),
            )),
        );
        assert_eq!(
            parse(
                "const [, foo, bar] = [3, 4, 5]; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ])),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::Int(1)),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_value_term(ValueTerm::Int(2)),
                            ),
                        ),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "const foo = true; const [bar] = [3, 4, 5]; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_value_term(ValueTerm::Boolean(true)),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_vector_term(allocator.create_list(vec![
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_value_term(ValueTerm::Float(4.0)),
                                factory.create_value_term(ValueTerm::Float(5.0)),
                            ])),
                            factory.create_value_term(ValueTerm::Int(0)),
                        ),
                    ),
                    factory.create_static_variable_term(1),
                )
            )),
        );
        assert_eq!(
            parse(
                "const foo = true; const [bar, baz] = [3, 4, 5]; foo;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_let_term(
                factory.create_value_term(ValueTerm::Boolean(true)),
                factory.create_let_term(
                    factory.create_vector_term(allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                        factory.create_value_term(ValueTerm::Float(5.0)),
                    ])),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                            ),
                            factory.create_static_variable_term(3),
                        ),
                    )
                )
            )),
        );
    }

    #[test]
    fn variable_scoping() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let expression = parse(
            "
            ((value) => {
                const foo = value * 2;
                if (foo === 3) return false;
                return foo;
              })(3);
            ",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0 * 2.0)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            ((value) => {
                const { foo, bar } = { foo: value * 2, bar: value };
                if (foo === 3) return false;
                return foo;
              })(3);
            ",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0 * 2.0)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn variable_dependencies() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let expression = parse(
            "
            const foo = 3;
            const bar = foo;
            const baz = bar;
            baz;
        ",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            const foo = (one) => (two) => (three) => 3;
            const bar = foo(1);
            const baz = bar(2);
            baz(3);
        ",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn not_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("!true", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Not),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(true))),
            )),
        );
        assert_eq!(
            parse("!false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Not),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Boolean(false))),
            )),
        );
        assert_eq!(
            parse("!3", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Not),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Float(3.0))),
            )),
        );
    }

    #[test]
    fn arithmetic_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("3 + 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Add),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 - 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Subtract),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 * 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Multiply),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 / 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Divide),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 % 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Remainder),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 ** 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Pow),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 < 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Lt),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 <= 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Lte),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 > 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Gt),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
        assert_eq!(
            parse("3 >= 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Gte),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
    }

    #[test]
    fn equality_expression() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true === false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Eq),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Boolean(true)),
                    factory.create_value_term(ValueTerm::Boolean(false)),
                ),
            )),
        );
        assert_eq!(
            parse("true !== false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Not),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Eq),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::Boolean(true)),
                        factory.create_value_term(ValueTerm::Boolean(false)),
                    ),
                )),
            )),
        );
    }

    #[test]
    fn logical_expression() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true && false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::And),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Boolean(true)),
                    factory.create_value_term(ValueTerm::Boolean(false)),
                ),
            )),
        );
        assert_eq!(
            parse("true || false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Or),
                allocator.create_pair(
                    factory.create_value_term(ValueTerm::Boolean(true)),
                    factory.create_value_term(ValueTerm::Boolean(false)),
                ),
            )),
        );
    }

    #[test]
    fn conditional_expression() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true ? 3 : 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::If),
                allocator.create_triple(
                    factory.create_value_term(ValueTerm::Boolean(true)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                ),
            )),
        );
    }

    #[test]
    fn if_statements() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse(
                "(() => { if (true) { return 3; } else { return 4; }})()",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    0,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::If),
                        allocator.create_triple(
                            factory.create_value_term(ValueTerm::Boolean(true)),
                            factory.create_value_term(ValueTerm::Float(3.0)),
                            factory.create_value_term(ValueTerm::Float(4.0)),
                        )
                    )
                ),
                allocator.create_empty_list(),
            ))
        );
        assert_eq!(
            parse("(() => { if (true) { throw new Error(\"foo\"); } else { throw new Error(\"bar\"); }})()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    0,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::If),
                        allocator.create_triple(
                            factory.create_value_term(ValueTerm::Boolean(true)),
                            factory.create_application_term(
                                factory.create_native_function_term(throw()),
                                allocator.create_unit_list(
                                    factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("foo")))),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_native_function_term(throw()),
                                allocator.create_unit_list(
                                    factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("bar")))),
                                ),
                            ),
                        ),
                    )
                ),
                allocator.create_empty_list(),
            ))
        );
        assert_eq!(
            parse("(() => { if (true) { const foo = 3; const bar = 4; return foo + bar; } else { const foo = 4; const bar = 3; return foo + bar; }})()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    0,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::If),
                        allocator.create_triple(
                            factory.create_value_term(ValueTerm::Boolean(true)),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_let_term(
                                    factory.create_value_term(ValueTerm::Float(4.0)),
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Float(4.0)),
                                factory.create_let_term(
                                    factory.create_value_term(ValueTerm::Float(3.0)),
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("(() => { if (true) { const foo = 3; const bar = 4; return foo + bar; } const foo = 4; const bar = 3; return foo + bar; })()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    0,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::If),
                        allocator.create_triple(
                            factory.create_value_term(ValueTerm::Boolean(true)),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_let_term(
                                    factory.create_value_term(ValueTerm::Float(4.0)),
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Float(4.0)),
                                factory.create_let_term(
                                    factory.create_value_term(ValueTerm::Float(3.0)),
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("(() => { if (true) throw new Error(\"foo\"); const foo = 3; const bar = 4; return foo + bar; })()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    0,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::If),
                        allocator.create_triple(
                            factory.create_value_term(ValueTerm::Boolean(true)),
                            factory.create_application_term(
                                factory.create_native_function_term(throw()),
                                allocator.create_unit_list(
                                    factory.create_value_term(ValueTerm::String(allocator.create_string(String::from("foo")))),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_value_term(ValueTerm::Float(3.0)),
                                factory.create_let_term(
                                    factory.create_value_term(ValueTerm::Float(4.0)),
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(1),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                allocator.create_empty_list(),
            )),
        );
    }

    #[test]
    fn throw_statements() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("throw new Error(\"foo\")", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_native_function_term(throw()),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("foo"))
                ))),
            )),
        );
        assert_eq!(
            parse("throw new Error(`foo${'bar'}`)", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_native_function_term(throw()),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Concat),
                    allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("foo"))
                        )),
                        factory.create_application_term(
                            factory.create_native_function_term(to_string()),
                            allocator.create_unit_list(factory.create_value_term(
                                ValueTerm::String(allocator.create_string(String::from("bar")))
                            )),
                        ),
                    ),
                )),
            )),
        );
    }

    #[test]
    fn arrow_function_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("() => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("(foo) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_value_term(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("(foo) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_static_variable_term(0))),
        );
        assert_eq!(
            parse("(foo) => foo + foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Add),
                    allocator.create_pair(
                        factory.create_static_variable_term(0),
                        factory.create_static_variable_term(0),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(foo, bar, baz) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Add),
                    allocator.create_pair(
                        factory.create_static_variable_term(2),
                        factory.create_static_variable_term(1),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(foo) => (bar) => (baz) => foo + bar",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_lambda_term(
                1,
                factory.create_lambda_term(
                    1,
                    factory.create_lambda_term(
                        1,
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_static_variable_term(2),
                                factory.create_static_variable_term(1),
                            ),
                        ),
                    ),
                ),
            )),
        );
    }

    #[test]
    fn arrow_function_object_destructuring() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("({}) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_value_term(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("({ foo }) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("foo"))
                            )),
                        ),
                    ),
                    factory.create_static_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("({ foo, bar }) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_static_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from("bar"))
                                    )),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Add),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_static_variable_term(0),
                                ),
                            ),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(first, { foo, bar }, second, third) => ((first + foo) + bar) + third",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_lambda_term(
                4,
                factory.create_let_term(
                    factory.create_static_variable_term(2),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("foo"))
                                )),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from("bar"))
                                    )),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Add),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_application_term(
                                                factory.create_builtin_term(BuiltinTerm::Add),
                                                allocator.create_pair(
                                                    factory.create_static_variable_term(6),
                                                    factory.create_static_variable_term(1),
                                                ),
                                            ),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                    factory.create_static_variable_term(3),
                                ),
                            ),
                        ),
                    ),
                ),
            )),
        );
    }

    #[test]
    fn arrow_function_array_destructuring() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("([]) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_value_term(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("([foo]) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::Int(0)),
                        ),
                    ),
                    factory.create_static_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("([foo, bar]) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_static_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Add),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_static_variable_term(0),
                                ),
                            ),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("([, , foo]) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Get),
                        allocator.create_pair(
                            factory.create_static_variable_term(0),
                            factory.create_value_term(ValueTerm::Int(2)),
                        ),
                    ),
                    factory.create_static_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("([, foo, bar]) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_static_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(1)),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::Int(2)),
                                ),
                            ),
                            factory.create_static_variable_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(first, [foo, bar], second, third) => ((first + foo) + bar) + third",
                &env,
                &factory,
                &allocator,
            ),
            Ok(factory.create_lambda_term(
                4,
                factory.create_let_term(
                    factory.create_static_variable_term(2),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Add),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_application_term(
                                                factory.create_builtin_term(BuiltinTerm::Add),
                                                allocator.create_pair(
                                                    factory.create_static_variable_term(6),
                                                    factory.create_static_variable_term(1),
                                                ),
                                            ),
                                            factory.create_static_variable_term(0),
                                        ),
                                    ),
                                    factory.create_static_variable_term(3),
                                ),
                            ),
                        ),
                    ),
                ),
            )),
        );
    }

    #[test]
    fn function_application_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("(() => 3)()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(0, factory.create_value_term(ValueTerm::Float(3.0))),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("((foo) => foo)(3)", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(1, factory.create_static_variable_term(0)),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Float(3.0)),),
            )),
        );
        assert_eq!(
            parse(
                "((foo, bar, baz) => foo + bar)(3, 4, 5)",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    3,
                    factory.create_application_term(
                        factory.create_builtin_term(BuiltinTerm::Add),
                        allocator.create_pair(
                            factory.create_static_variable_term(2),
                            factory.create_static_variable_term(1),
                        ),
                    ),
                ),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(3.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(5.0)),
                ]),
            )),
        );
        assert_eq!(
            parse(
                "((foo) => (bar) => (baz) => foo + bar)(3)(4)(5)",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_application_term(
                    factory.create_application_term(
                        factory.create_lambda_term(
                            1,
                            factory.create_lambda_term(
                                1,
                                factory.create_lambda_term(
                                    1,
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::Add),
                                        allocator.create_pair(
                                            factory.create_static_variable_term(2),
                                            factory.create_static_variable_term(1),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        allocator
                            .create_unit_list(factory.create_value_term(ValueTerm::Float(3.0))),
                    ),
                    allocator.create_unit_list(factory.create_value_term(ValueTerm::Float(4.0))),
                ),
                allocator.create_unit_list(factory.create_value_term(ValueTerm::Float(5.0))),
            )),
        );
    }

    #[test]
    fn function_arg_spreading() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("((x, y) => x + y)(...[3, 4])", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Apply),
                allocator.create_pair(
                    factory.create_lambda_term(
                        2,
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_static_variable_term(1),
                                factory.create_static_variable_term(0)
                            ),
                        )
                    ),
                    factory.create_vector_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                    )),
                ),
            )),
        );
        assert_eq!(
            parse("((x, y) => x + y)(1, 2, ...[3, 4])", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::Apply),
                allocator.create_pair(
                    factory.create_partial_application_term(
                        factory.create_lambda_term(
                            2,
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Add),
                                allocator.create_pair(
                                    factory.create_static_variable_term(1),
                                    factory.create_static_variable_term(0)
                                ),
                            )
                        ),
                        allocator.create_pair(
                            factory.create_value_term(ValueTerm::Float(1.0)),
                            factory.create_value_term(ValueTerm::Float(2.0)),
                        ),
                    ),
                    factory.create_vector_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::Float(3.0)),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn recursive_expressions() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(builtin_imports(&factory, &allocator));
        let expression = parse_module(
            "
            import { graph } from 'reflex::utils';
            export default graph((foo) => 3);
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0)),
                DependencyList::empty(),
            ),
        );
        let expression = parse_module(
            "
            import { graph } from 'reflex::utils';
            export default graph((foo) => ({
                foo,
                bar: 3,
                baz: 4,
            })).foo.foo.foo.bar;
        ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn import_scoping() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader =
            static_module_loader(vec![("foo", factory.create_value_term(ValueTerm::Null))]);
        let expression = parse_module(
            "
            import Foo from 'foo';
            const foo = 4;
            const bar = { foo: 3, bar: Foo };
            export default bar.foo;
            ",
            &env,
            &path,
            &loader,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &DynamicState::new(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Float(3.0)),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn js_interpreted() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let input = "
            const fullName = (first, last) => `${first} ${last}`;
            const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
            greet({ first: 'John', last: 'Doe' })";
        let expression = parse(input, &env, &factory, &allocator).unwrap();
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("Hello, John Doe!"))
                )),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn js_compiled() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let input = "
            const fullName = (first, last) => `${first} ${last}`;
            const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
            greet({ first: 'John', last: 'Doe' })";
        let expression = parse(input, &env, &factory, &allocator).unwrap();
        let compiled = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(
                &expression,
                CompilerMode::Expression,
                true,
                builtin_plugins(),
                &factory,
                &allocator,
            )
            .unwrap();
        let (program, builtins, plugins) = compiled.into_parts();
        let state = DynamicState::new();
        let mut cache = DefaultInterpreterCache::default();
        let (result, _) = execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("Hello, John Doe!"))
                )),
                DependencyList::empty(),
            ),
        );
    }
}
