// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{iter::once, path::Path};

use reflex::{
    cache::NoopCache,
    core::{
        as_integer, create_record, Expression, ExpressionFactory, FloatTermType, HeapAllocator,
        IntTermType, RefType, Rewritable, StringTermType, StringValue, Substitutions,
    },
};
use reflex_stdlib::Stdlib;
use swc_common::{source_map::Pos, sync::Lrc, FileName, SourceMap, Span, Spanned};
use swc_ecma_ast::{
    ArrayLit, ArrowExpr, BinExpr, BinaryOp, BlockStmt, BlockStmtOrExpr, Bool, CallExpr, CondExpr,
    Decl, EsVersion, Expr, ExprOrSpread, ExprOrSuper, ExprStmt, Ident, ImportDecl, ImportSpecifier,
    Lit, MemberExpr, Module, ModuleDecl, ModuleItem, NewExpr, Null, Number, ObjectLit,
    ObjectPatProp, Pat, Prop, PropName, PropOrSpread, Stmt, Str, TaggedTpl, Tpl, TplElement,
    UnaryExpr, UnaryOp, VarDeclKind, VarDeclarator,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

use crate::{
    globals::global_aggregate_error,
    stdlib::{get_builtin_field, Stdlib as JsStdlib},
    Env,
};

pub type ParserResult<T> = Result<T, ParserError>;
pub type ParserError = String;

fn err<T: std::fmt::Debug>(message: &str, _node: T) -> ParserError {
    String::from(message)
}

fn err_unimplemented<T: std::fmt::Debug>(node: T) -> String {
    err("Unsupported syntax", node)
}

fn err_unreachable<T: std::fmt::Debug>(node: T) -> String {
    err("Unreachable code", node)
}

#[derive(Clone)]
struct LexicalScope {
    bindings: Vec<Option<String>>,
}
impl LexicalScope {
    fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }
    fn from(identifiers: impl IntoIterator<Item = Option<String>>) -> Self {
        Self {
            bindings: identifiers.into_iter().collect(),
        }
    }
    fn depth(&self) -> usize {
        self.bindings.len()
    }
    fn create_child(&self, identifiers: impl IntoIterator<Item = Option<String>>) -> LexicalScope {
        LexicalScope {
            bindings: self.bindings.iter().cloned().chain(identifiers).collect(),
        }
    }
    fn get(&self, identifier: &str) -> Option<usize> {
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

pub fn parse<T: Expression + Rewritable<T>>(
    input: &str,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let program = parse_ast(input, None)?;
    parse_script_contents(program.body.into_iter(), env, factory, allocator)
}

pub fn parse_module<T: Expression + Rewritable<T>>(
    input: &str,
    env: &Env<T>,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let program = parse_ast(input, Some(path))?;
    parse_module_contents(
        program.body.into_iter(),
        env,
        path,
        loader,
        factory,
        allocator,
    )
}

fn format_source_error(location: Span, message: &str, source_map: &SourceMap) -> String {
    let location = match source_map.span_to_lines(location) {
        Ok(regions) => match regions.lines.len() {
            0 => format!("{}", regions.file.name),
            1 => {
                let line = regions.lines.first().unwrap();
                format!(
                    "{}:{}:{}-{}",
                    regions.file.name,
                    line.line_index + 1,
                    line.start_col.to_usize() + 1,
                    line.end_col.to_usize() + 1
                )
            }
            _ => {
                let first = regions.lines.first().unwrap();
                let last = regions.lines.last().unwrap();
                format!(
                    "{}:{}:{}-{}:{}",
                    regions.file.name,
                    first.line_index + 1,
                    first.start_col.to_usize() + 1,
                    last.line_index + 1,
                    last.end_col.to_usize() + 1
                )
            }
        },
        Err(_) => format!("{}", source_map.span_to_filename(location)),
    };
    format!("{}: {}", location, message)
}

fn parse_ast(input: &str, path: Option<&Path>) -> ParserResult<Module> {
    let source_map: Lrc<SourceMap> = Default::default();
    let source = source_map.new_source_file(
        match path {
            Some(path) => FileName::Real(path.to_path_buf()),
            None => FileName::Anon,
        },
        String::from(input),
    );
    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        EsVersion::latest(),
        StringInput::from(&*source),
        None,
    );
    let mut parser = Parser::new_from(lexer);
    let syntax_errors = parser.take_errors();
    if !syntax_errors.is_empty() {
        return Err(syntax_errors
            .into_iter()
            .map(|error| format_source_error(error.span(), &error.into_kind().msg(), &source_map))
            .collect::<Vec<_>>()
            .join("\n"));
    }

    parser
        .parse_module()
        .map_err(|err| format_source_error(err.span(), &err.into_kind().msg(), &source_map))
}

fn parse_script_contents<T: Expression + Rewritable<T>>(
    program: impl IntoIterator<Item = ModuleItem> + ExactSizeIterator,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let body = program
        .into_iter()
        .map(|node| match node {
            ModuleItem::Stmt(node) => Ok(node),
            _ => Err(err_unimplemented(node)),
        })
        .collect::<ParserResult<Vec<_>>>()?;
    match parse_block(&body, &LexicalScope::new(), &env, factory, allocator)? {
        None => Err(String::from("No expression to evaluate")),
        Some(expression) => Ok(expression),
    }
}

fn parse_module_contents<T: Expression + Rewritable<T>>(
    program: impl IntoIterator<Item = ModuleItem> + ExactSizeIterator,
    env: &Env<T>,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let num_statements = program.len();
    let (body, import_bindings) = program.into_iter().fold(
        Ok((Vec::with_capacity(num_statements), Vec::new())),
        |results, node| {
            let (mut body, mut import_bindings) = results?;
            match node {
                ModuleItem::ModuleDecl(node) => match node {
                    ModuleDecl::Import(node) => {
                        let bindings =
                            parse_module_import(&node, path, loader, factory, allocator)?;
                        import_bindings.extend(bindings);
                        Ok((body, import_bindings))
                    }
                    ModuleDecl::ExportDecl(node) => Err(err_unimplemented(node)),
                    ModuleDecl::ExportNamed(node) => Err(err_unimplemented(node)),
                    ModuleDecl::ExportDefaultDecl(node) => Err(err_unimplemented(node)),
                    ModuleDecl::ExportDefaultExpr(node) => {
                        body.push(Stmt::Expr(ExprStmt {
                            span: node.span,
                            expr: node.expr,
                        }));
                        Ok((body, import_bindings))
                    }
                    ModuleDecl::ExportAll(_) => Err(err_unimplemented(node)),
                    _ => Err(err_unimplemented(node)),
                },
                ModuleItem::Stmt(node) => {
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

fn parse_module_import<T: Expression + Rewritable<T>>(
    node: &ImportDecl,
    path: &Path,
    loader: &impl Fn(&str, &Path) -> Option<Result<T, String>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Vec<(String, T)>>
where
    T::Builtin: From<Stdlib>,
{
    let module_path = parse_string(&node.src);
    let module = match loader(&module_path, path)
        .unwrap_or_else(|| Err(String::from("No compatible loaders registered")))
    {
        Ok(module) => Ok(module),
        Err(error) => Err(err(
            &format!("Failed to import '{}': {}", module_path, error),
            node,
        )),
    }?;
    Ok(node
        .specifiers
        .iter()
        .map(|specifier| {
            let (identifier, value) = match specifier {
                ImportSpecifier::Default(node) => {
                    let identifier = parse_identifier(&node.local);
                    let value = get_static_field(module.clone(), "default", factory, allocator);
                    (identifier, value)
                }
                ImportSpecifier::Namespace(node) => {
                    let identifier = parse_identifier(&node.local);
                    let value = module.clone();
                    (identifier, value)
                }
                ImportSpecifier::Named(node) => {
                    let identifier = parse_identifier(&node.local);
                    let imported_field = node
                        .imported
                        .as_ref()
                        .map(parse_identifier)
                        .unwrap_or(identifier);
                    let value =
                        get_static_field(module.clone(), imported_field, factory, allocator);
                    (identifier, value)
                }
            };
            (String::from(identifier), value)
        })
        .collect())
}

fn parse_block<'a, T: Expression + Rewritable<T>>(
    body: impl IntoIterator<Item = &'a Stmt>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Option<T>>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    parse_block_statements(body, None, scope, env, factory, allocator)
}

fn parse_block_statements<'a, T: Expression + Rewritable<T>>(
    remaining: impl IntoIterator<Item = &'a Stmt>,
    result: Option<T>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Option<T>>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let mut remaining = remaining.into_iter();
    let node = remaining.next();
    match node {
        None => Ok(result),
        Some(statement) => {
            if result.is_some() {
                return Err(err_unreachable(statement));
            }
            match statement {
                Stmt::Decl(node) => match node {
                    Decl::Var(node) => match node.kind {
                        VarDeclKind::Const => {
                            let (initializers, child_scope) = parse_variable_declarators(
                                &node.decls,
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
                    Decl::Fn(node) => Err(err_unimplemented(node)),
                    Decl::Class(node) => Err(err_unimplemented(node)),
                    _ => Err(err_unimplemented(node)),
                },
                Stmt::Expr(node) => {
                    let expression = parse_expression(&node.expr, &scope, env, factory, allocator)?;
                    let result = Some(expression);
                    parse_block_statements(remaining, result, scope, env, factory, allocator)
                }
                Stmt::Return(node) => match &node.arg {
                    None => Err(err("Missing return value", node)),
                    Some(node) => {
                        let expression = parse_expression(node, &scope, env, factory, allocator)?;
                        let result = Some(expression);
                        parse_block_statements(remaining, result, scope, env, factory, allocator)
                    }
                },
                Stmt::Throw(node) => {
                    let expression =
                        parse_throw_statement(&node.arg, &scope, env, factory, allocator)?;
                    let result = Some(expression);
                    parse_block_statements(remaining, result, scope, env, factory, allocator)
                }
                Stmt::If(node) => {
                    let condition = parse_expression(&node.test, scope, env, factory, allocator)?;
                    let consequent = parse_if_branch(&node.cons, scope, env, factory, allocator)?;
                    match &node.alt {
                        Some(node) => {
                            let alternate = parse_if_branch(&node, scope, env, factory, allocator)?;
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
                Stmt::Try(node) => {
                    let BlockStmt { stmts: body, .. } = &node.block;
                    let body = parse_branch(&statement, body, scope, env, factory, allocator)?;
                    if let Some(node) = &node.finalizer {
                        Err(err_unimplemented(node))
                    } else if let Some(handler) = &node.handler {
                        let identifier = match &handler.param {
                            Some(pattern) => match pattern {
                                Pat::Ident(identifier) => {
                                    Ok(Some(String::from(parse_identifier(&identifier.id))))
                                }
                                // TODO: Support destructuring patterns in catch variable assignment
                                _ => Err(err_unimplemented(pattern)),
                            },
                            None => Ok(None),
                        }?;
                        let BlockStmt { stmts: handler, .. } = &handler.body;
                        let handler = factory.create_lambda_term(
                            1,
                            parse_branch(
                                &statement,
                                handler,
                                &scope.create_child(once(identifier)),
                                env,
                                factory,
                                allocator,
                            )?,
                        );
                        let expression =
                            create_try_catch_expression(body, handler, factory, allocator);
                        let result = Some(expression);
                        parse_block_statements(remaining, result, scope, env, factory, allocator)
                    } else {
                        Err(err_unimplemented(node))
                    }
                }
                Stmt::Switch(_) => Err(err_unimplemented(statement)),
                Stmt::Empty(_) => {
                    parse_block_statements(remaining, result, scope, env, factory, allocator)
                }
                _ => Err(err_unimplemented(statement)),
            }
        }
    }
}

fn create_declaration_block<T: Expression + Rewritable<T>>(
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

fn parse_variable_declarators<T: Expression + Rewritable<T>>(
    declarators: &[VarDeclarator],
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(
    impl IntoIterator<Item = T, IntoIter = impl DoubleEndedIterator<Item = T>>,
    Option<LexicalScope>,
)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
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

fn parse_variable_declarator<T: Expression + Rewritable<T>>(
    node: &VarDeclarator,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(impl IntoIterator<Item = T>, Option<LexicalScope>)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let init = node
        .init
        .as_ref()
        .ok_or_else(|| err("Missing variable initializer", node))?;
    let value = parse_expression(&init, scope, env, factory, allocator)?;
    match &node.name {
        Pat::Ident(node) => {
            let identifier = parse_identifier(&node.id);
            Ok((
                vec![value],
                Some(scope.create_child(once(Some(String::from(identifier))))),
            ))
        }
        Pat::Object(node) => {
            let (initializers, child_scope) = parse_object_destructuring_pattern_bindings(
                value,
                &node.props,
                scope,
                env,
                factory,
                allocator,
            )?;
            Ok((initializers.into_iter().collect::<Vec<_>>(), child_scope))
        }
        Pat::Array(node) => {
            let (initializers, child_scope) = parse_array_destructuring_pattern_bindings(
                value,
                &node.elems,
                scope,
                env,
                factory,
                allocator,
            )?;
            Ok((initializers.into_iter().collect::<Vec<_>>(), child_scope))
        }
        Pat::Rest(_) => Err(err_unimplemented(node)),
        Pat::Assign(_) => Err(err_unimplemented(node)),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_object_destructuring_pattern_bindings<T: Expression + Rewritable<T>>(
    target: T,
    properties: &[ObjectPatProp],
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(Vec<T>, Option<LexicalScope>)>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let properties = properties
        .iter()
        .map(|property| match property {
            ObjectPatProp::KeyValue(node) => {
                let identifier = match &*node.value {
                    Pat::Ident(node) => Ok(parse_identifier(&node.id)),
                    Pat::Object(_) => Err(err_unimplemented(node)),
                    Pat::Array(_) => Err(err_unimplemented(node)),
                    Pat::Rest(_) => Err(err_unimplemented(node)),
                    Pat::Assign(_) => Err(err_unimplemented(node)),
                    _ => Err(err_unimplemented(node)),
                }?;
                let field_name = parse_prop_name(&node.key, scope, env, factory, allocator)?;
                let field_accessor =
                    factory.create_string_term(allocator.create_string(field_name));
                Ok((identifier, field_accessor))
            }
            ObjectPatProp::Assign(node) => {
                if node.value.is_some() {
                    Err(err_unimplemented(node))
                } else {
                    let identifier = parse_identifier(&node.key);
                    let field_accessor = factory
                        .create_string_term(allocator.create_string(String::from(identifier)));
                    Ok((identifier, field_accessor))
                }
            }
            ObjectPatProp::Rest(node) => Err(err_unimplemented(node)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match properties.len() {
        0 => Ok((Vec::new(), None)),
        1 => {
            let (identifier, field_accessor) = properties.into_iter().next().unwrap();
            let initializer = get_dynamic_field(target, field_accessor, factory, allocator);
            Ok((
                vec![initializer],
                Some(scope.create_child(once(Some(String::from(identifier))))),
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
                        let (identifier, field_accessor) = property;
                        let scope_offset = existing_scope.depth() - initializer_depth;
                        let initializer = get_dynamic_field(
                            factory.create_variable_term(scope_offset),
                            field_accessor,
                            factory,
                            allocator,
                        );
                        initializers.push(initializer);
                        let next_scope =
                            existing_scope.create_child(once(Some(String::from(identifier))));
                        Ok((initializers, next_scope))
                    },
                )
                .map(|(initializers, scope)| (initializers, Some(scope)))
        }
    }
}

fn parse_array_destructuring_pattern_bindings<T: Expression + Rewritable<T>>(
    target: T,
    accessors: &[Option<Pat>],
    scope: &LexicalScope,
    _env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<(impl IntoIterator<Item = T>, Option<LexicalScope>)>
where
    T::Builtin: From<Stdlib>,
{
    let accessors = accessors
        .iter()
        .enumerate()
        .filter_map(|(index, accessor)| match accessor {
            Some(accessor) => Some((index, accessor)),
            None => None,
        })
        .map(|(index, accessor)| match accessor {
            Pat::Ident(identifier) => {
                let identifier = parse_identifier(&identifier.id);
                Ok((identifier, index))
            }
            Pat::Rest(_) => Err(err_unimplemented(accessor)),
            _ => Err(err_unimplemented(accessor)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match accessors.len() {
        0 => Ok((Vec::new(), None)),
        1 => {
            let (identifier, index) = accessors.into_iter().next().unwrap();
            let initializer = get_indexed_field(target, index, factory, allocator);
            Ok((
                vec![initializer],
                Some(scope.create_child(once(Some(String::from(identifier))))),
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
                        factory.create_variable_term(scope_offset),
                        index,
                        factory,
                        allocator,
                    );
                    initializers.push(initializer);
                    let next_scope =
                        existing_scope.create_child(once(Some(String::from(identifier))));
                    Ok((initializers, next_scope))
                })
                .map(|(initializers, scope)| (initializers, Some(scope)))
        }
    }
}

fn parse_identifier(node: &Ident) -> &str {
    &node.sym
}

fn parse_prop_name<T: Expression + Rewritable<T>>(
    node: &PropName,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<String>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match node {
        PropName::Ident(key) => Ok(String::from(parse_identifier(key))),
        PropName::Str(key) => Ok(parse_string(key)),
        PropName::Num(key) => {
            let key = key.value;
            match as_integer(key) {
                Some(key) => Ok(format!("{}", key)),
                _ => Ok(format!("{}", key)),
            }
        }
        PropName::BigInt(key) => Err(err_unimplemented(key)),
        PropName::Computed(key) => {
            let dynamic_key = parse_expression(&key.expr, scope, env, factory, allocator)?;
            if let Some(term) = factory.match_string_term(&dynamic_key) {
                Ok(String::from(term.value().as_deref().as_str()))
            } else if let Some(_) = factory.match_nil_term(&dynamic_key) {
                Ok(format!("{}", dynamic_key))
            } else if let Some(_) = factory.match_boolean_term(&dynamic_key) {
                Ok(format!("{}", dynamic_key))
            } else if let Some(_) = factory.match_int_term(&dynamic_key) {
                Ok(format!("{}", dynamic_key))
            } else if let Some(term) = factory.match_float_term(&dynamic_key) {
                Ok(if let Some(value) = as_integer(term.value()) {
                    format!("{}", value)
                } else {
                    format!("{}", dynamic_key)
                })
            } else {
                Err(err_unimplemented(dynamic_key))
            }
        }
    }
}

fn parse_throw_statement<T: Expression + Rewritable<T>>(
    value: &Expr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let error = parse_expression(value, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(JsStdlib::Throw),
        allocator.create_unit_list(factory.create_application_term(
            factory.create_builtin_term(Stdlib::ResolveDeep),
            allocator.create_unit_list(error),
        )),
    ))
}

fn parse_if_branch<T: Expression + Rewritable<T>>(
    node: &Stmt,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match node {
        Stmt::Block(block) => {
            let BlockStmt { stmts: body, .. } = block;
            parse_branch(node, body, scope, env, factory, allocator)
        }
        _ => parse_branch(node, &vec![node.clone()], scope, env, factory, allocator),
    }
}

fn parse_branch<'a, T: Expression + Rewritable<T>>(
    node: &Stmt,
    body: impl IntoIterator<Item = &'a Stmt>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let expression = parse_block(body, scope, env, factory, allocator)?;
    match expression {
        None => Err(err("Unterminated branch", node)),
        Some(expression) => Ok(expression),
    }
}

fn parse_expression<T: Expression + Rewritable<T>>(
    node: &Expr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match node {
        Expr::Paren(node) => parse_expression(&node.expr, scope, env, factory, allocator),
        Expr::Ident(node) => parse_variable_reference(node, scope, env, factory),
        Expr::Lit(node) => parse_literal(node, factory, allocator),
        Expr::Tpl(node) => parse_template_literal(node, scope, env, factory, allocator),
        Expr::TaggedTpl(node) => parse_tagged_template(node, scope, env, factory, allocator),
        Expr::Unary(node) => parse_unary_expression(node, scope, env, factory, allocator),
        Expr::Bin(node) => parse_binary_expression(node, scope, env, factory, allocator),
        Expr::Cond(node) => parse_conditional_expression(node, scope, env, factory, allocator),
        Expr::Arrow(node) => parse_arrow_function_expression(node, scope, env, factory, allocator),
        Expr::Member(node) => parse_member_expression(node, scope, env, factory, allocator),
        Expr::Call(node) => parse_call_expression(node, scope, env, factory, allocator),
        Expr::New(node) => parse_constructor_expression(node, scope, env, factory, allocator),
        Expr::Object(node) => parse_object_literal(node, scope, env, factory, allocator),
        Expr::Array(node) => parse_array_literal(node, scope, env, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_expressions<'a, T: Expression + Rewritable<T>>(
    expressions: impl IntoIterator<Item = &'a Expr>,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<Vec<T>>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    expressions
        .into_iter()
        .map(|node| parse_expression(node, scope, env, factory, allocator))
        .collect::<Result<Vec<_>, _>>()
}

fn parse_variable_reference<T: Expression + Rewritable<T>>(
    node: &Ident,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    let name = parse_identifier(node);
    let offset = scope.get(name);
    match offset {
        Some(offset) => Ok(factory.create_variable_term(offset)),
        None => match env.global(name) {
            Some(value) => Ok(value),
            None => Err(err(&format!("Invalid reference: '{}'", name), node)),
        },
    }
}

fn parse_literal<T: Expression + Rewritable<T>>(
    node: &Lit,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    match node {
        Lit::Null(node) => parse_null_literal(node, factory),
        Lit::Bool(node) => parse_boolean_literal(node, factory),
        Lit::Num(node) => parse_number_literal(node, factory),
        Lit::Str(node) => parse_string_literal(node, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_null_literal<T: Expression + Rewritable<T>>(
    _node: &Null,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    Ok(factory.create_nil_term())
}

fn parse_boolean_literal<T: Expression + Rewritable<T>>(
    node: &Bool,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    Ok(factory.create_boolean_term(node.value))
}

fn parse_number_literal<T: Expression + Rewritable<T>>(
    node: &Number,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<T> {
    Ok(factory.create_float_term(node.value))
}

fn parse_string_literal<T: Expression + Rewritable<T>>(
    node: &Str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T> {
    let value = parse_string(node);
    Ok(factory.create_string_term(allocator.create_string(value)))
}

fn parse_string(node: &Str) -> String {
    parse_escaped_string(&node.value)
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

fn parse_template_literal<T: Expression + Rewritable<T>>(
    node: &Tpl,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let args = node
        .quasis
        .iter()
        .map(|quasi| {
            let value = parse_template_element(quasi);
            if value.is_empty() {
                None
            } else {
                Some(factory.create_string_term(allocator.create_string(value)))
            }
        })
        .zip(
            node.exprs
                .iter()
                .map(|expression| {
                    let value = parse_expression(expression, scope, env, factory, allocator)?;
                    Ok(Some(factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(value),
                    )))
                })
                .chain(once(Ok(None))),
        )
        .flat_map(|(quasi, expression)| once(Ok(quasi)).chain(once(expression)))
        .filter_map(|arg| match arg {
            Err(error) => Some(Err(error)),
            Ok(Some(arg)) => Some(Ok(arg)),
            Ok(None) => None,
        })
        .collect::<ParserResult<Vec<_>>>()?;
    Ok(match args.len() {
        0 => factory.create_string_term(allocator.create_static_string("")),
        1 => args.into_iter().next().unwrap(),
        _ => factory.create_application_term(
            factory.create_builtin_term(Stdlib::Concat),
            allocator.create_list(args),
        ),
    })
}

fn parse_template_element(node: &TplElement) -> String {
    node.cooked
        .as_ref()
        .map(|value| {
            let value: &str = &value.value;
            String::from(value)
        })
        .unwrap_or_else(|| parse_string(&node.raw))
}

fn parse_tagged_template<T: Expression + Rewritable<T>>(
    node: &TaggedTpl,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    parse_template_literal(&node.tpl, scope, env, factory, allocator)
}

fn parse_object_literal<T: Expression + Rewritable<T>>(
    node: &ObjectLit,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    enum ObjectLiteralField<T> {
        Property(String, T),
        Spread(T),
    }
    let elements =
        node.props
            .iter()
            .fold(Ok(Vec::with_capacity(node.props.len())), |results, node| {
                let mut elements = results?;
                match node {
                    PropOrSpread::Prop(prop) => match &**prop {
                        Prop::KeyValue(prop) => {
                            let key = parse_prop_name(&prop.key, scope, env, factory, allocator)?;
                            let value =
                                parse_expression(&prop.value, scope, env, factory, allocator)?;
                            elements.push(ObjectLiteralField::Property(key, value));
                            Ok(elements)
                        }
                        Prop::Shorthand(prop) => {
                            let key = String::from(parse_identifier(&prop));
                            let value = parse_variable_reference(&prop, scope, env, factory)?;
                            elements.push(ObjectLiteralField::Property(key, value));
                            Ok(elements)
                        }
                        Prop::Method(_) => Err(err_unimplemented(prop)),
                        Prop::Getter(_) => Err(err_unimplemented(prop)),
                        Prop::Setter(_) => Err(err_unimplemented(prop)),
                        _ => Err(err_unimplemented(prop)),
                    },
                    PropOrSpread::Spread(node) => {
                        let value = parse_expression(&node.expr, scope, env, factory, allocator)?;
                        elements.push(ObjectLiteralField::Spread(value));
                        Ok(elements)
                    }
                }
            })?;

    let field_sets = elements
        .into_iter()
        .flat_map(|element| {
            let spread_delimiter: Option<Option<ObjectLiteralField<T>>> =
                if matches!(&element, ObjectLiteralField::Spread(_)) {
                    Some(None)
                } else {
                    None
                };
            spread_delimiter.into_iter().chain(once(Some(element)))
        })
        .chain(once(None))
        .fold((Vec::new(), Vec::new()), |results, property| {
            let (mut field_sets, mut current_set) = results;
            match property {
                Some(ObjectLiteralField::Spread(value)) => {
                    field_sets.push(value);
                    (field_sets, Vec::new())
                }
                Some(ObjectLiteralField::Property(key, value)) => {
                    current_set.push((
                        factory.create_string_term(allocator.create_string(key)),
                        value,
                    ));
                    (field_sets, current_set)
                }
                None => {
                    if !current_set.is_empty() {
                        field_sets.push(create_record(current_set, factory, allocator));
                    }
                    (field_sets, Vec::new())
                }
            }
        })
        .0;

    Ok(if field_sets.len() >= 2 {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Merge),
            allocator.create_list(field_sets),
        )
    } else {
        field_sets.into_iter().next().unwrap_or_else(|| {
            factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_empty_list()),
                allocator.create_empty_list(),
            )
        })
    })
}

fn parse_array_literal<T: Expression + Rewritable<T>>(
    node: &ArrayLit,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    enum ArrayLiteralFields<T> {
        Items(Vec<T>),
        Spread(T),
    }
    let elements =
        node.elems
            .iter()
            .fold(Ok(Vec::with_capacity(node.elems.len())), |results, node| {
                let mut elements = results?;
                match node {
                    None => Err(err("Missing array item", node)),
                    Some(node) => {
                        if node.spread.is_some() {
                            let value =
                                parse_expression(&node.expr, scope, env, factory, allocator)?;
                            elements.push(ArrayLiteralFields::Spread(value));
                            Ok(elements)
                        } else {
                            let value =
                                parse_expression(&node.expr, scope, env, factory, allocator)?;
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
                    }
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
                    factory.create_builtin_term(Stdlib::Push),
                    allocator
                        .create_pair(target.clone(), items.into_iter().next().cloned().unwrap()),
                ))
            }
            (ArrayLiteralFields::Items(items), ArrayLiteralFields::Spread(target))
                if items.len() == 1 =>
            {
                return Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::PushFront),
                    allocator
                        .create_pair(target.clone(), items.into_iter().next().cloned().unwrap()),
                ))
            }
            _ => {}
        }
    }

    let item_sets = elements.into_iter().map(|properties| match properties {
        ArrayLiteralFields::Spread(value) => value,
        ArrayLiteralFields::Items(items) => factory.create_list_term(allocator.create_list(items)),
    });
    Ok(if item_sets.len() >= 2 {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Append),
            allocator.create_list(item_sets),
        )
    } else {
        match item_sets.into_iter().next() {
            Some(value) => value,
            None => factory.create_list_term(allocator.create_empty_list()),
        }
    })
}

fn parse_unary_expression<T: Expression + Rewritable<T>>(
    node: &UnaryExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match node.op {
        UnaryOp::Minus => parse_unary_minus_expression(node, scope, env, factory, allocator),
        UnaryOp::Plus => parse_unary_plus_expression(node, scope, env, factory, allocator),
        UnaryOp::Bang => parse_unary_not_expression(node, scope, env, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_binary_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match node.op {
        BinaryOp::Add => parse_binary_add_expression(node, scope, env, factory, allocator),
        BinaryOp::Sub => parse_binary_subtract_expression(node, scope, env, factory, allocator),
        BinaryOp::Mul => parse_binary_multiply_expression(node, scope, env, factory, allocator),
        BinaryOp::Div => parse_binary_divide_expression(node, scope, env, factory, allocator),
        BinaryOp::Mod => parse_binary_remainder_expression(node, scope, env, factory, allocator),
        BinaryOp::Exp => parse_binary_pow_expression(node, scope, env, factory, allocator),
        BinaryOp::Lt => parse_binary_lt_expression(node, scope, env, factory, allocator),
        BinaryOp::Gt => parse_binary_gt_expression(node, scope, env, factory, allocator),
        BinaryOp::LtEq => parse_binary_lte_expression(node, scope, env, factory, allocator),
        BinaryOp::GtEq => parse_binary_gte_expression(node, scope, env, factory, allocator),
        BinaryOp::EqEq | BinaryOp::EqEqEq => {
            parse_binary_equal_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::NotEq | BinaryOp::NotEqEq => {
            parse_binary_not_equal_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::LogicalAnd => {
            parse_binary_logical_and_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::LogicalOr => {
            parse_binary_logical_or_expression(node, scope, env, factory, allocator)
        }
        BinaryOp::In => parse_binary_in_expression(node, scope, env, factory, allocator),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_unary_minus_expression<T: Expression + Rewritable<T>>(
    node: &UnaryExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let operand = parse_expression(&node.arg, scope, env, factory, allocator)?;
    Ok(if let Some(term) = factory.match_int_term(&operand) {
        factory.create_int_term(-term.value())
    } else if let Some(term) = factory.match_float_term(&operand) {
        factory.create_float_term(-term.value())
    } else {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Subtract),
            allocator.create_pair(factory.create_float_term(0.0), operand),
        )
    })
}

fn parse_unary_plus_expression<T: Expression + Rewritable<T>>(
    node: &UnaryExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let operand = parse_expression(&node.arg, scope, env, factory, allocator)?;
    Ok(if let Some(_) = factory.match_int_term(&operand) {
        operand
    } else if let Some(_) = factory.match_float_term(&operand) {
        operand
    } else {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Add),
            allocator.create_pair(factory.create_float_term(0.0), operand),
        )
    })
}

fn parse_unary_not_expression<T: Expression + Rewritable<T>>(
    node: &UnaryExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let operand = parse_expression(&node.arg, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Not),
        allocator.create_unit_list(operand),
    ))
}

fn parse_binary_add_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Add),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_subtract_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Subtract),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_multiply_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Multiply),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_divide_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Divide),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_remainder_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Remainder),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_pow_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Pow),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_lt_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Lt),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_gt_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Gt),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_lte_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Lte),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_gte_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Gte),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_equal_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Eq),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_not_equal_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let expression = parse_binary_equal_expression(node, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Not),
        allocator.create_unit_list(expression),
    ))
}

fn parse_binary_logical_and_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::And),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_logical_or_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Or),
        allocator.create_pair(left, right),
    ))
}

fn parse_binary_in_expression<T: Expression + Rewritable<T>>(
    node: &BinExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let left = parse_expression(&node.left, scope, env, factory, allocator)?;
    let right = parse_expression(&node.right, scope, env, factory, allocator)?;
    Ok(factory.create_application_term(
        factory.create_builtin_term(Stdlib::Contains),
        allocator.create_pair(right, left),
    ))
}

fn parse_conditional_expression<T: Expression + Rewritable<T>>(
    node: &CondExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let condition = parse_expression(&node.test, scope, env, factory, allocator)?;
    let consequent = parse_expression(&node.cons, scope, env, factory, allocator)?;
    let alternate = parse_expression(&node.alt, scope, env, factory, allocator)?;
    Ok(create_if_expression(
        condition, consequent, alternate, factory, allocator,
    ))
}

fn create_if_expression<T: Expression + Rewritable<T>>(
    condition: T,
    consequent: T,
    alternate: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::If),
        allocator.create_triple(condition, consequent, alternate),
    )
}

fn create_try_catch_expression<T: Expression + Rewritable<T>>(
    body: T,
    handler: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::IfError),
        allocator.create_pair(
            body,
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    handler
                        .substitute_static(
                            &Substitutions::increase_scope_offset(1, 0),
                            factory,
                            allocator,
                            &mut NoopCache::default(),
                        )
                        .unwrap_or(handler),
                    allocator.create_unit_list(factory.create_application_term(
                        global_aggregate_error(factory, allocator),
                        allocator.create_unit_list(factory.create_variable_term(0)),
                    )),
                ),
            ),
        ),
    )
}

fn parse_arrow_function_expression<T: Expression + Rewritable<T>>(
    node: &ArrowExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    if node.is_generator || node.is_async {
        Err(err_unimplemented(node))
    } else {
        let num_args = node.params.len();
        let arg_names = node.params.iter().map(|node| match node {
            Pat::Ident(node) => Some(String::from(parse_identifier(&node.id))),
            _ => None,
        });
        let inner_scope = scope.create_child(arg_names);
        let inner_depth = inner_scope.depth();
        let (initializers, body_scope) = node.params.iter().enumerate().fold(
            Ok((Vec::new(), inner_scope)),
            |result, (arg_index, node)| {
                let (mut combined_initializers, existing_scope) = result?;
                match node {
                    Pat::Ident(_) => Ok((combined_initializers, existing_scope)),
                    Pat::Object(pattern) => {
                        let scope_offset = existing_scope.depth() - inner_depth;
                        let arg =
                            factory.create_variable_term(num_args - arg_index - 1 + scope_offset);
                        let (initializers, child_scope) =
                            parse_object_destructuring_pattern_bindings(
                                arg,
                                &pattern.props,
                                &existing_scope,
                                env,
                                factory,
                                allocator,
                            )?;
                        let next_scope = child_scope.unwrap_or(existing_scope);
                        combined_initializers.extend(initializers);
                        Ok((combined_initializers, next_scope))
                    }
                    Pat::Array(node) => {
                        let scope_offset = existing_scope.depth() - inner_depth;
                        let arg =
                            factory.create_variable_term(num_args - arg_index - 1 + scope_offset);
                        let (initializers, child_scope) =
                            parse_array_destructuring_pattern_bindings(
                                arg,
                                &node.elems,
                                &existing_scope,
                                env,
                                factory,
                                allocator,
                            )?;
                        let next_scope = child_scope.unwrap_or(existing_scope);
                        combined_initializers.extend(initializers);
                        Ok((combined_initializers, next_scope))
                    }
                    Pat::Rest(_) => Err(err_unimplemented(node)),
                    Pat::Assign(_) => Err(err_unimplemented(node)),
                    _ => Err(err_unimplemented(node)),
                }
            },
        )?;
        let body = match &node.body {
            BlockStmtOrExpr::Expr(expression) => {
                let body = vec![Stmt::Expr(ExprStmt {
                    span: node.span,
                    expr: expression.clone(),
                })];
                parse_block(&body, &body_scope, env, factory, allocator)
            }
            BlockStmtOrExpr::BlockStmt(node) => {
                parse_block(&node.stmts, &body_scope, env, factory, allocator)
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

fn parse_member_expression<T: Expression + Rewritable<T>>(
    node: &MemberExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    match &node.obj {
        ExprOrSuper::Expr(target) => {
            let target = parse_expression(target, scope, env, factory, allocator)?;
            let field_name = parse_static_member_field_name(node)?;
            match field_name {
                Some(field_name) => Ok(get_static_field(target, &field_name, factory, allocator)),
                None => {
                    let field = parse_expression(&node.prop, scope, env, factory, allocator)?;
                    Ok(get_dynamic_field(target, field, factory, allocator))
                }
            }
        }
        ExprOrSuper::Super(_) => Err(err_unimplemented(node)),
    }
}

fn parse_static_member_field_name(node: &MemberExpr) -> ParserResult<Option<String>> {
    Ok(match &*node.prop {
        Expr::Ident(name) => Some(String::from(parse_identifier(&name))),
        Expr::Lit(name) => match name {
            Lit::Str(name) => Some(parse_string(&name)),
            _ => None,
        },
        _ => None,
    })
}

fn get_static_field<T: Expression + Rewritable<T>>(
    target: T,
    field: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    let field = factory.create_string_term(allocator.create_string(field));
    get_dynamic_field(target, field, factory, allocator)
}

fn get_dynamic_field<T: Expression + Rewritable<T>>(
    target: T,
    field: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::Get),
        allocator.create_pair(target, field),
    )
}

fn get_indexed_field<T: Expression + Rewritable<T>>(
    target: T,
    index: usize,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    get_dynamic_field(
        target,
        factory.create_int_term(index as i32),
        factory,
        allocator,
    )
}

fn parse_call_expression<T: Expression + Rewritable<T>>(
    node: &CallExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let static_dispatch = match &node.callee {
        ExprOrSuper::Expr(callee) => match &**callee {
            Expr::Member(callee) => match &callee.obj {
                ExprOrSuper::Expr(target) => {
                    let method_name = parse_static_member_field_name(callee)?;
                    match method_name {
                        Some(method_name) => Some(parse_static_method_call_expression(
                            target,
                            &method_name,
                            &node.args,
                            scope,
                            env,
                            factory,
                            allocator,
                        )?),
                        None => None,
                    }
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    };
    match static_dispatch {
        Some(expression) => Ok(expression),
        None => match &node.callee {
            ExprOrSuper::Expr(callee) => {
                let callee = parse_expression(callee, scope, env, factory, allocator)?;
                parse_function_application_expression(
                    callee, &node.args, scope, env, factory, allocator,
                )
            }
            ExprOrSuper::Super(_) => Err(err_unimplemented(node)),
        },
    }
}

fn parse_static_method_call_expression<T: Expression + Rewritable<T>>(
    target: &Expr,
    method_name: &str,
    args: &[ExprOrSpread],
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let target = parse_expression(target, scope, env, factory, allocator)?;
    let is_potential_builtin_method =
        get_builtin_field(None, method_name, factory, allocator).is_some();
    if is_potential_builtin_method {
        let method = factory.create_string_term(allocator.create_string(method_name));
        let num_args = args.len();
        let method_args = parse_expressions(
            args.iter().map(|arg| &*arg.expr),
            scope,
            env,
            factory,
            allocator,
        )?;
        let dynamic_fallback = parse_function_application_expression(
            get_static_field(target.clone(), method_name, factory, allocator),
            args,
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
            factory.create_builtin_term(JsStdlib::Dispatch),
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

fn parse_function_application_expression<T: Expression + Rewritable<T>>(
    target: T,
    args: &[ExprOrSpread],
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let num_args = args.len();
    let (args, spread) = args.into_iter().fold(
        (Vec::with_capacity(num_args), None),
        |(mut args, spread), node| {
            if node.spread.is_some() {
                (args, Some(&*node.expr))
            } else {
                args.push(&*node.expr);
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
            factory.create_builtin_term(Stdlib::Apply),
            allocator.create_pair(target, spread),
        ))
    } else {
        Ok(factory.create_application_term(target, allocator.create_list(args)))
    }
}

fn parse_constructor_expression<T: Expression + Rewritable<T>>(
    node: &NewExpr,
    scope: &LexicalScope,
    env: &Env<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let target = parse_expression(&node.callee, scope, env, factory, allocator);
    let args = (&node.args).iter().flat_map(|args| {
        args.iter().map(|arg| {
            if arg.spread.is_some() {
                Err(err_unimplemented(arg))
            } else {
                parse_expression(&arg.expr, scope, env, factory, allocator)
            }
        })
    });
    Ok(factory.create_application_term(
        factory.create_builtin_term(JsStdlib::Construct),
        allocator.create_list(once(target).chain(args).collect::<ParserResult<Vec<_>>>()?),
    ))
}

#[cfg(test)]
mod tests {
    use std::{iter::once, path::Path};

    use crate::{
        builtins::JsBuiltins,
        globals::{builtin_globals, global_error},
        imports::builtin_imports,
        static_module_loader,
        stdlib::Stdlib as JsStdlib,
        Env,
    };
    use reflex::{
        cache::SubstitutionCache,
        core::{
            create_record, evaluate, ConditionListType, ConditionType, DependencyList,
            EvaluationResult, Expression, ExpressionFactory, ExpressionListType, HeapAllocator,
            InstructionPointer, RecordTermType, SignalTermType, SignalType, StateCache,
            StringValue,
        },
        env::inject_env_vars,
    };
    use reflex_interpreter::{
        compiler::{hash_compiled_program, Compiler, CompilerMode, CompilerOptions},
        execute, DefaultInterpreterCache, InterpreterOptions,
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    fn get_combined_errors<T: Expression>(
        messages: impl IntoIterator<Item = String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Vec<String> {
        let combined_signal = factory.create_signal_term(allocator.create_signal_list(
            messages.into_iter().map(|message| {
                allocator.create_signal(
                    SignalType::Error,
                    allocator.create_unit_list(create_record(
                        once((
                            factory.create_string_term(allocator.create_static_string("name")),
                            factory.create_string_term(allocator.create_static_string("Error")),
                        ))
                        .chain(once((
                            factory.create_string_term(allocator.create_static_string("message")),
                            factory.create_string_term(allocator.create_string(message)),
                        ))),
                        factory,
                        allocator,
                    )),
                )
            }),
        ));
        let signals = factory
            .match_signal_term(&combined_signal)
            .unwrap()
            .signals()
            .as_deref();
        signals
            .iter()
            .map(|item| item.as_deref())
            .map(|signal| {
                let message = factory
                    .match_record_term(
                        signal
                            .as_deref()
                            .args()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .next()
                            .unwrap(),
                    )
                    .unwrap()
                    .get(&factory.create_string_term(allocator.create_static_string("message")))
                    .map(|item| item.as_deref())
                    .unwrap();
                String::from(
                    factory
                        .match_string_term(message)
                        .unwrap()
                        .value()
                        .as_deref()
                        .as_str(),
                )
            })
            .collect()
    }

    fn create_error_instance<T: Expression>(
        payload: T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T
    where
        T::Builtin: From<Stdlib> + From<JsStdlib>,
    {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::ResolveDeep),
            allocator.create_unit_list(factory.create_application_term(
                factory.create_builtin_term(JsStdlib::Construct),
                allocator.create_pair(global_error(factory, allocator), payload),
            )),
        )
    }

    #[test]
    fn null_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("null", &env, &factory, &allocator),
            Ok(factory.create_nil_term()),
        );
    }

    #[test]
    fn boolean_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true", &env, &factory, &allocator),
            Ok(factory.create_boolean_term(true)),
        );
        assert_eq!(
            parse("false", &env, &factory, &allocator),
            Ok(factory.create_boolean_term(false)),
        );
    }

    #[test]
    fn string_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("''", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string(""))),
        );
        assert_eq!(
            parse("\"\"", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string(""))),
        );
        assert_eq!(
            parse("'foo'", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("foo"))),
        );
        assert_eq!(
            parse("\"foo\"", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("foo"))),
        );
        assert_eq!(
            parse("'\"'", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("\""))),
        );
        assert_eq!(
            parse("'\\\"'", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("\""))),
        );
        assert_eq!(
            parse("\"\\\"\"", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("\""))),
        );
    }

    #[test]
    fn numeric_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("0", &env, &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("3", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.0)),
        );
        assert_eq!(
            parse("0.0", &env, &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("3.142", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.142)),
        );
        assert_eq!(
            parse("0.000", &env, &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("-0", &env, &factory, &allocator),
            Ok(factory.create_float_term(-0.0)),
        );
        assert_eq!(
            parse("-3", &env, &factory, &allocator),
            Ok(factory.create_float_term(-3.0)),
        );
        assert_eq!(
            parse("-0.0", &env, &factory, &allocator),
            Ok(factory.create_float_term(-0.0)),
        );
        assert_eq!(
            parse("-3.142", &env, &factory, &allocator),
            Ok(factory.create_float_term(-3.142)),
        );
        assert_eq!(
            parse("+0", &env, &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("+3", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.0)),
        );
        assert_eq!(
            parse("+0.0", &env, &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("+3.142", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.142)),
        );
    }

    #[test]
    fn template_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("``", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string(""))),
        );
        assert_eq!(
            parse("`foo`", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("foo"))),
        );
        assert_eq!(
            parse("`\"`", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_static_string("\""))),
        );
        assert_eq!(
            parse("`\\\"`", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_string(String::from("\"")))),
        );
        assert_eq!(
            parse("`\\\\\"`", &env, &factory, &allocator),
            Ok(factory.create_string_term(allocator.create_string(String::from("\\\"")))),
        );
        assert_eq!(
            parse("`${'foo'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(JsStdlib::ToString),
                allocator.create_unit_list(
                    factory.create_string_term(allocator.create_static_string("foo"))
                ),
            )),
        );
        assert_eq!(
            parse("`foo${'bar'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("bar"))
                        ),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}bar`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("foo"))
                        ),
                    ),
                    factory.create_string_term(allocator.create_static_string("bar")),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("foo"))
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("bar"))
                        ),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`foo${'bar'}baz`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("bar"))
                        ),
                    ),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}bar${'baz'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("foo"))
                        ),
                    ),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("baz"))
                        ),
                    ),
                ]),
            )),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}${'baz'}`", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("foo"))
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("bar"))
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("baz"))
                        ),
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
                factory.create_builtin_term(Stdlib::Concat),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("one"))
                        ),
                    ),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("two"))
                        ),
                    ),
                    factory.create_string_term(allocator.create_static_string("baz")),
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::ToString),
                        allocator.create_unit_list(
                            factory.create_string_term(allocator.create_static_string("three"))
                        ),
                    ),
                ]),
            ))
        )
    }

    #[test]
    fn object_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("({})", &env, &factory, &allocator),
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_empty_list()),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("({ foo: 3, bar: 4, baz: 5 })", &env, &factory, &allocator),
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ])),
                allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ])),
                allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ])),
                allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("3")),
                    factory.create_string_term(allocator.create_static_string("4")),
                    factory.create_string_term(allocator.create_static_string("5")),
                ])),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("3")),
                    factory.create_string_term(allocator.create_static_string("4")),
                    factory.create_string_term(allocator.create_static_string("5")),
                ])),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("3")),
                    factory.create_string_term(allocator.create_static_string("4")),
                    factory.create_string_term(allocator.create_static_string("5")),
                ])),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("3")),
                    factory.create_string_term(allocator.create_static_string("4")),
                    factory.create_string_term(allocator.create_static_string("5")),
                ])),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
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
            Ok(factory.create_record_term(
                allocator.create_struct_prototype(allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("1.1")),
                    factory.create_string_term(allocator.create_static_string("1.2")),
                    factory.create_string_term(allocator.create_static_string("1.3")),
                ])),
                allocator.create_list([
                    factory.create_string_term(allocator.create_static_string("foo")),
                    factory.create_string_term(allocator.create_static_string("bar")),
                    factory.create_string_term(allocator.create_static_string("baz")),
                ]),
            )),
        );
    }

    #[test]
    fn object_spread() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            factory.create_builtin_term(Stdlib::CollectList),
            allocator.create_list([
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("first")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("second")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("third")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("fourth")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("fifth")),
                    ),
                ),
            ]),
        );
        let result = evaluate(
            &query,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(6.0),
                    factory.create_float_term(2.0),
                    factory.create_float_term(7.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
                ])),
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
            factory.create_builtin_term(Stdlib::CollectList),
            allocator.create_list([
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("first")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("second")),
                    ),
                ),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        expression.clone(),
                        factory.create_string_term(allocator.create_static_string("third")),
                    ),
                ),
            ]),
        );
        let result = evaluate(
            &query,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(1.0),
                    factory.create_float_term(2.0),
                    factory.create_float_term(3.0),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn array_literals() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("[]", &env, &factory, &allocator),
            Ok(factory.create_list_term(allocator.create_empty_list())),
        );
        assert_eq!(
            parse("[3, 4, 5]", &env, &factory, &allocator),
            Ok(factory.create_list_term(allocator.create_list([
                factory.create_float_term(3.0),
                factory.create_float_term(4.0),
                factory.create_float_term(5.0),
            ]))),
        );
    }

    #[test]
    fn array_access() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(4.0), DependencyList::empty())
        );
    }

    #[test]
    fn array_spread() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("[...[3, 4, 5], 6]", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Push),
                allocator.create_pair(
                    factory.create_list_term(allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ])),
                    factory.create_float_term(6.0),
                ),
            ))
        );
        assert_eq!(
            parse("[3, ...[4, 5, 6]]", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::PushFront),
                allocator.create_pair(
                    factory.create_list_term(allocator.create_list([
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                        factory.create_float_term(6.0),
                    ])),
                    factory.create_float_term(3.0),
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
            factory.create_builtin_term(Stdlib::Collect),
            allocator.create_unit_list(expression),
        );
        let result = evaluate(
            &query,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(1.0),
                    factory.create_float_term(2.0),
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
                    factory.create_float_term(6.0),
                    factory.create_float_term(7.0),
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
            factory.create_builtin_term(Stdlib::Collect),
            allocator.create_unit_list(expression),
        );
        let result = evaluate(
            &query,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(1.0),
                    factory.create_float_term(2.0),
                    factory.create_float_term(3.0),
                ])),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn array_methods() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
                factory.create_builtin_term(JsStdlib::Dispatch),
                allocator.create_list([
                    factory.create_list_term(allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ])),
                    factory.create_string_term(allocator.create_static_string("map")),
                    factory.create_application_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_list_term(allocator.create_list([
                                    factory.create_float_term(3.0),
                                    factory.create_float_term(4.0),
                                    factory.create_float_term(5.0),
                                ])),
                                factory.create_string_term(allocator.create_static_string("map")),
                            ),
                        ),
                        allocator.create_unit_list(factory.create_lambda_term(
                            1,
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Multiply),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_float_term(2.0),
                                ),
                            ),
                        )),
                    ),
                    factory.create_lambda_term(
                        1,
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Multiply),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_float_term(2.0),
                            ),
                        ),
                    ),
                ]),
            )),
        );
    }

    #[test]
    fn parenthesized_expressions() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("(3)", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.0)),
        );
        assert_eq!(
            parse("(((3)))", &env, &factory, &allocator),
            Ok(factory.create_float_term(3.0)),
        );
    }

    #[test]
    fn modules() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            Ok(factory.create_float_term(3.0)),
        );
    }

    #[test]
    fn variable_declarations() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const foo = 3; foo;", &env, &factory, &allocator),
            Ok(factory.create_let_term(
                factory.create_float_term(3.0),
                factory.create_variable_term(0),
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
                factory.create_float_term(3.0),
                factory.create_let_term(
                    factory.create_float_term(4.0),
                    factory.create_variable_term(1),
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
                factory.create_float_term(3.0),
                factory.create_let_term(
                    factory.create_float_term(4.0),
                    factory.create_variable_term(0),
                ),
            )),
        );
    }

    #[test]
    fn variable_declaration_object_destructuring() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const {} = {}; true;", &env, &factory, &allocator),
            Ok(factory.create_boolean_term(true)),
        );
        assert_eq!(
            parse(
                "const {} = { foo: 3, bar: 4, baz: 5 }; true;",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_boolean_term(true)),
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
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        factory.create_record_term(
                            allocator.create_struct_prototype(allocator.create_list([
                                factory.create_string_term(allocator.create_static_string("foo")),
                                factory.create_string_term(allocator.create_static_string("bar")),
                                factory.create_string_term(allocator.create_static_string("baz")),
                            ])),
                            allocator.create_list([
                                factory.create_float_term(3.0),
                                factory.create_float_term(4.0),
                                factory.create_float_term(5.0),
                            ]),
                        ),
                        factory.create_string_term(allocator.create_static_string("foo")),
                    ),
                ),
                factory.create_boolean_term(true)
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_variable_term(0),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_variable_term(1),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_variable_term(1),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                        factory.create_string_term(allocator.create_static_string("baz")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_static_string("bar")),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_variable_term(1),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("first")),
                        factory.create_string_term(allocator.create_static_string("second")),
                        factory.create_string_term(allocator.create_static_string("third")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_string_term(
                                    allocator.create_static_string("first"),
                                ),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_string_term(
                                        allocator.create_static_string("second"),
                                    ),
                                ),
                            ),
                            factory.create_variable_term(1),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("first")),
                        factory.create_string_term(allocator.create_static_string("second")),
                        factory.create_string_term(allocator.create_static_string("third")),
                    ])),
                    allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_boolean_term(true),
                    factory.create_let_term(
                        factory.create_variable_term(1),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_string_term(
                                        allocator.create_static_string("first"),
                                    ),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(1),
                                        factory.create_string_term(
                                            allocator.create_static_string("second"),
                                        ),
                                    ),
                                ),
                                factory.create_variable_term(1),
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
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("one")),
                        factory.create_string_term(allocator.create_static_string("two")),
                    ])),
                    allocator.create_list([
                        factory.create_record_term(
                            allocator.create_struct_prototype(allocator.create_list([
                                factory.create_string_term(allocator.create_static_string("a")),
                                factory.create_string_term(allocator.create_static_string("b")),
                            ])),
                            allocator.create_list([
                                factory.create_float_term(1.0),
                                factory.create_float_term(2.0),
                            ]),
                        ),
                        factory.create_record_term(
                            allocator.create_struct_prototype(allocator.create_list([
                                factory.create_string_term(allocator.create_static_string("c")),
                                factory.create_string_term(allocator.create_static_string("d")),
                            ])),
                            allocator.create_list([
                                factory.create_float_term(3.0),
                                factory.create_float_term(4.0),
                            ]),
                        ),
                    ]),
                ),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_string(
                                "one"
                            )),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_string_term(allocator.create_string(
                                    "two"
                                )),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_variable_term(1),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(allocator.create_static_string("a")),
                                    ),
                                ),
                                factory.create_let_term(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_string_term(allocator.create_static_string("b")),
                                        ),
                                    ),
                                    factory.create_let_term(
                                        factory.create_variable_term(3),
                                        factory.create_let_term(
                                            factory.create_application_term(
                                                factory.create_builtin_term(Stdlib::Get),
                                                allocator.create_pair(
                                                    factory.create_variable_term(0),
                                                    factory.create_string_term(allocator.create_static_string("c")),
                                                ),
                                            ),
                                            factory.create_let_term(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Stdlib::Get),
                                                    allocator.create_pair(
                                                        factory.create_variable_term(1),
                                                        factory.create_string_term(allocator.create_static_string("d")),
                                                    ),
                                                ),
                                                factory.create_variable_term(4),
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
                        factory.create_variable_term(0),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_string_term(allocator.create_static_string("foo")),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(1),
                                        factory.create_string_term(allocator.create_static_string("bar")),
                                    ),
                                ),
                                factory.create_variable_term(0),
                            ),
                        ),
                    ),
                ),
                allocator.create_unit_list(factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("foo")),
                        factory.create_string_term(allocator.create_static_string("bar")),
                    ])),
                    allocator.create_list([
                        factory.create_boolean_term(false),
                        factory.create_boolean_term(true),
                    ]),
                )),
            )),
        );
    }

    #[test]
    fn variable_declaration_array_destructuring() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("const [] = [3, 4, 5]; true;", &env, &factory, &allocator),
            Ok(factory.create_boolean_term(true)),
        );
        assert_eq!(
            parse("const [foo] = [3, 4, 5]; foo;", &env, &factory, &allocator),
            Ok(factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        factory.create_list_term(allocator.create_list([
                            factory.create_float_term(3.0),
                            factory.create_float_term(4.0),
                            factory.create_float_term(5.0),
                        ])),
                        factory.create_int_term(0),
                    ),
                ),
                factory.create_variable_term(0),
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
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
                ])),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_int_term(1),
                            ),
                        ),
                        factory.create_variable_term(1),
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
                    factory.create_builtin_term(Stdlib::Get),
                    allocator.create_pair(
                        factory.create_list_term(allocator.create_list([
                            factory.create_float_term(3.0),
                            factory.create_float_term(4.0),
                            factory.create_float_term(5.0),
                        ])),
                        factory.create_int_term(2),
                    ),
                ),
                factory.create_variable_term(0),
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
                factory.create_list_term(allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
                ])),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_int_term(1),
                        ),
                    ),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_int_term(2),
                            ),
                        ),
                        factory.create_variable_term(1),
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
                factory.create_boolean_term(true),
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_list_term(allocator.create_list([
                                factory.create_float_term(3.0),
                                factory.create_float_term(4.0),
                                factory.create_float_term(5.0),
                            ])),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_variable_term(1),
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
                factory.create_boolean_term(true),
                factory.create_let_term(
                    factory.create_list_term(allocator.create_list([
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                        factory.create_float_term(5.0),
                    ])),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_int_term(1),
                                ),
                            ),
                            factory.create_variable_term(3),
                        ),
                    )
                )
            )),
        );
    }

    #[test]
    fn variable_scoping() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_float_term(3.0 * 2.0),
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_float_term(3.0 * 2.0),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn variable_dependencies() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
    }

    #[test]
    fn not_expressions() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("!true", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Not),
                allocator.create_unit_list(factory.create_boolean_term(true)),
            )),
        );
        assert_eq!(
            parse("!false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Not),
                allocator.create_unit_list(factory.create_boolean_term(false)),
            )),
        );
        assert_eq!(
            parse("!3", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Not),
                allocator.create_unit_list(factory.create_float_term(3.0)),
            )),
        );
    }

    #[test]
    fn arithmetic_expressions() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("3 + 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Add),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 - 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Subtract),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 * 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Multiply),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 / 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Divide),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 % 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Remainder),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 ** 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Pow),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 < 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Lt),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 <= 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Lte),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 > 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Gt),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
        assert_eq!(
            parse("3 >= 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Gte),
                allocator.create_pair(
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
    }

    #[test]
    fn equality_expression() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true === false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Eq),
                allocator.create_pair(
                    factory.create_boolean_term(true),
                    factory.create_boolean_term(false),
                ),
            )),
        );
        assert_eq!(
            parse("true !== false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Not),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Eq),
                    allocator.create_pair(
                        factory.create_boolean_term(true),
                        factory.create_boolean_term(false),
                    ),
                )),
            )),
        );
    }

    #[test]
    fn logical_expression() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true && false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::And),
                allocator.create_pair(
                    factory.create_boolean_term(true),
                    factory.create_boolean_term(false),
                ),
            )),
        );
        assert_eq!(
            parse("true || false", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Or),
                allocator.create_pair(
                    factory.create_boolean_term(true),
                    factory.create_boolean_term(false),
                ),
            )),
        );
    }

    #[test]
    fn conditional_expression() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("true ? 3 : 4", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::If),
                allocator.create_triple(
                    factory.create_boolean_term(true),
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                ),
            )),
        );
    }

    #[test]
    fn if_statements() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
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
                        factory.create_builtin_term(Stdlib::If),
                        allocator.create_triple(
                            factory.create_boolean_term(true),
                            factory.create_float_term(3.0),
                            factory.create_float_term(4.0),
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
                        factory.create_builtin_term(Stdlib::If),
                        allocator.create_triple(
                            factory.create_boolean_term(true),
                            factory.create_application_term(
                                factory.create_builtin_term(JsStdlib::Throw),
                                allocator.create_unit_list(
                                    create_error_instance(
                                        factory.create_string_term(allocator.create_static_string("foo")),
                                        &factory,
                                        &allocator,
                                    )
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(JsStdlib::Throw),
                                allocator.create_unit_list(
                                    create_error_instance(
                                        factory.create_string_term(allocator.create_static_string("bar")),
                                        &factory,
                                        &allocator,
                                    )
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
                        factory.create_builtin_term(Stdlib::If),
                        allocator.create_triple(
                            factory.create_boolean_term(true),
                            factory.create_let_term(
                                factory.create_float_term(3.0),
                                factory.create_let_term(
                                    factory.create_float_term(4.0),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_float_term(4.0),
                                factory.create_let_term(
                                    factory.create_float_term(3.0),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_variable_term(0),
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
                        factory.create_builtin_term(Stdlib::If),
                        allocator.create_triple(
                            factory.create_boolean_term(true),
                            factory.create_let_term(
                                factory.create_float_term(3.0),
                                factory.create_let_term(
                                    factory.create_float_term(4.0),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_variable_term(0),
                                        ),
                                    ),
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_float_term(4.0),
                                factory.create_let_term(
                                    factory.create_float_term(3.0),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_variable_term(0),
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
                        factory.create_builtin_term(Stdlib::If),
                        allocator.create_triple(
                            factory.create_boolean_term(true),
                            factory.create_application_term(
                                factory.create_builtin_term(JsStdlib::Throw),
                                allocator.create_unit_list(
                                    create_error_instance(
                                        factory.create_string_term(allocator.create_static_string("foo")),
                                        &factory,
                                        &allocator,
                                    )
                                ),
                            ),
                            factory.create_let_term(
                                factory.create_float_term(3.0),
                                factory.create_let_term(
                                    factory.create_float_term(4.0),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(1),
                                            factory.create_variable_term(0),
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
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        assert_eq!(
            parse("throw new Error(\"foo\")", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(JsStdlib::Throw),
                allocator.create_unit_list(create_error_instance(
                    factory.create_string_term(allocator.create_static_string("foo")),
                    &factory,
                    &allocator
                )),
            )),
        );
        assert_eq!(
            parse("throw new Error(`foo${'bar'}`)", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(JsStdlib::Throw),
                allocator.create_unit_list(create_error_instance(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Concat),
                        allocator.create_pair(
                            factory.create_string_term(allocator.create_static_string("foo")),
                            factory.create_application_term(
                                factory.create_builtin_term(JsStdlib::ToString),
                                allocator.create_unit_list(
                                    factory
                                        .create_string_term(allocator.create_static_string("bar"))
                                ),
                            ),
                        ),
                    ),
                    &factory,
                    &allocator
                )),
            )),
        );
    }

    #[test]
    fn try_catch_statements() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let expression = parse(
            "(() => {
                try {
                    return 3;
                } catch {
                    return 4;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
        let expression = parse(
            "(() => {
                try {
                    throw new Error('foo');
                } catch {
                    return 3;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
        let expression = parse(
            "(() => {
                try {
                    return 3;
                } catch (error) {
                    return 4;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
        let expression = parse(
            "((x) => {
                try {
                    return x;
                } catch (error) {
                    return 4;
                }
            })(3)",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
        let expression = parse(
            "(() => {
                try {
                    throw new Error('foo');
                } catch (error) {
                    return 3;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
        let expression = parse(
            "(() => {
                try {
                    throw new Error('foo');
                } catch (error) {
                    return error.message;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(allocator.create_static_string("foo")),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(() => {
                try {
                    throw new Error('foo');
                } catch (error) {
                    return error.errors[0].message;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(allocator.create_static_string("foo")),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(() => {
                try {
                    const foo = (() => { throw new Error('foo'); })();
                    const bar = (() => { throw new Error('bar'); })();
                    return foo + bar;
                } catch (error) {
                    return error.message;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(
                    allocator.create_string(
                        get_combined_errors(
                            vec![String::from("foo"), String::from("bar")],
                            &factory,
                            &allocator
                        )
                        .join("\n")
                    )
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(() => {
                try {
                    const foo = (() => { throw new Error('foo'); })();
                    const bar = (() => { throw new Error('bar'); })();
                    return foo + bar;
                } catch (error) {
                    return error.errors[0].message;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(
                    allocator.create_string(
                        get_combined_errors(
                            vec![String::from("foo"), String::from("bar")],
                            &factory,
                            &allocator
                        )
                        .get(0)
                        .cloned()
                        .unwrap()
                    )
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(() => {
                try {
                    const foo = (() => { throw new Error('foo'); })();
                    const bar = (() => { throw new Error('bar'); })();
                    return foo + bar;
                } catch (error) {
                    return error.errors[1].message;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(
                    allocator.create_string(
                        get_combined_errors(
                            vec![String::from("foo"), String::from("bar")],
                            &factory,
                            &allocator
                        )
                        .get(1)
                        .cloned()
                        .unwrap()
                    )
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(() => {
                try {
                    const foo = (() => { throw new Error('foo'); })();
                    const bar = (() => { throw new Error('bar'); })();
                    return foo + bar;
                } catch (error) {
                    throw error;
                }
            })()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_signal_term(
                    allocator.create_signal_list(
                        vec![String::from("foo"), String::from("bar")]
                            .into_iter()
                            .map(|message| {
                                allocator.create_signal(
                                    SignalType::Error,
                                    allocator.create_unit_list(create_record(
                                        once((
                                            factory.create_string_term(
                                                allocator.create_static_string("name"),
                                            ),
                                            factory.create_string_term(
                                                allocator.create_static_string("Error"),
                                            ),
                                        ))
                                        .chain(once((
                                            factory.create_string_term(
                                                allocator.create_static_string("message"),
                                            ),
                                            factory.create_string_term(
                                                allocator.create_string(message),
                                            ),
                                        ))),
                                        &factory,
                                        &allocator,
                                    )),
                                )
                            }),
                    )
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn in_operator() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        assert_eq!(
            parse("'foo' in { bar: true }", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Contains),
                allocator.create_pair(
                    create_record(
                        once((
                            factory.create_string_term(allocator.create_static_string("bar")),
                            factory.create_boolean_term(true)
                        )),
                        &factory,
                        &allocator
                    ),
                    factory.create_string_term(allocator.create_static_string("foo")),
                ),
            )),
        );
    }

    #[test]
    fn arrow_function_expressions() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("() => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(0, factory.create_float_term(3.0))),
        );
        assert_eq!(
            parse("(foo) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_float_term(3.0))),
        );
        assert_eq!(
            parse("(foo) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_variable_term(0))),
        );
        assert_eq!(
            parse("(foo) => foo + foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Add),
                    allocator.create_pair(
                        factory.create_variable_term(0),
                        factory.create_variable_term(0),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(foo, bar, baz) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                3,
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Add),
                    allocator.create_pair(
                        factory.create_variable_term(2),
                        factory.create_variable_term(1),
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
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_variable_term(2),
                                factory.create_variable_term(1),
                            ),
                        ),
                    ),
                ),
            )),
        );
    }

    #[test]
    fn arrow_function_object_destructuring() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("({}) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_float_term(3.0))),
        );
        assert_eq!(
            parse("({ foo }) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_string_term(allocator.create_static_string("foo")),
                        ),
                    ),
                    factory.create_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("({ foo, bar }) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory
                                        .create_string_term(allocator.create_static_string("bar")),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_variable_term(0),
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
                    factory.create_variable_term(2),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_string_term(allocator.create_static_string("foo")),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory
                                        .create_string_term(allocator.create_static_string("bar")),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_application_term(
                                                factory.create_builtin_term(Stdlib::Add),
                                                allocator.create_pair(
                                                    factory.create_variable_term(6),
                                                    factory.create_variable_term(1),
                                                ),
                                            ),
                                            factory.create_variable_term(0),
                                        ),
                                    ),
                                    factory.create_variable_term(3),
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
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("([]) => 3", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(1, factory.create_float_term(3.0))),
        );
        assert_eq!(
            parse("([foo]) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_application_term(
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("([foo, bar]) => foo + bar", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_int_term(1),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_variable_term(0),
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
                        factory.create_builtin_term(Stdlib::Get),
                        allocator.create_pair(
                            factory.create_variable_term(0),
                            factory.create_int_term(2),
                        ),
                    ),
                    factory.create_variable_term(0),
                ),
            )),
        );
        assert_eq!(
            parse("([, foo, bar]) => foo", &env, &factory, &allocator),
            Ok(factory.create_lambda_term(
                1,
                factory.create_let_term(
                    factory.create_variable_term(0),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(1),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_int_term(2),
                                ),
                            ),
                            factory.create_variable_term(1),
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
                    factory.create_variable_term(2),
                    factory.create_let_term(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_let_term(
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_int_term(1),
                                ),
                            ),
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_application_term(
                                                factory.create_builtin_term(Stdlib::Add),
                                                allocator.create_pair(
                                                    factory.create_variable_term(6),
                                                    factory.create_variable_term(1),
                                                ),
                                            ),
                                            factory.create_variable_term(0),
                                        ),
                                    ),
                                    factory.create_variable_term(3),
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
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("(() => 3)()", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(0, factory.create_float_term(3.0)),
                allocator.create_empty_list(),
            )),
        );
        assert_eq!(
            parse("((foo) => foo)(3)", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(1, factory.create_variable_term(0)),
                allocator.create_unit_list(factory.create_float_term(3.0)),
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
                        factory.create_builtin_term(Stdlib::Add),
                        allocator.create_pair(
                            factory.create_variable_term(2),
                            factory.create_variable_term(1),
                        ),
                    ),
                ),
                allocator.create_list([
                    factory.create_float_term(3.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(5.0),
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
                                        factory.create_builtin_term(Stdlib::Add),
                                        allocator.create_pair(
                                            factory.create_variable_term(2),
                                            factory.create_variable_term(1),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        allocator.create_unit_list(factory.create_float_term(3.0)),
                    ),
                    allocator.create_unit_list(factory.create_float_term(4.0)),
                ),
                allocator.create_unit_list(factory.create_float_term(5.0)),
            )),
        );
    }

    #[test]
    fn function_arg_spreading() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        assert_eq!(
            parse("((x, y) => x + y)(...[3, 4])", &env, &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Apply),
                allocator.create_pair(
                    factory.create_lambda_term(
                        2,
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Add),
                            allocator.create_pair(
                                factory.create_variable_term(1),
                                factory.create_variable_term(0)
                            ),
                        )
                    ),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                    )),
                ),
            )),
        );
        assert_eq!(
            parse(
                "((x, y) => x + y)(1, 2, ...[3, 4])",
                &env,
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::Apply),
                allocator.create_pair(
                    factory.create_partial_application_term(
                        factory.create_lambda_term(
                            2,
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Add),
                                allocator.create_pair(
                                    factory.create_variable_term(1),
                                    factory.create_variable_term(0)
                                ),
                            )
                        ),
                        allocator.create_pair(
                            factory.create_float_term(1.0),
                            factory.create_float_term(2.0),
                        ),
                    ),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_float_term(3.0),
                        factory.create_float_term(4.0),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn recursive_expressions() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty()),
        );
    }

    #[test]
    fn import_scoping() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let path = Path::new("./foo.js");
        let loader = static_module_loader(vec![("foo", factory.create_nil_term())]);
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
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_float_term(3.0), DependencyList::empty())
        );
    }

    #[test]
    fn env_vars() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let env_vars = [(String::from("FOO"), String::from("foo"))];
        let expression = parse("process.env.FOO;", &env, &factory, &allocator)
            .map(|expression| inject_env_vars(expression, env_vars, &factory, &allocator))
            .unwrap();
        let result = evaluate(
            &expression,
            &StateCache::default(),
            &factory,
            &allocator,
            &mut SubstitutionCache::new(),
        );
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(allocator.create_string(String::from("foo"))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn js_interpreted() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let input = "
            const fullName = (first, last) => `${first} ${last}`;
            const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
            greet({ first: 'John', last: 'Doe' })";
        let expression = parse(input, &env, &factory, &allocator).unwrap();
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(allocator.create_static_string("Hello, John Doe!")),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn js_compiled() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new();
        let input = "
            const fullName = (first, last) => `${first} ${last}`;
            const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
            greet({ first: 'John', last: 'Doe' })";
        let expression = parse(input, &env, &factory, &allocator).unwrap();
        let program = Compiler::new(CompilerOptions::unoptimized(), None)
            .compile(&expression, CompilerMode::Function, &factory, &allocator)
            .unwrap();
        let mut cache = DefaultInterpreterCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
        let state = StateCache::default();
        let state_id = 0;
        let (result, _) = execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &state,
            &factory,
            &allocator,
            &InterpreterOptions::default(),
            &mut cache,
        )
        .unwrap();
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_string_term(allocator.create_static_string("Hello, John Doe!")),
                DependencyList::empty(),
            ),
        );
    }
}
