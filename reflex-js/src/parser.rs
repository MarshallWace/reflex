// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{borrow::Cow, iter::once};

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, StackOffset, StaticVariableTerm,
        StructPrototype, StructTerm, Term, VariableTerm,
    },
    hash::Hashable,
    stdlib::{
        builtin::BuiltinTerm,
        collection::{vector::VectorTerm, CollectionTerm},
        value::{StringValue, ValueTerm},
    },
};
use resast::prelude::*;
use ressa::Builder;

use crate::{
    builtins::{construct, dispatch, get_builtin_field, throw, to_string},
    Env, SymbolCache,
};

pub type ParserResult<T> = Result<T, ParserError>;
pub type ParserError = String;

type ScopeBindings<'src> = Vec<(&'src str, Expression)>;

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

pub fn parse<'src>(
    input: &'src str,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let program = parse_ast(input)?;
    let is_module = program.iter().any(|node| match node {
        ProgramPart::Decl(node) => match node {
            Decl::Import(_) | Decl::Export(_) => true,
            _ => false,
        },
        _ => false,
    });
    if is_module {
        parse_module(program.into_iter(), symbol_cache, &env)
    } else {
        parse_script(program.into_iter(), symbol_cache, &env)
    }
}

fn parse_ast(input: &str) -> ParserResult<Vec<ProgramPart>> {
    Builder::new()
        .module(true)
        .js(input)
        .build()
        .and_then(|parser| parser.collect::<Result<Vec<_>, ressa::Error>>())
        .or_else(|error| Err(format!("Parse error: {}", error)))
}

fn parse_script<'src>(
    program: impl IntoIterator<Item = ProgramPart<'src>> + ExactSizeIterator,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
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
    match parse_block(&body, &LexicalScope::new(), symbol_cache, &env)? {
        None => Err(String::from("No expression to evaluate")),
        Some(expression) => Ok(expression),
    }
}

fn parse_module<'src>(
    program: impl IntoIterator<Item = ProgramPart<'src>> + ExactSizeIterator,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let num_statements = program.len();
    let (body, import_bindings) = program.into_iter().fold(
        Ok((Vec::with_capacity(num_statements), Vec::new())),
        |results, node| {
            let (mut body, mut import_bindings) = results?;
            match node {
                ProgramPart::Decl(node) => match node {
                    Decl::Import(node) => {
                        let bindings = parse_module_import(&node, symbol_cache, &env)?;
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
    match parse_block(&body, &scope, symbol_cache, &env)? {
        None => Err(String::from("Missing default module export")),
        Some(expression) => Ok(if import_initializers.is_empty() {
            expression
        } else {
            create_declaration_block(import_initializers, expression)
        }),
    }
}

fn parse_module_import<'src>(
    node: &ModImport<'src>,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Vec<(&'src str, Expression)>> {
    let module_path = match &node.source {
        Lit::String(node) => parse_string(node),
        _ => Err(err_unimplemented(node)),
    }?;
    let module = match env.import(module_path) {
        Some(module) => Ok(module),
        None => Err(err(&format!("Invalid import: '{}'", module_path), node)),
    }?;
    node.specifiers
        .iter()
        .fold(Ok(Vec::new()), |bindings, specifier| {
            let mut bindings = bindings?;
            let binding = match specifier {
                ImportSpecifier::Default(node) => {
                    let identifier = parse_identifier(node)?;
                    let value =
                        get_static_field(Expression::clone(&module), "default", symbol_cache);
                    (identifier, value)
                }
                ImportSpecifier::Namespace(node) => {
                    let identifier = parse_identifier(node)?;
                    let value = Expression::clone(&module);
                    (identifier, value)
                }
                ImportSpecifier::Normal(node) => {
                    let imported_field = parse_identifier(&node.imported)?;
                    let identifier = parse_identifier(&node.local)?;
                    let value =
                        get_static_field(Expression::clone(&module), imported_field, symbol_cache);
                    (identifier, value)
                }
            };
            bindings.push(binding);
            Ok(bindings)
        })
}

fn parse_block<'src: 'temp, 'temp>(
    body: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Option<Expression>> {
    parse_block_statements(body, None, scope, symbol_cache, env)
}

fn parse_block_statements<'src: 'temp, 'temp>(
    remaining: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    result: Option<Expression>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Option<Expression>> {
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
                    let expression = parse_expression(&value, &scope, symbol_cache, env)?;
                    let result = Some(expression);
                    parse_block_statements(remaining, result, scope, symbol_cache, env)
                }
                ProgramPart::Decl(node) => match node {
                    Decl::Var(kind, declarators) => match kind {
                        VarKind::Const => {
                            let bindings =
                                parse_variable_declarators(declarators, &scope, symbol_cache, env)?;
                            let (keys, initializers): (Vec<_>, Vec<_>) =
                                bindings.into_iter().unzip();
                            let child_scope = scope.create_child(keys.into_iter().map(Some));
                            let body = parse_block_statements(
                                remaining,
                                result,
                                &child_scope,
                                symbol_cache,
                                env,
                            )?;
                            match body {
                                None => Ok(None),
                                Some(body) => Ok(Some(initializers.into_iter().fold(
                                    body,
                                    |body, initializer| {
                                        create_declaration_block(vec![initializer], body)
                                    },
                                ))),
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
                            let expression = parse_expression(node, &scope, symbol_cache, env)?;
                            let result = Some(expression);
                            parse_block_statements(remaining, result, scope, symbol_cache, env)
                        }
                    },
                    Stmt::Throw(node) => {
                        let expression = parse_throw_statement(node, &scope, symbol_cache, env)?;
                        let result = Some(expression);
                        parse_block_statements(remaining, result, scope, symbol_cache, env)
                    }
                    Stmt::If(node) => {
                        let condition = parse_expression(&node.test, scope, symbol_cache, env)?;
                        let consequent =
                            parse_if_branch(&node.consequent, scope, symbol_cache, env)?;
                        match &node.alternate {
                            Some(node) => {
                                let alternate = parse_if_branch(&node, scope, symbol_cache, env)?;
                                let expression =
                                    create_if_expression(condition, consequent, alternate);
                                let result = Some(expression);
                                parse_block_statements(remaining, result, scope, symbol_cache, env)
                            }
                            None => {
                                let alternate =
                                    parse_branch(&statement, remaining, scope, symbol_cache, env)?;
                                let result = create_if_expression(condition, consequent, alternate);
                                Ok(Some(result))
                            }
                        }
                    }
                    Stmt::Try(_) => Err(err_unimplemented(statement)),
                    Stmt::Switch(_) => Err(err_unimplemented(statement)),
                    Stmt::Empty => {
                        parse_block_statements(remaining, result, scope, symbol_cache, env)
                    }
                    _ => Err(err_unimplemented(statement)),
                },
            }
        }
    }
}

fn create_declaration_block(initializers: Vec<Expression>, body: Expression) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Lambda(LambdaTerm::new(
            Arity::from(0, initializers.len() as u8, None),
            body,
        ))),
        initializers,
    )))
}

fn parse_variable_declarators<'src>(
    declarators: &[VarDecl<'src>],
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<ScopeBindings<'src>> {
    declarators.iter().fold(Ok(Vec::new()), |results, node| {
        let mut results = results?;
        let bindings = parse_variable_declarator(node, &scope, symbol_cache, env)?;
        results.extend(bindings);
        Ok(results)
    })
}

fn parse_variable_declarator<'src>(
    node: &VarDecl<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<ScopeBindings<'src>> {
    let VarDecl { id, init } = node;
    let value = match init {
        Some(expression) => parse_expression(expression, scope, symbol_cache, env),
        None => Err(err("Missing variable initializer", node)),
    }?;
    match id {
        Pat::Ident(node) => {
            let identifier = parse_identifier(node)?;
            let bindings = vec![(identifier, value)];
            Ok(bindings)
        }
        Pat::Obj(properties) => parse_variable_object_destructuring_pattern(
            &value,
            properties,
            scope,
            symbol_cache,
            env,
        ),
        Pat::Array(_) => Err(err_unimplemented(node)),
        Pat::RestElement(_) => Err(err_unimplemented(node)),
        Pat::Assign(_) => Err(err_unimplemented(node)),
    }
}

fn parse_variable_object_destructuring_pattern<'src>(
    target: &Expression,
    properties: &[ObjPatPart<'src>],
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<ScopeBindings<'src>> {
    properties
        .iter()
        .map(|property| match property {
            ObjPatPart::Assign(node) => {
                if node.computed {
                    Err(err_unimplemented(node))
                } else {
                    let field_name = parse_destructuring_pattern_property_field_name(
                        node,
                        scope,
                        symbol_cache,
                        env,
                    )?;
                    let identifier = parse_destructuring_pattern_property_identifier(node)?;
                    let value = get_dynamic_field(Expression::clone(target), field_name);
                    Ok((identifier, value))
                }
            }
            ObjPatPart::Rest(node) => Err(err_unimplemented(node)),
        })
        .collect()
}

fn parse_destructuring_pattern_property_field_name<'src>(
    node: &Prop<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match &node.key {
        PropKey::Lit(key) => match key {
            Lit::String(key) => {
                let field_name = parse_string(key)?;
                Ok(Expression::new(Term::Value(ValueTerm::Symbol(
                    symbol_cache.get(String::from(field_name)),
                ))))
            }
            _ => parse_literal(key, scope, symbol_cache, env),
        },
        PropKey::Pat(node) => match node {
            Pat::Ident(node) => {
                let field_name = parse_identifier(node)?;
                Ok(Expression::new(Term::Value(ValueTerm::Symbol(
                    symbol_cache.get(String::from(field_name)),
                ))))
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

fn parse_identifier<'src>(node: &Ident<'src>) -> ParserResult<&'src str> {
    match node.name {
        Cow::Borrowed(value) => Ok(value),
        Cow::Owned(_) => Err(err("Invalid identifier", node)),
    }
}

fn parse_throw_statement<'src>(
    value: &Expr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
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
                            .map(|arg| parse_expression(arg, scope, symbol_cache, env))
                            .next()
                            .unwrap()?;
                        Ok(Expression::new(Term::Application(ApplicationTerm::new(
                            throw(),
                            vec![message],
                        ))))
                    }
                }
                _ => Err(err_unimplemented(node)),
            },
            _ => Err(err_unimplemented(value)),
        },
        _ => Err(err_unimplemented(value)),
    }
}

fn parse_if_branch<'src>(
    node: &Stmt<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node {
        Stmt::Block(block) => {
            let BlockStmt(body) = block;
            parse_branch(node, body, scope, symbol_cache, env)
        }
        _ => parse_branch(
            node,
            &vec![ProgramPart::Stmt(node.clone())],
            scope,
            symbol_cache,
            env,
        ),
    }
}

fn parse_branch<'src: 'temp, 'temp>(
    node: &Stmt<'src>,
    body: impl IntoIterator<Item = &'temp ProgramPart<'src>>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let expression = parse_block(body, scope, symbol_cache, env)?;
    match expression {
        None => Err(err("Unterminated branch", node)),
        Some(expression) => Ok(expression),
    }
}

fn parse_expression<'src>(
    node: &Expr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node {
        Expr::Ident(node) => parse_variable_reference(node, scope, env),
        Expr::Lit(node) => parse_literal(node, scope, symbol_cache, env),
        Expr::Unary(node) => parse_unary_expression(node, scope, symbol_cache, env),
        Expr::Binary(node) => parse_binary_expression(node, scope, symbol_cache, env),
        Expr::Logical(node) => parse_logical_expression(node, scope, symbol_cache, env),
        Expr::Conditional(node) => parse_conditional_expression(node, scope, symbol_cache, env),
        Expr::ArrowFunc(node) => parse_arrow_function_expression(node, scope, symbol_cache, env),
        Expr::Member(node) => parse_member_expression(node, scope, symbol_cache, env),
        Expr::Call(node) => parse_call_expression(node, scope, symbol_cache, env),
        Expr::New(node) => parse_constructor_expression(node, scope, symbol_cache, env),
        Expr::Obj(node) => parse_object_literal(node, scope, symbol_cache, env),
        Expr::Array(node) => parse_array_literal(node, scope, symbol_cache, env),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_expressions<'src: 'temp, 'temp>(
    expressions: impl IntoIterator<Item = &'temp Expr<'src>> + ExactSizeIterator,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Vec<Expression>> {
    let num_expressions = expressions.len();
    expressions
        .into_iter()
        .fold(Ok(Vec::with_capacity(num_expressions)), |results, node| {
            let mut args = results?;
            match parse_expression(node, scope, symbol_cache, env) {
                Ok(arg) => {
                    args.push(arg);
                    Ok(args)
                }
                Err(err) => Err(err),
            }
        })
}

fn parse_variable_reference<'src>(
    node: &Ident<'src>,
    scope: &LexicalScope,
    env: &Env,
) -> ParserResult<Expression> {
    let name = parse_identifier(node)?;
    let offset = scope.get(name);
    match offset {
        Some(offset) => Ok(Expression::new(Term::Variable(VariableTerm::Static(
            StaticVariableTerm::new(offset as StackOffset),
        )))),
        None => match env.global(name) {
            Some(value) => Ok(value),
            None => Err(err(&format!("Invalid reference: '{}'", name), node)),
        },
    }
}

fn parse_literal<'src>(
    node: &Lit<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node {
        Lit::Null => parse_null_literal(),
        Lit::Boolean(node) => parse_boolean_literal(node),
        Lit::Number(node) => parse_number_literal(node),
        Lit::String(node) => parse_string_literal(node),
        Lit::Template(node) => parse_template_literal(node, scope, symbol_cache, env),
        Lit::RegEx(_) => Err(err_unimplemented(node)),
    }
}

fn parse_null_literal() -> ParserResult<Expression> {
    Ok(Expression::new(Term::Value(ValueTerm::Null)))
}

fn parse_boolean_literal(node: &bool) -> ParserResult<Expression> {
    Ok(Expression::new(Term::Value(ValueTerm::Boolean(*node))))
}

fn parse_number_literal(node: &Cow<str>) -> ParserResult<Expression> {
    match node.parse::<f64>() {
        Ok(value) => Ok(Expression::new(Term::Value(ValueTerm::Float(value)))),
        Err(_) => Err(err("Invalid number literal", node)),
    }
}

fn parse_string_literal(node: &StringLit) -> ParserResult<Expression> {
    let value = parse_string(node)?;
    Ok(Expression::new(Term::Value(ValueTerm::String(
        StringValue::from(value),
    ))))
}

fn parse_string<'src>(node: &StringLit<'src>) -> ParserResult<&'src str> {
    let value = match node {
        StringLit::Double(value) => value,
        StringLit::Single(value) => value,
    };
    match value {
        Cow::Borrowed(value) => Ok(value),
        Cow::Owned(_) => Err(err("Invalid string", node)),
    }
}

fn parse_template_literal<'src>(
    node: &TemplateLit<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let args = node
        .quasis
        .iter()
        .map(|quasi| match parse_template_element(quasi)? {
            "" => Ok(None),
            value => Ok(Some(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from(value),
            ))))),
        })
        .zip(
            node.expressions
                .iter()
                .map(|expression| {
                    let value = parse_expression(expression, scope, symbol_cache, env)?;
                    Ok(Some(Expression::new(Term::Application(
                        ApplicationTerm::new(to_string(), vec![value]),
                    ))))
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
        0 => Expression::new(Term::Value(ValueTerm::String(StringValue::from("")))),
        1 => args.into_iter().next().unwrap(),
        _ => Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Concat)),
            args,
        ))),
    })
}

fn parse_template_element<'src>(node: &TemplateElement<'src>) -> ParserResult<&'src str> {
    match node.cooked {
        Cow::Borrowed(value) => Ok(value),
        Cow::Owned(_) => Err(err("Invalid template string", node)),
    }
}

fn parse_object_literal<'src>(
    node: &ObjExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let properties = node
        .iter()
        .fold(Ok(Vec::with_capacity(node.len())), |results, node| {
            let mut properties = results?;
            match node {
                ObjProp::Prop(prop) => match prop.kind {
                    PropKind::Init => {
                        let key = match &prop.key {
                            PropKey::Lit(key) => match key {
                                Lit::String(key) if !prop.computed => {
                                    let field_name = parse_string(key)?;
                                    Ok(Expression::new(Term::Value(ValueTerm::Symbol(
                                        symbol_cache.get(String::from(field_name)),
                                    ))))
                                }
                                _ => parse_literal(key, scope, symbol_cache, env),
                            },
                            PropKey::Expr(key) => match key {
                                Expr::Ident(key) if !prop.computed => {
                                    let field_name = parse_identifier(key)?;
                                    Ok(Expression::new(Term::Value(ValueTerm::Symbol(
                                        symbol_cache.get(String::from(field_name)),
                                    ))))
                                }
                                _ => parse_expression(key, scope, symbol_cache, env),
                            },
                            PropKey::Pat(key) => match key {
                                Pat::Ident(key) if !prop.computed => {
                                    let field_name = parse_identifier(key)?;
                                    Ok(Expression::new(Term::Value(ValueTerm::Symbol(
                                        symbol_cache.get(String::from(field_name)),
                                    ))))
                                }
                                _ => Err(err_unimplemented(node)),
                            },
                        }?;
                        let value = match &prop.value {
                            PropValue::Expr(node) => {
                                parse_expression(node, scope, symbol_cache, env)
                            }
                            PropValue::None => match prop.short_hand {
                                true => match &prop.key {
                                    PropKey::Pat(node) => match node {
                                        Pat::Ident(node) => {
                                            parse_variable_reference(&node, scope, env)
                                        }
                                        _ => Err(err_unimplemented(node)),
                                    },
                                    _ => Err(err_unimplemented(prop)),
                                },
                                _ => Err(err_unimplemented(prop)),
                            },
                            PropValue::Pat(node) => Err(err_unimplemented(node)),
                        }?;
                        properties.push((key, value));
                        Ok(properties)
                    }
                    PropKind::Method => Err(err_unimplemented(prop)),
                    PropKind::Ctor => Err(err_unimplemented(prop)),
                    PropKind::Get => Err(err_unimplemented(prop)),
                    PropKind::Set => Err(err_unimplemented(prop)),
                },
                ObjProp::Spread(node) => Err(err_unimplemented(node)),
            }
        })?;
    let (keys, values): (Vec<_>, Vec<_>) = properties.into_iter().unzip();
    let has_dynamic_keys = keys.iter().any(|key| match key.value() {
        Term::Value(_) => false,
        _ => true,
    });
    if has_dynamic_keys {
        Err(err_unimplemented(node))
    } else {
        Ok(Expression::new(Term::Struct(StructTerm::new(
            Some(StructPrototype::new(
                keys.iter().map(|key| key.hash()).collect(),
            )),
            values,
        ))))
    }
}

fn parse_array_literal<'src>(
    node: &ArrayExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let values = node
        .iter()
        .fold(Ok(Vec::with_capacity(node.len())), |results, node| {
            let mut values = results?;
            match node {
                None => Err(err("Missing array item", node)),
                Some(node) => match parse_expression(node, scope, symbol_cache, env) {
                    Ok(value) => {
                        values.push(value);
                        Ok(values)
                    }
                    Err(error) => Err(error),
                },
            }
        })?;
    Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
        VectorTerm::new(values),
    ))))
}

fn parse_unary_expression<'src>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node.operator {
        UnaryOp::Minus => parse_unary_minus_expression(node, scope, symbol_cache, env),
        UnaryOp::Plus => parse_unary_plus_expression(node, scope, symbol_cache, env),
        UnaryOp::Not => parse_unary_not_expression(node, scope, symbol_cache, env),
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_binary_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node.operator {
        BinaryOp::Plus => parse_binary_add_expression(node, scope, symbol_cache, env),
        BinaryOp::Minus => parse_binary_subtract_expression(node, scope, symbol_cache, env),
        BinaryOp::Times => parse_binary_multiply_expression(node, scope, symbol_cache, env),
        BinaryOp::Over => parse_binary_divide_expression(node, scope, symbol_cache, env),
        BinaryOp::Mod => parse_binary_remainder_expression(node, scope, symbol_cache, env),
        BinaryOp::PowerOf => parse_binary_pow_expression(node, scope, symbol_cache, env),
        BinaryOp::LessThan => parse_binary_lt_expression(node, scope, symbol_cache, env),
        BinaryOp::GreaterThan => parse_binary_gt_expression(node, scope, symbol_cache, env),
        BinaryOp::LessThanEqual => parse_binary_lte_expression(node, scope, symbol_cache, env),
        BinaryOp::GreaterThanEqual => parse_binary_gte_expression(node, scope, symbol_cache, env),
        BinaryOp::StrictEqual => parse_binary_equal_expression(node, scope, symbol_cache, env),
        BinaryOp::StrictNotEqual => {
            parse_binary_not_equal_expression(node, scope, symbol_cache, env)
        }
        _ => Err(err_unimplemented(node)),
    }
}

fn parse_logical_expression<'src>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    match node.operator {
        LogicalOp::And => parse_logical_and_expression(node, scope, symbol_cache, env),
        LogicalOp::Or => parse_logical_or_expression(node, scope, symbol_cache, env),
    }
}

fn parse_unary_minus_expression<'src>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let operand = parse_expression(&node.argument, scope, symbol_cache, env)?;
    Ok(match operand.value() {
        Term::Value(ValueTerm::Int(value)) => Expression::new(Term::Value(ValueTerm::Int(-*value))),
        Term::Value(ValueTerm::Float(value)) => {
            Expression::new(Term::Value(ValueTerm::Float(-*value)))
        }
        _ => Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Subtract)),
            vec![Expression::new(Term::Value(ValueTerm::Float(0.0))), operand],
        ))),
    })
}

fn parse_unary_plus_expression<'src>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let operand = parse_expression(&node.argument, scope, symbol_cache, env)?;
    Ok(match operand.value() {
        Term::Value(ValueTerm::Int(_)) => operand,
        Term::Value(ValueTerm::Float(_)) => operand,
        _ => Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Add)),
            vec![Expression::new(Term::Value(ValueTerm::Float(0.0))), operand],
        ))),
    })
}

fn parse_unary_not_expression<'src>(
    node: &UnaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let operand = parse_expression(&node.argument, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Not)),
        vec![operand],
    ))))
}

fn parse_binary_add_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Add)),
        vec![left, right],
    ))))
}

fn parse_binary_subtract_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Subtract)),
        vec![left, right],
    ))))
}

fn parse_binary_multiply_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
        vec![left, right],
    ))))
}

fn parse_binary_divide_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Divide)),
        vec![left, right],
    ))))
}

fn parse_binary_remainder_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Remainder)),
        vec![left, right],
    ))))
}

fn parse_binary_pow_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Pow)),
        vec![left, right],
    ))))
}

fn parse_binary_lt_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Lt)),
        vec![left, right],
    ))))
}

fn parse_binary_gt_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Gt)),
        vec![left, right],
    ))))
}

fn parse_binary_lte_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Lte)),
        vec![left, right],
    ))))
}

fn parse_binary_gte_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Gte)),
        vec![left, right],
    ))))
}

fn parse_binary_equal_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Eq)),
        vec![left, right],
    ))))
}

fn parse_binary_not_equal_expression<'src>(
    node: &BinaryExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let expression = parse_binary_equal_expression(node, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Not)),
        vec![expression],
    ))))
}

fn parse_logical_and_expression<'src>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::And)),
        vec![left, right],
    ))))
}

fn parse_logical_or_expression<'src>(
    node: &LogicalExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let left = parse_expression(&node.left, scope, symbol_cache, env)?;
    let right = parse_expression(&node.right, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Or)),
        vec![left, right],
    ))))
}

fn parse_conditional_expression<'src>(
    node: &ConditionalExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let condition = parse_expression(&node.test, scope, symbol_cache, env)?;
    let consequent = parse_expression(&node.consequent, scope, symbol_cache, env)?;
    let alternate = parse_expression(&node.alternate, scope, symbol_cache, env)?;
    Ok(create_if_expression(condition, consequent, alternate))
}

fn create_if_expression(
    condition: Expression,
    consequent: Expression,
    alternate: Expression,
) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::If)),
        vec![condition, consequent, alternate],
    )))
}

fn parse_arrow_function_expression<'src>(
    node: &ArrowFuncExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    if node.generator || node.is_async {
        Err(err_unimplemented(node))
    } else {
        let (arg_names, destructuring_assignments) = node.params.iter().enumerate().fold(
            Ok((Vec::with_capacity(node.params.len()), Vec::new())),
            |result, (arg_index, node)| {
                let (mut arg_names, mut destructuring_assignments) = result?;
                match node {
                    FuncArg::Pat(node) => match node {
                        Pat::Ident(node) => {
                            let name = parse_identifier(node)?;
                            arg_names.push(Some(name));
                            Ok((arg_names, destructuring_assignments))
                        }
                        Pat::Obj(properties) => {
                            arg_names.push(None);
                            let bindings = properties
                                .iter()
                                .map(|node| match node {
                                    ObjPatPart::Assign(node) => {
                                        if node.computed {
                                            Err(err_unimplemented(node))
                                        } else {
                                            let field_name = parse_destructuring_pattern_property_field_name(node, scope, symbol_cache, env)?;
                                            let identifier = parse_destructuring_pattern_property_identifier(node)?;
                                            Ok((identifier, (arg_index, field_name)))
                                        }
                                    }
                                    ObjPatPart::Rest(_) => Err(err_unimplemented(node)),
                                })
                                .collect::<ParserResult<Vec<_>>>()?;
                            destructuring_assignments.extend(bindings);
                            Ok((arg_names, destructuring_assignments))
                        }
                        Pat::Array(_) => Err(err_unimplemented(node)),
                        Pat::RestElement(_) => Err(err_unimplemented(node)),
                        Pat::Assign(_) => Err(err_unimplemented(node)),
                    },
                    _ => Err(err_unimplemented(node)),
                }
            },
        )?;
        let num_args = arg_names.len();
        let function_scope = if arg_names.is_empty() {
            None
        } else {
            Some(scope.create_child(arg_names.into_iter()))
        };
        let function_scope = function_scope.as_ref().unwrap_or(scope);
        let (destructured_identifiers, destructured_arg_indices): (Vec<_>, Vec<_>) =
            destructuring_assignments.into_iter().unzip();
        let destructuring_initializers = destructured_arg_indices
            .into_iter()
            .map(|(arg_index, field_name)| {
                get_dynamic_field(
                    Expression::new(Term::Variable(VariableTerm::Static(
                        StaticVariableTerm::new((num_args - arg_index - 1) as StackOffset),
                    ))),
                    field_name,
                )
            })
            .collect::<Vec<_>>();
        let destructured_scope = if destructured_identifiers.is_empty() {
            None
        } else {
            Some(function_scope.create_child(destructured_identifiers.into_iter().map(Some)))
        };
        let child_scope = destructured_scope.as_ref().unwrap_or(function_scope);
        let body = match &node.body {
            ArrowFuncBody::Expr(node) => {
                let body = &vec![ProgramPart::Stmt(Stmt::Return(Some(*node.clone())))];
                parse_block(body, child_scope, symbol_cache, env)
            }
            ArrowFuncBody::FuncBody(node) => {
                let FuncBody(body) = node;
                parse_block(body, child_scope, symbol_cache, env)
            }
        }?;
        match body {
            None => Err(err("Missing function return statement", node)),
            Some(body) => Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, num_args as u8, None),
                if destructuring_initializers.is_empty() {
                    body
                } else {
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, destructuring_initializers.len() as u8, None),
                            body,
                        ))),
                        destructuring_initializers,
                    )))
                },
            )))),
        }
    }
}

fn parse_member_expression<'src>(
    node: &MemberExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let target = parse_expression(&node.object, scope, symbol_cache, env)?;
    let field_name = parse_static_member_field_name(node)?;
    match field_name {
        Some(field_name) => Ok(get_static_field(target, field_name, symbol_cache)),
        None => {
            let field = parse_expression(&node.property, scope, symbol_cache, env)?;
            Ok(get_dynamic_field(target, field))
        }
    }
}

fn parse_static_member_field_name<'src>(
    node: &MemberExpr<'src>,
) -> ParserResult<Option<&'src str>> {
    Ok(match &*node.property {
        Expr::Ident(name) => Some(parse_identifier(&name)?),
        Expr::Lit(name) => match name {
            Lit::String(name) => Some(parse_string(&name)?),
            _ => None,
        },
        _ => None,
    })
}

fn get_static_field<'src>(
    target: Expression,
    field: &'src str,
    symbol_cache: &mut SymbolCache,
) -> Expression {
    let field = Expression::new(Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from(field)))));
    get_dynamic_field(target, field)
}

fn get_dynamic_field<'src>(target: Expression, field: Expression) -> Expression {
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::Get)),
        vec![target, field],
    )))
}

fn parse_call_expression<'src>(
    node: &CallExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let static_dispatch = match &*node.callee {
        Expr::Member(callee) => {
            let method_name = parse_static_member_field_name(callee)?;
            match method_name {
                Some(method_name) => Some(parse_static_method_call_expression(
                    &callee.object,
                    method_name,
                    node.arguments.iter(),
                    scope,
                    symbol_cache,
                    env,
                )?),
                None => None,
            }
        }
        _ => None,
    };
    match static_dispatch {
        Some(expression) => Ok(expression),
        None => {
            let callee = parse_expression(&node.callee, scope, symbol_cache, env)?;
            parse_function_application_expression(
                callee,
                node.arguments.iter(),
                scope,
                symbol_cache,
                env,
            )
        }
    }
}

fn parse_static_method_call_expression<'src: 'temp, 'temp>(
    target: &Expr<'src>,
    method_name: &'src str,
    args: impl IntoIterator<Item = &'temp Expr<'src>> + ExactSizeIterator,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let target = parse_expression(target, scope, symbol_cache, env)?;
    let is_potential_builtin_method = get_builtin_field(None, method_name).is_some();
    if is_potential_builtin_method {
        let method = Expression::new(Term::Value(ValueTerm::String(StringValue::from(
            method_name,
        ))));
        let num_args = args.len();
        let args = args.into_iter().collect::<Vec<_>>();
        let method_args = parse_expressions(args.iter().cloned(), scope, symbol_cache, env)?;
        let dynamic_fallback = parse_function_application_expression(
            get_static_field(Expression::clone(&target), method_name, symbol_cache),
            args.iter().cloned(),
            scope,
            symbol_cache,
            env,
        )?;
        let mut combined_args = Vec::with_capacity(3 + num_args);
        combined_args.push(target);
        combined_args.push(method);
        combined_args.push(dynamic_fallback);
        combined_args.extend(method_args);
        Ok(Expression::new(Term::Application(ApplicationTerm::new(
            dispatch(),
            combined_args,
        ))))
    } else {
        parse_function_application_expression(
            get_static_field(Expression::clone(&target), method_name, symbol_cache),
            args,
            scope,
            symbol_cache,
            env,
        )
    }
}

fn parse_function_application_expression<'src: 'temp, 'temp>(
    target: Expression,
    args: impl IntoIterator<Item = &'temp Expr<'src>> + ExactSizeIterator,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let args = parse_expressions(args, scope, symbol_cache, env)?;
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        target, args,
    ))))
}

fn parse_constructor_expression<'src>(
    node: &NewExpr<'src>,
    scope: &LexicalScope,
    symbol_cache: &mut SymbolCache,
    env: &Env,
) -> ParserResult<Expression> {
    let target = parse_expression(&node.callee, scope, symbol_cache, env);
    let args = node
        .arguments
        .iter()
        .map(|arg| parse_expression(arg, scope, symbol_cache, env));
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        construct(),
        once(target).chain(args).collect::<ParserResult<Vec<_>>>()?,
    ))))
}

#[cfg(test)]
mod tests {
    use super::{parse, SymbolCache};
    use crate::{
        builtin_imports,
        builtins::{dispatch, throw, to_string},
        Env,
    };
    use reflex::{
        cache::EvaluationCache,
        core::{
            ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
            LambdaTerm, StaticVariableTerm, StructPrototype, StructTerm, Term, VariableTerm,
        },
        hash::Hashable,
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn null_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("null", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Null))),
        );
    }

    #[test]
    fn boolean_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("true", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Boolean(true)))),
        );
        assert_eq!(
            parse("false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Boolean(false)))),
        );
    }

    #[test]
    fn string_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("''", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                String::from("")
            )))),
        );
        assert_eq!(
            parse("\"\"", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                String::from("")
            )))),
        );
        assert_eq!(
            parse("'foo'", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                String::from("foo")
            )))),
        );
        assert_eq!(
            parse("\"foo\"", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                String::from("foo")
            )))),
        );
    }

    #[test]
    fn numeric_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0)))),
        );
        assert_eq!(
            parse("3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("0.0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0)))),
        );
        assert_eq!(
            parse("3.142", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.142)))),
        );
        assert_eq!(
            parse("0.000", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0)))),
        );
        assert_eq!(
            parse("-0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-0.0)))),
        );
        assert_eq!(
            parse("-3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-3.0)))),
        );
        assert_eq!(
            parse("-0.0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-0.0)))),
        );
        assert_eq!(
            parse("-3.142", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-3.142)))),
        );
        assert_eq!(
            parse("+0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0)))),
        );
        assert_eq!(
            parse("+3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("+0.0", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0)))),
        );
        assert_eq!(
            parse("+3.142", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.142)))),
        );
    }

    #[test]
    fn template_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("``", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("")
            )))),
        );
        assert_eq!(
            parse("`foo`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo")
            )))),
        );
        assert_eq!(
            parse("`\"`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("\"")
            )))),
        );
        assert_eq!(
            parse("`\\\"`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("\\\"")
            )))),
        );
        assert_eq!(
            parse("`${'foo'}`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                to_string(),
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("foo")
                )))],
            ),))),
        );
        assert_eq!(
            parse("`foo${'bar'}`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("bar")
                        )))],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("`${'foo'}bar`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("foo")
                        )))],
                    ))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                ],
            )))),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("foo")
                        )))],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("bar")
                        )))],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("`foo${'bar'}baz`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("bar")
                        )))],
                    ))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                ],
            )))),
        );
        assert_eq!(
            parse("`${'foo'}bar${'baz'}`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("foo")
                        )))],
                    ))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("baz")
                        )))],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("`${'foo'}${'bar'}${'baz'}`", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("foo")
                        )))],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("bar")
                        )))],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("baz")
                        )))],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "`foo${'one'}bar${'two'}baz${'three'}`",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("one")
                        )))],
                    ))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("two")
                        )))],
                    ))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        to_string(),
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("three")
                        )))],
                    ))),
                ],
            ))))
        )
    }

    #[test]
    fn object_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("({})", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![])),
                vec![],
            )))),
        );
        assert_eq!(
            parse("({ foo: 3, bar: 4, baz: 5 })", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                    Expression::new(Term::Value(ValueTerm::Float(5.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("({ foo: 3, \"bar\": 4, baz: 5 })", &mut symbol_cache, &env,),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                    Expression::new(Term::Value(ValueTerm::Float(5.0))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "({ foo: 3, [\"bar\"]: 4, baz: 5 })",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                    Term::Value(ValueTerm::String(StringValue::from("bar"))).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                    Expression::new(Term::Value(ValueTerm::Float(5.0))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", 4: \"bar\", 5: \"baz\" })",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Float(3.0)).hash(),
                    Term::Value(ValueTerm::Float(4.0)).hash(),
                    Term::Value(ValueTerm::Float(5.0)).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", [4]: \"bar\", 5: \"baz\" })",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Float(3.0)).hash(),
                    Term::Value(ValueTerm::Float(4.0)).hash(),
                    Term::Value(ValueTerm::Float(5.0)).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", \"4\": \"bar\", 5: \"baz\" })",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Float(3.0)).hash(),
                    Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("4")))).hash(),
                    Term::Value(ValueTerm::Float(5.0)).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "({ 3: \"foo\", [\"4\"]: \"bar\", 5: \"baz\" })",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(vec![
                    Term::Value(ValueTerm::Float(3.0)).hash(),
                    Term::Value(ValueTerm::String(StringValue::from("4"))).hash(),
                    Term::Value(ValueTerm::Float(5.0)).hash(),
                ])),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                ],
            )))),
        );
    }

    #[test]
    fn array_literals() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("[]", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                VectorTerm::new(vec![]),
            )))),
        );
        assert_eq!(
            parse("[3, 4, 5]", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                VectorTerm::new(vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                    Expression::new(Term::Value(ValueTerm::Float(5.0))),
                ]),
            )))),
        );
    }

    #[test]
    fn array_methods() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse(
                "[3, 4, 5].map((value) => value * 2)",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                dispatch(),
                vec![
                    Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                        vec![
                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                            Expression::new(Term::Value(ValueTerm::Float(5.0))),
                        ],
                    )))),
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from("map")))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Collection(CollectionTerm::Vector(
                                    VectorTerm::new(vec![
                                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                                    ]),
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("map"))
                                ))),
                            ],
                        ))),
                        vec![Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::Static(
                                        StaticVariableTerm::new(0)
                                    ))),
                                    Expression::new(Term::Value(ValueTerm::Float(2.0))),
                                ],
                            ))),
                        ))),],
                    ))),
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0)
                                ))),
                                Expression::new(Term::Value(ValueTerm::Float(2.0))),
                            ],
                        ))),
                    ))),
                ],
            )))),
        );
    }

    #[test]
    fn parenthesized_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("(3)", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
        );
        assert_eq!(
            parse("(((3)))", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
        );
    }

    #[test]
    fn modules() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("export default 3;", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
        );
    }

    #[test]
    fn variable_declarations() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("const foo = 3; foo;", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Variable(VariableTerm::Static(
                        StaticVariableTerm::new(0)
                    ))),
                ))),
                vec![Expression::new(Term::Value(ValueTerm::Float(3.0)))],
            )))),
        );
        assert_eq!(
            parse(
                "const { bar, foo } = { foo: 3, bar: 4, baz: 5 }; bar;",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(1)
                            ))),
                        ))),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Struct(StructTerm::new(
                                    Some(StructPrototype::new(vec![
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz"))))
                                            .hash(),
                                    ])),
                                    vec![
                                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                                    ],
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("bar"))
                                ))),
                            ],
                        )))],
                    ))),
                ))),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                    vec![
                        Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(vec![
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                            ])),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                Expression::new(Term::Value(ValueTerm::Float(5.0))),
                            ],
                        ))),
                        Expression::new(Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))),
                    ],
                )))],
            )))),
        );
        assert_eq!(
            parse(
                "const { bar: qux, foo } = { foo: 3, bar: 4, baz: 5 }; qux;",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(1)
                            ))),
                        ))),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Struct(StructTerm::new(
                                    Some(StructPrototype::new(vec![
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz"))))
                                            .hash(),
                                    ])),
                                    vec![
                                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                                    ],
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("bar"))
                                ))),
                            ],
                        )))],
                    ))),
                ))),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                    vec![
                        Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(vec![
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                            ])),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                Expression::new(Term::Value(ValueTerm::Float(5.0))),
                            ],
                        ))),
                        Expression::new(Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))),
                    ],
                )))],
            )))),
        );
        assert_eq!(
            parse(
                "const { bar: foo, foo: bar } = { foo: 3, bar: 4, baz: 5 }; foo;",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(1)
                            ))),
                        ))),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Struct(StructTerm::new(
                                    Some(StructPrototype::new(vec![
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar"))))
                                            .hash(),
                                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz"))))
                                            .hash(),
                                    ])),
                                    vec![
                                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                                    ],
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("bar"))
                                ))),
                            ],
                        )))],
                    ))),
                ))),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                    vec![
                        Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(vec![
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                                Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                            ])),
                            vec![
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                Expression::new(Term::Value(ValueTerm::Float(5.0))),
                            ],
                        ))),
                        Expression::new(Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo"))))),
                    ],
                )))],
            ))))
        );
    }

    #[test]
    fn variable_scoping() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        let expression = parse(
            "
            ((value) => {
                const foo = value * 2;
                if (foo === 3) return false;
                return foo;
              })(3);
            ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0 * 2.0)))),
                DependencyList::empty(),
            )
        )
    }

    #[test]
    fn variable_dependencies() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        let expression = parse(
            "
            const foo = 3;
            const bar = foo;
            const baz = bar;
            baz;
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
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
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn not_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("!true", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Not)),
                vec![Expression::new(Term::Value(ValueTerm::Boolean(true)))],
            )))),
        );
        assert_eq!(
            parse("!false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Not)),
                vec![Expression::new(Term::Value(ValueTerm::Boolean(false)))],
            )))),
        );
        assert_eq!(
            parse("!3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Not)),
                vec![Expression::new(Term::Value(ValueTerm::Float(3.0)))],
            )))),
        );
    }

    #[test]
    fn arithmetic_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("3 + 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 - 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Subtract)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 * 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 / 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Divide)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 % 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Remainder)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 ** 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Pow)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 < 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Lt)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 <= 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Lte)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 > 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Gt)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
        assert_eq!(
            parse("3 >= 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Gte)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
    }

    #[test]
    fn equality_expression() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("true === false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Eq)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Boolean(true))),
                    Expression::new(Term::Value(ValueTerm::Boolean(false))),
                ],
            )))),
        );
        assert_eq!(
            parse("true !== false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Not)),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Eq)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Boolean(true))),
                        Expression::new(Term::Value(ValueTerm::Boolean(false))),
                    ],
                )))],
            )))),
        );
    }

    #[test]
    fn logical_expression() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("true && false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::And)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Boolean(true))),
                    Expression::new(Term::Value(ValueTerm::Boolean(false))),
                ],
            )))),
        );
        assert_eq!(
            parse("true || false", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Or)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Boolean(true))),
                    Expression::new(Term::Value(ValueTerm::Boolean(false))),
                ],
            )))),
        );
    }

    #[test]
    fn conditional_expression() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("true ? 3 : 4", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::If)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Boolean(true))),
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                ],
            )))),
        );
    }

    #[test]
    fn if_statements() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse(
                "(() => { if (true) { return 3; } else { return 4; }})()",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Boolean(true))),
                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        ]
                    )))
                ))),
                vec![],
            ))))
        );
        assert_eq!(
            parse("(() => { if (true) { throw new Error(\"foo\"); } else { throw new Error(\"bar\"); }})()", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Boolean(true))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                throw(),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::String(String::from("foo")))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                throw(),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::String(String::from("bar")))),
                                ],
                            ))),
                        ],
                    )))
                ))),
                vec![],
            ))))
        );
        assert_eq!(
            parse("(() => { if (true) { const foo = 3; const bar = 4; return foo + bar; } else { const foo = 4; const bar = 3; return foo + bar; }})()", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Boolean(true))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Lambda(LambdaTerm::new(
                                            Arity::from(0, 1, None),
                                            Expression::new(Term::Application(ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                vec![
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(1)))),
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0)))),
                                                ]
                                            ))),
                                        ))),
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        ],
                                    ))),
                                ))),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Lambda(LambdaTerm::new(
                                            Arity::from(0, 1, None),
                                            Expression::new(Term::Application(ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                vec![
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(1)))),
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0)))),
                                                ]
                                            ))),
                                        ))),
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        ],
                                    ))),
                                ))),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                ],
                            ))),
                        ]
                    )))
                ))),
                vec![],
            ))))
        );
        assert_eq!(
            parse("(() => { if (true) { const foo = 3; const bar = 4; return foo + bar; } const foo = 4; const bar = 3; return foo + bar; })()", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Boolean(true))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Lambda(LambdaTerm::new(
                                            Arity::from(0, 1, None),
                                            Expression::new(Term::Application(ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                vec![
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(1)))),
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0)))),
                                                ]
                                            ))),
                                        ))),
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        ],
                                    ))),
                                ))),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Lambda(LambdaTerm::new(
                                            Arity::from(0, 1, None),
                                            Expression::new(Term::Application(ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                vec![
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(1)))),
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0)))),
                                                ]
                                            ))),
                                        ))),
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                        ],
                                    ))),
                                ))),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                ],
                            ))),
                        ]
                    )))
                ))),
                vec![],
            ))))
        );
        assert_eq!(
            parse("(() => { if (true) throw new Error(\"foo\"); const foo = 3; const bar = 4; return foo + bar; })()", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::If)),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Boolean(true))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                throw(),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::String(String::from("foo")))),
                                ],
                            ))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Lambda(LambdaTerm::new(
                                            Arity::from(0, 1, None),
                                            Expression::new(Term::Application(ApplicationTerm::new(
                                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                vec![
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(1)))),
                                                    Expression::new(Term::Variable(VariableTerm::Static(StaticVariableTerm::new(0)))),
                                                ]
                                            ))),
                                        ))),
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                                        ],
                                    ))),
                                ))),
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                                ],
                            ))),
                        ]
                    )))
                ))),
                vec![],
            ))))
        );
    }

    #[test]
    fn throw_statements() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("throw new Error(\"foo\")", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                throw(),
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("foo")
                )))],
            )))),
        );
        assert_eq!(
            parse("throw new Error(`foo${'bar'}`)", &mut symbol_cache, &env,),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                throw(),
                vec![Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Concat)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            to_string(),
                            vec![Expression::new(Term::Value(ValueTerm::String(
                                StringValue::from("bar")
                            )))],
                        ))),
                    ],
                )))],
            )))),
        );
    }

    #[test]
    fn arrow_function_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("() => 3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 0, None),
                Expression::new(Term::Value(ValueTerm::Float(3.0))),
            )))),
        );
        assert_eq!(
            parse("(foo) => 3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Value(ValueTerm::Float(3.0))),
            )))),
        );
        assert_eq!(
            parse("(foo) => foo", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::Static(
                    StaticVariableTerm::new(0)
                ))),
            )))),
        );
        assert_eq!(
            parse("(foo) => foo + foo", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0)
                        ))),
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0)
                        ))),
                    ],
                ))),
            )))),
        );
        assert_eq!(
            parse("(foo, bar, baz) => foo + bar", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 3, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(2)
                        ))),
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(1)
                        ))),
                    ],
                ))),
            )))),
        );
        assert_eq!(
            parse(
                "(foo) => (bar) => (baz) => foo + bar",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(2)
                                ))),
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(1)
                                ))),
                            ],
                        ))),
                    ))),
                ))),
            )))),
        );
    }

    #[test]
    fn arrow_function_destructuring() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("({}) => 3", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Value(ValueTerm::Float(3.0))),
            )))),
        );
        assert_eq!(
            parse("({ foo }) => foo", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Variable(VariableTerm::Static(
                            StaticVariableTerm::new(0)
                        ))),
                    ))),
                    vec![Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(0)
                            ))),
                            Expression::new(Term::Value(ValueTerm::Symbol(
                                symbol_cache.get(String::from("foo"))
                            ))),
                        ],
                    ))),],
                ))),
            )))),
        );
        assert_eq!(
            parse("({ foo, bar }) => foo + bar", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(1)
                                ))),
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0)
                                ))),
                            ],
                        ))),
                    ))),
                    vec![
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0)
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("foo"))
                                ))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(0)
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("bar"))
                                ))),
                            ],
                        ))),
                    ],
                ))),
            )))),
        );
        assert_eq!(
            parse(
                "(first, { foo, bar }, second, third) => ((first + foo) + bar) + third",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 4, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                    vec![
                                        Expression::new(Term::Application(ApplicationTerm::new(
                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                            vec![
                                                Expression::new(Term::Variable(
                                                    VariableTerm::Static(StaticVariableTerm::new(
                                                        5
                                                    ))
                                                )),
                                                Expression::new(Term::Variable(
                                                    VariableTerm::Static(StaticVariableTerm::new(
                                                        1
                                                    ))
                                                )),
                                            ],
                                        ))),
                                        Expression::new(Term::Variable(VariableTerm::Static(
                                            StaticVariableTerm::new(0)
                                        ))),
                                    ],
                                ))),
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(2)
                                ))),
                            ],
                        ))),
                    ))),
                    vec![
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(2)
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("foo"))
                                ))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::Static(
                                    StaticVariableTerm::new(2)
                                ))),
                                Expression::new(Term::Value(ValueTerm::Symbol(
                                    symbol_cache.get(String::from("bar"))
                                ))),
                            ],
                        ))),
                    ],
                ))),
            )))),
        );
    }

    #[test]
    fn function_application_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new();
        assert_eq!(
            parse("(() => 3)()", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                ))),
                vec![],
            )))),
        );
        assert_eq!(
            parse("((foo) => foo)(3)", &mut symbol_cache, &env),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Variable(VariableTerm::Static(
                        StaticVariableTerm::new(0)
                    ))),
                ))),
                vec![Expression::new(Term::Value(ValueTerm::Float(3.0))),],
            )))),
        );
        assert_eq!(
            parse(
                "((foo, bar, baz) => foo + bar)(3, 4, 5)",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 3, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(2)
                            ))),
                            Expression::new(Term::Variable(VariableTerm::Static(
                                StaticVariableTerm::new(1)
                            ))),
                        ],
                    ))),
                ))),
                vec![
                    Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    Expression::new(Term::Value(ValueTerm::Float(4.0))),
                    Expression::new(Term::Value(ValueTerm::Float(5.0))),
                ],
            )))),
        );
        assert_eq!(
            parse(
                "((foo) => (bar) => (baz) => foo + bar)(3)(4)(5)",
                &mut symbol_cache,
                &env,
            ),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Lambda(LambdaTerm::new(
                                Arity::from(0, 1, None),
                                Expression::new(Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::Static(
                                                StaticVariableTerm::new(2)
                                            ))),
                                            Expression::new(Term::Variable(VariableTerm::Static(
                                                StaticVariableTerm::new(1)
                                            ))),
                                        ],
                                    ))),
                                ))),
                            ))),
                        ))),
                        vec![Expression::new(Term::Value(ValueTerm::Float(3.0)))],
                    ))),
                    vec![Expression::new(Term::Value(ValueTerm::Float(4.0)))],
                ))),
                vec![Expression::new(Term::Value(ValueTerm::Float(5.0)))],
            )))),
        );
    }

    #[test]
    fn constructors() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new().with_imports(builtin_imports(&mut symbol_cache));
        let expression = parse(
            "
            import { Types } from 'reflex::utils';
            export default Types.Struct({
                foo: Types.Scalar.Int,
                bar: Types.Scalar.Int,
                baz: Types.Scalar.Int,
            });
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::StructConstructor(
                    StructPrototype::new(vec![
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                    ])
                ))),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Struct({
                foo: Types.Scalar.Int,
                bar: Types.Scalar.Int,
                baz: Types.Scalar.Int,
            });
            export default new Foo({ foo: 3, bar: 4, baz: 5 });
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                    ],
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Struct({
                foo: Types.Scalar.Int,
                bar: Types.Scalar.Int,
                baz: Types.Scalar.Int,
            });
            export default new Foo({ baz: 5, bar: 4, foo: 3 });
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("foo")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("bar")))).hash(),
                        Term::Value(ValueTerm::Symbol(symbol_cache.get(String::from("baz")))).hash(),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(5.0))),
                    ],
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            import { Types } from 'reflex::utils';
            const Foo = Types.Struct({
                foo: Types.Scalar.Int,
                bar: Types.Scalar.Int,
                baz: Types.Scalar.Int,
            });
            export default new Foo({ foo: 3, bar: 4, baz: 5 }).bar;
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(4.0)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn recursive_expressions() {
        let mut symbol_cache = SymbolCache::new();
        let env = Env::new().with_imports(builtin_imports(&mut symbol_cache));
        let expression = parse(
            "
            import { graph } from 'reflex::utils';
            export default graph((foo) => 3);
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "
            import { graph } from 'reflex::utils';
            export default graph((foo) => ({
                foo,
                bar: 3,
                baz: 4,
            })).foo.foo.foo.bar;
        ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn import_scoping() {
        let mut symbol_cache = SymbolCache::new();
        let env =
            Env::new().with_imports(vec![("foo", Expression::new(Term::Value(ValueTerm::Null)))]);
        let expression = parse(
            "
            import Foo from 'foo';
            const foo = 4;
            const bar = { foo: 3, bar: Foo };
            export default bar.foo;
            ",
            &mut symbol_cache,
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&DynamicState::new(), &mut EvaluationCache::new());
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Float(3.0)))),
                DependencyList::empty(),
            )
        );
    }
}
