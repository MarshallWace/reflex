// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::HashMap,
    fmt,
    iter::{empty, once},
};

use reflex::{
    cache::SubstitutionCache,
    core::{
        Builtin, EvaluationCache, Expression, ExpressionFactory, ExpressionListType, HeapAllocator,
        Reducible, Rewritable, Substitutions, SymbolId,
    },
};
use reflex_stdlib::*;

mod lexer;
use lexer::{parse_syntax, SyntaxDatum};

type ParserResult<'src, T> = Result<T, ParserError<'src>>;

#[derive(Debug, PartialEq)]
pub struct ParserError<'src> {
    message: String,
    source: Option<SyntaxDatum<'src>>,
}
impl<'src> ParserError<'src> {
    fn new(message: String, source: &SyntaxDatum<'src>) -> Self {
        ParserError {
            message,
            source: Some(source.clone()),
        }
    }
    pub fn message(&self) -> &str {
        &self.message
    }
}
impl<'src> fmt::Display for ParserError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone)]
struct LexicalScope<'src> {
    bindings: Vec<&'src str>,
}
impl<'src> LexicalScope<'src> {
    fn new() -> Self {
        LexicalScope {
            bindings: Vec::new(),
        }
    }
    fn create_child(&self, identifiers: &[&'src str]) -> LexicalScope<'src> {
        LexicalScope {
            bindings: self
                .bindings
                .iter()
                .chain(identifiers.iter())
                .map(|key| *key)
                .collect(),
        }
    }
    fn get(&self, identifier: &'src str) -> Option<usize> {
        Some(
            self.bindings
                .iter()
                .rev()
                .enumerate()
                .find(|(_, key)| **key == identifier)
                .map(|(i, _)| i)?,
        )
    }
}

struct SymbolCache<'src> {
    cache: HashMap<&'src str, SymbolId>,
}
impl<'src> SymbolCache<'src> {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    pub fn get(&mut self, identifier: &'src str) -> SymbolId {
        self.cache
            .get(identifier)
            .map(|value| *value)
            .unwrap_or_else(|| {
                let id = self.cache.len() as u64;
                self.cache.insert(identifier, id);
                id
            })
    }
}

pub trait LispParserBuiltin:
    Builtin
    + From<Get>
    + From<Add>
    + From<Subtract>
    + From<Multiply>
    + From<Divide>
    + From<Equal>
    + From<Abs>
    + From<And>
    + From<Car>
    + From<Cdr>
    + From<Concat>
    + From<Cons>
    + From<Eq>
    + From<Gt>
    + From<Gte>
    + From<If>
    + From<Lt>
    + From<Lte>
    + From<Not>
    + From<Or>
    + From<Pow>
    + From<Remainder>
{
}
impl<T> LispParserBuiltin for T where
    T: Builtin
        + From<Get>
        + From<Add>
        + From<Subtract>
        + From<Multiply>
        + From<Divide>
        + From<Equal>
        + From<Abs>
        + From<And>
        + From<Car>
        + From<Cdr>
        + From<Concat>
        + From<Cons>
        + From<Eq>
        + From<Gt>
        + From<Gte>
        + From<If>
        + From<Lt>
        + From<Lte>
        + From<Not>
        + From<Or>
        + From<Pow>
        + From<Remainder>
{
}

pub fn parse<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &'src str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    let syntax = match parse_syntax(input) {
        Ok(syntax) => Ok(syntax),
        Err(message) => Err(ParserError {
            message,
            source: None,
        }),
    }?;
    let scope = LexicalScope::new();
    let mut symbol_cache = SymbolCache::new();
    let mut evaluation_cache = SubstitutionCache::new();
    parse_expression(
        &syntax,
        &scope,
        &mut symbol_cache,
        &mut evaluation_cache,
        factory,
        allocator,
    )
}

fn parse_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    match input {
        SyntaxDatum::IntegerLiteral(value) => Ok(parse_integer_literal(input, *value, factory)),
        SyntaxDatum::FloatLiteral(value) => Ok(parse_float_literal(input, *value, factory)),
        SyntaxDatum::StringLiteral(value) => {
            Ok(parse_string_literal(input, &value, factory, allocator))
        }
        SyntaxDatum::Symbol(identifier) => parse_variable(input, identifier, scope, factory),
        SyntaxDatum::List(items) => match items.split_first() {
            None => Err(ParserError::new(String::from("Expected expression"), input)),
            Some((target, args)) => {
                match parse_special_form(
                    input,
                    target,
                    args,
                    scope,
                    symbol_cache,
                    evaluation_cache,
                    factory,
                    allocator,
                )? {
                    Some(result) => Ok(result),
                    _ => parse_function_application(
                        input,
                        target,
                        args,
                        scope,
                        symbol_cache,
                        evaluation_cache,
                        factory,
                        allocator,
                    ),
                }
            }
        },
    }
}

fn parse_boolean_literal<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum,
    value: bool,
    factory: &impl ExpressionFactory<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_boolean_term(value)
}

fn parse_integer_literal<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum,
    value: i32,
    factory: &impl ExpressionFactory<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_int_term(value)
}

fn parse_float_literal<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum,
    value: f64,
    factory: &impl ExpressionFactory<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_float_term(value)
}

fn parse_string_literal<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum,
    value: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_string_term(allocator.create_string(value))
}

fn parse_symbol_literal<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum<'src>,
    symbol: &'src str,
    symbol_cache: &mut SymbolCache<'src>,
    factory: &impl ExpressionFactory<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_symbol_term(symbol_cache.get(symbol))
}

fn parse_variable<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    identifier: &'src str,
    scope: &LexicalScope<'src>,
    factory: &impl ExpressionFactory<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    match scope.get(identifier) {
        None => match parse_global(input, identifier, factory) {
            Some(result) => Ok(result),
            None => Err(ParserError::new(
                format!("Undefined identifier: {}", identifier),
                input,
            )),
        },
        Some(offset) => Ok(factory.create_variable_term(offset)),
    }
}

fn parse_global<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    identifier: &'src str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: LispParserBuiltin,
{
    match identifier {
        "#t" => Some(parse_boolean_literal(input, true, factory)),
        "#f" => Some(parse_boolean_literal(input, false, factory)),
        identifier => parse_builtin_procedure(identifier, factory),
    }
}
fn parse_builtin_procedure<T: Expression + Rewritable<T> + Reducible<T>>(
    name: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<T>
where
    T::Builtin: LispParserBuiltin,
{
    match name {
        "+" => Some(factory.create_builtin_term(Add)),
        "-" => Some(factory.create_builtin_term(Subtract)),
        "*" => Some(factory.create_builtin_term(Multiply)),
        "/" => Some(factory.create_builtin_term(Divide)),
        "=" => Some(factory.create_builtin_term(Equal)),
        "abs" => Some(factory.create_builtin_term(Abs)),
        "and" => Some(factory.create_builtin_term(And)),
        "car" => Some(factory.create_builtin_term(Car)),
        "cdr" => Some(factory.create_builtin_term(Cdr)),
        "concat" => Some(factory.create_builtin_term(Concat)),
        "cons" => Some(factory.create_builtin_term(Cons)),
        "eq" => Some(factory.create_builtin_term(Eq)),
        "gt" => Some(factory.create_builtin_term(Gt)),
        "gte" => Some(factory.create_builtin_term(Gte)),
        "if" => Some(factory.create_builtin_term(If)),
        "lt" => Some(factory.create_builtin_term(Lt)),
        "lte" => Some(factory.create_builtin_term(Lte)),
        "not" => Some(factory.create_builtin_term(Not)),
        "or" => Some(factory.create_builtin_term(Or)),
        "pow" => Some(factory.create_builtin_term(Pow)),
        "remainder" => Some(factory.create_builtin_term(Remainder)),
        _ => None,
    }
}

fn parse_special_form<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    target: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, Option<T>>
where
    T::Builtin: LispParserBuiltin,
{
    match target {
        SyntaxDatum::Symbol(identifier) => match *identifier {
            "quote" => {
                parse_quote_expression(input, args, symbol_cache, factory, allocator).map(Some)
            }
            "lambda" => parse_lambda_expression(
                input,
                args,
                scope,
                symbol_cache,
                evaluation_cache,
                factory,
                allocator,
            )
            .map(Some),
            "let" => parse_let_expression(
                input,
                args,
                scope,
                symbol_cache,
                evaluation_cache,
                factory,
                allocator,
            )
            .map(Some),
            "letrec" => parse_letrec_expression(
                input,
                args,
                scope,
                symbol_cache,
                evaluation_cache,
                factory,
                allocator,
            )
            .map(Some),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn parse_function_application<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    _input: &SyntaxDatum<'src>,
    target: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    Ok(factory.create_application_term(
        parse_expression(
            target,
            scope,
            symbol_cache,
            evaluation_cache,
            factory,
            allocator,
        )?,
        parse_function_arguments(
            args,
            scope,
            symbol_cache,
            evaluation_cache,
            factory,
            allocator,
        )?,
    ))
}

fn parse_function_arguments<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    items: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T::ExpressionList>
where
    T::Builtin: LispParserBuiltin,
{
    Ok(allocator.create_list(
        items
            .iter()
            .map(|item| {
                parse_expression(
                    item,
                    scope,
                    symbol_cache,
                    evaluation_cache,
                    factory,
                    allocator,
                )
            })
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn parse_quote_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    symbol_cache: &mut SymbolCache<'src>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    if args.len() != 1 {
        return Err(ParserError::new(
            String::from("Invalid quote expression"),
            input,
        ));
    }
    let mut args = args.iter();
    let arg = args.next().unwrap();
    Ok(parse_quoted_value(arg, symbol_cache, factory, allocator))
}

fn parse_quoted_value<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    match input {
        SyntaxDatum::IntegerLiteral(value) => parse_integer_literal(input, *value, factory),
        SyntaxDatum::FloatLiteral(value) => parse_float_literal(input, *value, factory),
        SyntaxDatum::StringLiteral(value) => parse_string_literal(input, value, factory, allocator),
        SyntaxDatum::Symbol(symbol) => parse_symbol_literal(input, symbol, symbol_cache, factory),
        SyntaxDatum::List(values) => parse_quoted_list(values, symbol_cache, factory, allocator),
    }
}

fn parse_quoted_list<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    values: &[SyntaxDatum<'src>],
    symbol_cache: &mut SymbolCache<'src>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    match values.len() {
        0 => create_enum(0, empty(), factory, allocator),
        _ => {
            let value = values.iter().next().unwrap();
            create_enum(
                1,
                vec![
                    parse_quoted_value(value, symbol_cache, factory, allocator),
                    parse_quoted_list(&values[1..], symbol_cache, factory, allocator),
                ]
                .into_iter(),
                factory,
                allocator,
            )
        }
    }
}

fn create_enum<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    discriminant: usize,
    args: impl ExactSizeIterator<Item = T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: LispParserBuiltin,
{
    factory.create_list_term({
        let args = args.into_iter();
        allocator.create_sized_list(
            1 + args.len(),
            once(factory.create_int_term(discriminant as i32)).chain(args),
        )
    })
}

fn parse_lambda_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    let mut args = args.iter();
    let arg_list = match args.next() {
        Some(arg_list) => Ok(arg_list),
        _ => Err(ParserError::new(
            String::from("Missing lambda expression arguments"),
            input,
        )),
    }?;
    let arg_names = match arg_list {
        SyntaxDatum::List(items) => parse_lambda_argument_names(&items),
        _ => Err(ParserError::new(
            String::from("Invalid lambda expression argument definition"),
            input,
        )),
    }?;
    let body = match args.next() {
        Some(body) => Ok(body),
        _ => Err(ParserError::new(
            String::from("Missing lambda expression body"),
            input,
        )),
    }?;
    let arity = arg_names.len();
    let child_scope = scope.create_child(&arg_names);
    let body = parse_expression(
        body,
        &child_scope,
        symbol_cache,
        evaluation_cache,
        factory,
        allocator,
    )?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(factory.create_lambda_term(arity, body)),
    }
}

fn parse_lambda_argument_names<'src>(
    items: &[SyntaxDatum<'src>],
) -> ParserResult<'src, Vec<&'src str>> {
    items
        .iter()
        .map(|arg| match arg {
            SyntaxDatum::Symbol(arg_name) => Ok(*arg_name),
            _ => Err(ParserError::new(String::from("Invalid argument name"), arg)),
        })
        .collect()
}

fn parse_let_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    parse_binding_expression(
        &BindingExpressionType::Let,
        input,
        args,
        scope,
        symbol_cache,
        evaluation_cache,
        factory,
        allocator,
    )
}

fn parse_letrec_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    input: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    parse_binding_expression(
        &BindingExpressionType::LetRec,
        input,
        args,
        scope,
        symbol_cache,
        evaluation_cache,
        factory,
        allocator,
    )
}

enum BindingExpressionType {
    Let,
    LetRec,
}

fn get_binding_type_name(binding_type: &BindingExpressionType) -> &'static str {
    match binding_type {
        BindingExpressionType::Let => "let",
        BindingExpressionType::LetRec => "letrec",
    }
}

fn parse_binding_expression<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    binding_type: &BindingExpressionType,
    input: &SyntaxDatum<'src>,
    args: &[SyntaxDatum<'src>],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T>
where
    T::Builtin: LispParserBuiltin,
{
    let mut args = args.iter();
    let binding_definitions = match args.next() {
        Some(binding_definitions) => Ok(binding_definitions),
        _ => Err(ParserError::new(
            format!(
                "Missing {} expression bindings",
                get_binding_type_name(binding_type)
            ),
            input,
        )),
    }?;
    let binding_definitions = match binding_definitions {
        SyntaxDatum::List(items) => parse_binding_definitions(&items),
        _ => Err(ParserError::new(
            format!(
                "Invalid {} expression bindings",
                get_binding_type_name(binding_type)
            ),
            input,
        )),
    }?;
    let body = match args.next() {
        Some(body) => Ok(body),
        _ => Err(ParserError::new(
            format!(
                "Missing {} expression body",
                get_binding_type_name(binding_type)
            ),
            input,
        )),
    }?;
    let initializers = parse_binding_initializers(
        binding_type,
        &binding_definitions,
        scope,
        symbol_cache,
        evaluation_cache,
        factory,
        allocator,
    )?;
    let child_scope = scope.create_child(
        &binding_definitions
            .iter()
            .map(|(identifier, _)| *identifier)
            .collect::<Vec<_>>(),
    );
    let body = parse_expression(
        body,
        &child_scope,
        symbol_cache,
        evaluation_cache,
        factory,
        allocator,
    )?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(match initializers.len() {
            0 => body,
            num_bindings => match binding_type {
                BindingExpressionType::Let => factory.create_application_term(
                    factory.create_lambda_term(num_bindings, body),
                    initializers,
                ),
                BindingExpressionType::LetRec => match num_bindings {
                    1 => factory.create_application_term(
                        factory.create_lambda_term(1, body),
                        allocator.create_list(once(factory.create_recursive_term(
                            factory.create_lambda_term(1, initializers.iter().next().unwrap()),
                        ))),
                    ),
                    _ => {
                        let initializer_replacements = (0..num_bindings)
                            .map(|index| {
                                (
                                    (num_bindings - index - 1),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(index as i32),
                                        ),
                                    ),
                                )
                            })
                            .collect::<Vec<_>>();
                        let initializer_substitutions =
                            Substitutions::named(&initializer_replacements, None);
                        let bindings = factory.create_recursive_term(factory.create_lambda_term(
                            1,
                            factory.create_list_term(allocator.create_list(
                                initializers.iter().map(|initializer| {
                                    initializer
                                        .substitute_static(
                                            &initializer_substitutions,
                                            factory,
                                            allocator,
                                            evaluation_cache,
                                        )
                                        .unwrap_or(initializer)
                                }),
                            )),
                        ));
                        factory.create_application_term(
                            factory.create_lambda_term(num_bindings, body),
                            allocator.create_list((0..num_bindings).map(|index| {
                                factory.create_application_term(
                                    factory.create_builtin_term(Get),
                                    allocator.create_pair(
                                        bindings.clone(),
                                        factory.create_int_term(index as i32),
                                    ),
                                )
                            })),
                        )
                    }
                },
            },
        }),
    }
}

fn parse_binding_initializers<'src, T: Expression + Rewritable<T> + Reducible<T>>(
    binding_type: &BindingExpressionType,
    binding_definitions: &[(&'src str, &SyntaxDatum<'src>)],
    scope: &LexicalScope<'src>,
    symbol_cache: &mut SymbolCache<'src>,
    evaluation_cache: &mut impl EvaluationCache<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> ParserResult<'src, T::ExpressionList>
where
    T::Builtin: LispParserBuiltin,
{
    match binding_type {
        BindingExpressionType::Let => Ok(allocator.create_list(
            binding_definitions
                .iter()
                .map(|(_, initializer)| {
                    parse_expression(
                        initializer,
                        scope,
                        symbol_cache,
                        evaluation_cache,
                        factory,
                        allocator,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        BindingExpressionType::LetRec => {
            let child_scope = scope.create_child(
                &binding_definitions
                    .iter()
                    .map(|(identifier, _)| *identifier)
                    .collect::<Vec<_>>(),
            );
            Ok(allocator.create_list(
                binding_definitions
                    .iter()
                    .map(|(_, initializer)| {
                        parse_expression(
                            initializer,
                            &child_scope,
                            symbol_cache,
                            evaluation_cache,
                            factory,
                            allocator,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
    }
}

fn parse_binding_definitions<'src, 'b>(
    items: &'b [SyntaxDatum<'src>],
) -> ParserResult<'src, Vec<(&'src str, &'b SyntaxDatum<'src>)>> {
    items
        .iter()
        .map(|binding| match binding {
            SyntaxDatum::List(binding) if binding.len() == 2 => {
                let mut items = binding.iter();
                let identifier = match items.next().unwrap() {
                    SyntaxDatum::Symbol(identifier) => Ok(*identifier),
                    identifier => Err(ParserError::new(
                        String::from("Invalid binding identifier"),
                        identifier,
                    )),
                }?;
                let initializer = items.next().unwrap();
                return Ok((identifier, initializer));
            }
            _ => Err(ParserError::new(
                String::from("Invalid binding definition"),
                binding,
            )),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::iter::once;

    use reflex::{
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, Expression, ExpressionFactory, GraphNode,
            HeapAllocator, Rewritable, SignalType, StateCache,
        },
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    //     #[test]
    //     fn invalid_expression() {
    //         assert_eq!(
    //             parse("#"),
    //             Err(ParserError {
    //                 message: String::from("Expected expression, received '#'"),
    //                 source: None
    //             })
    //         );
    //         assert_eq!(
    //             parse("1."),
    //             Err(ParserError {
    //                 message: String::from("Expected end of input, received '.'"),
    //                 source: None
    //             })
    //         );
    //         assert_eq!(
    //             parse(".1"),
    //             Err(ParserError {
    //                 message: String::from("Expected expression, received '.'"),
    //                 source: None
    //             })
    //         );
    //         assert_eq!(
    //             parse("-.1"),
    //             Err(ParserError {
    //                 message: String::from("Expected end of input, received '.'"),
    //                 source: None
    //             })
    //         );
    //     }

    //     #[test]
    //     fn multiple_expressions() {
    //         assert_eq!(
    //             parse("3 3"),
    //             Err(ParserError {
    //                 message: String::from("Expected end of input, received '3'"),
    //                 source: None,
    //             })
    //         );
    //         assert_eq!(
    //             parse("3 foo"),
    //             Err(ParserError {
    //                 message: String::from("Expected end of input, received 'f'"),
    //                 source: None,
    //             })
    //         );
    //     }

    //     #[test]
    //     fn ignore_extra_whitespace() {
    //         assert_eq!(
    //             parse("  \n\r\t3\n\r\t  "),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(3))))
    //         );
    //     }

    //     #[test]
    //     fn global_primitives() {
    //         assert_eq!(
    //             parse("#t"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Boolean(true))))
    //         );
    //         assert_eq!(
    //             parse("#f"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Boolean(false))))
    //         );
    //     }

    //     #[test]
    //     fn primitive_values() {
    //         assert_eq!(
    //             parse("0"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(0))))
    //         );
    //         assert_eq!(
    //             parse("-0"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(0))))
    //         );
    //         assert_eq!(
    //             parse("3"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(3))))
    //         );
    //         assert_eq!(
    //             parse("-3"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(-3))))
    //         );
    //         assert_eq!(
    //             parse("123"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(123))))
    //         );
    //         assert_eq!(
    //             parse("-123"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(-123))))
    //         );
    //         assert_eq!(
    //             parse("0.0"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(0.0))))
    //         );
    //         assert_eq!(
    //             parse("-0.0"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(-0.0))))
    //         );
    //         assert_eq!(
    //             parse("3.142"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(3.142))))
    //         );
    //         assert_eq!(
    //             parse("-3.142"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(-3.142))))
    //         );
    //         assert_eq!(
    //             parse("123.45"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(123.45))))
    //         );
    //         assert_eq!(
    //             parse("-123.45"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(-123.45))))
    //         );
    //         assert_eq!(
    //             parse("\"\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\" \""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from(" ")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo bar\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo bar")
    //             ))))
    //         );
    //     }

    //     #[test]
    //     fn symbols() {
    //         assert_eq!(
    //             parse("'foo"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
    //         );
    //         assert_eq!(
    //             parse("(quote foo)"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
    //         );
    //         assert_eq!(
    //             parse("(+ 'foo 'foo)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Builtin(Stdlib::Add)),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Symbol(0))),
    //                     Expression::new(Term::Value(ValueTerm::Symbol(0))),
    //                 ],
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(+ 'foo 'bar)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Builtin(Stdlib::Add)),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Symbol(0))),
    //                     Expression::new(Term::Value(ValueTerm::Symbol(1))),
    //                 ],
    //             )))),
    //         );
    //     }

    //     #[test]
    //     fn escaped_string_literals() {
    //         assert_eq!(
    //             parse("\"foo\\\\bar\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo\\bar")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo\\nbar\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo\nbar")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo\\tbar\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo\tbar")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo\\rbar\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo\rbar")
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("\"foo\\\"bar\\\"baz\""),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo\"bar\"baz")
    //             ))))
    //         );
    //     }

    //     #[test]
    //     fn identifiers() {
    //         assert_eq!(
    //             parse("foo"),
    //             Err(ParserError::new(
    //                 String::from("Undefined identifier: foo"),
    //                 &SyntaxDatum::Symbol("foo")
    //             )),
    //         );
    //         assert_eq!(
    //             parse("(lambda (a) a)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (_) _)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo) foo)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (_foo) _foo)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo!) foo!)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo?) foo?)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo_bar) foo_bar)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo-bar) foo-bar)"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //             ))))
    //         );
    //     }

    //     #[test]
    //     fn quoted_expressions() {
    //         assert_eq!(
    //             parse("(quote 3)"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
    //         );
    //         assert_eq!(
    //             parse("(quote 3.142)"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Float(3.142)))),
    //         );
    //         assert_eq!(
    //             parse("(quote \"foo\")"),
    //             Ok(Expression::new(Term::Value(ValueTerm::String(
    //                 StringValue::from("foo")
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(quote foo)"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
    //         );
    //         assert_eq!(
    //             parse("'()"),
    //             Ok(Expression::new(Term::Struct(StructTerm::new(
    //                 None,
    //                 vec![Expression::new(Term::Value(ValueTerm::Int(0))),]
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("'(3 4 5)"),
    //             Ok(Expression::new(Term::Struct(StructTerm::new(
    //                 None,
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Int(1))),
    //                     Expression::new(Term::Value(ValueTerm::Int(3))),
    //                     Expression::new(Term::Struct(StructTerm::new(
    //                         None,
    //                         vec![
    //                             Expression::new(Term::Value(ValueTerm::Int(1))),
    //                             Expression::new(Term::Value(ValueTerm::Int(4))),
    //                             Expression::new(Term::Struct(StructTerm::new(
    //                                 None,
    //                                 vec![
    //                                     Expression::new(Term::Value(ValueTerm::Int(1))),
    //                                     Expression::new(Term::Value(ValueTerm::Int(5))),
    //                                     Expression::new(Term::Struct(StructTerm::new(
    //                                         None,
    //                                         vec![Expression::new(Term::Value(ValueTerm::Int(0))),]
    //                                     ))),
    //                                 ],
    //                             ),)),
    //                         ],
    //                     ),)),
    //                 ],
    //             )),)),
    //         );
    //     }

    //     #[test]
    //     fn let_bindings() {
    //         assert_eq!(
    //             parse("(let () 3)"),
    //             Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
    //         );
    //         assert_eq!(
    //             parse("(let ((foo 3)) foo)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                 ))),
    //                 vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(let ((foo 3) (bar 4)) foo)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 2, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                 ))),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Int(3))),
    //                     Expression::new(Term::Value(ValueTerm::Int(4))),
    //                 ],
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(let ((foo 3) (bar 4)) bar)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 2, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                 ))),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Int(3))),
    //                     Expression::new(Term::Value(ValueTerm::Int(4))),
    //                 ],
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(let ((foo (lambda (bar baz) (+ bar baz)))) (foo 3 4))"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Application(ApplicationTerm::new(
    //                         Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         vec![
    //                             Expression::new(Term::Value(ValueTerm::Int(3))),
    //                             Expression::new(Term::Value(ValueTerm::Int(4))),
    //                         ],
    //                     ))),
    //                 ))),
    //                 vec![Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 2, None),
    //                     Expression::new(Term::Application(ApplicationTerm::new(
    //                         Expression::new(Term::Builtin(Stdlib::Add)),
    //                         vec![
    //                             Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                             Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         ],
    //                     ))),
    //                 ))),],
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (outer) (let ((foo 3) (bar 4)) outer))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Application(ApplicationTerm::new(
    //                     Expression::new(Term::Lambda(LambdaTerm::new(
    //                         Arity::from(0, 2, None),
    //                         Expression::new(Term::Variable(VariableTerm::scoped(2))),
    //                     ))),
    //                     vec![
    //                         Expression::new(Term::Value(ValueTerm::Int(3))),
    //                         Expression::new(Term::Value(ValueTerm::Int(4))),
    //                     ],
    //                 ))),
    //             )))),
    //         );
    //     }

    #[test]
    fn letrec_bindings() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        assert_eq!(
            parse("(letrec () 3)", &factory, &allocator),
            Ok(factory.create_int_term(3)),
        );
        assert_eq!(
            parse("(letrec ((foo 3)) foo)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(1, factory.create_variable_term(0)),
                allocator.create_unit_list(factory.create_recursive_term(
                    factory.create_lambda_term(1, factory.create_int_term(3))
                )),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) foo)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(1)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                ),),
                            )),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                ),),
                            )),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) bar)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(0)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                ),),
                            )),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_int_term(4),
                                ),),
                            )),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(lambda (outer) (letrec ((foo 3)) outer))",
                &factory,
                &allocator
            ),
            Ok(factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_lambda_term(1, factory.create_variable_term(1)),
                    allocator.create_unit_list(factory.create_recursive_term(
                        factory.create_lambda_term(1, factory.create_int_term(3)),
                    )),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(lambda (outer) (letrec ((foo 3) (bar 4)) outer))",
                &factory,
                &allocator
            ),
            Ok(factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_lambda_term(2, factory.create_variable_term(2)),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(
                                factory.create_recursive_term(factory.create_lambda_term(
                                    1,
                                    factory.create_list_term(allocator.create_pair(
                                        factory.create_int_term(3),
                                        factory.create_int_term(4),
                                    )),
                                ),),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(
                                factory.create_recursive_term(factory.create_lambda_term(
                                    1,
                                    factory.create_list_term(allocator.create_pair(
                                        factory.create_int_term(3),
                                        factory.create_int_term(4),
                                    )),
                                ),),
                                factory.create_int_term(1),
                            ),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) foo)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(1)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            ),),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            ),),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) bar)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(0)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            ),),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            ),),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(letrec ((foo 3) (bar foo)) (+ foo bar))",
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    2,
                    factory.create_application_term(
                        factory.create_builtin_term(Add),
                        allocator.create_pair(
                            factory.create_variable_term(1),
                            factory.create_variable_term(0),
                        ),
                    )
                ),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            )),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(3),
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(0),
                                        ),
                                    ),
                                )),
                            )),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) foo)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(1)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(1),
                                        )
                                    ),
                                    factory.create_int_term(3),
                                ))
                            )),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(1),
                                        )
                                    ),
                                    factory.create_int_term(3),
                                ))
                            )),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) bar)", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(2, factory.create_variable_term(0)),
                allocator.create_pair(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(1),
                                        )
                                    ),
                                    factory.create_int_term(3),
                                ))
                            )),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(factory.create_lambda_term(
                                1,
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_int_term(1),
                                        )
                                    ),
                                    factory.create_int_term(3),
                                ))
                            )),
                            factory.create_int_term(1),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse("(letrec ((foo (lambda (bar baz) (+ (+ first bar) (+ second baz)))) (first 3) (second 4)) (foo 5 6))", &factory, &allocator),
            Ok(factory.create_application_term(
                factory.create_lambda_term(3, factory.create_application_term(
                    factory.create_variable_term(2),
                    allocator.create_pair(
                        factory.create_int_term(5),
                        factory.create_int_term(6),
                    )
                )),
                allocator.create_triple(
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(
                                factory.create_lambda_term(1, factory.create_list_term(
                                    allocator.create_triple(
                                        factory.create_lambda_term(2, factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(1),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(1),
                                                    ),
                                                ),
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(2),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(0),
                                                    ),
                                                ),
                                            ),
                                        )),
                                        factory.create_int_term(3),
                                        factory.create_int_term(4),
                                    ),
                                )),
                            ),
                            factory.create_int_term(0),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(
                                factory.create_lambda_term(1, factory.create_list_term(
                                    allocator.create_triple(
                                        factory.create_lambda_term(2, factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(1),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(1),
                                                    ),
                                                ),
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(2),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(0),
                                                    ),
                                                ),
                                            ),
                                        )),
                                        factory.create_int_term(3),
                                        factory.create_int_term(4),
                                    ),
                                )),
                            ),
                            factory.create_int_term(1),
                        ),
                    ),
                    factory.create_application_term(
                        factory.create_builtin_term(Get),
                        allocator.create_pair(
                            factory.create_recursive_term(
                                factory.create_lambda_term(1, factory.create_list_term(
                                    allocator.create_triple(
                                        factory.create_lambda_term(2, factory.create_application_term(
                                            factory.create_builtin_term(Add),
                                            allocator.create_pair(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(1),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(1),
                                                    ),
                                                ),
                                                factory.create_application_term(
                                                    factory.create_builtin_term(Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(Get),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(2),
                                                                factory.create_int_term(2),
                                                            ),
                                                        ),
                                                        factory.create_variable_term(0),
                                                    ),
                                                ),
                                            ),
                                        )),
                                        factory.create_int_term(3),
                                        factory.create_int_term(4),
                                    ),
                                )),
                            ),
                            factory.create_int_term(2),
                        ),
                    ),
                ),
            )),
        );
        assert_eq!(
            parse(
                "(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))",
                &factory,
                &allocator
            ),
            Ok(factory.create_application_term(
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_variable_term(0),
                        allocator.create_unit_list(factory.create_int_term(5)),
                    )
                ),
                allocator.create_unit_list(
                    factory.create_recursive_term(
                        factory.create_lambda_term(
                            1,
                            factory.create_lambda_term(
                                1,
                                factory.create_application_term(
                                    factory.create_builtin_term(If),
                                    allocator.create_triple(
                                        factory.create_application_term(
                                            factory.create_builtin_term(Equal),
                                            allocator.create_pair(
                                                factory.create_variable_term(0),
                                                factory.create_int_term(1),
                                            ),
                                        ),
                                        factory.create_variable_term(0),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Multiply),
                                            allocator.create_pair(
                                                factory.create_variable_term(0),
                                                factory.create_application_term(
                                                    factory.create_variable_term(1),
                                                    allocator.create_unit_list(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(
                                                                Stdlib::Subtract
                                                            ),
                                                            allocator.create_pair(
                                                                factory.create_variable_term(0),
                                                                factory.create_int_term(1),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            )
                        ),
                    )
                ),
            )),
        );
    }

    //     #[test]
    //     fn function_expressions() {
    //         assert_eq!(
    //             parse("(lambda () (+ 3 4))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 0, None),
    //                 Expression::new(Term::Application(ApplicationTerm::new(
    //                     Expression::new(Term::Builtin(Stdlib::Add)),
    //                     vec![
    //                         Expression::new(Term::Value(ValueTerm::Int(3))),
    //                         Expression::new(Term::Value(ValueTerm::Int(4))),
    //                     ]
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo) (+ foo 4))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Application(ApplicationTerm::new(
    //                     Expression::new(Term::Builtin(Stdlib::Add)),
    //                     vec![
    //                         Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         Expression::new(Term::Value(ValueTerm::Int(4)))
    //                     ]
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo bar) (+ foo bar))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 2, None),
    //                 Expression::new(Term::Application(ApplicationTerm::new(
    //                     Expression::new(Term::Builtin(Stdlib::Add)),
    //                     vec![
    //                         Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                         Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //                     ]
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (first) (lambda (second) (lambda (foo bar) (+ foo bar))))",),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Lambda(LambdaTerm::new(
    //                         Arity::from(0, 2, None),
    //                         Expression::new(Term::Application(ApplicationTerm::new(
    //                             Expression::new(Term::Builtin(Stdlib::Add)),
    //                             vec![
    //                                 Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                                 Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //                             ]
    //                         ))),
    //                     ))),
    //                 ))),
    //             )))),
    //         );
    //     }

    //     #[test]
    //     fn closures() {
    //         assert_eq!(
    //             parse("(lambda (foo) (lambda () foo))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 0, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo bar) (lambda () foo))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 2, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 0, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(1)))
    //                 ))),
    //             ))))
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo bar) (lambda () bar))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 2, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 0, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo bar) (lambda (baz) (+ foo baz)))",),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 2, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Application(ApplicationTerm::new(
    //                         Expression::new(Term::Builtin(Stdlib::Add)),
    //                         vec![
    //                             Expression::new(Term::Variable(VariableTerm::scoped(2))),
    //                             Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         ]
    //                     ))),
    //                 ))),
    //             )))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (first second third) (lambda (fourth fifth) (lambda (sixth) (+ first (+ second (+ third (+ fourth (+ fifth sixth))))))))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 3, None),
    //                 Expression::new(Term::Lambda(
    //                     LambdaTerm::new(
    //                         Arity::from(0, 2, None),
    //                         Expression::new(Term::Lambda(
    //                             LambdaTerm::new(
    //                                 Arity::from(0, 1, None),
    //                                 Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(Stdlib::Add)), vec![
    //                                     Expression::new(Term::Variable(VariableTerm::scoped(5))),
    //                                     Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(Stdlib::Add)), vec![
    //                                         Expression::new(Term::Variable(VariableTerm::scoped(4))),
    //                                         Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(Stdlib::Add)), vec![
    //                                             Expression::new(Term::Variable(VariableTerm::scoped(3))),
    //                                             Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(Stdlib::Add)), vec![
    //                                                 Expression::new(Term::Variable(VariableTerm::scoped(2))),
    //                                                 Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(Stdlib::Add)), vec![
    //                                                     Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                                                     Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                                                 ]))),
    //                                             ]))),
    //                                         ]))),
    //                                     ]))),
    //                                 ]))),
    //                             ),
    //                         )),
    //                 )),
    //             ))))),
    //         );
    //         assert_eq!(
    //             parse("(lambda (foo) (lambda () (lambda () foo)))"),
    //             Ok(Expression::new(Term::Lambda(LambdaTerm::new(
    //                 Arity::from(0, 1, None),
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 0, None),
    //                     Expression::new(Term::Lambda(LambdaTerm::new(
    //                         Arity::from(0, 0, None),
    //                         Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                     ))),
    //                 ))),
    //             )))),
    //         );
    //     }

    //     #[test]
    //     fn function_applications() {
    //         assert_eq!(
    //             parse("((lambda (foo) foo) 3)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(0)))
    //                 ))),
    //                 vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
    //             ),))),
    //         );
    //         assert_eq!(
    //             parse("((lambda (foo bar baz) foo) 3 4 5)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 3, None),
    //                     Expression::new(Term::Variable(VariableTerm::scoped(2)))
    //                 ))),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Int(3))),
    //                     Expression::new(Term::Value(ValueTerm::Int(4))),
    //                     Expression::new(Term::Value(ValueTerm::Int(5))),
    //                 ],
    //             ),))),
    //         );
    //         assert_eq!(
    //             parse("((lambda (+) (+ 3 4)) (lambda (first second) (+ first second)))",),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 1, None),
    //                     Expression::new(Term::Application(ApplicationTerm::new(
    //                         Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         vec![
    //                             Expression::new(Term::Value(ValueTerm::Int(3))),
    //                             Expression::new(Term::Value(ValueTerm::Int(4))),
    //                         ],
    //                     ))),
    //                 ))),
    //                 vec![Expression::new(Term::Lambda(LambdaTerm::new(
    //                     Arity::from(0, 2, None),
    //                     Expression::new(Term::Application(ApplicationTerm::new(
    //                         Expression::new(Term::Builtin(Stdlib::Add)),
    //                         vec![
    //                             Expression::new(Term::Variable(VariableTerm::scoped(1))),
    //                             Expression::new(Term::Variable(VariableTerm::scoped(0))),
    //                         ]
    //                     ))),
    //                 )))],
    //             ),))),
    //         );
    //     }

    //     #[test]
    //     fn builtin_applications() {
    //         assert_eq!(
    //             parse("(+ 3 4)"),
    //             Ok(Expression::new(Term::Application(ApplicationTerm::new(
    //                 Expression::new(Term::Builtin(Stdlib::Add)),
    //                 vec![
    //                     Expression::new(Term::Value(ValueTerm::Int(3))),
    //                     Expression::new(Term::Value(ValueTerm::Int(4))),
    //                 ],
    //             )))),
    //         );
    //     }

    fn create_error_signal_term<T: Expression>(
        message: impl Into<String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T {
        factory.create_signal_term(
            allocator.create_signal_list(once(create_error_signal(message, factory, allocator))),
        )
    }

    fn create_error_signal<T: Expression>(
        message: impl Into<String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T::Signal {
        allocator.create_signal(
            SignalType::Error,
            factory.create_string_term(allocator.create_string(message)),
            factory.create_nil_term(),
        )
    }

    #[test]
    fn value_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("3", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),)
        );
    }

    #[test]
    fn lambda_optimizations() {
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(lambda (foo) foo)", &factory, &allocator).unwrap();
        let result = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(result, None);

        let expression = parse("(lambda (foo bar) (+ foo bar))", &factory, &allocator).unwrap();
        let normalized = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+", &factory, &allocator).unwrap()));

        let expression = parse("(lambda (foo bar baz) (+ foo bar))", &factory, &allocator).unwrap();
        let normalized = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+", &factory, &allocator).unwrap()));
    }

    #[test]
    fn lambda_application_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression =
            parse("((lambda (foo bar) (+ foo bar)) 3 4)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
        let expression =
            parse("((lambda (foo bar) (((lambda (foo bar) (lambda (foo bar) (+ foo bar))) foo bar) foo bar)) 3 4)", &factory, &allocator)
                .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
    }

    #[test]
    fn invalid_function_applications() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<function:1>: Expected 1 argument, received 0",
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "((lambda (first second third) first) 3)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<function:3>: Expected 3 arguments, received 1",
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "((lambda (first second third) first) 3 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<function:3>: Expected 3 arguments, received 2",
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("+", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_builtin_term(Add), DependencyList::empty(),),
        );
    }

    #[test]
    fn builtin_application_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ 1 2)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
    }

    #[test]
    fn nested_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4 + 5), DependencyList::empty(),),
        );
    }

    #[test]
    fn signal_short_circuiting() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((/ 3 0) 4 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) 4)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ 3 (/ 4 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 4 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 4 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_signal_term(HeapAllocator::create_signal_list(
                    &allocator,
                    vec![
                        create_error_signal(
                            "<stdlib:Divide>: Division by zero: 3 / 0",
                            &factory,
                            &allocator,
                        ),
                        create_error_signal(
                            "<stdlib:Divide>: Division by zero: 4 / 0",
                            &factory,
                            &allocator,
                        ),
                    ]
                )),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 3 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (+ (/ 3 0) 4) 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(let () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(let ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression =
            parse("(let ((foo 3) (bar 4)) (+ foo bar))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
        let expression = parse(
            "(let ((first 3)) (let ((second 4)) first))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
    }

    #[test]
    fn letrec_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(letrec () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo 3) (bar 4)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((first 3)) (let ((second 4)) first))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );

        let expression = parse(
            "(letrec ((foo 3) (bar foo)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 3), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo bar) (bar 3)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 3), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo 3) (bar (+ foo 1))) bar)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 1), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo (+ bar 1)) (bar 3)) foo)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 1), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_int_term(5 * 4 * 3 * 2 * 1),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-even? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_boolean_term(false), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_boolean_term(true), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo (cons 3 foo))) (car (cdr (cdr (cdr foo)))))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse(
            "(letrec ((foo (cons 1 bar)) (bar (cons 2 foo))) (car (cdr (cdr (cdr foo)))))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(2), DependencyList::empty(),),
        );
    }

    #[test]
    fn partial_evaluation() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse(
            "((lambda (foo) ((lambda (bar) (foo 3)) #f)) (lambda (foo) #t))",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_boolean_term(true))
        );

        let expression = parse(
            "
            (let ((identity (lambda (value) value)))
                (let ((identity2 (lambda (value) (identity value))))
                    ((lambda (value) (identity2 (* value 2)))
                      3)))
        ",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_int_term(6))
        );
    }
}
