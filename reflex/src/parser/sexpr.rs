// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, fmt};

use crate::{
    cache::{EvaluationCache, GenerationalGc},
    core::{
        ApplicationTerm, Arity, EnumTerm, Expression, LambdaTerm, RecursiveTerm, Rewritable,
        StructTerm, Substitutions, Term, VariableTerm,
    },
    stdlib::{
        builtin::BuiltinTerm,
        value::{StringValue, SymbolId, ValueTerm},
    },
};

mod lexer;
use lexer::{parse_syntax, SyntaxDatum};

type ParserResult<'a, T> = Result<T, ParserError<'a>>;

#[derive(Debug, PartialEq)]
pub struct ParserError<'a> {
    message: String,
    source: Option<SyntaxDatum<'a>>,
}
impl<'a> ParserError<'a> {
    fn new(message: String, source: &SyntaxDatum<'a>) -> Self {
        ParserError {
            message,
            source: Some(source.clone()),
        }
    }
    pub fn message(&self) -> &str {
        &self.message
    }
}
impl<'a> fmt::Display for ParserError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone)]
struct LexicalScope<'a> {
    bindings: Vec<&'a str>,
}
impl<'a> LexicalScope<'a> {
    fn new() -> Self {
        LexicalScope {
            bindings: Vec::new(),
        }
    }
    fn create_child(&self, identifiers: &[&'a str]) -> LexicalScope<'a> {
        LexicalScope {
            bindings: self
                .bindings
                .iter()
                .chain(identifiers.iter())
                .map(|key| *key)
                .collect(),
        }
    }
    fn get(&self, identifier: &'a str) -> Option<usize> {
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

struct SymbolCache<'a> {
    cache: HashMap<&'a str, SymbolId>,
}
impl<'a> SymbolCache<'a> {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    pub fn get(&mut self, identifier: &'a str) -> SymbolId {
        self.cache
            .get(identifier)
            .map(|value| *value)
            .unwrap_or_else(|| {
                let id = SymbolId::from(self.cache.len());
                self.cache.insert(identifier, id);
                id
            })
    }
}

pub fn parse<'a>(input: &'a str) -> ParserResult<'a, Expression> {
    let syntax = match parse_syntax(input) {
        Ok(syntax) => Ok(syntax),
        Err(message) => Err(ParserError {
            message,
            source: None,
        }),
    }?;
    let scope = LexicalScope::new();
    let mut symbol_cache = SymbolCache::new();
    let mut evaluation_cache = GenerationalGc::new();
    parse_expression(&syntax, &scope, &mut symbol_cache, &mut evaluation_cache)
}

fn parse_expression<'a>(
    input: &SyntaxDatum<'a>,
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
    match input {
        SyntaxDatum::IntegerLiteral(value) => Ok(parse_integer_literal(input, *value)),
        SyntaxDatum::FloatLiteral(value) => Ok(parse_float_literal(input, *value)),
        SyntaxDatum::StringLiteral(value) => Ok(parse_string_literal(input, &value)),
        SyntaxDatum::Symbol(identifier) => parse_variable(input, identifier, scope),
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
                )? {
                    Some(result) => Ok(result),
                    _ => parse_function_application(
                        input,
                        target,
                        args,
                        scope,
                        symbol_cache,
                        evaluation_cache,
                    ),
                }
            }
        },
    }
}

fn parse_boolean_literal(_input: &SyntaxDatum, value: bool) -> Expression {
    Expression::new(Term::Value(ValueTerm::Boolean(value)))
}

fn parse_integer_literal(_input: &SyntaxDatum, value: i32) -> Expression {
    Expression::new(Term::Value(ValueTerm::Int(value)))
}

fn parse_float_literal(_input: &SyntaxDatum, value: f64) -> Expression {
    Expression::new(Term::Value(ValueTerm::Float(value)))
}

fn parse_string_literal(_input: &SyntaxDatum, value: &str) -> Expression {
    Expression::new(Term::Value(ValueTerm::String(StringValue::from(value))))
}

fn parse_symbol_literal<'a>(
    _input: &SyntaxDatum<'a>,
    symbol: &'a str,
    symbol_cache: &mut SymbolCache<'a>,
) -> Expression {
    Expression::new(Term::Value(ValueTerm::Symbol(symbol_cache.get(symbol))))
}

fn parse_variable<'a>(
    input: &SyntaxDatum<'a>,
    identifier: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression> {
    match scope.get(identifier) {
        None => match parse_global(input, identifier) {
            Some(result) => Ok(result),
            None => Err(ParserError::new(
                format!("Undefined identifier: {}", identifier),
                input,
            )),
        },
        Some(offset) => Ok(Expression::new(Term::Variable(VariableTerm::scoped(
            offset,
        )))),
    }
}

fn parse_global<'a>(input: &SyntaxDatum<'a>, identifier: &'a str) -> Option<Expression> {
    match identifier {
        "#t" => Some(parse_boolean_literal(input, true)),
        "#f" => Some(parse_boolean_literal(input, false)),
        identifier => parse_builtin_procedure(identifier)
            .map(|procedure| Expression::new(Term::Builtin(procedure))),
    }
}

fn parse_builtin_procedure(name: &str) -> Option<BuiltinTerm> {
    match name {
        "+" => Some(BuiltinTerm::Add),
        "-" => Some(BuiltinTerm::Subtract),
        "*" => Some(BuiltinTerm::Multiply),
        "/" => Some(BuiltinTerm::Divide),
        "=" => Some(BuiltinTerm::Equal),
        "abs" => Some(BuiltinTerm::Abs),
        "and" => Some(BuiltinTerm::And),
        "car" => Some(BuiltinTerm::Car),
        "cdr" => Some(BuiltinTerm::Cdr),
        "concat" => Some(BuiltinTerm::Concat),
        "cons" => Some(BuiltinTerm::Cons),
        "eq" => Some(BuiltinTerm::Eq),
        "gt" => Some(BuiltinTerm::Gt),
        "gte" => Some(BuiltinTerm::Gte),
        "if" => Some(BuiltinTerm::If),
        "lt" => Some(BuiltinTerm::Lt),
        "lte" => Some(BuiltinTerm::Lte),
        "not" => Some(BuiltinTerm::Not),
        "or" => Some(BuiltinTerm::Or),
        "pow" => Some(BuiltinTerm::Pow),
        "remainder" => Some(BuiltinTerm::Remainder),
        _ => None,
    }
}

fn parse_special_form<'a>(
    input: &SyntaxDatum<'a>,
    target: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Option<Expression>> {
    match target {
        SyntaxDatum::Symbol(identifier) => match *identifier {
            "quote" => parse_quote_expression(input, args, symbol_cache).map(Some),
            "lambda" => parse_lambda_expression(input, args, scope, symbol_cache, evaluation_cache)
                .map(Some),
            "let" => {
                parse_let_expression(input, args, scope, symbol_cache, evaluation_cache).map(Some)
            }
            "letrec" => parse_letrec_expression(input, args, scope, symbol_cache, evaluation_cache)
                .map(Some),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn parse_function_application<'a>(
    _input: &SyntaxDatum<'a>,
    target: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
    Ok(Expression::new(Term::Application(ApplicationTerm::new(
        parse_expression(target, scope, symbol_cache, evaluation_cache)?,
        parse_function_arguments(args, scope, symbol_cache, evaluation_cache)?,
    ))))
}

fn parse_function_arguments<'a>(
    items: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Vec<Expression>> {
    items
        .iter()
        .map(|item| parse_expression(item, scope, symbol_cache, evaluation_cache))
        .collect()
}

fn parse_quote_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    symbol_cache: &mut SymbolCache<'a>,
) -> ParserResult<'a, Expression> {
    if args.len() != 1 {
        return Err(ParserError::new(
            String::from("Invalid quote expression"),
            input,
        ));
    }
    let mut args = args.iter();
    let arg = args.next().unwrap();
    Ok(parse_quoted_value(arg, symbol_cache))
}

fn parse_quoted_value<'a>(
    input: &SyntaxDatum<'a>,
    symbol_cache: &mut SymbolCache<'a>,
) -> Expression {
    match input {
        SyntaxDatum::IntegerLiteral(value) => parse_integer_literal(input, *value),
        SyntaxDatum::FloatLiteral(value) => parse_float_literal(input, *value),
        SyntaxDatum::StringLiteral(value) => parse_string_literal(input, value),
        SyntaxDatum::Symbol(symbol) => parse_symbol_literal(input, symbol, symbol_cache),
        SyntaxDatum::List(values) => parse_quoted_list(values, symbol_cache),
    }
}

fn parse_quoted_list<'a>(
    values: &[SyntaxDatum<'a>],
    symbol_cache: &mut SymbolCache<'a>,
) -> Expression {
    match values.len() {
        0 => Expression::new(Term::Enum(EnumTerm::new(0, Vec::new()))),
        _ => {
            let value = values.iter().next().unwrap();
            Expression::new(Term::Enum(EnumTerm::new(
                1,
                vec![
                    parse_quoted_value(value, symbol_cache),
                    parse_quoted_list(&values[1..], symbol_cache),
                ],
            )))
        }
    }
}

fn parse_lambda_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
    let mut args = args.iter();
    let arg_list = match args.next() {
        Some(arg_list) => Ok(arg_list),
        _ => Err(ParserError::new(
            String::from("Missing lambda expression arguments"),
            input,
        )),
    }?;
    let arg_names = match arg_list {
        SyntaxDatum::List(items) => parse_lambda_argument_names(items),
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
    let body = parse_expression(body, &child_scope, symbol_cache, evaluation_cache)?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(Expression::new(Term::Lambda(LambdaTerm::new(
            Arity::from(0, arity, None),
            body,
        )))),
    }
}

fn parse_lambda_argument_names<'a>(items: &[SyntaxDatum<'a>]) -> ParserResult<'a, Vec<&'a str>> {
    items
        .iter()
        .map(|arg| match arg {
            SyntaxDatum::Symbol(arg_name) => Ok(*arg_name),
            _ => Err(ParserError::new(String::from("Invalid argument name"), arg)),
        })
        .collect()
}

fn parse_let_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
    parse_binding_expression(
        &BindingExpressionType::Let,
        input,
        args,
        scope,
        symbol_cache,
        evaluation_cache,
    )
}

fn parse_letrec_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
    parse_binding_expression(
        &BindingExpressionType::LetRec,
        input,
        args,
        scope,
        symbol_cache,
        evaluation_cache,
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

fn parse_binding_expression<'a>(
    binding_type: &BindingExpressionType,
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Expression> {
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
        SyntaxDatum::List(items) => parse_binding_definitions(items),
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
    )?;
    let child_scope = scope.create_child(
        &binding_definitions
            .iter()
            .map(|(identifier, _)| *identifier)
            .collect::<Vec<_>>(),
    );
    let body = parse_expression(body, &child_scope, symbol_cache, evaluation_cache)?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(match initializers.len() {
            0 => body,
            num_bindings => match binding_type {
                BindingExpressionType::Let => {
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, num_bindings, None),
                            body,
                        ))),
                        initializers,
                    )))
                }
                BindingExpressionType::LetRec => match num_bindings {
                    1 => Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            body,
                        ))),
                        vec![Expression::new(Term::Recursive(RecursiveTerm::new(
                            Expression::new(Term::Lambda(LambdaTerm::new(
                                Arity::from(0, 1, None),
                                initializers.into_iter().next().unwrap(),
                            ))),
                        )))],
                    ))),
                    _ => {
                        let initializer_replacements = (0..num_bindings)
                            .map(|index| {
                                (
                                    (num_bindings - index - 1),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                0,
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(
                                                index as i32,
                                            ))),
                                        ],
                                    ))),
                                )
                            })
                            .collect::<Vec<_>>();
                        let initializer_substitutions =
                            Substitutions::named(&initializer_replacements, None);
                        let bindings = Expression::new(Term::Recursive(RecursiveTerm::new(
                            Expression::new(Term::Lambda(LambdaTerm::new(
                                Arity::from(0, 1, None),
                                Expression::new(Term::Struct(StructTerm::new(
                                    None,
                                    initializers
                                        .iter()
                                        .map(|initializer| {
                                            initializer
                                                .substitute_static(
                                                    &initializer_substitutions,
                                                    evaluation_cache,
                                                )
                                                .unwrap_or_else(|| Expression::clone(initializer))
                                        })
                                        .collect(),
                                ))),
                            ))),
                        )));
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Lambda(LambdaTerm::new(
                                Arity::from(0, num_bindings, None),
                                body,
                            ))),
                            (0..num_bindings)
                                .map(|index| {
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                        vec![
                                            Expression::clone(&bindings),
                                            Expression::new(Term::Value(ValueTerm::Int(
                                                index as i32,
                                            ))),
                                        ],
                                    )))
                                })
                                .collect(),
                        )))
                    }
                },
            },
        }),
    }
}

fn parse_binding_initializers<'a>(
    binding_type: &BindingExpressionType,
    binding_definitions: &[(&'a str, &SyntaxDatum<'a>)],
    scope: &LexicalScope<'a>,
    symbol_cache: &mut SymbolCache<'a>,
    evaluation_cache: &mut impl EvaluationCache,
) -> ParserResult<'a, Vec<Expression>> {
    match binding_type {
        BindingExpressionType::Let => binding_definitions
            .iter()
            .map(|(_, initializer)| {
                parse_expression(initializer, scope, symbol_cache, evaluation_cache)
            })
            .collect(),
        BindingExpressionType::LetRec => {
            let child_scope = scope.create_child(
                &binding_definitions
                    .iter()
                    .map(|(identifier, _)| *identifier)
                    .collect::<Vec<_>>(),
            );
            binding_definitions
                .iter()
                .map(|(_, initializer)| {
                    parse_expression(initializer, &child_scope, symbol_cache, evaluation_cache)
                })
                .collect()
        }
    }
}

fn parse_binding_definitions<'a, 'b>(
    items: &'b [SyntaxDatum<'a>],
) -> ParserResult<'a, Vec<(&'a str, &'b SyntaxDatum<'a>)>> {
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
    use crate::{
        core::{
            ApplicationTerm, Arity, EnumTerm, Expression, LambdaTerm, RecursiveTerm, StructTerm,
            Term, VariableTerm,
        },
        stdlib::{
            builtin::BuiltinTerm,
            value::{StringValue, ValueTerm},
        },
    };

    use super::{parse, ParserError, SyntaxDatum};

    #[test]
    fn invalid_expression() {
        assert_eq!(
            parse("#"),
            Err(ParserError {
                message: String::from("Expected expression, received '#'"),
                source: None
            })
        );
        assert_eq!(
            parse("1."),
            Err(ParserError {
                message: String::from("Expected end of input, received '.'"),
                source: None
            })
        );
        assert_eq!(
            parse(".1"),
            Err(ParserError {
                message: String::from("Expected expression, received '.'"),
                source: None
            })
        );
        assert_eq!(
            parse("-.1"),
            Err(ParserError {
                message: String::from("Expected end of input, received '.'"),
                source: None
            })
        );
    }

    #[test]
    fn multiple_expressions() {
        assert_eq!(
            parse("3 3"),
            Err(ParserError {
                message: String::from("Expected end of input, received '3'"),
                source: None,
            })
        );
        assert_eq!(
            parse("3 foo"),
            Err(ParserError {
                message: String::from("Expected end of input, received 'f'"),
                source: None,
            })
        );
    }

    #[test]
    fn ignore_extra_whitespace() {
        assert_eq!(
            parse("  \n\r\t3\n\r\t  "),
            Ok(Expression::new(Term::Value(ValueTerm::Int(3))))
        );
    }

    #[test]
    fn global_primitives() {
        assert_eq!(
            parse("#t"),
            Ok(Expression::new(Term::Value(ValueTerm::Boolean(true))))
        );
        assert_eq!(
            parse("#f"),
            Ok(Expression::new(Term::Value(ValueTerm::Boolean(false))))
        );
    }

    #[test]
    fn primitive_values() {
        assert_eq!(
            parse("0"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(0))))
        );
        assert_eq!(
            parse("-0"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(0))))
        );
        assert_eq!(
            parse("3"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(3))))
        );
        assert_eq!(
            parse("-3"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(-3))))
        );
        assert_eq!(
            parse("123"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(123))))
        );
        assert_eq!(
            parse("-123"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(-123))))
        );
        assert_eq!(
            parse("0.0"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(0.0))))
        );
        assert_eq!(
            parse("-0.0"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-0.0))))
        );
        assert_eq!(
            parse("3.142"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.142))))
        );
        assert_eq!(
            parse("-3.142"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-3.142))))
        );
        assert_eq!(
            parse("123.45"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(123.45))))
        );
        assert_eq!(
            parse("-123.45"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(-123.45))))
        );
        assert_eq!(
            parse("\"\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("")
            ))))
        );
        assert_eq!(
            parse("\" \""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from(" ")
            ))))
        );
        assert_eq!(
            parse("\"foo\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo")
            ))))
        );
        assert_eq!(
            parse("\"foo bar\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo bar")
            ))))
        );
    }

    #[test]
    fn symbols() {
        assert_eq!(
            parse("'foo"),
            Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
        );
        assert_eq!(
            parse("(quote foo)"),
            Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
        );
        assert_eq!(
            parse("(+ 'foo 'foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Symbol(0))),
                    Expression::new(Term::Value(ValueTerm::Symbol(0))),
                ],
            )))),
        );
        assert_eq!(
            parse("(+ 'foo 'bar)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Symbol(0))),
                    Expression::new(Term::Value(ValueTerm::Symbol(1))),
                ],
            )))),
        );
    }

    #[test]
    fn escaped_string_literals() {
        assert_eq!(
            parse("\"foo\\\\bar\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo\\bar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\nbar\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo\nbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\tbar\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo\tbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\rbar\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo\rbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\\"bar\\\"baz\""),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo\"bar\"baz")
            ))))
        );
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            parse("foo"),
            Err(ParserError::new(
                String::from("Undefined identifier: foo"),
                &SyntaxDatum::Symbol("foo")
            )),
        );
        assert_eq!(
            parse("(lambda (a) a)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (_) _)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (foo) foo)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (_foo) _foo)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (foo!) foo!)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (foo?) foo?)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (foo_bar) foo_bar)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
        assert_eq!(
            parse("(lambda (foo-bar) foo-bar)"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Variable(VariableTerm::scoped(0)))
            ))))
        );
    }

    #[test]
    fn quoted_expressions() {
        assert_eq!(
            parse("(quote 3)"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
        );
        assert_eq!(
            parse("(quote 3.142)"),
            Ok(Expression::new(Term::Value(ValueTerm::Float(3.142)))),
        );
        assert_eq!(
            parse("(quote \"foo\")"),
            Ok(Expression::new(Term::Value(ValueTerm::String(
                StringValue::from("foo")
            )))),
        );
        assert_eq!(
            parse("(quote foo)"),
            Ok(Expression::new(Term::Value(ValueTerm::Symbol(0)))),
        );
        assert_eq!(
            parse("'()"),
            Ok(Expression::new(Term::Enum(EnumTerm::new(0, Vec::new())))),
        );
        assert_eq!(
            parse("'(3 4 5)"),
            Ok(Expression::new(Term::Enum(EnumTerm::new(
                1,
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Enum(EnumTerm::new(
                        1,
                        vec![
                            Expression::new(Term::Value(ValueTerm::Int(4))),
                            Expression::new(Term::Enum(EnumTerm::new(
                                1,
                                vec![
                                    Expression::new(Term::Value(ValueTerm::Int(5))),
                                    Expression::new(Term::Enum(EnumTerm::new(0, Vec::new()))),
                                ],
                            ),)),
                        ],
                    ),)),
                ],
            )),)),
        );
    }

    #[test]
    fn let_bindings() {
        assert_eq!(
            parse("(let () 3)"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
        );
        assert_eq!(
            parse("(let ((foo 3)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
            )))),
        );
        assert_eq!(
            parse("(let ((foo 3) (bar 4)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(1))),
                ))),
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Value(ValueTerm::Int(4))),
                ],
            )))),
        );
        assert_eq!(
            parse("(let ((foo 3) (bar 4)) bar)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Value(ValueTerm::Int(4))),
                ],
            )))),
        );
        assert_eq!(
            parse("(let ((foo (lambda (bar baz) (+ bar baz)))) (foo 3 4))"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                            Expression::new(Term::Value(ValueTerm::Int(4))),
                        ],
                    ))),
                ))),
                vec![Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::scoped(1))),
                            Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        ],
                    ))),
                ))),],
            )))),
        );
        assert_eq!(
            parse("(lambda (outer) (let ((foo 3) (bar 4)) outer))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                    ))),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                    ],
                ))),
            )))),
        );
    }

    #[test]
    fn letrec_bindings() {
        assert_eq!(
            parse("(letrec () 3)"),
            Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
        );
        assert_eq!(
            parse("(letrec ((foo 3)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![Expression::new(Term::Recursive(RecursiveTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                    ))),
                ))),],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(1))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ]
                                    )),),
                                ))
                            ),))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ]
                                    )),),
                                ))
                            ),))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) bar)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(lambda (outer) (letrec ((foo 3)) outer))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Variable(VariableTerm::scoped(1))),
                    ))),
                    vec![Expression::new(Term::Recursive(RecursiveTerm::new(
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                        ))),
                    )))],
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (outer) (letrec ((foo 3) (bar 4)) outer))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                    ))),
                    vec![
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Recursive(RecursiveTerm::new(
                                    Expression::new(Term::Lambda(LambdaTerm::new(
                                        Arity::from(0, 1, None),
                                        Expression::new(Term::Struct(StructTerm::new(
                                            None,
                                            vec![
                                                Expression::new(Term::Value(ValueTerm::Int(3))),
                                                Expression::new(Term::Value(ValueTerm::Int(4))),
                                            ]
                                        )),),
                                    ))),
                                ))),
                                Expression::new(Term::Value(ValueTerm::Int(0))),
                            ],
                        ))),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Get)),
                            vec![
                                Expression::new(Term::Recursive(RecursiveTerm::new(
                                    Expression::new(Term::Lambda(LambdaTerm::new(
                                        Arity::from(0, 1, None),
                                        Expression::new(Term::Struct(StructTerm::new(
                                            None,
                                            vec![
                                                Expression::new(Term::Value(ValueTerm::Int(3))),
                                                Expression::new(Term::Value(ValueTerm::Int(4))),
                                            ]
                                        )),),
                                    ))),
                                ))),
                                Expression::new(Term::Value(ValueTerm::Int(1))),
                            ],
                        ))),
                    ],
                ))),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(1))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(0)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(0)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) bar)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(0)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(0)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) foo)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(1))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(1)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(1)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) bar)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(1)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::Get
                                                    )),
                                                    vec![
                                                        Expression::new(Term::Variable(
                                                            VariableTerm::scoped(0)
                                                        )),
                                                        Expression::new(Term::Value(
                                                            ValueTerm::Int(1)
                                                        )),
                                                    ],
                                                ),
                                            )),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                        ]
                                    )),),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo (lambda (bar baz) (+ (+ first bar) (+ second baz)))) (first 3) (second 4)) (foo 5 6))"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 3, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Int(5))),
                            Expression::new(Term::Value(ValueTerm::Int(6))),
                        ],
                    ))),
                ))),
                vec![
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(None,vec![
                                            Expression::new(Term::Lambda(LambdaTerm::new(
                                                Arity::from(0, 2, None),
                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                    vec![
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(1))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(1))),
                                                            ],
                                                        ))),
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(2))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                                            ],
                                                        ))),
                                                    ],
                                                )))
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ])),
                                    ),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(0))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(None,vec![
                                            Expression::new(Term::Lambda(LambdaTerm::new(
                                                Arity::from(0, 2, None),
                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                    vec![
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(1))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(1))),
                                                            ],
                                                        ))),
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(2))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                                            ],
                                                        ))),
                                                    ],
                                                )))
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ])),
                                    ),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ],
                    ))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                                Term::Lambda(LambdaTerm::new(
                                    Arity::from(0, 1, None),
                                    Expression::new(Term::Struct(StructTerm::new(
                                        None,
                                        vec![
                                            Expression::new(Term::Lambda(LambdaTerm::new(
                                                Arity::from(0, 2, None),
                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                    vec![
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(1))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(1))),
                                                            ],
                                                        ))),
                                                        Expression::new(Term::Application(ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                                            vec![
                                                                Expression::new(Term::Application(ApplicationTerm::new(
                                                                    Expression::new(Term::Builtin(BuiltinTerm::Get)),
                                                                    vec![
                                                                        Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                                        Expression::new(Term::Value(ValueTerm::Int(2))),
                                                                    ],
                                                                ))),
                                                                Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                                            ],
                                                        ))),
                                                    ],
                                                )))
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(3))),
                                            Expression::new(Term::Value(ValueTerm::Int(4))),
                                        ])),
                                    ),
                                )),
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(2))),
                        ],
                    ))),
                ],
            )))),
        );
        assert_eq!(
            parse("(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        vec![Expression::new(Term::Value(ValueTerm::Int(5)))],
                    ))),
                ))),
                vec![Expression::new(Term::Recursive(RecursiveTerm::new(
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 1, None),
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 1, None),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::If)),
                                vec![
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Equal)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                0
                                            ))),
                                            Expression::new(Term::Value(ValueTerm::Int(1))),
                                        ],
                                    ))),
                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                0
                                            ))),
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Variable(
                                                        VariableTerm::scoped(1),
                                                    )),
                                                    vec![Expression::new(Term::Application(
                                                        ApplicationTerm::new(
                                                            Expression::new(Term::Builtin(
                                                                BuiltinTerm::Subtract,
                                                            )),
                                                            vec![
                                                                Expression::new(Term::Variable(
                                                                    VariableTerm::scoped(0),
                                                                )),
                                                                Expression::new(Term::Value(
                                                                    ValueTerm::Int(1),
                                                                )),
                                                            ],
                                                        ),
                                                    ))],
                                                ),
                                            )),
                                        ],
                                    ))),
                                ],
                            ))),
                        ))),
                    ))),
                )))],
            )))),
        );
    }

    #[test]
    fn function_expressions() {
        assert_eq!(
            parse("(lambda () (+ 3 4))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 0, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                    ]
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo) (+ foo 4))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        Expression::new(Term::Value(ValueTerm::Int(4)))
                    ]
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (+ foo bar))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 2, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Variable(VariableTerm::scoped(1))),
                        Expression::new(Term::Variable(VariableTerm::scoped(0)))
                    ]
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (first) (lambda (second) (lambda (foo bar) (+ foo bar))))",),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Add)),
                            vec![
                                Expression::new(Term::Variable(VariableTerm::scoped(1))),
                                Expression::new(Term::Variable(VariableTerm::scoped(0)))
                            ]
                        ))),
                    ))),
                ))),
            )))),
        );
    }

    #[test]
    fn closures() {
        assert_eq!(
            parse("(lambda (foo) (lambda () foo))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0)))
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () foo))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 2, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(1)))
                ))),
            ))))
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () bar))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 2, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda (baz) (+ foo baz)))",),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 2, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::scoped(2))),
                            Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        ]
                    ))),
                ))),
            )))),
        );
        assert_eq!(
            parse("(lambda (first second third) (lambda (fourth fifth) (lambda (sixth) (+ first (+ second (+ third (+ fourth (+ fifth sixth))))))))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 3, None),
                Expression::new(Term::Lambda(
                    LambdaTerm::new(
                        Arity::from(0, 2, None),
                        Expression::new(Term::Lambda(
                            LambdaTerm::new(
                                Arity::from(0, 1, None),
                                Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(BuiltinTerm::Add)), vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(5))),
                                    Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(BuiltinTerm::Add)), vec![
                                        Expression::new(Term::Variable(VariableTerm::scoped(4))),
                                        Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(BuiltinTerm::Add)), vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(3))),
                                            Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(BuiltinTerm::Add)), vec![
                                                Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                                Expression::new(Term::Application(ApplicationTerm::new(Expression::new(Term::Builtin(BuiltinTerm::Add)), vec![
                                                    Expression::new(Term::Variable(VariableTerm::scoped(1))),
                                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                                ]))),
                                            ]))),
                                        ]))),
                                    ]))),
                                ]))),
                            ),
                        )),
                )),
            ))))),
        );
        assert_eq!(
            parse("(lambda (foo) (lambda () (lambda () foo)))"),
            Ok(Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 0, None),
                    Expression::new(Term::Lambda(LambdaTerm::new(
                        Arity::from(0, 0, None),
                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                    ))),
                ))),
            )))),
        );
    }

    #[test]
    fn function_applications() {
        assert_eq!(
            parse("((lambda (foo) foo) 3)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(0)))
                ))),
                vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
            ),))),
        );
        assert_eq!(
            parse("((lambda (foo bar baz) foo) 3 4 5)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 3, None),
                    Expression::new(Term::Variable(VariableTerm::scoped(2)))
                ))),
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Value(ValueTerm::Int(4))),
                    Expression::new(Term::Value(ValueTerm::Int(5))),
                ],
            ),))),
        );
        assert_eq!(
            parse("((lambda (+) (+ 3 4)) (lambda (first second) (+ first second)))",),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 1, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        vec![
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                            Expression::new(Term::Value(ValueTerm::Int(4))),
                        ],
                    ))),
                ))),
                vec![Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::scoped(1))),
                            Expression::new(Term::Variable(VariableTerm::scoped(0))),
                        ]
                    ))),
                )))],
            ),))),
        );
    }

    #[test]
    fn builtin_applications() {
        assert_eq!(
            parse("(+ 3 4)"),
            Ok(Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                vec![
                    Expression::new(Term::Value(ValueTerm::Int(3))),
                    Expression::new(Term::Value(ValueTerm::Int(4))),
                ],
            )))),
        );
    }
}
