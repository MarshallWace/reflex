// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    expression::{AstNodePackage, Expression},
    node::{
        core::{
            CoreNode, FunctionApplicationNode, FunctionNode, LetNode, LetRecNode, LetStarNode,
            ReferenceNode, StringValue, ValueNode,
        },
        lexer::{parse_syntax, SyntaxDatum},
        Node,
    },
};

#[derive(Clone)]
struct LexicalScope<'a> {
    bindings: Vec<&'a str>,
}
impl<'a> LexicalScope<'a> {
    pub fn new(_: &'a str) -> Self {
        LexicalScope {
            bindings: Vec::new(),
        }
    }
    pub fn create_child(&self, identifiers: &[&'a str]) -> LexicalScope<'a> {
        LexicalScope {
            bindings: self
                .bindings
                .iter()
                .chain(identifiers.iter())
                .map(|key| *key)
                .collect(),
        }
    }
    pub fn get(&self, identifier: &'a str) -> Option<usize> {
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

pub fn parse<'a>(input: &'a str) -> ParserResult<'a, Expression<Node>> {
    let syntax = match parse_syntax(input) {
        Ok(syntax) => Ok(syntax),
        Err(message) => Err(ParserError {
            message,
            source: None,
        }),
    }?;
    let mut scope = LexicalScope::new(input);
    parse_expression(&syntax, &mut scope)
}

fn parse_expression<'a>(
    input: &SyntaxDatum<'a>,
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
    match input {
        SyntaxDatum::IntegerLiteral(value) => Ok(parse_integer_literal(input, *value)),
        SyntaxDatum::FloatLiteral(value) => Ok(parse_float_literal(input, *value)),
        SyntaxDatum::StringLiteral(value) => Ok(parse_string_literal(input, &value)),
        SyntaxDatum::Symbol(identifier) => parse_reference(input, identifier, scope),
        SyntaxDatum::List(items) => match items.split_first() {
            None => Err(ParserError::new(String::from("Expected expression"), input)),
            Some((target, args)) => match parse_special_form(input, target, args, scope)? {
                Some(result) => Ok(result),
                _ => parse_function_application(input, target, args, scope),
            },
        },
    }
}

fn parse_nil_literal(_input: &SyntaxDatum) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
}

fn parse_boolean_literal(_input: &SyntaxDatum, value: bool) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(value))))
}

fn parse_integer_literal(_input: &SyntaxDatum, value: i32) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(value))))
}

fn parse_float_literal(_input: &SyntaxDatum, value: f64) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(value))))
}

fn parse_string_literal(_input: &SyntaxDatum, value: &str) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
        StringValue::new(String::from(value)),
    ))))
}

fn parse_reference<'a>(
    input: &SyntaxDatum<'a>,
    identifier: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
    match scope.get(identifier) {
        None => match parse_global(input, identifier) {
            Some(result) => Ok(result),
            None => Err(ParserError::new(
                format!("Undefined identifier: {}", identifier),
                input,
            )),
        },
        Some(offset) => Ok(Expression::new(Node::Core(CoreNode::Reference(
            ReferenceNode::new(offset),
        )))),
    }
}

fn parse_global<'a>(input: &SyntaxDatum<'a>, identifier: &'a str) -> Option<Expression<Node>> {
    match identifier {
        "null" => Some(parse_nil_literal(input)),
        "#t" => Some(parse_boolean_literal(input, true)),
        "#f" => Some(parse_boolean_literal(input, false)),
        _ => None,
    }
}

fn parse_special_form<'a>(
    input: &SyntaxDatum<'a>,
    target: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Option<Expression<Node>>> {
    match target {
        SyntaxDatum::Symbol(identifier) => match *identifier {
            "lambda" => parse_lambda_expression(input, args, scope).map(Some),
            "let" => parse_let_expression(input, args, scope).map(Some),
            "letrec" => parse_letrec_expression(input, args, scope).map(Some),
            "let*" => parse_letstar_expression(input, args, scope).map(Some),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn parse_function_application<'a>(
    input: &SyntaxDatum<'a>,
    target: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
    match target {
        SyntaxDatum::Symbol(identifier) if scope.get(identifier).is_none() => {
            match Node::factory(identifier, &parse_function_arguments(args, scope)?) {
                Some(result) => match result {
                    Ok(result) => Ok(Expression::new(result)),
                    Err(message) => Err(ParserError::new(message, input)),
                },
                None => Err(ParserError::new(
                    format!("Unknown expression type: {}", identifier),
                    input,
                )),
            }
        }
        _ => Ok(Expression::new(Node::Core(CoreNode::FunctionApplication(
            FunctionApplicationNode::new(
                parse_expression(target, scope)?,
                parse_function_arguments(args, scope)?,
            ),
        )))),
    }
}

fn parse_function_arguments<'a>(
    items: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Vec<Expression<Node>>> {
    items
        .iter()
        .map(|item| parse_expression(item, scope))
        .collect()
}

fn parse_lambda_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
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
    let body = parse_expression(body, &child_scope)?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(Expression::new(Node::Core(CoreNode::Function(
            FunctionNode::new(arity, body),
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
) -> ParserResult<'a, Expression<Node>> {
    parse_binding_expression(&BindingExpressionType::Let, input, args, scope)
}

fn parse_letrec_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
    parse_binding_expression(&BindingExpressionType::LetRec, input, args, scope)
}

fn parse_letstar_expression<'a>(
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
    parse_binding_expression(&BindingExpressionType::LetStar, input, args, scope)
}

enum BindingExpressionType {
    Let,
    LetRec,
    LetStar,
}

fn get_binding_type_name(binding_type: &BindingExpressionType) -> &'static str {
    match binding_type {
        BindingExpressionType::Let => "let",
        BindingExpressionType::LetRec => "letrec",
        BindingExpressionType::LetStar => "let*",
    }
}

fn parse_binding_expression<'a>(
    binding_type: &BindingExpressionType,
    input: &SyntaxDatum<'a>,
    args: &[SyntaxDatum<'a>],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Expression<Node>> {
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
    let initializers = parse_binding_initializers(binding_type, &binding_definitions, scope)?;
    let child_scope = scope.create_child(
        &binding_definitions
            .iter()
            .map(|(identifier, _)| *identifier)
            .collect::<Vec<_>>(),
    );
    let body = parse_expression(body, &child_scope)?;
    match args.next() {
        Some(arg) => Err(ParserError::new(String::from("Unexpected expression"), arg)),
        None => Ok(Expression::new(Node::Core(match binding_type {
            BindingExpressionType::Let => CoreNode::Let(LetNode::new(initializers, body)),
            BindingExpressionType::LetRec => CoreNode::LetRec(LetRecNode::new(initializers, body)),
            BindingExpressionType::LetStar => {
                CoreNode::LetStar(LetStarNode::new(initializers, body))
            }
        }))),
    }
}

fn parse_binding_initializers<'a>(
    binding_type: &BindingExpressionType,
    binding_definitions: &[(&'a str, &SyntaxDatum<'a>)],
    scope: &LexicalScope<'a>,
) -> ParserResult<'a, Vec<Expression<Node>>> {
    match binding_type {
        BindingExpressionType::Let => binding_definitions
            .iter()
            .map(|(_, initializer)| parse_expression(initializer, scope))
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
                .map(|(_, initializer)| parse_expression(initializer, &child_scope))
                .collect()
        }
        BindingExpressionType::LetStar => {
            let mut initializers = Vec::with_capacity(binding_definitions.len());
            let mut scope = scope.clone();
            for (identifier, initializer) in binding_definitions {
                initializers.push(parse_expression(initializer, &scope)?);
                scope = scope.create_child(&[identifier]);
            }
            Ok(initializers)
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
        expression::Expression,
        node::{
            arithmetic::{AbsNode, AddNode, ArithmeticNode, EqualNode, MultiplyNode, SubtractNode},
            core::{
                CoreNode, FunctionApplicationNode, FunctionNode, LetNode, LetRecNode, LetStarNode,
                ReferenceNode, StringValue, ValueNode,
            },
            lexer::SyntaxDatum,
            logic::{ConditionalNode, LogicNode},
            Node,
        },
    };

    use super::{parse, ParserError};

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
        assert_eq!(
            parse("'foo'"),
            Err(ParserError {
                message: String::from("Expected expression, received '''"),
                source: None
            })
        );
    }

    #[test]
    fn multiple_expressions() {
        assert_eq!(
            parse("null null"),
            Err(ParserError {
                message: String::from("Expected end of input, received 'n'"),
                source: None,
            })
        );
        assert_eq!(
            parse("null foo"),
            Err(ParserError {
                message: String::from("Expected end of input, received 'f'"),
                source: None,
            })
        );
    }

    #[test]
    fn ignore_extra_whitespace() {
        assert_eq!(
            parse("  \n\r\tnull\n\r\t  "),
            Ok(Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))))
        );
    }

    #[test]
    fn primitive_values() {
        assert_eq!(
            parse("null"),
            Ok(Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))))
        );
        assert_eq!(
            parse("#t"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        assert_eq!(
            parse("#f"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        assert_eq!(
            parse("0"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(0)
            ))))
        );
        assert_eq!(
            parse("-0"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(0)
            ))))
        );
        assert_eq!(
            parse("3"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            ))))
        );
        assert_eq!(
            parse("-3"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(-3)
            ))))
        );
        assert_eq!(
            parse("123"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(123)
            ))))
        );
        assert_eq!(
            parse("-123"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(-123)
            ))))
        );
        assert_eq!(
            parse("0.0"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(0.0)
            ))))
        );
        assert_eq!(
            parse("-0.0"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(0.0)
            ))))
        );
        assert_eq!(
            parse("3.142"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(3.142)
            ))))
        );
        assert_eq!(
            parse("-3.142"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-3.142)
            ))))
        );
        assert_eq!(
            parse("123.45"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(123.45)
            ))))
        );
        assert_eq!(
            parse("-123.45"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-123.45)
            ))))
        );
        assert_eq!(
            parse("\"\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("")))
            ))))
        );
        assert_eq!(
            parse("\" \""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from(" ")))
            ))))
        );
        assert_eq!(
            parse("\"foo\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo")))
            ))))
        );
        assert_eq!(
            parse("\"foo bar\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo bar")))
            ))))
        );
    }

    #[test]
    fn escaped_string_literals() {
        assert_eq!(
            parse("\"foo\\\\bar\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo\\bar")))
            ))))
        );
        assert_eq!(
            parse("\"foo\\nbar\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo\nbar")))
            ))))
        );
        assert_eq!(
            parse("\"foo\\tbar\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo\tbar")))
            ))))
        );
        assert_eq!(
            parse("\"foo\\rbar\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo\rbar")))
            ))))
        );
        assert_eq!(
            parse("\"foo\\\"bar\\\"baz\""),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(String::from("foo\"bar\"baz")))
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
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (_) _)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (_foo) _foo)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo!) foo!)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo?) foo?)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo_bar) foo_bar)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo-bar) foo-bar)"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                )
            ))))
        );
    }

    #[test]
    fn let_bindings() {
        assert_eq!(
            parse("(let () 3)"),
            Ok(Expression::new(Node::Core(CoreNode::Let(LetNode::new(
                vec![],
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
            ),)))),
        );
        assert_eq!(
            parse("(let ((foo 3)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::Let(LetNode::new(
                vec![Expression::new(Node::Core(CoreNode::Value(
                    ValueNode::Int(3)
                )))],
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
            ),)))),
        );
        assert_eq!(
            parse("(let ((foo 3) (bar 4)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::Let(LetNode::new(
                vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ],
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
            ),)))),
        );
        assert_eq!(
            parse("(let ((foo 3) (bar 4)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::Let(LetNode::new(
                vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ],
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
            ),)))),
        );
        assert_eq!(
            parse("(let ((foo (lambda (bar baz) (+ bar baz)))) (foo 3 4))"),
            Ok(Expression::new(Node::Core(CoreNode::Let(LetNode::new(
                vec![Expression::new(Node::Core(CoreNode::Function(
                    FunctionNode::new(
                        2,
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        )))),
                    ),
                )))],
                Expression::new(Node::Core(CoreNode::FunctionApplication(
                    FunctionApplicationNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                        ],
                    ),
                ))),
            ))))),
        );
    }

    #[test]
    fn letrec_bindings() {
        assert_eq!(
            parse("(letrec () 3)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![],
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![Expression::new(Node::Core(CoreNode::Value(
                        ValueNode::Int(3)
                    )))],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar 4)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo 3) (bar foo)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo bar) (bar 3)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((first second) (second third) (third first)) third)"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(letrec ((foo (lambda (bar baz) (+ (+ first bar) (+ second baz)))) (first 3) (second 4)) (foo 5 6))"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(LetRecNode::new(
                vec![
                    Expression::new(Node::Core(CoreNode::Function(
                        FunctionNode::new(
                            2,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(3)))),
                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                                )))),
                                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                                )))),
                            )))),
                        ),
                    ))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ],
                Expression::new(Node::Core(CoreNode::FunctionApplication(
                    FunctionApplicationNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                        ],
                    ),
                ))),
            ))))),
        );
        assert_eq!(
            parse("(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))"),
            Ok(Expression::new(Node::Core(CoreNode::LetRec(
                LetRecNode::new(
                    vec![Expression::new(Node::Core(CoreNode::Function(
                        FunctionNode::new(
                            1,
                            Expression::new(Node::Logic(LogicNode::Conditional(
                                ConditionalNode::new(
                                    Expression::new(Node::Arithmetic(ArithmeticNode::Equal(
                                        EqualNode::new(
                                            Expression::new(Node::Core(CoreNode::Reference(
                                                ReferenceNode::new(0)
                                            ))),
                                            Expression::new(Node::Core(CoreNode::Value(
                                                ValueNode::Int(1)
                                            ))),
                                        )
                                    ))),
                                    Expression::new(Node::Core(CoreNode::Reference(
                                        ReferenceNode::new(0)
                                    ))),
                                    Expression::new(Node::Arithmetic(ArithmeticNode::Multiply(
                                        MultiplyNode::new(
                                            Expression::new(Node::Core(CoreNode::Reference(
                                                ReferenceNode::new(0)
                                            ))),
                                            Expression::new(Node::Core(
                                                CoreNode::FunctionApplication(
                                                    FunctionApplicationNode::new(
                                                        Expression::new(Node::Core(
                                                            CoreNode::Reference(
                                                                ReferenceNode::new(1)
                                                            )
                                                        )),
                                                        vec![Expression::new(Node::Arithmetic(
                                                            ArithmeticNode::Subtract(
                                                                SubtractNode::new(
                                                                    Expression::new(Node::Core(
                                                                        CoreNode::Reference(
                                                                            ReferenceNode::new(0)
                                                                        )
                                                                    )),
                                                                    Expression::new(Node::Core(
                                                                        CoreNode::Value(
                                                                            ValueNode::Int(1)
                                                                        )
                                                                    )),
                                                                )
                                                            )
                                                        )),],
                                                    )
                                                )
                                            )),
                                        )
                                    ))),
                                )
                            ))),
                        ),
                    ))),],
                    Expression::new(Node::Core(CoreNode::FunctionApplication(
                        FunctionApplicationNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                            vec![Expression::new(Node::Core(CoreNode::Value(
                                ValueNode::Int(5)
                            ))),],
                        ),
                    ))),
                )
            )))),
        );
    }

    #[test]
    fn letstar_bindings() {
        assert_eq!(
            parse("(let* () 3)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![],
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((foo 3)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![Expression::new(Node::Core(CoreNode::Value(
                        ValueNode::Int(3)
                    )))],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((foo 3) (bar 4)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((foo 3) (bar 4)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((foo 3) (bar foo)) foo)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((foo 3) (bar foo)) bar)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
        assert_eq!(
            parse("(let* ((first 3) (second first) (third second) (fourth first)) fourth)"),
            Ok(Expression::new(Node::Core(CoreNode::LetStar(
                LetStarNode::new(
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                    ],
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ),
            )))),
        );
    }

    #[test]
    fn function_expressions() {
        assert_eq!(
            parse("(lambda () (+ 3 4))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    0,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    )))),
                ),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo) (+ foo 4))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4),)))
                    )))),
                ),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (+ foo bar))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                    )))),
                ),
            )))),
        );
        assert_eq!(
            parse("(lambda (first) (lambda (second) (lambda (foo bar) (+ foo bar))))",),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                            2,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(1)
                                ))),
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(0)
                                )))
                            )))),
                        )))),
                    )))),
                ),
            )))),
        );
    }

    #[test]
    fn closures() {
        assert_eq!(
            parse("(lambda (foo) (lambda () foo))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        0,
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                    ),))),
                ),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () foo))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        0,
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1))))
                    ),))),
                ),
            ))))
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () bar))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        0,
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                    ),))),
                ),
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda (baz) (+ foo baz)))",),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        ))))
                    ),))),
                ),
            ))))
        );
        assert_eq!(
            parse("(lambda (first second third) (lambda (fourth fifth) (lambda (sixth) (+ first (+ second (+ third (+ fourth (+ fifth sixth))))))))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                3,
                Expression::new(Node::Core(CoreNode::Function(
                    FunctionNode::new(
                        2,
                        Expression::new(Node::Core(CoreNode::Function(
                            FunctionNode::new(
                                1,
                                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(5)))),
                                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(4)))),
                                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(3)))),
                                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                                                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                                                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                                                )))),
                                            )))),
                                        )))),
                                    )))),
                                )))),
                            ),
                        ))),
                    ),
                ))),
            ))))),
        );
        assert_eq!(
            parse("(lambda (foo) (lambda () (lambda () foo)))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        0,
                        Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                            0,
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        ),))),
                    )))),
                ),
            )))),
        );
    }

    #[test]
    fn function_applications() {
        assert_eq!(
            parse("((lambda (foo) foo) 3)"),
            Ok(Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                    )))),
                    vec![Expression::new(Node::Core(CoreNode::Value(
                        ValueNode::Int(3)
                    )))],
                )
            )))),
        );
        assert_eq!(
            parse("((lambda (foo bar baz) foo) 3 4 5)"),
            Ok(Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        3,
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2))))
                    )))),
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    ],
                )
            )),))
        );
        assert_eq!(
            parse("((lambda (+) (+ 3 4)) (lambda (first second) (+ first second)))",),
            Ok(Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Core(CoreNode::FunctionApplication(
                            FunctionApplicationNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(0)
                                ))),
                                vec![
                                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                                ],
                            )
                        ))),
                    )))),
                    vec![Expression::new(Node::Core(CoreNode::Function(
                        FunctionNode::new(
                            2,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(1)
                                ))),
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(0)
                                ))),
                            )))),
                        )
                    )))],
                )
            )))),
        );
    }

    #[test]
    fn primitive_expressions() {
        assert_eq!(
            parse("(abs -123.45)"),
            Ok(Expression::new(Node::Arithmetic(ArithmeticNode::Abs(
                AbsNode::new(Expression::new(Node::Core(CoreNode::Value(
                    ValueNode::Float(-123.45)
                ))))
            )))),
        );
        assert_eq!(
            parse("(+ 3 4)"),
            Ok(Expression::new(Node::Arithmetic(ArithmeticNode::Add(
                AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )
            ))))
        );
    }
}
