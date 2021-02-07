// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::borrow::Cow;

use crate::{
    env::StackOffset,
    expression::{Expression, NodeType},
    node::{
        core::{
            ApplicationNode, ClosureNode, CoreNode, FunctionNode, ReferenceNode, StringValue,
            ValueNode,
        },
        Node,
    },
};

type ParserError = String;
type ParserResult<T> = Result<T, ParserError>;

struct ParserOutput<'a, T> {
    parsed: T,
    remaining: &'a str,
}
impl<'a, T> ParserOutput<'a, T> {
    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> ParserOutput<'a, U> {
        ParserOutput {
            parsed: f(self.parsed),
            remaining: self.remaining,
        }
    }
}

const EOF: &'static str = "end of input";

fn consume_whitespace(input: &str) -> &str {
    input.trim_start()
}

fn consume_char(char: char, input: &str) -> Option<&str> {
    input
        .chars()
        .next()
        .filter(|value| *value == char)
        .map(|_| &input[1..])
}

fn consume_while(predicate: Box<dyn Fn(char) -> bool>, input: &str) -> Option<ParserOutput<&str>> {
    let length = input
        .char_indices()
        .find_map(
            |(index, char)| {
                if predicate(char) {
                    None
                } else {
                    Some(index)
                }
            },
        )
        .unwrap_or_else(|| input.len());
    if length > 0 {
        Some(ParserOutput {
            parsed: &input[0..length],
            remaining: &input[length..],
        })
    } else {
        None
    }
}

fn consume_keyword<'a>(keyword: &str, input: &'a str) -> Option<&'a str> {
    if keyword
        .chars()
        .zip(input.chars())
        .all(|(left, right)| left == right)
    {
        Some(&input[keyword.len()..])
    } else {
        None
    }
}

fn consume_identifier(input: &str) -> Option<ParserOutput<&str>> {
    let first_char = input.chars().next()?;
    if !is_valid_identifier_leading_char(first_char) {
        return None;
    }
    let identifier_length = input
        .char_indices()
        .skip(1)
        .find_map(|(index, char)| {
            if is_valid_identifier_char(char) {
                None
            } else {
                Some(index)
            }
        })
        .unwrap_or_else(|| input.len());
    Some(ParserOutput {
        parsed: &input[0..identifier_length],
        remaining: &input[identifier_length..],
    })
}

fn is_valid_identifier_leading_char(char: char) -> bool {
    if char.is_ascii_alphabetic() {
        return true;
    }
    match char {
        '_' => true,
        _ => false,
    }
}

fn is_valid_identifier_char(char: char) -> bool {
    if char.is_ascii_alphanumeric() {
        return true;
    }
    match char {
        '_' | '-' | '!' | '?' => true,
        _ => false,
    }
}

fn peek_next_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn consume_primitive(input: &str) -> Option<ParserOutput<ValueNode>> {
    None.or_else(|| consume_nil_literal(input))
        .or_else(|| consume_boolean_literal(input))
        .or_else(|| consume_number_literal(input))
        .or_else(|| consume_string_literal(input))
}

fn consume_nil_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    consume_keyword("null", input).map(|remaining| ParserOutput {
        parsed: ValueNode::Nil,
        remaining,
    })
}

fn consume_boolean_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    None.or_else(|| consume_true_literal(input))
        .or_else(|| consume_false_literal(input))
}

fn consume_true_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    consume_keyword("true", input).map(|remaining| ParserOutput {
        parsed: ValueNode::Boolean(true),
        remaining,
    })
}

fn consume_false_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    consume_keyword("false", input).map(|remaining| ParserOutput {
        parsed: ValueNode::Boolean(false),
        remaining,
    })
}

fn consume_number_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    let (is_negative, input) =
        consume_char('-', input).map_or_else(|| (false, input), |remaining| (true, remaining));
    let result = consume_while(Box::new(is_digit_char), input);
    if let None = result {
        return None;
    }
    let (int_chars, input) = result
        .map(|ParserOutput { parsed, remaining }| (parsed, remaining))
        .unwrap();
    let (is_float, input) =
        consume_char('.', input).map_or_else(|| (false, input), |remaining| (true, remaining));
    if !is_float {
        let int_value = int_chars.parse::<i32>();
        return match int_value {
            Ok(value) => Some(ParserOutput {
                parsed: ValueNode::Int(if is_negative { -value } else { value }),
                remaining: input,
            }),
            Err(_) => None,
        };
    }
    let result = consume_while(Box::new(is_digit_char), input);
    if let None = result {
        return None;
    }
    let (decimal_chars, input) = result
        .map(|ParserOutput { parsed, remaining }| (parsed, remaining))
        .unwrap();
    let float_value = format!("{}.{}", int_chars, decimal_chars).parse::<f64>();
    match float_value {
        Ok(value) => Some(ParserOutput {
            parsed: ValueNode::Float(if is_negative { -value } else { value }),
            remaining: input,
        }),
        Err(_) => None,
    }
}

fn is_digit_char(char: char) -> bool {
    char.is_ascii_digit()
}

fn consume_string_literal(input: &str) -> Option<ParserOutput<ValueNode>> {
    let input = consume_char('"', input)?;
    let ParserOutput {
        parsed,
        remaining: input,
    } = consume_string_contents(input);
    let input = consume_char('"', input)?;
    Some(ParserOutput {
        parsed: ValueNode::String(match parsed {
            Cow::Borrowed(value) => StringValue::new(String::from(value)),
            Cow::Owned(value) => StringValue::new(value),
        }),
        remaining: input,
    })
}

fn consume_string_contents(input: &str) -> ParserOutput<Cow<str>> {
    let result = consume_while(Box::new(is_string_char), input).map_or_else(
        || ParserOutput {
            parsed: Cow::Borrowed(""),
            remaining: input,
        },
        |result| result.map(Cow::Borrowed),
    );
    let mut lookahead_iter = result.remaining.chars();
    match lookahead_iter.next() {
        Some('\\') => {
            let escaped_char = lookahead_iter.next();
            match escaped_char {
                Some(char) => {
                    let next = consume_string_contents(&result.remaining[2..]);
                    ParserOutput {
                        parsed: Cow::Owned(format!(
                            "{}{}{}",
                            result.parsed,
                            parse_escape_code(char)
                                .map(String::from)
                                .unwrap_or(char.to_string()),
                            next.parsed
                        )),
                        remaining: next.remaining,
                    }
                }
                None => result,
            }
        }
        _ => result,
    }
}

fn parse_escape_code(char: char) -> Option<&'static str> {
    match char {
        '\\' => Some("\\"),
        '"' => Some("\""),
        'n' => Some("\n"),
        't' => Some("\t"),
        'r' => Some("\r"),
        _ => None,
    }
}

fn is_string_char(char: char) -> bool {
    match char {
        '"' | '\\' => false,
        _ => true,
    }
}

fn consume_special_form<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Expression<Node>>>> {
    consume_lambda_expression(input, scope)
}

fn consume_lambda_expression<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Expression<Node>>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            match consume_keyword("lambda", input) {
                None => Ok(None),
                Some(input) => {
                    let input = consume_whitespace(input);
                    match consume_char('(', input) {
                        None => Err(String::from("Invalid function argument list")),
                        Some(input) => {
                            let input = consume_whitespace(input);
                            let ParserOutput {
                                parsed: arg_names,
                                remaining: input,
                            } = consume_fn_arg_names(input)?;
                            let input = consume_whitespace(input);
                            match consume_char(')', input) {
                                None => Err(format!(
                                    "Expected ')', received '{}'",
                                    peek_next_char(input)
                                        .map(String::from)
                                        .unwrap_or(String::from(EOF))
                                )),
                                Some(input) => {
                                    let input = consume_whitespace(input);
                                    let arity = arg_names.len();
                                    let child_scope = scope
                                        .create_child(arg_names.iter().map(|name| *name).collect());
                                    match consume_expression(input, &child_scope)? {
                                        None => Err(String::from("Missing function body")),
                                        Some(ParserOutput {
                                            parsed: body,
                                            remaining: input,
                                        }) => {
                                            let input = consume_whitespace(input);
                                            let captures = get_free_variables(&body, arity);
                                            match consume_char(')', input) {
                                                None => Err(format!(
                                                    "Expected ')', received '{}'",
                                                    peek_next_char(input)
                                                        .map(String::from)
                                                        .unwrap_or(String::from(EOF))
                                                )),
                                                Some(input) => Ok(Some(ParserOutput {
                                                    parsed: match captures {
                                                        Some(captures) => Expression::new(
                                                            Node::Core(CoreNode::Closure(
                                                                ClosureNode::new(
                                                                    captures,
                                                                    FunctionNode::new(arity, body),
                                                                ),
                                                            )),
                                                        ),
                                                        None => Expression::new(Node::Core(
                                                            CoreNode::Function(FunctionNode::new(
                                                                arity, body,
                                                            )),
                                                        )),
                                                    },
                                                    remaining: input,
                                                })),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn consume_fn_arg_names<'a>(input: &'a str) -> ParserResult<ParserOutput<'a, Vec<&'a str>>> {
    consume_fn_arg_names_iter(input, Vec::new())
}

fn get_free_variables(expression: &Expression<Node>, arity: usize) -> Option<Vec<StackOffset>> {
    let mut results = get_free_variables_iter(expression, arity, Vec::new());
    if results.len() == 0 {
        return None;
    }
    results.sort_unstable();
    results.dedup();
    Some(results)
}

fn get_free_variables_iter(
    expression: &Expression<Node>,
    arity: usize,
    mut results: Vec<StackOffset>,
) -> Vec<StackOffset> {
    match expression.value() {
        Node::Core(CoreNode::Reference(node)) => {
            let offset = node.offset();
            if offset >= arity {
                results.push(offset - arity);
            }
            results
        }
        Node::Core(CoreNode::Closure(node)) => {
            results.extend(
                node.captures()
                    .iter()
                    .filter(|offset| **offset >= arity)
                    .map(|offset| offset - arity),
            );
            results
        }
        Node::Core(CoreNode::Function(_)) => results,
        node => node
            .expressions()
            .iter()
            .fold(results, |results, expression| {
                get_free_variables_iter(expression, arity, results)
            }),
    }
}

fn consume_fn_arg_names_iter<'a>(
    input: &'a str,
    mut results: Vec<&'a str>,
) -> ParserResult<ParserOutput<'a, Vec<&'a str>>> {
    match consume_identifier(input) {
        None => Ok(ParserOutput {
            parsed: results,
            remaining: input,
        }),
        Some(ParserOutput {
            parsed: arg_name,
            remaining: input,
        }) => {
            results.push(arg_name);
            let input = consume_whitespace(input);
            consume_fn_arg_names_iter(input, results)
        }
    }
}

fn consume_s_expression<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Expression<Node>>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            match consume_application_target(input, scope)? {
                None => Err(format!(
                    "Expected identifier or expression, received {}",
                    peek_next_char(input)
                        .map(String::from)
                        .unwrap_or(String::from(EOF))
                )),
                Some(ParserOutput {
                    parsed: target,
                    remaining: input,
                }) => {
                    let input = consume_whitespace(input);
                    let ParserOutput {
                        parsed: args,
                        remaining: input,
                    } = consume_list_items(input, scope)?;
                    let input = consume_whitespace(input);
                    match consume_char(')', input) {
                        None => Err(format!(
                            "Expected ')', received '{}'",
                            peek_next_char(input)
                                .map(String::from)
                                .unwrap_or(String::from(EOF))
                        )),
                        Some(input) => match target {
                            ApplicationTarget::Primitive(identifier) => {
                                match Node::factory(identifier, &args)? {
                                    Some(expression) => Ok(Some(ParserOutput {
                                        parsed: expression,
                                        remaining: input,
                                    })),
                                    None => Err(format!("Unknown expression type: {}", identifier)),
                                }
                            }
                            ApplicationTarget::Expression(target) => Ok(Some(ParserOutput {
                                parsed: Expression::new(Node::Core(CoreNode::Application(
                                    ApplicationNode::new(
                                        match target.value() {
                                            Node::Core(CoreNode::Closure(node)) => {
                                                Expression::new(Node::Core(CoreNode::Function(
                                                    node.function().clone(),
                                                )))
                                            }
                                            _ => target,
                                        },
                                        args,
                                    ),
                                ))),
                                remaining: input,
                            })),
                        },
                    }
                }
            }
        }
    }
}

enum ApplicationTarget<'a> {
    Primitive(&'a str),
    Expression(Expression<Node>),
}
fn consume_application_target<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, ApplicationTarget<'a>>>> {
    let identifier = consume_identifier(input);
    match identifier.filter(|output| scope.get(output.parsed).is_none()) {
        Some(output) => {
            Ok(Some(output.map(|identifier| {
                ApplicationTarget::Primitive(identifier)
            })))
        }
        _ => match consume_expression(input, scope)? {
            Some(output) => {
                Ok(Some(output.map(|expression| {
                    ApplicationTarget::Expression(expression)
                })))
            }
            _ => Ok(None),
        },
    }
}

fn consume_list_items<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<ParserOutput<'a, Vec<Expression<Node>>>> {
    consume_list_items_iter(input, scope, Vec::new())
}

fn consume_list_items_iter<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
    mut results: Vec<Expression<Node>>,
) -> ParserResult<ParserOutput<'a, Vec<Expression<Node>>>> {
    match consume_expression(input, scope)? {
        None => Ok(ParserOutput {
            parsed: results,
            remaining: input,
        }),
        Some(ParserOutput {
            parsed: expression,
            remaining: input,
        }) => {
            results.push(expression);
            let input = consume_whitespace(input);
            consume_list_items_iter(input, scope, results)
        }
    }
}

fn consume_reference<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Expression<Node>>>> {
    match consume_identifier(input) {
        None => Ok(None),
        Some(ParserOutput {
            parsed: identifier,
            remaining: input,
        }) => match scope.get(identifier) {
            None => Err(format!("Undefined identifier: {}", identifier)),
            Some(offset) => Ok(Some(ParserOutput {
                parsed: Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(
                    offset,
                )))),
                remaining: input,
            })),
        },
    }
}

fn consume_expression<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Expression<Node>>>> {
    match consume_special_form(input, scope)? {
        Some(result) => Ok(Some(result)),
        _ => match consume_primitive(input) {
            Some(result) => {
                Ok(Some(result.map(|value| {
                    Expression::new(Node::Core(CoreNode::Value(value)))
                })))
            }
            _ => match consume_reference(input, scope)? {
                Some(result) => Ok(Some(result)),
                _ => match consume_s_expression(input, scope)? {
                    Some(result) => Ok(Some(result)),
                    _ => Ok(None),
                },
            },
        },
    }
}

struct LexicalScope<'a> {
    bindings: Vec<&'a str>,
}
impl<'a> LexicalScope<'a> {
    pub fn new(_: &'a str) -> Self {
        LexicalScope {
            bindings: Vec::new(),
        }
    }
    pub fn create_child(&self, identifiers: Vec<&'a str>) -> LexicalScope<'a> {
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

pub fn parse(input: &str) -> ParserResult<Expression<Node>> {
    let mut scope = LexicalScope::new(input);
    let input = consume_whitespace(input);
    consume_expression(input, &mut scope)
        .and_then(|result| result.ok_or_else(|| String::from("Invalid expression")))
        .and_then(
            |ParserOutput {
                 parsed: expression,
                 remaining: input,
             }| {
                let input = consume_whitespace(input);
                if !input.is_empty() {
                    return Err(String::from("Unexpected input after expression"));
                }
                Ok(expression)
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::{
        expression::Expression,
        node::{
            arithmetic::{AbsNode, AddNode, ArithmeticNode},
            core::{
                ApplicationNode, ClosureNode, CoreNode, FunctionNode, ReferenceNode, StringValue,
                ValueNode,
            },
            Node,
        },
    };

    use super::parse;

    #[test]
    fn invalid_expression() {
        assert_eq!(parse("#"), Err(String::from("Invalid expression")));
        assert_eq!(parse("1."), Err(String::from("Invalid expression")));
        assert_eq!(parse(".1"), Err(String::from("Invalid expression")));
        assert_eq!(parse("-.1"), Err(String::from("Invalid expression")));
        assert_eq!(parse("'foo'"), Err(String::from("Invalid expression")));
    }

    #[test]
    fn multiple_expressions() {
        assert_eq!(
            parse("null null"),
            Err(String::from("Unexpected input after expression"))
        );
        assert_eq!(
            parse("null foo"),
            Err(String::from("Unexpected input after expression"))
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
            parse("true"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        assert_eq!(
            parse("false"),
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
            parse("1"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(1)
            ))))
        );
        assert_eq!(
            parse("-1"),
            Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(-1)
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
        assert_eq!(parse("foo"), Err(String::from("Undefined identifier: foo")),);
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
    fn function_expressions() {
        assert_eq!(
            parse("(lambda () (add 3 4))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    0,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo) (add foo 4))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4),)))
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo bar) (add foo bar))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (first) (lambda (second) (lambda (foo bar) (add foo bar))))",),
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
                )
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
                    Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                        vec![0],
                        FunctionNode::new(
                            0,
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                        ),
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () foo))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                        vec![1],
                        FunctionNode::new(
                            0,
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1))))
                        ),
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda () bar))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                        vec![0],
                        FunctionNode::new(
                            0,
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))))
                        ),
                    )))),
                )
            )))),
        );
        assert_eq!(
            parse("(lambda (foo bar) (lambda (baz) (add foo baz)))",),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                        vec![1],
                        FunctionNode::new(
                            1,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(2)
                                ))),
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(0)
                                ))),
                            ))))
                        ),
                    )))),
                )
            ))))
        );
        assert_eq!(
            parse("(lambda (first second third) (lambda (fourth fifth) (lambda (sixth) (add first (add second (add third (add fourth (add fifth sixth))))))))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                3,
                Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                    vec![0, 1, 2],
                    FunctionNode::new(
                        2,
                        Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                            vec![0, 1, 2, 3, 4],
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
                        )))),
                    ),
                )))),
            ))))),
        );
        assert_eq!(
            parse("(lambda (foo) (lambda () (lambda () foo)))"),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    1,
                    Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                        vec![0],
                        FunctionNode::new(
                            0,
                            Expression::new(Node::Core(CoreNode::Closure(ClosureNode::new(
                                vec![0],
                                FunctionNode::new(
                                    0,
                                    Expression::new(Node::Core(CoreNode::Reference(
                                        ReferenceNode::new(0)
                                    ))),
                                ),
                            )))),
                        ),
                    )))),
                )
            )))),
        );
    }

    #[test]
    fn function_applications() {
        assert_eq!(
            parse("((lambda (foo) foo) 3)"),
            Ok(Expression::new(Node::Core(CoreNode::Application(
                ApplicationNode::new(
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
            Ok(Expression::new(Node::Core(CoreNode::Application(
                ApplicationNode::new(
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
            parse("((lambda (add) (add 3 4)) (lambda (first second) (add first second)))",),
            Ok(Expression::new(Node::Core(CoreNode::Application(
                ApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Core(CoreNode::Application(ApplicationNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                            vec![
                                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                            ],
                        )))),
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
    fn immediately_invoked_closures() {
        assert_eq!(
            parse("(lambda (foo bar) ((lambda (one two) (add one foo)) 1 2))",),
            Ok(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Core(CoreNode::Application(ApplicationNode::new(
                        Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                            2,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(1)
                                ))),
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(3)
                                ))),
                            ))))
                        )))),
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                        ]
                    )))),
                )
            ))))
        );
    }

    #[test]
    fn primitive_expressions() {
        assert_eq!(
            parse("(abs -123.45)"),
            Ok(Expression::new(Node::Arithmetic(ArithmeticNode::Abs(
                AbsNode::new(Expression::new(Node::Core(CoreNode::Value(
                    ValueNode::Float(-123.45)
                )),))
            )))),
        );
        assert_eq!(
            parse("(add 3 4)"),
            Ok(Expression::new(Node::Arithmetic(ArithmeticNode::Add(
                AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )
            ))))
        );
    }
}
