// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{borrow::Cow, rc::Rc};

use crate::expression::{Expression, Function, Node, Object, StackOffset, StringValue, Value};

type ParserError = String;
type ParserResult<T> = Result<T, ParserError>;
type NodeFactory = dyn Fn(&str, Vec<Rc<Expression>>) -> ParserResult<Option<Node>>;

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

fn consume_primitive(input: &str) -> Option<ParserOutput<Value>> {
    None.or_else(|| consume_nil_literal(input))
        .or_else(|| consume_boolean_literal(input))
        .or_else(|| consume_number_literal(input))
        .or_else(|| consume_string_literal(input))
}

fn consume_nil_literal(input: &str) -> Option<ParserOutput<Value>> {
    consume_keyword("null", input).map(|remaining| ParserOutput {
        parsed: Value::Nil,
        remaining,
    })
}

fn consume_boolean_literal(input: &str) -> Option<ParserOutput<Value>> {
    None.or_else(|| consume_true_literal(input))
        .or_else(|| consume_false_literal(input))
}

fn consume_true_literal(input: &str) -> Option<ParserOutput<Value>> {
    consume_keyword("true", input).map(|remaining| ParserOutput {
        parsed: Value::Boolean(true),
        remaining,
    })
}

fn consume_false_literal(input: &str) -> Option<ParserOutput<Value>> {
    consume_keyword("false", input).map(|remaining| ParserOutput {
        parsed: Value::Boolean(false),
        remaining,
    })
}

fn consume_number_literal(input: &str) -> Option<ParserOutput<Value>> {
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
                parsed: Value::Int(if is_negative { -value } else { value }),
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
            parsed: Value::Float(if is_negative { -value } else { value }),
            remaining: input,
        }),
        Err(_) => None,
    }
}

fn is_digit_char(char: char) -> bool {
    char.is_ascii_digit()
}

fn consume_string_literal(input: &str) -> Option<ParserOutput<Value>> {
    let input = consume_char('"', input)?;
    let ParserOutput {
        parsed,
        remaining: input,
    } = consume_string_contents(input);
    let input = consume_char('"', input)?;
    Some(ParserOutput {
        parsed: Value::String(match parsed {
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

fn consume_object_literal<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    match consume_char('{', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            let ParserOutput {
                parsed: entries,
                remaining: input,
            } = consume_object_literal_entries(input, factory, scope)?;
            match consume_char('}', input) {
                None => Err(String::from("Unterminated object literal")),
                Some(input) => Ok(Some(ParserOutput {
                    parsed: Rc::new(Expression::Object(Object::new(entries))),
                    remaining: input,
                })),
            }
        }
    }
}

fn consume_object_literal_entries<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<ParserOutput<'a, Vec<(StringValue, Rc<Expression>)>>> {
    consume_object_literal_entries_iter(input, factory, scope, Vec::new())
}

fn consume_object_literal_entries_iter<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
    mut results: Vec<(StringValue, Rc<Expression>)>,
) -> ParserResult<ParserOutput<'a, Vec<(StringValue, Rc<Expression>)>>> {
    match consume_identifier(input) {
        None => Ok(ParserOutput {
            parsed: results,
            remaining: input,
        }),
        Some(ParserOutput {
            parsed: key,
            remaining: input,
        }) => {
            let input = consume_whitespace(input);
            match consume_char(':', input) {
                None => Err(format!("Invalid object literal entry for key: {}", key)),
                Some(input) => {
                    let input = consume_whitespace(input);
                    match consume_expression(input, factory, scope)? {
                        None => Err(format!("Invalid object literal value for key: {}", key)),
                        Some(ParserOutput {
                            parsed: value,
                            remaining: input,
                        }) => {
                            results.push((StringValue::new(String::from(key)), value));
                            let input = consume_whitespace(input);
                            match consume_char(',', input) {
                                None => Ok(ParserOutput {
                                    parsed: results,
                                    remaining: input,
                                }),
                                Some(input) => {
                                    let input = consume_whitespace(input);
                                    consume_object_literal_entries_iter(
                                        input, factory, scope, results,
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn consume_special_form<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    consume_function_expression(input, factory, scope)
}

fn consume_function_expression<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            match consume_keyword("fn", input) {
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
                                None => Err(String::from("Unterminated function argument list")),
                                Some(input) => {
                                    let input = consume_whitespace(input);
                                    let arity = arg_names.len();
                                    let child_scope = scope
                                        .create_child(arg_names.iter().map(|name| *name).collect());
                                    match consume_expression(input, factory, &child_scope)? {
                                        None => Err(String::from("Missing function body")),
                                        Some(ParserOutput {
                                            parsed: body,
                                            remaining: input,
                                        }) => {
                                            let input = consume_whitespace(input);
                                            match consume_char(')', input) {
                                                None => Err(String::from(
                                                    "Unterminated function expression",
                                                )),
                                                Some(input) => Ok(Some(ParserOutput {
                                                    parsed: Rc::new(Expression::Function(
                                                        parse_function(arity, body),
                                                    )),
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

fn parse_function(arity: usize, body: Rc<Expression>) -> Function {
    Function {
        arity,
        captures: get_free_variables(&body, arity),
        body,
    }
}
fn get_free_variables(expression: &Expression, arity: usize) -> Option<Vec<StackOffset>> {
    let mut results = get_free_variables_iter(expression, arity, Vec::new());
    if results.len() == 0 {
        return None;
    }
    results.sort_unstable();
    results.dedup();
    Some(results)
}
fn get_free_variables_iter(
    expression: &Expression,
    arity: usize,
    mut results: Vec<StackOffset>,
) -> Vec<StackOffset> {
    match expression {
        Expression::Reference(offset) => {
            let offset = *offset;
            if offset >= arity {
                results.push(offset - arity);
            }
            results
        }
        Expression::Node(node) => match node.children() {
            Some(expressions) => expressions.iter().fold(results, |results, expression| {
                get_free_variables_iter(expression, arity, results)
            }),
            None => results,
        },
        Expression::Function(Function {
            arity: _,
            captures: Some(captures),
            body: _,
        }) if captures.iter().any(|offset| *offset >= arity) => {
            results.extend(
                captures
                    .iter()
                    .filter(|offset| **offset >= arity)
                    .map(|offset| offset - arity),
            );
            results
        }
        _ => results,
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
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            match consume_identifier(input) {
                None => Err(String::from("Expected expression identifier")),
                Some(ParserOutput {
                    parsed: identifier,
                    remaining: input,
                }) => {
                    let input = consume_whitespace(input);
                    let ParserOutput {
                        parsed: args,
                        remaining: input,
                    } = consume_list_items(input, factory, scope)?;
                    let input = consume_whitespace(input);
                    match consume_char(')', input) {
                        None => Err(String::from("Unterminated expression")),
                        Some(input) => match expression_factory(identifier, args, factory) {
                            Err(err) => Err(err),
                            Ok(Some(result)) => Ok(Some(ParserOutput {
                                parsed: result,
                                remaining: input,
                            })),
                            Ok(None) => Err(format!("Unknown expression type: {}", identifier)),
                        },
                    }
                }
            }
        }
    }
}

fn consume_list_items<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<ParserOutput<'a, Vec<Rc<Expression>>>> {
    consume_list_items_iter(input, factory, scope, Vec::new())
}

fn consume_list_items_iter<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
    mut results: Vec<Rc<Expression>>,
) -> ParserResult<ParserOutput<'a, Vec<Rc<Expression>>>> {
    match consume_expression(input, factory, scope)? {
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
            consume_list_items_iter(input, factory, scope, results)
        }
    }
}

fn expression_factory(
    identifier: &str,
    args: Vec<Rc<Expression>>,
    factory: &NodeFactory,
) -> ParserResult<Option<Rc<Expression>>> {
    match identifier {
        "throw" => parse_throw_expression(args).map(Some),
        "await" => parse_await_expression(args).map(Some),
        _ => match factory(identifier, args)? {
            Some(node) => Ok(Some(Rc::new(Expression::Node(node)))),
            None => Ok(None),
        },
    }
}

fn parse_throw_expression(args: Vec<Rc<Expression>>) -> ParserResult<Rc<Expression>> {
    if args.len() != 1 {
        return Err(String::from("Invalid number of arguments"));
    }
    let args = &mut args.into_iter();
    let target = args.next().unwrap();
    match &*target {
        Expression::Value(Value::String(value)) => {
            Ok(Rc::new(Expression::Error(String::from(value.get()))))
        }
        _ => Err(String::from("Invalid arguments")),
    }
}

fn parse_await_expression(args: Vec<Rc<Expression>>) -> ParserResult<Rc<Expression>> {
    if args.len() != 0 {
        return Err(String::from("Invalid number of arguments"));
    }
    Ok(Rc::new(Expression::Pending))
}

fn consume_reference<'a>(
    input: &'a str,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    match consume_identifier(input) {
        Some(ParserOutput {
            parsed: identifier,
            remaining: input,
        }) => match scope.get(identifier) {
            None => Err(format!("Undefined identifier: {}", identifier)),
            Some(offset) => Ok(Some(ParserOutput {
                parsed: Rc::new(Expression::Reference(offset)),
                remaining: input,
            })),
        },
        None => Ok(None),
    }
}

fn consume_expression<'a>(
    input: &'a str,
    factory: &NodeFactory,
    scope: &LexicalScope<'a>,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    match consume_special_form(input, factory, scope)? {
        Some(result) => Ok(Some(result)),
        _ => match consume_primitive(input) {
            Some(result) => Ok(Some(result.map(|value| Rc::new(Expression::Value(value))))),
            None => match consume_reference(input, scope)? {
                Some(result) => Ok(Some(result)),
                _ => match consume_object_literal(input, factory, scope)? {
                    Some(result) => Ok(Some(result)),
                    _ => match consume_s_expression(input, factory, scope)? {
                        Some(result) => Ok(Some(result)),
                        _ => Ok(None),
                    },
                },
            },
        },
    }
}

struct LexicalScope<'a> {
    bindings: Vec<&'a str>,
}
impl<'a> LexicalScope<'a> {
    pub fn new(_: &'a str) -> LexicalScope {
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

pub fn parse(input: &str, factory: &NodeFactory) -> ParserResult<Rc<Expression>> {
    let mut scope = LexicalScope::new(input);
    let input = consume_whitespace(input);
    consume_expression(input, factory, &mut scope)
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
    use std::rc::Rc;

    use crate::expression::{
        node::{AbsNode, AddNode},
        Expression, Function, Node, Object, StringValue, Value,
    };

    use super::parse;

    #[test]
    fn invalid_expression() {
        assert_eq!(
            parse("#", &Node::factory),
            Err(String::from("Invalid expression"))
        );
        assert_eq!(
            parse("1.", &Node::factory),
            Err(String::from("Invalid expression"))
        );
        assert_eq!(
            parse(".1", &Node::factory),
            Err(String::from("Invalid expression"))
        );
        assert_eq!(
            parse("-.1", &Node::factory),
            Err(String::from("Invalid expression"))
        );
        assert_eq!(
            parse("'foo'", &Node::factory),
            Err(String::from("Invalid expression"))
        );
    }

    #[test]
    fn multiple_expressions() {
        assert_eq!(
            parse("null null", &Node::factory),
            Err(String::from("Unexpected input after expression"))
        );
        assert_eq!(
            parse("null foo", &Node::factory),
            Err(String::from("Unexpected input after expression"))
        );
    }

    #[test]
    fn ignore_extra_whitespace() {
        assert_eq!(
            parse("  \n\r\tnull\n\r\t  ", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Nil)))
        );
    }

    #[test]
    fn primitive_values() {
        assert_eq!(
            parse("null", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Nil)))
        );
        assert_eq!(
            parse("true", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Boolean(true))))
        );
        assert_eq!(
            parse("false", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Boolean(false))))
        );
        assert_eq!(
            parse("0", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(0))))
        );
        assert_eq!(
            parse("-0", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(0))))
        );
        assert_eq!(
            parse("1", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(1))))
        );
        assert_eq!(
            parse("-1", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(-1))))
        );
        assert_eq!(
            parse("123", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(123))))
        );
        assert_eq!(
            parse("-123", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Int(-123))))
        );
        assert_eq!(
            parse("0.0", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(0.0))))
        );
        assert_eq!(
            parse("-0.0", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(0.0))))
        );
        assert_eq!(
            parse("3.142", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(3.142))))
        );
        assert_eq!(
            parse("-3.142", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(-3.142))))
        );
        assert_eq!(
            parse("123.45", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(123.45))))
        );
        assert_eq!(
            parse("-123.45", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::Float(-123.45))))
        );
        assert_eq!(
            parse("\"\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("")
            )))))
        );
        assert_eq!(
            parse("\" \"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from(" ")
            )))))
        );
        assert_eq!(
            parse("\"foo\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo")
            )))))
        );
        assert_eq!(
            parse("\"foo bar\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo bar")
            )))))
        );
    }

    #[test]
    fn escaped_string_literals() {
        assert_eq!(
            parse("\"foo\\\\bar\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo\\bar")
            )))))
        );
        assert_eq!(
            parse("\"foo\\nbar\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo\nbar")
            )))))
        );
        assert_eq!(
            parse("\"foo\\tbar\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo\tbar")
            )))))
        );
        assert_eq!(
            parse("\"foo\\rbar\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo\rbar")
            )))))
        );
        assert_eq!(
            parse("\"foo\\\"bar\\\"baz\"", &Node::factory),
            Ok(Rc::new(Expression::Value(Value::String(StringValue::new(
                String::from("foo\"bar\"baz")
            )))))
        );
    }
    #[test]
    fn identifiers() {
        assert_eq!(
            parse("foo", &Node::factory),
            Err(String::from("Undefined identifier: foo")),
        );
        assert_eq!(
            parse("(fn (a) a)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (_) _)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (foo) foo)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (_foo) _foo)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (foo!) foo!)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (foo?) foo?)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (foo_bar) foo_bar)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
        assert_eq!(
            parse("(fn (foo-bar) foo-bar)", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Reference(0))
            })))
        );
    }

    #[test]
    fn object_literals() {
        assert_eq!(
            parse("{}", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![])))),
        );
        assert_eq!(
            parse("{   }", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![])))),
        );
        assert_eq!(
            parse("{foo:true}", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![(
                StringValue::new(String::from("foo")),
                Rc::new(Expression::Value(Value::Boolean(true)))
            )])))),
        );
        assert_eq!(
            parse("{foo:true,}", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![(
                StringValue::new(String::from("foo")),
                Rc::new(Expression::Value(Value::Boolean(true)))
            )])))),
        );
        assert_eq!(
            parse("{first:1, second: 2, third: 3}", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![
                (
                    StringValue::new(String::from("first")),
                    Rc::new(Expression::Value(Value::Int(1)))
                ),
                (
                    StringValue::new(String::from("second")),
                    Rc::new(Expression::Value(Value::Int(2)))
                ),
                (
                    StringValue::new(String::from("third")),
                    Rc::new(Expression::Value(Value::Int(3)))
                )
            ])))),
        );
        assert_eq!(
            parse("{first:1, second: 2, third: 3,}", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![
                (
                    StringValue::new(String::from("first")),
                    Rc::new(Expression::Value(Value::Int(1)))
                ),
                (
                    StringValue::new(String::from("second")),
                    Rc::new(Expression::Value(Value::Int(2)))
                ),
                (
                    StringValue::new(String::from("third")),
                    Rc::new(Expression::Value(Value::Int(3)))
                )
            ])))),
        );
        assert_eq!(
            parse("{\n\t foo\n\t :\n\t true\n\t ,\n\t }", &Node::factory),
            Ok(Rc::new(Expression::Object(Object::new(vec![(
                StringValue::new(String::from("foo")),
                Rc::new(Expression::Value(Value::Boolean(true)))
            )])))),
        );
        assert_eq!(
            parse("{foo:true,,}", &Node::factory),
            Err(String::from("Unterminated object literal")),
        );
        assert_eq!(
            parse("{,}", &Node::factory),
            Err(String::from("Unterminated object literal")),
        );
    }

    #[test]
    fn function_expressions() {
        assert_eq!(
            parse("(fn () (add 3 4))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 0,
                captures: None,
                body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                    Rc::new(Expression::Value(Value::Int(3))),
                    Rc::new(Expression::Value(Value::Int(4)))
                ))))
            })))
        );
        assert_eq!(
            parse("(fn (foo) (add foo 4))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                    Rc::new(Expression::Reference(0)),
                    Rc::new(Expression::Value(Value::Int(4)))
                ))))
            })))
        );
        assert_eq!(
            parse("(fn (foo bar) (add foo bar))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 2,
                captures: None,
                body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                    Rc::new(Expression::Reference(1)),
                    Rc::new(Expression::Reference(0)),
                ))))
            })))
        );
        assert_eq!(
            parse("(fn (first second third) (fn (fourth fifth) (fn (sixth) (add first (add second (add third (add fourth (add fifth sixth))))))))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 3,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 2,
                    captures: Some(vec![0, 1, 2]),
                    body: Rc::new(Expression::Function(Function {
                        arity: 1,
                        captures: Some(vec![0, 1, 2, 3, 4]),
                        body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                            Rc::new(Expression::Reference(5)),
                            Rc::new(Expression::Node(Node::Add(AddNode::new(
                                Rc::new(Expression::Reference(4)),
                                Rc::new(Expression::Node(Node::Add(AddNode::new(
                                    Rc::new(Expression::Reference(3)),
                                    Rc::new(Expression::Node(Node::Add(AddNode::new(
                                        Rc::new(Expression::Reference(2)),
                                        Rc::new(Expression::Node(Node::Add(AddNode::new(
                                            Rc::new(Expression::Reference(1)),
                                            Rc::new(Expression::Reference(0)),
                                        )))),
                                    )))),
                                )))),
                            ))))
                        ))))
                    })),
                })),
            })))
        );
    }

    #[test]
    fn closures() {
        assert_eq!(
            parse("(fn (foo) (fn () foo))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 0,
                    captures: Some(vec![0]),
                    body: Rc::new(Expression::Reference(0)),
                })),
            })))
        );
        assert_eq!(
            parse("(fn (foo bar) (fn () foo))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 2,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 0,
                    captures: Some(vec![1]),
                    body: Rc::new(Expression::Reference(1)),
                })),
            })))
        );
        assert_eq!(
            parse("(fn (foo bar) (fn () bar))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 2,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 0,
                    captures: Some(vec![0]),
                    body: Rc::new(Expression::Reference(0)),
                })),
            })))
        );
        assert_eq!(
            parse("(fn (foo bar) (fn (baz) (add foo baz)))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 2,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 1,
                    captures: Some(vec![1]),
                    body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                        Rc::new(Expression::Reference(2)),
                        Rc::new(Expression::Reference(0)),
                    )))),
                })),
            })))
        );
        assert_eq!(
            parse("(fn (foo) (fn () (fn () foo)))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 1,
                captures: None,
                body: Rc::new(Expression::Function(Function {
                    arity: 0,
                    captures: Some(vec![0]),
                    body: Rc::new(Expression::Function(Function {
                        arity: 0,
                        captures: Some(vec![0]),
                        body: Rc::new(Expression::Reference(0))
                    })),
                })),
            })))
        );
    }

    #[test]
    fn await_expressions() {
        assert_eq!(
            parse("(await)", &Node::factory),
            Ok(Rc::new(Expression::Pending))
        );
    }

    #[test]
    fn throw_expressions() {
        assert_eq!(
            parse("(throw \"foo\")", &Node::factory),
            Ok(Rc::new(Expression::Error(String::from("foo"))))
        );
    }

    #[test]
    fn s_expressions() {
        assert_eq!(
            parse("(abs -123.45)", &Node::factory),
            Ok(Rc::new(Expression::Node(Node::Abs(AbsNode::new(Rc::new(
                Expression::Value(Value::Float(-123.45)),
            ))))))
        );
        assert_eq!(
            parse("(add 3 4)", &Node::factory),
            Ok(Rc::new(Expression::Node(Node::Add(AddNode::new(
                Rc::new(Expression::Value(Value::Int(3))),
                Rc::new(Expression::Value(Value::Int(4))),
            )))))
        );
        assert_eq!(
            parse("(fn (foo bar) (add foo bar))", &Node::factory),
            Ok(Rc::new(Expression::Function(Function {
                arity: 2,
                captures: None,
                body: Rc::new(Expression::Node(Node::Add(AddNode::new(
                    Rc::new(Expression::Reference(1)),
                    Rc::new(Expression::Reference(0)),
                ))))
            })))
        );
    }
}
