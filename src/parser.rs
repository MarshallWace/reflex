// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{borrow::Cow, rc::Rc};

use crate::{
    expression::Expression,
    node::NodeFactoryResult,
    value::{StringValue, Value},
};

type ParserError = String;
type ParserResult<T> = Result<T, ParserError>;
type NodeFactory = dyn Fn(&str, Vec<Rc<Expression>>) -> NodeFactoryResult;

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
    consume_while(Box::new(is_valid_identifier_char), input)
}

fn is_valid_identifier_char(char: char) -> bool {
    match char {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '.' | '!' | '?' | '#' => true,
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
    consume_char('"', input)
        .map(|input| consume_string_contents(input))
        .and_then(
            |ParserOutput {
                 parsed,
                 remaining: input,
             }| {
                consume_char('"', input).map(|remaining| ParserOutput {
                    parsed: Value::String(match parsed {
                        Cow::Borrowed(value) => StringValue::new(String::from(value)),
                        Cow::Owned(value) => StringValue::new(value),
                    }),
                    remaining,
                })
            },
        )
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

fn consume_s_expression<'a>(
    input: &'a str,
    factory: &NodeFactory,
) -> ParserResult<Option<ParserOutput<'a, Expression>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            consume_identifier(input)
                .ok_or_else(|| String::from("Expected expression identifier"))
                .and_then(
                    |ParserOutput {
                         parsed: identifier,
                         remaining: input,
                     }| {
                        let input = consume_whitespace(input);
                        consume_args(input, factory, Vec::new()).and_then(
                            |ParserOutput {
                                 parsed: args,
                                 remaining: input,
                             }| {
                                let input = consume_whitespace(input);
                                consume_char(')', input)
                                    .ok_or_else(|| String::from("Unterminated expression"))
                                    .and_then(|input| {
                                        expression_factory(identifier, args, factory).map(
                                            |expression| {
                                                Some(ParserOutput {
                                                    parsed: expression,
                                                    remaining: input,
                                                })
                                            },
                                        )
                                    })
                            },
                        )
                    },
                )
        }
    }
}

fn expression_factory(
    identifier: &str,
    args: Vec<Rc<Expression>>,
    factory: &NodeFactory,
) -> ParserResult<Rc<Expression>> {
    match factory(identifier, args) {
        NodeFactoryResult::Err(message) => Err(message),
        NodeFactoryResult::Some(node) => Ok(Rc::new(Expression::Node(node))),
        NodeFactoryResult::None(args) => {
            builtin_expression_factory(identifier, args).and_then(|result| {
                result.map_or_else(
                    || Err(String::from("Invalid arguments")),
                    |result| Ok(result),
                )
            })
        }
    }
}

fn builtin_expression_factory(
    identifier: &str,
    args: Vec<Rc<Expression>>,
) -> ParserResult<Option<Rc<Expression>>> {
    match identifier {
        "throw" => {
            if args.len() != 1 {
                return Err(String::from("Invalid number of arguments"));
            }
            let args = &mut args.into_iter();
            let target = args.next().unwrap();
            match &*target {
                Expression::Value(Value::String(value)) => {
                    Ok(Some(Rc::new(Expression::Error(String::from(value.get())))))
                }
                _ => Err(String::from("Invalid arguments")),
            }
        }
        "await" => {
            if args.len() != 0 {
                return Err(String::from("Invalid number of arguments"));
            }
            Ok(Some(Rc::new(Expression::Pending)))
        }
        _ => Ok(None),
    }
}

fn consume_args<'a>(
    input: &'a str,
    factory: &NodeFactory,
    mut args: Vec<Rc<Expression>>,
) -> ParserResult<ParserOutput<'a, Vec<Rc<Expression>>>> {
    let result = consume_expression(input, factory)?;
    let (expression, input) = match result {
        Some(ParserOutput { parsed, remaining }) => (parsed, remaining),
        None => {
            return Ok(ParserOutput {
                parsed: args,
                remaining: input,
            })
        }
    };
    {
        let args = &mut args;
        args.push(expression);
    }
    let input = consume_whitespace(input);
    consume_args(input, factory, args)
}

fn consume_expression<'a>(
    input: &'a str,
    factory: &NodeFactory,
) -> ParserResult<Option<ParserOutput<'a, Rc<Expression>>>> {
    consume_s_expression(input, factory).map(|result| {
        result.or_else(|| {
            consume_primitive(input)
                .map(|result| result.map(|value| Rc::new(Expression::Value(value))))
        })
    })
}

pub fn parse(input: &str, factory: &NodeFactory) -> ParserResult<Rc<Expression>> {
    let input = consume_whitespace(input);
    consume_expression(input, factory)
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

    use crate::{
        expression::Expression,
        node::{abs::AbsNode, add::AddNode, Node},
        value::{StringValue, Value},
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
            Ok(Expression::Value(Value::Nil))
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
    fn builtin_procedures() {
        assert_eq!(
            parse("(await)", &&Node::factory),
            Ok(Rc::new(Expression::Pending))
        );
        assert_eq!(
            parse("(throw \"foo\")", &&Node::factory),
            Ok(Rc::new(Expression::Error(String::from("foo"))))
        );
    }

    #[test]
    fn s_expressions() {
        assert_eq!(
            parse("(abs -123.45)", &&Node::factory),
            Ok(Rc::new(Expression::Node(Node::Abs(AbsNode::new(Rc::new(
                Expression::Value(Value::Float(-123.45)),
            ))))))
        );
        assert_eq!(
            parse("(add 3 4)", &&Node::factory),
            Ok(Rc::new(Expression::Node(Node::Add(AddNode::new(
                Rc::new(Expression::Value(Value::Int(3))),
                Rc::new(Expression::Value(Value::Int(4))),
            )))))
        );
    }
}
