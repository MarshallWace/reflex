// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::borrow::Cow;

use crate::{
    node::Node,
    types::{Expression, StringValue, Value},
};

type ParserError = &'static str;
type ParserResult<T> = Result<T, ParserError>;
type NodeFactory = dyn Fn(&str, Vec<Expression>) -> ParserResult<Node>;

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
) -> ParserResult<Option<ParserOutput<'a, Node>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            consume_identifier(input)
                .ok_or_else(|| "Expected expression identifier")
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
                                    .ok_or_else(|| "Unterminated expression")
                                    .and_then(|input| {
                                        factory(identifier, args).map(|node| {
                                            Some(ParserOutput {
                                                parsed: node,
                                                remaining: input,
                                            })
                                        })
                                    })
                            },
                        )
                    },
                )
        }
    }
}

fn consume_args<'a>(
    input: &'a str,
    factory: &NodeFactory,
    mut args: Vec<Expression>,
) -> ParserResult<ParserOutput<'a, Vec<Expression>>> {
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
) -> ParserResult<Option<ParserOutput<'a, Expression>>> {
    consume_s_expression(input, factory).map(|result| {
        result
            .map(|result| result.map(Expression::Node))
            .or_else(|| consume_primitive(input).map(|result| result.map(Expression::Value)))
    })
}

pub fn parse(input: &str, factory: &NodeFactory) -> ParserResult<Expression> {
    let input = consume_whitespace(input);
    consume_expression(input, factory)
        .and_then(|result| result.ok_or_else(|| "Invalid expression"))
        .and_then(
            |ParserOutput {
                 parsed: expression,
                 remaining: input,
             }| {
                let input = consume_whitespace(input);
                if !input.is_empty() {
                    return Err("Unexpected input after expression");
                }
                Ok(expression)
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::{node::{Node, abs::AbsNode, add::AddNode}, types::{Expression, StringValue, Value}};

    use super::{parse, NodeFactory};

    #[test]
    fn invalid_expression() {
        let factory: &NodeFactory = &|_, _| Err("Not implemented");
        assert_eq!(parse("foo", factory), Err("Invalid expression"));
        assert_eq!(parse("1.", factory), Err("Invalid expression"));
        assert_eq!(parse(".1", factory), Err("Invalid expression"));
        assert_eq!(parse("-.1", factory), Err("Invalid expression"));
        assert_eq!(parse("'foo'", factory), Err("Invalid expression"));
    }

    #[test]
    fn multiple_expressions() {
        let factory: &NodeFactory = &|_, _| Err("Not implemented");
        assert_eq!(
            parse("null null", factory),
            Err("Unexpected input after expression")
        );
        assert_eq!(
            parse("null foo", factory),
            Err("Unexpected input after expression")
        );
    }

    #[test]
    fn ignore_extra_whitespace() {
        let factory: &NodeFactory = &|_, _| Err("Not implemented");
        assert_eq!(
            parse("  \n\r\tnull\n\r\t  ", factory),
            Ok(Expression::Value(Value::Nil))
        );
    }

    #[test]
    fn primitive_values() {
        let factory: &NodeFactory = &|_, _| Err("Not implemented");
        assert_eq!(parse("null", factory), Ok(Expression::Value(Value::Nil)));
        assert_eq!(
            parse("true", factory),
            Ok(Expression::Value(Value::Boolean(true)))
        );
        assert_eq!(
            parse("false", factory),
            Ok(Expression::Value(Value::Boolean(false)))
        );
        assert_eq!(parse("0", factory), Ok(Expression::Value(Value::Int(0))));
        assert_eq!(parse("-0", factory), Ok(Expression::Value(Value::Int(0))));
        assert_eq!(parse("1", factory), Ok(Expression::Value(Value::Int(1))));
        assert_eq!(parse("-1", factory), Ok(Expression::Value(Value::Int(-1))));
        assert_eq!(
            parse("123", factory),
            Ok(Expression::Value(Value::Int(123)))
        );
        assert_eq!(
            parse("-123", factory),
            Ok(Expression::Value(Value::Int(-123)))
        );
        assert_eq!(
            parse("0.0", factory),
            Ok(Expression::Value(Value::Float(0.0)))
        );
        assert_eq!(
            parse("-0.0", factory),
            Ok(Expression::Value(Value::Float(0.0)))
        );
        assert_eq!(
            parse("3.142", factory),
            Ok(Expression::Value(Value::Float(3.142)))
        );
        assert_eq!(
            parse("-3.142", factory),
            Ok(Expression::Value(Value::Float(-3.142)))
        );
        assert_eq!(
            parse("123.45", factory),
            Ok(Expression::Value(Value::Float(123.45)))
        );
        assert_eq!(
            parse("-123.45", factory),
            Ok(Expression::Value(Value::Float(-123.45)))
        );
        assert_eq!(
            parse("\"\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("")
            ))))
        );
        assert_eq!(
            parse("\" \"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from(" ")
            ))))
        );
        assert_eq!(
            parse("\"foo\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo")
            ))))
        );
        assert_eq!(
            parse("\"foo bar\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo bar")
            ))))
        );
    }

    #[test]
    fn escaped_string_literals() {
        let factory: &NodeFactory = &|_, _| Err("Not implemented");
        assert_eq!(
            parse("\"foo\\\\bar\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo\\bar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\nbar\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo\nbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\tbar\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo\tbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\rbar\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo\rbar")
            ))))
        );
        assert_eq!(
            parse("\"foo\\\"bar\\\"baz\"", factory),
            Ok(Expression::Value(Value::String(StringValue::new(
                String::from("foo\"bar\"baz")
            ))))
        );
    }

    #[test]
    fn s_expressions() {
        assert_eq!(
            parse("(abs -123.45)", &Node::new),
            Ok(Expression::Node(Node::Abs(AbsNode::new(
                Expression::Value(Value::Float(-123.45))
            ))))
        );
        assert_eq!(
            parse("(add 3 4)", &Node::new),
            Ok(Expression::Node(Node::Add(AddNode::new(
                Expression::Value(Value::Int(3)),
                Expression::Value(Value::Int(4))
            ))))
        );
    }
}
