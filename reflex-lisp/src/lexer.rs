// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{borrow::Cow, fmt};

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

#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxDatum<'a> {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Symbol(&'a str),
    List(Vec<SyntaxDatum<'a>>),
}
impl<'a> fmt::Display for SyntaxDatum<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

const EOF: &'static str = "end of input";

pub fn parse_syntax<'a>(input: &'a str) -> ParserResult<SyntaxDatum<'a>> {
    let input = consume_whitespace(input);
    let ParserOutput {
        parsed,
        remaining: input,
    } = match consume_syntax(input)? {
        Some(result) => Ok(result),
        None => Err(format!(
            "Expected expression, received '{}'",
            peek_next_char(input)
                .map(String::from)
                .unwrap_or(String::from(EOF))
        )),
    }?;
    let input = consume_whitespace(input);
    if input.len() > 0 {
        return Err(format!(
            "Expected {}, received '{}'",
            EOF,
            peek_next_char(input)
                .map(String::from)
                .unwrap_or(String::from(EOF))
        ));
    }
    return Ok(parsed);
}

fn consume_syntax<'a>(input: &'a str) -> ParserResult<Option<ParserOutput<SyntaxDatum<'a>>>> {
    match consume_primitive(input) {
        Some(result) => Ok(Some(result)),
        _ => match consume_symbol(input) {
            Some(result) => Ok(Some(result)),
            _ => match consume_quote(input)? {
                Some(result) => Ok(Some(result)),
                _ => match consume_list(input)? {
                    Some(result) => Ok(Some(result)),
                    _ => Ok(None),
                },
            },
        },
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

fn consume_while(predicate: impl Fn(char) -> bool, input: &str) -> Option<ParserOutput<&str>> {
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

fn peek_next_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn consume_symbol(input: &str) -> Option<ParserOutput<SyntaxDatum>> {
    let mut chars = input.chars();
    let first_char = chars.next()?;
    let identifier_length = if first_char == '#' {
        let constant_name = chars.next()?;
        let is_valid_constant = match constant_name {
            't' | 'f' => true,
            _ => false,
        };
        if !is_valid_constant {
            return None;
        }
        let name_length = 1;
        let identifier_length = 1 + name_length;
        Some(identifier_length)
    } else if is_valid_symbol_char(first_char) {
        Some(
            input
                .char_indices()
                .skip(1)
                .find_map(|(index, char)| {
                    if is_valid_symbol_char(char) {
                        None
                    } else {
                        Some(index)
                    }
                })
                .unwrap_or_else(|| input.len()),
        )
    } else {
        None
    }?;
    Some(ParserOutput {
        parsed: SyntaxDatum::Symbol(&input[0..identifier_length]),
        remaining: &input[identifier_length..],
    })
}

fn is_valid_symbol_char(char: char) -> bool {
    if char.is_ascii_alphanumeric() {
        return true;
    }
    match char {
        '_' | '!' | '?' | '+' | '-' | '*' | '/' | '=' | '<' | '>' => true,
        _ => false,
    }
}

fn consume_primitive(input: &str) -> Option<ParserOutput<SyntaxDatum>> {
    None.or_else(|| consume_number_literal(input))
        .or_else(|| consume_string_literal(input))
}

fn consume_number_literal(input: &str) -> Option<ParserOutput<SyntaxDatum>> {
    let (is_negative, input) =
        consume_char('-', input).map_or_else(|| (false, input), |remaining| (true, remaining));
    let result = consume_while(is_digit_char, input);
    if let None = result {
        return None;
    }
    let (int_chars, input) = result
        .map(|ParserOutput { parsed, remaining }| (parsed, remaining))
        .unwrap();
    let (is_float, input) =
        consume_char('.', input).map_or_else(|| (false, input), |remaining| (true, remaining));
    if !is_float {
        let int_value = int_chars.parse::<i64>();
        return match int_value {
            Ok(value) => Some(ParserOutput {
                parsed: SyntaxDatum::IntegerLiteral(if is_negative { -value } else { value }),
                remaining: input,
            }),
            Err(_) => None,
        };
    }
    let result = consume_while(is_digit_char, input);
    if let None = result {
        return None;
    }
    let (decimal_chars, input) = result
        .map(|ParserOutput { parsed, remaining }| (parsed, remaining))
        .unwrap();
    let float_value = format!("{}.{}", int_chars, decimal_chars).parse::<f64>();
    match float_value {
        Ok(value) => Some(ParserOutput {
            parsed: SyntaxDatum::FloatLiteral(if is_negative { -value } else { value }),
            remaining: input,
        }),
        Err(_) => None,
    }
}

fn is_digit_char(char: char) -> bool {
    char.is_ascii_digit()
}

fn consume_string_literal(input: &str) -> Option<ParserOutput<SyntaxDatum>> {
    let input = consume_char('"', input)?;
    let ParserOutput {
        parsed,
        remaining: input,
    } = consume_string_contents(input);
    let input = consume_char('"', input)?;
    Some(ParserOutput {
        parsed: SyntaxDatum::StringLiteral(match parsed {
            Cow::Borrowed(value) => String::from(value),
            Cow::Owned(value) => value,
        }),
        remaining: input,
    })
}

fn consume_string_contents(input: &str) -> ParserOutput<Cow<str>> {
    let result = consume_while(is_string_char, input).map_or_else(
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

fn consume_quote<'a>(input: &'a str) -> ParserResult<Option<ParserOutput<SyntaxDatum>>> {
    match consume_char('\'', input) {
        None => Ok(None),
        Some(input) => match consume_syntax(input)? {
            Some(result) => Ok(Some(result.map(|content| {
                SyntaxDatum::List(vec![SyntaxDatum::Symbol("quote"), content])
            }))),
            None => Err(format!(
                "Expected quoted expression, received '{}'",
                peek_next_char(input)
                    .map(String::from)
                    .unwrap_or(String::from(EOF))
            )),
        },
    }
}

fn consume_list<'a>(input: &'a str) -> ParserResult<Option<ParserOutput<SyntaxDatum>>> {
    match consume_char('(', input) {
        None => Ok(None),
        Some(input) => {
            let input = consume_whitespace(input);
            let ParserOutput {
                parsed: items,
                remaining: input,
            } = consume_list_items(input, Vec::new())?;
            let input = consume_whitespace(input);
            match consume_char(')', input) {
                None => Err(format!(
                    "Expected ')', received '{}'",
                    peek_next_char(input)
                        .map(String::from)
                        .unwrap_or(String::from(EOF)),
                )),
                Some(input) => Ok(Some(ParserOutput {
                    parsed: SyntaxDatum::List(items),
                    remaining: input,
                })),
            }
        }
    }
}

fn consume_list_items<'a>(
    input: &'a str,
    mut results: Vec<SyntaxDatum<'a>>,
) -> ParserResult<ParserOutput<'a, Vec<SyntaxDatum<'a>>>> {
    match consume_syntax(input)? {
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
            consume_list_items(input, results)
        }
    }
}
