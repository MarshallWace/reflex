// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use prost_reflect::{DynamicMessage, MessageDescriptor, Value};
use reflex::core::{
    Expression, ExpressionFactory, HeapAllocator, RefType, StringTermType, StringValue,
};

use crate::{utils::get_message_field, CustomType};

pub struct DurationMessage;
impl DurationMessage {
    pub const NAME: &'static str = "google.protobuf.Duration";
}
impl CustomType for DurationMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(term) = factory.match_string_term(value) {
            match parse_duration(term.value().as_deref().as_str().deref()) {
                None => Err(format!("Invalid duration: {}", value)),
                Some((seconds, nanos)) => {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(seconds));
                    message.set_field_by_name("nanos", Value::I32(nanos));
                    Ok(message)
                }
            }
        } else {
            Err(format!("Expected String, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let seconds = get_message_field(message, "seconds")?;
        let nanos = get_message_field(message, "nanos")?;
        match (seconds, nanos) {
            (Value::I64(seconds), Value::I32(nanos)) => match format_duration(*seconds, *nanos) {
                None => Err(format!(
                    "Invalid Duration: {{ \"seconds\": {:?}, \"nanos\": {:?} }}",
                    seconds, nanos
                )),
                Some(value) => Ok(factory.create_string_term(allocator.create_string(value))),
            },
            _ => Err(format!(
                "Expected Duration, received {{ \"seconds\": {:?}, \"nanos\": {:?} }}",
                seconds, nanos
            )),
        }
    }
}

fn parse_duration(input: &str) -> Option<(i64, i32)> {
    self::parser::parse(input).ok()
}

fn format_duration(seconds: i64, nanos: i32) -> Option<String> {
    if nanos != 0 && ((nanos < 0) != (seconds < 0)) {
        None
    } else {
        Some(format!(
            "{}{}{}s",
            if seconds < 0 { "-" } else { "" },
            seconds.abs(),
            if nanos == 0 {
                String::new()
            } else {
                let mut nanos = nanos.abs();
                while nanos % 10 == 0 {
                    nanos /= 10;
                }
                format!(".{}", nanos)
            }
        ))
    }
}

mod parser {
    use crate::nom::map_err;
    use std::num::ParseIntError;

    use nom::{
        bytes::complete::tag,
        character::complete::digit1,
        combinator::{eof, map_res, opt},
        error::{ErrorKind, FromExternalError},
        sequence::tuple,
        Parser,
    };

    pub(super) fn parse(input: &str) -> Result<(i64, i32), nom::Err<DurationParserError<&str>>> {
        create_duration_parser()
            .parse(input)
            .map(|(_remaining, (seconds, nanos))| (seconds, nanos))
    }

    pub(super) enum DurationParserError<I> {
        Parse((I, ErrorKind)),
        ValidateSeconds(ParseIntError),
        ValidateNanos(ParseNanoDecimalsError),
    }
    impl<I> FromExternalError<I, DurationParserError<I>> for DurationParserError<I> {
        fn from_external_error(_input: I, _kind: ErrorKind, err: DurationParserError<I>) -> Self {
            err
        }
    }
    pub(super) fn create_duration_parser<'a>(
    ) -> impl Parser<&'a str, (i64, i32), DurationParserError<&'a str>> {
        let parse_decimal = tuple((
            opt(tag("-")).map(|negative| negative.is_some()),
            digit1.map(|digits| i64::from_str_radix(digits, 10)),
            opt(tuple((tag("."), digit1.map(parse_nano_decimals)))),
        ));
        let parse_unit = tag("s");
        let parse_duration = tuple((parse_decimal, parse_unit, eof));
        map_res(
            map_err(parse_duration, DurationParserError::Parse),
            |((is_negative, seconds, decimals), _unit, _eof)| -> Result<(i64, i32), DurationParserError<&'a str>> {
                let seconds = seconds.map_err(DurationParserError::ValidateSeconds)?;
                let nanos = match decimals {
                    None => Ok(0),
                    Some((_separator, digits)) => digits
                        .map_err(DurationParserError::ValidateNanos)
                }?;
                let (seconds, nanos) = if is_negative {
                    (-seconds, -nanos)
                } else {
                    (seconds, nanos)
                };
                Ok((seconds, nanos))
            },
        )
    }

    pub(super) enum ParseNanoDecimalsError {
        ParseInt(ParseIntError),
        Overflow,
    }
    fn parse_nano_decimals(digits: &str) -> Result<i32, ParseNanoDecimalsError> {
        let decimals = i32::from_str_radix(digits, 10).map_err(ParseNanoDecimalsError::ParseInt)?;
        let num_digits = digits.len();
        if num_digits > 9 {
            Err(ParseNanoDecimalsError::Overflow)
        } else if num_digits == 9 {
            Ok(decimals)
        } else {
            let exponent = 10_i32.pow(9 - digits.len() as u32);
            Ok(decimals * exponent)
        }
    }
}

#[cfg(test)]
mod tests {
    use prost_reflect::{prost_types::Duration, ReflectMessage, Value};
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    #[test]
    fn name() {
        assert_eq!(DurationMessage.name(), "google.protobuf.Duration");
    }

    #[test]
    fn serialize() {
        let serializer = DurationMessage;
        let message_type = Duration::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        {
            let value = factory.create_string_term(allocator.create_string("0s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(0));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("3s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(3));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("3.142s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(3));
                    message.set_field_by_name("nanos", Value::I32(142000000));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("3.141592654s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(3));
                    message.set_field_by_name("nanos", Value::I32(141592654));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("3.1415926536s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Err(String::from("Invalid duration: \"3.1415926536s\""))
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("-3s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(-3));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("-3.142s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(-3));
                    message.set_field_by_name("nanos", Value::I32(-142000000));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("-3.141592654s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(-3));
                    message.set_field_by_name("nanos", Value::I32(-141592654));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("-3.1415926536s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Err(String::from("Invalid duration: \"-3.1415926536s\"")),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string(" 3s"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(result, Err(String::from("Invalid duration: \" 3s\"")),);
        }
        {
            let value = factory.create_string_term(allocator.create_string("3s "));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(result, Err(String::from("Invalid duration: \"3s \"")),);
        }
    }

    #[test]
    fn deserialize() {
        let serializer = DurationMessage;
        let message_type = Duration::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(0));
                message.set_field_by_name("nanos", Value::I32(0));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("0s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(3));
                message.set_field_by_name("nanos", Value::I32(0));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("3s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(3));
                message.set_field_by_name("nanos", Value::I32(142000000));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("3.142s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(3));
                message.set_field_by_name("nanos", Value::I32(141592654));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("3.141592654s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(-3));
                message.set_field_by_name("nanos", Value::I32(0));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("-3s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(-3));
                message.set_field_by_name("nanos", Value::I32(-142000000));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("-3.142s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(-3));
                message.set_field_by_name("nanos", Value::I32(-141592654));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("-3.141592654s"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(-3));
                message.set_field_by_name("nanos", Value::I32(142000000));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Err(String::from(
                    "Invalid Duration: { \"seconds\": -3, \"nanos\": 142000000 }"
                )),
            );
        }
    }
}
