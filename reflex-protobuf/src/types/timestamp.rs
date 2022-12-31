// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use chrono::{DateTime, NaiveDateTime, SecondsFormat, Utc};
use prost_reflect::{DynamicMessage, MessageDescriptor, Value};
use reflex::core::{
    Expression, ExpressionFactory, HeapAllocator, RefType, StringTermType, StringValue,
};

use crate::{utils::get_message_field, CustomType};

pub struct TimestampMessage;
impl TimestampMessage {
    pub const NAME: &'static str = "google.protobuf.Timestamp";
}
impl CustomType for TimestampMessage {
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
            match parse_timestamp(term.value().as_deref().as_str().deref()) {
                None => Err(format!("Invalid timestamp: {}", value)),
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
            (Value::I64(seconds), Value::I32(nanos)) => match format_timestamp(*seconds, *nanos) {
                Some(timestamp) => {
                    Ok(factory.create_string_term(allocator.create_string(timestamp)))
                }
                None => Err(format!(
                    "Invalid Timestamp, received {{ \"seconds\": {:?}, \"nanos\": {:?} }}",
                    seconds, nanos
                )),
            },
            _ => Err(format!(
                "Expected Timestamp, received {{ \"seconds\": {:?}, \"nanos\": {:?} }}",
                seconds, nanos
            )),
        }
    }
}

fn parse_timestamp(value: &str) -> Option<(i64, i32)> {
    DateTime::parse_from_rfc3339(value)
        .ok()
        .map(|date| (date.timestamp(), date.timestamp_subsec_nanos() as i32))
}

fn format_timestamp(seconds: i64, nanos: i32) -> Option<String> {
    Some(
        DateTime::<Utc>::from_utc(
            NaiveDateTime::from_timestamp_opt(seconds, nanos as u32)?,
            Utc,
        )
        .to_rfc3339_opts(SecondsFormat::AutoSi, true),
    )
}

#[cfg(test)]
mod tests {
    use prost_reflect::{prost_types::Timestamp, ReflectMessage, Value};
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    #[test]
    fn name() {
        assert_eq!(TimestampMessage.name(), "google.protobuf.Timestamp");
    }

    #[test]
    fn serialize() {
        let serializer = TimestampMessage;
        let message_type = Timestamp::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        {
            let value = factory.create_string_term(allocator.create_string("2000-01-01T00:00:00Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value =
                factory.create_string_term(allocator.create_string("2000-01-01T00:00:00.000Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value =
                factory.create_string_term(allocator.create_string("2000-01-01T00:00:00.000000Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value = factory
                .create_string_term(allocator.create_string("2000-01-01T00:00:00.000000000Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                }),
            );
        }
        {
            let value =
                factory.create_string_term(allocator.create_string("2000-01-01T00:00:00.999Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(999000000));
                    message
                }),
            );
        }
        {
            let value =
                factory.create_string_term(allocator.create_string("2000-01-01T00:00:00.999999Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(999999000));
                    message
                }),
            );
        }
        {
            let value = factory
                .create_string_term(allocator.create_string("2000-01-01T00:00:00.999999999Z"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(999999999));
                    message
                }),
            );
        }
        {
            let value = factory.create_string_term(allocator.create_string("2000-01-01T00:00:00"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Err(String::from("Invalid timestamp: \"2000-01-01T00:00:00\"")),
            );
        }
    }

    #[test]
    fn deserialize() {
        let serializer = TimestampMessage;
        let message_type = Timestamp::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(946684800));
                message.set_field_by_name("nanos", Value::I32(0));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("2000-01-01T00:00:00Z"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(946684800));
                message.set_field_by_name("nanos", Value::I32(999000000));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_string("2000-01-01T00:00:00.999Z"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(946684800));
                message.set_field_by_name("nanos", Value::I32(999999000));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory
                    .create_string_term(allocator.create_string("2000-01-01T00:00:00.999999Z"))),
            );
        }
        {
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(946684800));
                message.set_field_by_name("nanos", Value::I32(999999999));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory
                    .create_string_term(allocator.create_string("2000-01-01T00:00:00.999999999Z"))),
            );
        }
    }
}
