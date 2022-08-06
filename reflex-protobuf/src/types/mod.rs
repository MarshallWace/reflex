// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use prost_reflect::{DynamicMessage, MessageDescriptor, ReflectMessage};
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

use crate::{CustomType, GenericTranscoder, ProtoTranscoder, TranscodeError};

use self::{
    any::AnyMessage,
    duration::DurationMessage,
    empty::EmptyMessage,
    timestamp::TimestampMessage,
    wrappers::{
        BoolValueMessage, BytesValueMessage, DoubleValueMessage, FloatValueMessage,
        Int32ValueMessage, Int64ValueMessage, StringValueMessage, UInt32ValueMessage,
        UInt64ValueMessage,
    },
};

pub mod any;
pub mod duration;
pub mod empty;
pub mod timestamp;
pub mod wrappers;

#[derive(Clone, Copy, Debug)]
pub struct WellKnownTypesTranscoder;
impl ProtoTranscoder for WellKnownTypesTranscoder {
    fn serialize_message<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<DynamicMessage, TranscodeError> {
        match message_type.full_name() {
            AnyMessage::NAME => AnyMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            DurationMessage::NAME => DurationMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            EmptyMessage::NAME => EmptyMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            TimestampMessage::NAME => TimestampMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            DoubleValueMessage::NAME => DoubleValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            FloatValueMessage::NAME => FloatValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            Int64ValueMessage::NAME => Int64ValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            UInt64ValueMessage::NAME => UInt64ValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            Int32ValueMessage::NAME => Int32ValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            UInt32ValueMessage::NAME => UInt32ValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            BoolValueMessage::NAME => BoolValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            StringValueMessage::NAME => StringValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            BytesValueMessage::NAME => BytesValueMessage
                .serialize(value, message_type, factory)
                .map_err(TranscodeError::from),
            _ => GenericTranscoder
                .serialize_message(value, message_type, transcoder, factory, allocator)
                .map_err(TranscodeError::from),
        }
    }
    fn deserialize_message<T: Expression>(
        &self,
        message: &DynamicMessage,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, TranscodeError> {
        match message.descriptor().full_name() {
            AnyMessage::NAME => AnyMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            DurationMessage::NAME => DurationMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            EmptyMessage::NAME => EmptyMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            TimestampMessage::NAME => TimestampMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            DoubleValueMessage::NAME => DoubleValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            FloatValueMessage::NAME => FloatValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            Int64ValueMessage::NAME => Int64ValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            UInt64ValueMessage::NAME => UInt64ValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            Int32ValueMessage::NAME => Int32ValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            UInt32ValueMessage::NAME => UInt32ValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            BoolValueMessage::NAME => BoolValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            StringValueMessage::NAME => StringValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            BytesValueMessage::NAME => BytesValueMessage
                .deserialize(message, factory, allocator)
                .map_err(TranscodeError::from),
            _ => GenericTranscoder.deserialize_message(message, transcoder, factory, allocator),
        }
    }
}

#[cfg(test)]
mod tests {
    use bytes::Bytes;
    use prost_reflect::{ReflectMessage, Value};
    use reflex::core::create_record;
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use crate::ErrorPath;

    use super::*;

    mod generic_types {
        use crate::load_proto_library;

        use super::*;

        #[test]
        fn serialize() {
            let protos = load_proto_library(include_bytes!(concat!(
                env!("OUT_DIR"),
                "/grpc/mocks.proto.bin"
            )))
            .unwrap();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::default();
            let transcoder = WellKnownTypesTranscoder;
            let message_type = protos
                .as_inner()
                .get_message_by_name("hello_world.HelloWorld")
                .unwrap();
            let value = create_record(
                [(
                    factory.create_string_term(allocator.create_static_string("user")),
                    factory.create_string_term(allocator.create_static_string("foo")),
                )],
                &factory,
                &allocator,
            );
            let result = transcoder.serialize_message(
                &value,
                &message_type,
                &transcoder,
                &factory,
                &allocator,
            );
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("user", Value::String(String::from("foo")));
                    message
                })
            );
        }

        #[test]
        fn deserialize() {
            let protos = load_proto_library(include_bytes!(concat!(
                env!("OUT_DIR"),
                "/grpc/mocks.proto.bin"
            )))
            .unwrap();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::default();
            let transcoder = WellKnownTypesTranscoder;
            let message_type = protos
                .as_inner()
                .get_message_by_name("hello_world.HelloWorld")
                .unwrap();
            let message = {
                let mut message = DynamicMessage::new(message_type);
                message.set_field_by_name("user", Value::String(String::from("foo")));
                message
            };
            let result =
                transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
            assert_eq!(
                result,
                Ok(create_record(
                    [(
                        factory.create_string_term(allocator.create_static_string("user")),
                        factory.create_string_term(allocator.create_static_string("foo"))
                    )],
                    &factory,
                    &allocator
                )),
            );
        }
    }

    mod any {
        use prost_reflect::prost_types::Any;

        use super::*;

        #[test]
        fn serialize() {
            let message_type = Any::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::default();
            let transcoder = WellKnownTypesTranscoder;
            let value = create_record(
                [
                    (
                        factory.create_string_term(allocator.create_static_string("typeUrl")),
                        factory.create_string_term(
                            allocator.create_static_string("http://example.com/"),
                        ),
                    ),
                    (
                        factory.create_string_term(allocator.create_static_string("value")),
                        factory.create_string_term(allocator.create_static_string("foo")),
                    ),
                ],
                &factory,
                &allocator,
            );
            let result = transcoder.serialize_message(
                &value,
                &message_type,
                &transcoder,
                &factory,
                &allocator,
            );
            assert_eq!(
                result,
                Err(TranscodeError {
                    message_type: None,
                    error: String::from("Unsupported type: google.protobuf.Any"),
                    path: ErrorPath::default(),
                })
            );
        }

        #[test]
        fn deserialize() {
            let message_type = Any::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::default();
            let transcoder = WellKnownTypesTranscoder;
            let message = {
                let mut message = DynamicMessage::new(message_type);
                message.set_field_by_name(
                    "type_url",
                    Value::String(String::from("http://example.com/")),
                );
                message.set_field_by_name("value", Value::Bytes(Bytes::from_static(b"foo")));
                message
            };
            let result =
                transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
            assert_eq!(
                result,
                Err(TranscodeError {
                    message_type: None,
                    error: String::from("Unsupported type: google.protobuf.Any"),
                    path: ErrorPath::default(),
                }),
            );
        }
    }

    mod duration {
        use prost_reflect::prost_types::Duration;

        use super::*;

        #[test]
        fn serialize() {
            let message_type = Duration::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let value = factory.create_string_term(allocator.create_static_string("3.142s"));
            let result = transcoder.serialize_message(
                &value,
                &message_type,
                &transcoder,
                &factory,
                &allocator,
            );
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(3));
                    message.set_field_by_name("nanos", Value::I32(142000000));
                    message
                })
            );
        }

        #[test]
        fn deserialize() {
            let message_type = Duration::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(3));
                message.set_field_by_name("nanos", Value::I32(142000000));
                message
            };
            let result =
                transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory.create_string_term(allocator.create_static_string("3.142s"))),
            );
        }
    }

    mod empty {
        use super::*;

        #[test]
        fn serialize() {
            let message_type = ().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let value = create_record([], &factory, &allocator);
            let result = transcoder.serialize_message(
                &value,
                &message_type,
                &transcoder,
                &factory,
                &allocator,
            );
            assert_eq!(result, Ok(DynamicMessage::new(message_type.clone())));
        }

        #[test]
        fn deserialize() {
            let message_type = ().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let message = DynamicMessage::new(message_type.clone());
            let result =
                transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
            assert_eq!(result, Ok(create_record([], &factory, &allocator)));
        }
    }

    mod timestamp {
        use prost_reflect::prost_types::Timestamp;

        use super::*;

        #[test]
        fn serialize() {
            let message_type = Timestamp::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let value =
                factory.create_string_term(allocator.create_static_string("2000-01-01T00:00:00Z"));
            let result = transcoder.serialize_message(
                &value,
                &message_type,
                &transcoder,
                &factory,
                &allocator,
            );
            assert_eq!(
                result,
                Ok({
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("seconds", Value::I64(946684800));
                    message.set_field_by_name("nanos", Value::I32(0));
                    message
                })
            );
        }

        #[test]
        fn deserialize() {
            let message_type = Timestamp::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let transcoder = WellKnownTypesTranscoder;
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("seconds", Value::I64(946684800));
                message.set_field_by_name("nanos", Value::I32(0));
                message
            };
            let result =
                transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
            assert_eq!(
                result,
                Ok(factory
                    .create_string_term(allocator.create_static_string("2000-01-01T00:00:00Z"))),
            );
        }
    }

    mod wrappers {
        use super::*;

        mod double_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = f64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_float_term(3.142);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::F64(3.142));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = f64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::F64(3.142));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_float_term(3.142)));
            }
        }

        mod float_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = f32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_float_term(3.142_f32 as f64);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::F32(3.142));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = f32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::F32(3.142));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_float_term(3.142_f32 as f64)));
            }
        }

        mod int64_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = i64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_int_term(3);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I64(3));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = i64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I64(3));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
        }

        mod uint64_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = u64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_int_term(3);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U64(3));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = u64::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U64(3));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
        }

        mod int32_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = i32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_int_term(3);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I32(3));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = i32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I32(3));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
        }

        mod uint32_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = u32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_int_term(3);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U32(3));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = u32::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U32(3));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
        }

        mod bool_value {
            use super::*;

            #[test]
            fn serialize() {
                let message_type = bool::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::default();
                let transcoder = WellKnownTypesTranscoder;
                let value = factory.create_boolean_term(true);
                let result = transcoder.serialize_message(
                    &value,
                    &message_type,
                    &transcoder,
                    &factory,
                    &allocator,
                );
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::Bool(true));
                        message
                    })
                );
            }

            #[test]
            fn deserialize() {
                let message_type = bool::default().descriptor();
                let factory = SharedTermFactory::<Stdlib>::default();
                let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
                let transcoder = WellKnownTypesTranscoder;
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::Bool(true));
                    message
                };
                let result =
                    transcoder.deserialize_message(&message, &transcoder, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_boolean_term(true)));
            }
        }
    }
}
