// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::empty};

use prost_reflect::{DynamicMessage, FieldDescriptor, Kind, MapKey, ReflectMessage, Value};
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};

use crate::{
    utils::{count_iterator_items, create_invalid_field_type_error_message, CountedIteratorItems},
    ProtoTranscoder, TranscodeError,
};

pub(crate) fn deserialize_generic_message<T: Expression>(
    message: &DynamicMessage,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    let message_type = message.descriptor();
    let simple_fields = message_type.fields().map(|field_type| {
        let field_value = if message.has_field(&field_type) {
            let value = message.get_field(&field_type);
            deserialize_field_value(&value, &field_type, transcoder, factory, allocator)
        } else {
            deserialize_missing_field_value(&field_type, message, transcoder, factory, allocator)
        }
        .map_err(|err| TranscodeError {
            error: err.error,
            message_type: err.message_type.or_else(|| Some(message_type.clone())),
            path: err.path.with_prefix(field_type.name().into()),
        })?;
        Ok((field_type, field_value))
    });
    let oneof_fields = message_type.oneofs().filter_map(|oneof_type| {
        deserialize_oneof_field(&oneof_type, message, transcoder, factory, allocator).transpose()
    });
    Ok(create_record(
        simple_fields
            .chain(oneof_fields)
            .map(|result| {
                result.map(|(field, value)| {
                    (
                        // TODO: cache protobuf schema field name strings
                        factory.create_string_term(allocator.create_string(field.json_name())),
                        value,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
        factory,
        allocator,
    ))
}

fn deserialize_oneof_field<T: Expression>(
    oneof_type: &prost_reflect::OneofDescriptor,
    message: &DynamicMessage,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<Option<(FieldDescriptor, T)>, TranscodeError> {
    let fields = oneof_type
        .fields()
        .filter_map(|field_type| {
            if !message.has_field(&field_type) {
                None
            } else {
                Some(
                    deserialize_field_value(
                        &message.get_field(&field_type),
                        &field_type,
                        transcoder,
                        factory,
                        allocator,
                    )
                    .map_err(|err| TranscodeError {
                        error: err.error,
                        message_type: err.message_type.or_else(|| Some(message.descriptor())),
                        path: err.path.with_prefix(field_type.name().into()),
                    })
                    .map(|value| (field_type, value)),
                )
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    match count_iterator_items(fields) {
        CountedIteratorItems::None => Ok(None),
        CountedIteratorItems::One((field_type, field_value)) => Ok(Some((field_type, field_value))),
        CountedIteratorItems::Many(fields) => Err(TranscodeError::from(format!(
            "Multiple oneof fields specified: {}",
            fields
                .map(|(field_type, _)| String::from(field_type.json_name()))
                .collect::<Vec<_>>()
                .join(", ")
        ))),
    }
}

fn deserialize_field_value<T: Expression>(
    value: &Value,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    match value {
        Value::Bool(value) => Ok(factory.create_boolean_term(*value)),
        // TODO: prevent lossy transcoding of gRPC integer types
        Value::I32(value) => Ok(factory.create_int_term(*value)),
        Value::I64(value) => Ok(factory.create_float_term(*value as f64)),
        Value::U32(value) => Ok(factory.create_int_term(*value as i32)),
        Value::U64(value) => Ok(factory.create_float_term(*value as f64)),
        Value::F32(value) => Ok(factory.create_float_term(*value as f64)),
        Value::F64(value) => Ok(factory.create_float_term(*value)),
        Value::String(value) => {
            Ok(factory.create_string_term(allocator.create_string(value.as_str())))
        }
        Value::Bytes(_) => Err(TranscodeError::from(
            create_invalid_field_type_error_message("bytes"),
        )),
        Value::EnumNumber(value) => {
            deserialize_enum_field_value(*value, field_type, factory, allocator)
        }
        Value::Message(value) => transcoder
            .deserialize_message(value, transcoder, factory, allocator)
            .map_err(|err| TranscodeError {
                error: err.error,
                message_type: err.message_type.or_else(|| Some(value.descriptor())),
                path: err.path,
            }),
        Value::List(value) => {
            deserialize_list_field_value(value, field_type, transcoder, factory, allocator)
        }
        Value::Map(value) => {
            deserialize_map_field_value(value, field_type, transcoder, factory, allocator)
        }
    }
}

fn deserialize_missing_field_value<T: Expression>(
    field_type: &FieldDescriptor,
    message: &DynamicMessage,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    if field_type.is_list() {
        Ok(factory.create_list_term(allocator.create_empty_list()))
    } else if field_type.is_map() {
        Ok(factory.create_hashmap_term(empty()))
    } else if matches!(field_type.kind(), Kind::Message(_)) {
        Ok(factory.create_nil_term())
    } else {
        let value = message.get_field(field_type);
        deserialize_field_value(&value, &field_type, transcoder, factory, allocator)
    }
}

fn deserialize_list_field_value<T: Expression>(
    value: &[Value],
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    Ok(factory.create_list_term(
        allocator.create_list(
            value
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    deserialize_field_value(value, field_type, transcoder, factory, allocator)
                        .map_err(|err| err.with_path_prefix(index.into()))
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
    ))
}

fn deserialize_map_field_value<T: Expression>(
    value: &HashMap<MapKey, Value>,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    let entries = value
        .iter()
        .map(|(key, value)| {
            match (
                deserialize_map_key(key, factory, allocator),
                deserialize_field_value(value, field_type, transcoder, factory, allocator),
            ) {
                (Ok(key), Ok(value)) => Ok((key, value)),
                (Err(key_err), _) => Err(key_err),
                (_, Err(value_err)) => Err(value_err),
            }
            .map_err(|err| err.with_path_prefix(format_map_key(key).into()))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(factory.create_hashmap_term(entries))
}

fn deserialize_map_key<T: Expression>(
    value: &MapKey,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    match value {
        MapKey::Bool(value) => Ok(factory.create_boolean_term(*value)),
        MapKey::I32(value) => Ok(factory.create_int_term(*value)),
        MapKey::String(value) => {
            Ok(factory.create_string_term(allocator.create_string(value.as_str())))
        }
        MapKey::U32(_) => Err("u32"),
        MapKey::I64(_) => Err("i64"),
        MapKey::U64(_) => Err("u64"),
    }
    .map_err(|value_type| TranscodeError::from(format!("Invalid map key type: {}", value_type)))
}

fn format_map_key(key: &MapKey) -> String {
    match key {
        MapKey::Bool(value) => format!("{}", value),
        MapKey::I32(value) => format!("{}", value),
        MapKey::I64(value) => format!("{}", value),
        MapKey::U32(value) => format!("{}", value),
        MapKey::U64(value) => format!("{}", value),
        MapKey::String(value) => format!("{}", value),
    }
}

fn deserialize_enum_field_value<T: Expression>(
    value: i32,
    field_type: &FieldDescriptor,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, TranscodeError> {
    match field_type.kind() {
        Kind::Enum(enum_type) => match enum_type.get_value(value) {
            Some(enum_variant) => {
                Ok(factory.create_string_term(allocator.create_string(enum_variant.name())))
            }
            None => Err(TranscodeError::from(format!(
                "Invalid {} enum variant: {}",
                enum_type.name(),
                value
            ))),
        },
        _ => Err(TranscodeError::from(format!("Unexpected enum value"))),
    }
}
