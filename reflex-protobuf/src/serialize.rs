// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashMap;

use prost_reflect::{
    DynamicMessage, EnumDescriptor, FieldDescriptor, Kind, MapKey, MessageDescriptor,
    OneofDescriptor, Value,
};
use reflex::{
    core::{Expression, ExpressionFactory, StringValue},
    lang::{
        as_integer,
        term::{FloatValue, IntValue, RecordTerm},
    },
};

use crate::{
    utils::{count_iterator_items, create_invalid_field_type_error_message, CountedIteratorItems},
    ProtoTranscoder, TranscodeError,
};

pub(crate) fn serialize_generic_message<T: Expression>(
    value: &T,
    message_type: &MessageDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<DynamicMessage, TranscodeError> {
    let term = factory
        .match_record_term(value)
        .ok_or_else(|| TranscodeError {
            message_type: Some(message_type.clone()),
            error: format!("Expected <struct>, received {}", value,),
            path: Default::default(),
        })?;
    let simple_fields = message_type
        .fields()
        .filter_map(|field_type| {
            term.get(field_type.json_name())
                .map(|value| (field_type, value))
        })
        .map(|(field_type, value)| {
            let field_value = serialize_field_value(value, &field_type, transcoder, factory)
                .map_err(|err| TranscodeError {
                    error: err.error,
                    message_type: err.message_type.or_else(|| Some(message_type.clone())),
                    path: err.path.with_prefix(field_type.name().into()),
                })?;
            Ok((field_type, field_value))
        });
    let oneof_fields = message_type.oneofs().filter_map(|oneof_type| {
        serialize_oneof_field(term, &oneof_type, message_type, transcoder, factory).transpose()
    });
    simple_fields.chain(oneof_fields).fold(
        Ok(DynamicMessage::new(message_type.clone())),
        |result, field| {
            let mut dynamic_message = result?;
            let (field_type, field_value) = field?;
            dynamic_message.set_field(&field_type, field_value);
            Ok(dynamic_message)
        },
    )
}

fn serialize_oneof_field<T: Expression>(
    term: &RecordTerm<T>,
    oneof_type: &OneofDescriptor,
    message_type: &MessageDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<(FieldDescriptor, Value)>, TranscodeError> {
    let fields = oneof_type
        .fields()
        .filter_map(|field_type| {
            term.get(field_type.json_name()).and_then(|field_value| {
                if let Some(_) = factory.match_nil_term(field_value) {
                    None
                } else {
                    Some(
                        serialize_field_value(field_value, &field_type, transcoder, factory)
                            .map_err(|err| TranscodeError {
                                error: err.error,
                                message_type: err
                                    .message_type
                                    .or_else(|| Some(message_type.clone())),
                                path: err.path.with_prefix(field_type.name().into()),
                            })
                            .map(|value| (field_type, value)),
                    )
                }
            })
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

fn serialize_field_value<T: Expression>(
    value: &T,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<Value, TranscodeError> {
    if field_type.is_list() {
        serialize_list_field_value(value, field_type, transcoder, factory)
    } else if field_type.is_map() {
        serialize_map_field_value(value, field_type, transcoder, factory)
    } else {
        serialize_simple_field_value(value, field_type, transcoder, factory)
    }
}

fn serialize_list_field_value<T: Expression>(
    value: &T,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<Value, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Value::List(Default::default()))
    } else if let Some(term) = factory.match_list_term(value) {
        Ok(Value::List(
            term.items()
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    serialize_simple_field_value(value, field_type, transcoder, factory)
                        .map_err(|err| err.with_path_prefix(index.into()))
                })
                .collect::<Result<Vec<_>, _>>()?,
        ))
    } else {
        Err(TranscodeError::from(format!(
            "Expected <vector>, received {}",
            value
        )))
    }
}

fn serialize_map_field_value<T: Expression>(
    value: &T,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<Value, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Value::Map(Default::default()))
    } else if let Some(term) = factory.match_record_term(value) {
        Ok(Value::Map(
            term.entries()
                .into_iter()
                .map(|(key, value)| {
                    let field_value =
                        serialize_simple_field_value(value, field_type, transcoder, factory)
                            .map_err(|err| err.with_path_prefix(key.as_str().into()))?;
                    Ok((MapKey::String(String::from(key)), field_value))
                })
                .collect::<Result<HashMap<_, _>, TranscodeError>>()?,
        ))
    } else if let Some(term) = factory.match_hashmap_term(value) {
        Ok(Value::Map(
            term.keys()
                .iter()
                .zip(term.values().iter())
                .map(|(key, value)| {
                    serialize_map_key(key, factory).and_then(|key| {
                        match serialize_simple_field_value(value, field_type, transcoder, factory) {
                            Ok(value) => Ok((key, value)),
                            Err(err) => Err(err.with_path_prefix(key.into())),
                        }
                    })
                })
                .collect::<Result<HashMap<_, _>, _>>()?,
        ))
    } else {
        Err(TranscodeError::from(format!(
            "Expected <struct> or Map, received {}",
            value
        )))
    }
}

fn serialize_map_key<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<MapKey, TranscodeError> {
    if let Some(term) = factory.match_string_term(value) {
        Ok(MapKey::String(String::from(term.value.as_str())))
    } else if let Some(term) = factory.match_int_term(value) {
        Ok(MapKey::I32(term.value))
    } else if let Some(term) = factory.match_boolean_term(value) {
        Ok(MapKey::Bool(term.value))
    } else {
        Err(TranscodeError::from(format!("Invalid map key: {}", value)))
    }
}

fn serialize_simple_field_value<T: Expression>(
    value: &T,
    field_type: &FieldDescriptor,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
) -> Result<Value, TranscodeError> {
    match field_type.kind() {
        Kind::Int32 | Kind::Sint32 | Kind::Sfixed32 => {
            parse_integer_value(value, factory).map(Value::I32)
        }
        Kind::Int64 | Kind::Sint64 | Kind::Sfixed64 => parse_integer_value(value, factory)
            .map(|value| value as i64)
            .map(Value::I64),
        Kind::Uint32 | Kind::Fixed32 => parse_integer_value(value, factory)
            .map(|value| value as u32)
            .map(Value::U32),
        Kind::Uint64 | Kind::Fixed64 => parse_integer_value(value, factory)
            .map(|value| value as u64)
            .map(Value::U64),
        Kind::Float => parse_float_value(value, factory)
            .map(|value| value as f32)
            .map(Value::F32),
        Kind::Double => parse_float_value(value, factory).map(Value::F64),
        Kind::Bool => parse_boolean_value(value, factory).map(Value::Bool),
        Kind::String => parse_string_value(value, factory).map(Value::String),
        Kind::Enum(enum_type) => serialize_enum_field_value(value, &enum_type, factory),
        Kind::Message(message_type) => {
            if factory.match_nil_term(value).is_some() {
                Ok(Value::Message(DynamicMessage::new(message_type)))
            } else {
                transcoder
                    .serialize_message(value, &message_type, transcoder, factory)
                    .map(Value::Message)
            }
        }
        // TODO: allow encoding byte-array fields
        Kind::Bytes => Err(TranscodeError::from(
            create_invalid_field_type_error_message("bytes"),
        )),
    }
}

fn serialize_enum_field_value<T: Expression>(
    value: &T,
    enum_type: &EnumDescriptor,
    factory: &impl ExpressionFactory<T>,
) -> Result<Value, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Value::EnumNumber(enum_type.default_value().number()))
    } else if let Some(term) = factory.match_string_term(value) {
        enum_type
            .get_value_by_name(term.value.as_str())
            .map(|value| Value::EnumNumber(value.number()))
            .ok_or_else(|| {
                TranscodeError::from(format!("Invalid {} variant: {}", enum_type.name(), value))
            })
    } else {
        Err(TranscodeError::from(format!(
            "Expected {} variant, received {}",
            enum_type.name(),
            value
        )))
    }
}

fn parse_boolean_value<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<bool, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Default::default())
    } else if let Some(value) = factory.match_boolean_term(value) {
        Ok(value.value)
    } else {
        Err(TranscodeError::from(format!(
            "Expected Boolean, received {}",
            value
        )))
    }
}

fn parse_integer_value<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<IntValue, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Default::default())
    } else if let Some(value) = factory.match_float_term(value) {
        as_integer(value.value)
            .ok_or_else(|| TranscodeError::from(format!("Expected Int, received {}", value)))
    } else if let Some(value) = factory.match_int_term(value) {
        Ok(value.value)
    } else {
        Err(TranscodeError::from(format!(
            "Expected Int, received {}",
            value
        )))
    }
}

fn parse_float_value<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<FloatValue, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Default::default())
    } else if let Some(value) = factory.match_float_term(value) {
        Ok(value.value)
    } else if let Some(value) = factory.match_int_term(value) {
        Ok(value.value as f64)
    } else {
        Err(TranscodeError::from(format!(
            "Expected Float, received {}",
            value
        )))
    }
}

fn parse_string_value<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<String, TranscodeError> {
    if let Some(_) = factory.match_nil_term(value) {
        Ok(Default::default())
    } else if let Some(value) = factory.match_string_term(value) {
        Ok(String::from(value.value.as_str()))
    } else {
        Err(TranscodeError::from(format!(
            "Expected String, received {}",
            value
        )))
    }
}
