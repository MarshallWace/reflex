// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{borrow::Cow, time::Duration};

use crc::{Crc, CRC_32_ISCSI};
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, StringValue},
    lang::{create_struct, is_integer, term::StructTerm},
};

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct GrpcServiceId(pub(super) i32);
impl GrpcServiceId {
    pub fn as_i32(&self) -> i32 {
        let Self(value) = self;
        *value
    }
}
impl std::fmt::Display for GrpcServiceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self)
    }
}
impl std::fmt::LowerHex for GrpcServiceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:08x}", self.0 as u32)
    }
}

pub const fn get_grpc_schema_checksum(input: &[u8]) -> GrpcServiceId {
    GrpcServiceId(i32::from_ne_bytes(
        Crc::<u32>::new(&CRC_32_ISCSI).checksum(input).to_ne_bytes(),
    ))
}

pub fn create_null<T: Expression>(factory: &impl ExpressionFactory<T>) -> T {
    factory.create_nil_term()
}

pub fn create_integer<T: Expression>(value: i32, factory: &impl ExpressionFactory<T>) -> T {
    factory.create_int_term(value)
}

pub fn create_optional_integer<T: Expression>(
    value: i32,
    factory: &impl ExpressionFactory<T>,
) -> T {
    create_optional(
        |value| create_integer(value, factory),
        match value {
            0 => None,
            _ => Some(value),
        },
        factory,
    )
}

pub fn create_boolean<T: Expression>(value: bool, factory: &impl ExpressionFactory<T>) -> T {
    factory.create_boolean_term(value)
}

pub fn create_float<T: Expression>(value: f64, factory: &impl ExpressionFactory<T>) -> T {
    factory.create_float_term(value)
}

pub fn create_string<T: Expression>(
    value: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_string_term(allocator.create_string(value))
}

pub fn create_byte_string<T: Expression>(
    value: Vec<u8>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_string(
        match String::from_utf8_lossy(&value) {
            Cow::Borrowed(value) => String::from(value),
            Cow::Owned(value) => value,
        },
        factory,
        allocator,
    )
}

pub fn create_object<'a, T: Expression>(
    items: impl IntoIterator<Item = (&'a str, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        items
            .into_iter()
            .map(|(key, value)| (String::from(key), value)),
        factory,
        allocator,
    )
}

pub fn create_duration<T: Expression>(
    value: Duration,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_object(
        [
            ("seconds", factory.create_int_term(value.as_secs() as i32)),
            (
                "nanos",
                factory.create_int_term(value.subsec_nanos() as i32),
            ),
        ],
        factory,
        allocator,
    )
}

pub fn create_optional<V, T: Expression>(
    value_factory: impl Fn(V) -> T,
    value: Option<V>,
    factory: &impl ExpressionFactory<T>,
) -> T {
    match value {
        Some(value) => value_factory(value),
        None => create_null(factory),
    }
}

pub fn create_list<V, T: Expression>(
    item_factory: impl Fn(V) -> T,
    items: impl IntoIterator<Item = V, IntoIter = impl ExactSizeIterator<Item = V>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_vector_term(allocator.create_list(items.into_iter().map(item_factory)))
}

pub fn create_oneof<'a, V, T: Expression>(
    variant_factory: impl Fn(V) -> (i32, T),
    value: V,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let (enum_case, value) = variant_factory(value);
    create_struct(
        vec![
            (String::from("case"), create_integer(enum_case, factory)),
            (String::from("value"), value),
        ],
        factory,
        allocator,
    )
}

pub fn get_serialized_object_field<'a, T: Expression>(
    value: &'a StructTerm<T>,
    key: &str,
    factory: &impl ExpressionFactory<T>,
) -> Result<&'a T, String> {
    get_optional_serialized_object_field(value, key, factory)
        .ok_or_else(|| format!("Field not found: {}", key))
}

pub fn get_optional_serialized_object_field<'a, T: Expression>(
    value: &'a StructTerm<T>,
    key: &str,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T> {
    value.get(key).and_then(|value| {
        if let Some(_) = factory.match_nil_term(value) {
            None
        } else {
            Some(value)
        }
    })
}

pub fn parse_boolean<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<bool, String> {
    match factory.match_boolean_term(value) {
        Some(term) => Ok(term.value),
        _ => Err(format!("Expected Boolean, received {}", value)),
    }
}

pub fn parse_optional_boolean<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<bool, String> {
    Ok(parse_optional(|value| parse_boolean(value, factory), value)?.unwrap_or(false))
}

pub fn parse_integer<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<i32, String> {
    match factory.match_int_term(value) {
        Some(term) => Ok(term.value),
        _ => match factory.match_float_term(value) {
            Some(term) if is_integer(term.value) => Ok(term.value as i32),
            _ => Err(format!("Expected Int, received {}", value)),
        },
    }
}

pub fn parse_optional_integer<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<i32, String> {
    Ok(parse_optional(|value| parse_integer(value, factory), value)?.unwrap_or(0))
}

pub fn parse_float<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<f64, String> {
    match factory.match_float_term(value) {
        Some(term) => Ok(term.value),
        _ => match factory.match_int_term(value) {
            Some(term) => Ok(term.value as f64),
            _ => Err(format!("Expected Float, received {}", value)),
        },
    }
}

pub fn parse_optional_float<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<f64, String> {
    Ok(parse_optional(|value| parse_float(value, factory), value)?.unwrap_or(0.0))
}

pub fn parse_string<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<String, String> {
    match factory.match_string_term(value) {
        Some(term) => Ok(String::from(term.value.as_str())),
        _ => Err(format!("Expected String, received {}", value)),
    }
}

pub fn parse_optional_string<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<String, String> {
    Ok(parse_optional(|value| parse_string(value, factory), value)?.unwrap_or(String::new()))
}

pub fn parse_enum<V: Into<i32> + Default, T: Expression>(
    parse: impl Fn(&str) -> Result<V, String>,
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<V, String> {
    match factory.match_string_term(value) {
        Some(term) => parse(term.value.as_str()),
        _ => Err(format!("Expected enum value, received {}", value)),
    }
}

pub fn parse_optional_enum<V: Into<i32> + Default, T: Expression>(
    parse: impl Fn(&T) -> Result<V, String>,
    value: Option<&T>,
) -> Result<V, String> {
    Ok(parse_optional(parse, value)?.unwrap_or(Default::default()))
}

pub fn parse_bytes<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Vec<u8>, String> {
    let value = parse_string(value, factory)?;
    Ok(value.as_bytes().to_vec())
}

pub fn parse_optional_bytes<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Vec<u8>, String> {
    Ok(parse_optional(|value| parse_bytes(value, factory), value)?.unwrap_or(Vec::new()))
}

pub fn parse_duration<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Duration, String> {
    match factory.match_struct_term(value).and_then(|value| {
        value.get("seconds").and_then(|seconds| {
            parse_integer(seconds, factory).ok().and_then(|seconds| {
                value.get("nanos").and_then(|nanos| {
                    parse_integer(nanos, factory)
                        .ok()
                        .map(|nanos| (seconds, nanos))
                })
            })
        })
    }) {
        Some((seconds, nanos)) => Ok(Duration::from_nanos(
            seconds as u64 * 1000000000 + nanos as u64,
        )),
        None => Err(format!(
            "Expected {{ seconds: Int, nanos: Int }}, received {}",
            value
        )),
    }
}

pub fn parse_optional_duration<T: Expression>(
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<Duration>, String> {
    Ok(
        parse_optional(|value| parse_duration(value, factory), value)?
            .map(Some)
            .unwrap_or(None),
    )
}

pub fn parse_object<V, T: Expression>(
    parse: impl Fn(&StructTerm<T>) -> Result<V, String>,
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<V, String> {
    match factory.match_struct_term(value) {
        Some(value) => parse(value),
        _ => Err(format!("Expected object value, received {}", value)),
    }
}

pub fn parse_optional<V, T: Expression>(
    parse: impl Fn(&T) -> Result<V, String>,
    value: Option<&T>,
) -> Result<Option<V>, String> {
    match value {
        Some(value) => Ok(Some(parse(value)?)),
        None => Ok(None),
    }
}

pub fn parse_repeated<V, T: Expression>(
    parse: impl Fn(&T) -> Result<V, String>,
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Vec<V>, String> {
    match factory.match_vector_term(value) {
        Some(value) => value
            .items()
            .iter()
            .map(parse)
            .collect::<Result<Vec<_>, _>>(),
        _ => Err(format!("Expected <list>, received {}", value)),
    }
}

pub fn parse_optional_repeated<V, T: Expression>(
    parse: impl Fn(&T) -> Result<V, String>,
    value: Option<&T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Vec<V>, String> {
    match value {
        Some(value) => parse_repeated(parse, value, factory),
        None => Ok(Vec::new()),
    }
}

pub fn parse_oneof<V, T: Expression>(
    parse: impl Fn(i32, &T) -> Result<V, String>,
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<V, String> {
    parse_object(
        |value| {
            let enum_case = value
                .get("case")
                .ok_or_else(|| String::from("Missing oneof type discriminant"))
                .and_then(|value| parse_integer(value, factory))?;
            let enum_value = value
                .get("value")
                .ok_or_else(|| String::from("Missing oneof value"))?;
            parse(enum_case, enum_value)
        },
        value,
        factory,
    )
}
