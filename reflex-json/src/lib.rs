// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::FromIterator;

use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use serde_json::{Map, Value};

pub mod stdlib;

pub use serde_json::{json, Map as JsonMap, Number as JsonNumber, Value as JsonValue};

pub fn json_object(properties: impl IntoIterator<Item = (String, JsonValue)>) -> JsonValue {
    JsonValue::Object(JsonMap::from_iter(properties))
}
pub fn json_array(items: impl IntoIterator<Item = JsonValue>) -> JsonValue {
    JsonValue::Array(items.into_iter().collect())
}

pub fn parse<T: Expression>(
    input: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    deserialize(input).and_then(|value| hydrate(value, factory, allocator))
}

pub fn stringify<'a, T: Expression>(value: &T) -> Result<String, String> {
    value
        .to_json()
        .and_then(|value| serde_json::to_string(&value).map_err(|err| format!("{}", err)))
        .map_err(|err| format!("JSON serialization failed: {}", err))
}

pub fn deserialize(value: &str) -> Result<JsonValue, String> {
    serde_json::from_str(value).map_err(|err| format!("JSON deserialization failed: {}", err))
}

pub fn sanitize<'a, T: Expression>(value: &T) -> Result<JsonValue, String> {
    value.to_json()
}

pub fn hydrate<T: Expression>(
    value: Value,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    match value {
        Value::Null => Ok(factory.create_nil_term()),
        Value::Bool(value) => Ok(factory.create_boolean_term(value)),
        Value::String(value) => Ok(factory.create_string_term(allocator.create_string(value))),
        Value::Number(value) => match value.as_i64() {
            Some(value) => Ok(factory.create_int_term(value as i32)),
            None => match value.as_f64() {
                Some(value) => Ok(factory.create_float_term(value)),
                None => Err(format!(
                    "JSON deserialization encountered invalid number: {}",
                    value
                )),
            },
        },
        Value::Array(value) => hydrate_array(value, factory, allocator),
        Value::Object(value) => hydrate_object(value, factory, allocator),
    }
}

fn hydrate_array<T: Expression>(
    value: Vec<Value>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    let items = value
        .into_iter()
        .map(|item| hydrate(item, factory, allocator))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(factory.create_list_term(allocator.create_list(items)))
}

fn hydrate_object<T: Expression>(
    value: Map<String, Value>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    let entries = value
        .into_iter()
        .map(|(key, value)| hydrate(value, factory, allocator).map(|value| (key, value)))
        .collect::<Result<Vec<_>, _>>()?;
    let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
    Ok(factory.create_record_term(
        allocator.create_struct_prototype(keys),
        allocator.create_list(values),
    ))
}

#[cfg(test)]
mod tests {
    use std::iter::empty;

    use reflex::{
        allocator::DefaultAllocator,
        core::{ExpressionFactory, HeapAllocator},
        lang::{create_record, SharedTermFactory},
        stdlib::Stdlib,
    };

    use super::{parse, stringify};

    #[test]
    fn stringify_primitives() {
        let factory = SharedTermFactory::<Stdlib>::default();
        assert_eq!(
            stringify(&factory.create_symbol_term(3)),
            Err(String::from(
                "JSON serialization failed: Unable to serialize term: <symbol:0x0000000000000003>"
            )),
        );
        assert_eq!(
            stringify(&factory.create_nil_term()),
            Ok(String::from("null")),
        );
        assert_eq!(
            stringify(&factory.create_boolean_term(false)),
            Ok(String::from("false")),
        );
        assert_eq!(
            stringify(&factory.create_boolean_term(true)),
            Ok(String::from("true")),
        );
        assert_eq!(
            stringify(&factory.create_int_term(3)),
            Ok(String::from("3")),
        );
        assert_eq!(
            stringify(&factory.create_int_term(0)),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(&factory.create_int_term(-0)),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(&factory.create_int_term(-3)),
            Ok(String::from("-3")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(3.142)),
            Ok(String::from("3.142")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(3.0)),
            Ok(String::from("3.0")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(0.0)),
            Ok(String::from("0.0")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(-0.0)),
            Ok(String::from("-0.0")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(-3.0)),
            Ok(String::from("-3.0")),
        );
        assert_eq!(
            stringify(&factory.create_float_term(-3.142)),
            Ok(String::from("-3.142")),
        );
        assert_eq!(
            stringify(&factory.create_string_term(String::from(""))),
            Ok(String::from("\"\"")),
        );
        assert_eq!(
            stringify(&factory.create_string_term(String::from("foo"))),
            Ok(String::from("\"foo\"")),
        );
        assert_eq!(
            stringify(&factory.create_string_term(String::from("\"\'\n\r"))),
            Ok(String::from("\"\\\"\'\\n\\r\"")),
        );
    }

    #[test]
    fn stringify_lists() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        assert_eq!(
            stringify(&factory.create_list_term(allocator.create_empty_list())),
            Ok(String::from("[]"))
        );
        assert_eq!(
            stringify(&factory.create_list_term(allocator.create_list(vec![
                factory.create_int_term(3),
                factory.create_int_term(4),
                factory.create_int_term(5),
            ]))),
            Ok(String::from("[3,4,5]")),
        );
    }

    #[test]
    fn stringify_objects() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        assert_eq!(
            stringify(&create_record(empty(), &factory, &allocator)),
            Ok(String::from("{}"))
        );
        assert_eq!(
            stringify(&create_record(
                vec![
                    (String::from("first"), factory.create_int_term(3),),
                    (String::from("second"), factory.create_int_term(4),),
                    (String::from("third"), factory.create_int_term(5),)
                ],
                &factory,
                &allocator
            )),
            Ok(String::from("{\"first\":3,\"second\":4,\"third\":5}")),
        );
        assert_eq!(
            stringify(&create_record(
                vec![(String::from("\"\'\n\r"), factory.create_int_term(3),)],
                &factory,
                &allocator,
            )),
            Ok(String::from("{\"\\\"\'\\n\\r\":3}")),
        );
    }

    #[test]
    fn parse_numbers() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        assert_eq!(
            parse("3", &factory, &allocator),
            Ok(factory.create_int_term(3)),
        );
        assert_eq!(
            parse("0", &factory, &allocator),
            Ok(factory.create_int_term(0)),
        );
        assert_eq!(
            parse("-0", &factory, &allocator),
            Ok(factory.create_float_term(-0.0)),
        );
        assert_eq!(
            parse("-3", &factory, &allocator),
            Ok(factory.create_int_term(-3)),
        );
        assert_eq!(
            parse("3.142", &factory, &allocator),
            Ok(factory.create_float_term(3.142)),
        );
        assert_eq!(
            parse("3.0", &factory, &allocator),
            Ok(factory.create_float_term(3.0)),
        );
        assert_eq!(
            parse("0.0", &factory, &allocator),
            Ok(factory.create_float_term(0.0)),
        );
        assert_eq!(
            parse("-0.0", &factory, &allocator),
            Ok(factory.create_float_term(-0.0)),
        );
        assert_eq!(
            parse("-3.0", &factory, &allocator),
            Ok(factory.create_float_term(-3.0)),
        );
        assert_eq!(
            parse("-3.142", &factory, &allocator),
            Ok(factory.create_float_term(-3.142)),
        );
    }
}
