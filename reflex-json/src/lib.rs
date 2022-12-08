// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use serde_json::{Map, Value};

pub mod stdlib;

pub use serde_json::{json, Map as JsonMap, Number as JsonNumber, Value as JsonValue};

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
        .map(|(key, value)| {
            hydrate(value, factory, allocator).map(|value| {
                (
                    factory.create_string_term(allocator.create_string(key)),
                    value,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(create_record(entries, factory, allocator))
}

#[cfg(test)]
mod tests {
    use std::iter::empty;

    use crate::{hydrate, JsonValue};
    use reflex::core::{create_record, ExpressionFactory, HeapAllocator, SerializeJson};
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

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
                    (
                        factory.create_string_term(allocator.create_static_string("first")),
                        factory.create_int_term(3),
                    ),
                    (
                        factory.create_string_term(allocator.create_static_string("second")),
                        factory.create_int_term(4),
                    ),
                    (
                        factory.create_string_term(allocator.create_static_string("third")),
                        factory.create_int_term(5),
                    )
                ],
                &factory,
                &allocator
            )),
            Ok(String::from("{\"first\":3,\"second\":4,\"third\":5}")),
        );
        assert_eq!(
            stringify(&create_record(
                vec![(
                    factory.create_string_term(allocator.create_static_string("\"\'\n\r")),
                    factory.create_int_term(3),
                )],
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

    #[test]
    fn patch() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();

        let initial = factory.create_int_term(5);
        let updated = factory.create_int_term(5);
        let patch = initial.patch(&updated).unwrap();
        assert_eq!(patch, None);

        let initial = factory.create_int_term(5);
        let updated = factory.create_int_term(6);
        let patch = serde_json::to_string(&initial.patch(&updated).unwrap().unwrap()).unwrap();
        assert_eq!(patch, "6");

        let initial = make_term(r#"{"a":"b", "c": true, "b":[1,2,3]}"#, &factory, &allocator);
        let updated = make_term(
            r#"{"a":"c", "c": false, "b":[1,2,3,4]}"#,
            &factory,
            &allocator,
        );
        let patch = initial.patch(&updated).unwrap().unwrap();
        assert_eq!(
            patch,
            make_value(r#"{"a":"c", "c": false, "b":{"3":4,"length":4}}"#)
        );

        // error when keys mismatch
        let initial = make_term(r#"{"a":"b", "c":"d"}"#, &factory, &allocator);
        let updated = make_term(r#"{"a":"c"}"#, &factory, &allocator);
        let patch = initial.patch(&updated);
        assert!(patch.is_err());

        let initial = make_term(r#"{"a":"b"}"#, &factory, &allocator);
        let updated = make_term(r#"{"a":"c", "c":"d"}"#, &factory, &allocator);
        let patch = initial.patch(&updated);
        assert!(patch.is_err());

        // recurse through objects properly
        let initial = make_term(r#"{"a":{"b":"c"}}"#, &factory, &allocator);
        let updated = make_term(r#"{"a":{"b":"d"}}"#, &factory, &allocator);
        let patch = initial.patch(&updated).unwrap().unwrap();
        assert_eq!(patch, make_value(r#"{"a":{"b":"d"}}"#));

        // reordered object keys
        let initial = make_term(r#"{"a":1,"b":2,"c":3}"#, &factory, &allocator);
        let updated = make_term(r#"{"c":3,"a":4,"b":2}"#, &factory, &allocator);
        let patch = initial.patch(&updated).unwrap().unwrap();
        assert_eq!(patch, make_value(r#"{"a":4}"#));

        // added object keys
        let initial = make_term(r#"{"a":1,"b":2,"c":3}"#, &factory, &allocator);
        let updated = make_term(r#"{"a":1,"b":2,"c":3,"d":4}"#, &factory, &allocator);
        let patch = initial.patch(&updated);
        assert_eq!(
            patch,
            Err(String::from(
                "Prototype has changed from {\"a\",\"b\",\"c\"} to {\"a\",\"b\",\"c\",\"d\"}"
            ))
        );

        // removed object keys
        let initial = make_term(r#"{"a":1,"b":2,"c":3}"#, &factory, &allocator);
        let updated = make_term(r#"{"a":1,"b":2}"#, &factory, &allocator);
        let patch = initial.patch(&updated);
        assert_eq!(
            patch,
            Err(String::from(
                "Prototype has changed from {\"a\",\"b\",\"c\"} to {\"a\",\"b\"}"
            ))
        );

        // lists shrink
        let initial = make_term(r#"[1,2,3]"#, &factory, &allocator);
        let updated = make_term(r#"[1,2]"#, &factory, &allocator);
        let patch = initial.patch(&updated).unwrap().unwrap();
        assert_eq!(patch, make_value(r#"{"length":2}"#));

        // list changes
        let initial = make_term(r#"[1,2,3]"#, &factory, &allocator);
        let updated = make_term(r#"[1,2,4]"#, &factory, &allocator);
        let patch = initial.patch(&updated).unwrap().unwrap();
        assert_eq!(patch, make_value(r#"{"2":4}"#));
    }

    fn make_value(json: &str) -> JsonValue {
        serde_json::from_str::<JsonValue>(json).unwrap()
    }
    fn make_term(
        json: &str,
        factory: &SharedTermFactory<Stdlib>,
        allocator: &DefaultAllocator<CachedSharedTerm<Stdlib>>,
    ) -> CachedSharedTerm<Stdlib> {
        hydrate(make_value(json), factory, allocator).unwrap()
    }
}
