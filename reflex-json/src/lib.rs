// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::FromIterator;

use reflex::{
    core::{Expression, SerializedListTerm, SerializedObjectTerm, SerializedTerm},
    stdlib::value::ValueTerm,
};
use serde_json::{Map, Value};

pub fn parse(value: &str) -> Result<Expression, String> {
    match serde_json::from_str::<Value>(value) {
        Err(error) => Err(format!("JSON deserialization failed: {}", error)),
        Ok(value) => deserialize(&value).map(|result| result.deserialize()),
    }
}

pub fn serialized(value: &str) -> Result<SerializedTerm, String> {
    match serde_json::from_str::<Value>(value) {
        Err(error) => Err(format!("JSON parse failed: {}", error)),
        Ok(value) => parse_value(value),
    }
}

pub fn stringify(value: SerializedTerm) -> Result<String, String> {
    match sanitize_term(value) {
        Err(error) => Err(error),
        Ok(value) => match serde_json::to_string(&value) {
            Err(error) => Err(format!("JSON serialization failed: {}", error)),
            Ok(value) => Ok(value),
        },
    }
}

pub fn sanitize_term(term: SerializedTerm) -> Result<serde_json::Value, String> {
    match term {
        SerializedTerm::Value(value) => sanitize_value(value),
        SerializedTerm::Object(value) => sanitize_object(value),
        SerializedTerm::List(value) => sanitize_list(value),
    }
}

fn sanitize_value(value: ValueTerm) -> Result<serde_json::Value, String> {
    match value {
        ValueTerm::Symbol(_) => Err(format!("Unable to format value: {}", value)),
        ValueTerm::Null => Ok(serde_json::Value::Null),
        ValueTerm::Boolean(value) => Ok(serde_json::Value::Bool(value)),
        ValueTerm::Int(value) => Ok(serde_json::Value::Number(value.into())),
        ValueTerm::Float(value) => sanitize_float(value),
        ValueTerm::String(value) => Ok(serde_json::Value::String(value)),
    }
}

fn sanitize_float(value: f64) -> Result<serde_json::Value, String> {
    match parse_integer_float(value) {
        Some(value) => Ok(serde_json::Value::Number(value.into())),
        None => match serde_json::Number::from_f64(value) {
            Some(value) => Ok(serde_json::Value::Number(value)),
            None => Err(format!("Unable to format value: {}", value)),
        },
    }
}

fn parse_integer_float(value: f64) -> Option<u64> {
    let int_value = value as u64;
    if value == (int_value as f64) {
        Some(int_value)
    } else {
        None
    }
}

fn sanitize_object(value: SerializedObjectTerm) -> Result<serde_json::Value, String> {
    let properties = value
        .into_entries()
        .into_iter()
        .map(|(key, value)| match sanitize_term(value) {
            Ok(value) => Ok((key, value)),
            Err(error) => Err(error),
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(serde_json::Value::Object(serde_json::Map::from_iter(
        properties,
    )))
}

fn sanitize_list(value: SerializedListTerm) -> Result<serde_json::Value, String> {
    let items = value
        .into_items()
        .into_iter()
        .map(sanitize_term)
        .collect::<Result<Vec<_>, String>>()?;
    Ok(serde_json::Value::Array(items))
}

pub fn deserialize(value: &Value) -> Result<SerializedTerm, String> {
    match value {
        Value::Null => Ok(SerializedTerm::Value(ValueTerm::Null)),
        Value::Bool(value) => Ok(SerializedTerm::Value(ValueTerm::Boolean(*value))),
        Value::String(value) => Ok(SerializedTerm::Value(ValueTerm::String(value.clone()))),
        Value::Number(value) => match value.as_f64() {
            Some(value) => Ok(SerializedTerm::Value(ValueTerm::Float(value))),
            None => Err(format!(
                "JSON deserialization encountered invalid number: {}",
                value
            )),
        },
        Value::Array(value) => deserialize_array(value),
        Value::Object(value) => deserialize_object(value),
    }
}

fn deserialize_array(value: &Vec<Value>) -> Result<SerializedTerm, String> {
    let items = value
        .into_iter()
        .map(deserialize)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(SerializedTerm::List(SerializedListTerm::new(items)))
}

fn deserialize_object(value: &Map<String, Value>) -> Result<SerializedTerm, String> {
    let entries = value
        .into_iter()
        .map(|(key, value)| match deserialize(value) {
            Ok(value) => Ok((String::from(key), value)),
            Err(error) => Err(error),
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(SerializedTerm::Object(SerializedObjectTerm::new(entries)))
}

fn parse_value(value: Value) -> Result<SerializedTerm, String> {
    match value {
        Value::Null => Ok(SerializedTerm::null()),
        Value::Bool(value) => Ok(SerializedTerm::boolean(value)),
        Value::String(value) => Ok(SerializedTerm::string(value)),
        Value::Number(value) => match value.as_f64() {
            Some(value) => Ok(SerializedTerm::float(value)),
            None => Err(format!(
                "JSON deserialization encountered invalid number: {}",
                value
            )),
        },
        Value::Array(value) => parse_array(value),
        Value::Object(value) => parse_object(value),
    }
}

fn parse_array(value: Vec<Value>) -> Result<SerializedTerm, String> {
    let items = value
        .into_iter()
        .map(parse_value)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(SerializedTerm::list(items))
}

fn parse_object(value: Map<String, Value>) -> Result<SerializedTerm, String> {
    let entries = value
        .into_iter()
        .map(|(key, value)| match parse_value(value) {
            Ok(value) => Ok((key, value)),
            Err(error) => Err(error),
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(SerializedTerm::object(entries))
}

#[cfg(test)]
mod tests {
    use reflex::{
        serialize::{SerializedListTerm, SerializedObjectTerm, SerializedTerm},
        stdlib::value::{StringValue, ValueTerm},
    };

    use super::stringify;

    #[test]
    fn stringify_primitives() {
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Symbol(3))),
            Err(String::from("Unable to format value: <symbol:3>")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Null)),
            Ok(String::from("null")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Boolean(false))),
            Ok(String::from("false")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Boolean(true))),
            Ok(String::from("true")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Int(0))),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Int(3))),
            Ok(String::from("3")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Int(-0))),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Int(-3))),
            Ok(String::from("-3")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Float(0.0))),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Float(3.142))),
            Ok(String::from("3.142")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Float(-0.0))),
            Ok(String::from("0")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::Float(-3.142))),
            Ok(String::from("-3.142")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::String(StringValue::from(
                ""
            )))),
            Ok(String::from("\"\"")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::String(StringValue::from(
                "foo"
            )))),
            Ok(String::from("\"foo\"")),
        );
        assert_eq!(
            stringify(SerializedTerm::Value(ValueTerm::String(StringValue::from(
                "\"\'\n\r"
            )))),
            Ok(String::from("\"\\\"\'\\n\\r\"")),
        );
    }

    #[test]
    fn stringify_lists() {
        assert_eq!(
            stringify(SerializedTerm::List(SerializedListTerm::new(Vec::new()))),
            Ok(String::from("[]"))
        );
        assert_eq!(
            stringify(SerializedTerm::List(SerializedListTerm::new(vec![
                SerializedTerm::value(ValueTerm::Int(3)),
                SerializedTerm::value(ValueTerm::Int(4)),
                SerializedTerm::value(ValueTerm::Int(5)),
            ]))),
            Ok(String::from("[3,4,5]")),
        );
    }

    #[test]
    fn stringify_objects() {
        assert_eq!(
            stringify(SerializedTerm::Object(
                SerializedObjectTerm::new(Vec::new())
            )),
            Ok(String::from("{}"))
        );
        assert_eq!(
            stringify(SerializedTerm::Object(SerializedObjectTerm::new(vec![
                (
                    String::from("first"),
                    SerializedTerm::value(ValueTerm::Int(3)),
                ),
                (
                    String::from("second"),
                    SerializedTerm::value(ValueTerm::Int(4)),
                ),
                (
                    String::from("third"),
                    SerializedTerm::value(ValueTerm::Int(5)),
                )
            ]))),
            Ok(String::from("{\"first\":3,\"second\":4,\"third\":5}")),
        );
        assert_eq!(
            stringify(SerializedTerm::Object(SerializedObjectTerm::new(vec![(
                String::from("\"\'\n\r"),
                SerializedTerm::value(ValueTerm::Int(3)),
            ),]))),
            Ok(String::from("{\"\\\"\'\\n\\r\":3}")),
        );
    }
}
