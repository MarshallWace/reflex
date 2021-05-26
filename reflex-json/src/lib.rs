// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, SerializedTerm, StructPrototype, StructTerm, Term},
    stdlib::{
        collection::{vector::VectorTerm, CollectionTerm},
        value::ValueTerm,
    },
};
use serde_json::{Map, Value};

pub fn parse(value: &str) -> Result<Expression, String> {
    match serde_json::from_str::<Value>(value) {
        Err(error) => Err(format!("JSON deserialization failed: {}", error)),
        Ok(value) => deserialize_value(value),
    }
}

pub fn serialized(value: &str) -> Result<SerializedTerm, String> {
    match serde_json::from_str::<Value>(value) {
        Err(error) => Err(format!("JSON parse failed: {}", error)),
        Ok(value) => parse_value(value),
    }
}

fn deserialize_value(value: Value) -> Result<Expression, String> {
    match value {
        Value::Null => Ok(Expression::new(Term::Value(ValueTerm::Null))),
        Value::Bool(value) => Ok(Expression::new(Term::Value(ValueTerm::Boolean(value)))),
        Value::String(value) => Ok(Expression::new(Term::Value(ValueTerm::String(value)))),
        Value::Number(value) => match value.as_f64() {
            Some(value) => Ok(Expression::new(Term::Value(ValueTerm::Float(value)))),
            None => Err(format!(
                "JSON deserialization encountered invalid number: {}",
                value
            )),
        },
        Value::Array(value) => deserialize_array(value),
        Value::Object(value) => deserialize_object(value),
    }
}

fn deserialize_array(value: Vec<Value>) -> Result<Expression, String> {
    let items = value
        .into_iter()
        .map(deserialize_value)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
        VectorTerm::new(items),
    ))))
}

fn deserialize_object(value: Map<String, Value>) -> Result<Expression, String> {
    let entries = value
        .into_iter()
        .map(|(key, value)| match deserialize_value(value) {
            Ok(value) => Ok((ValueTerm::String(key), value)),
            Err(error) => Err(error),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
    Ok(Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    ))))
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
