// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    core::{Expression, StructPrototype, StructTerm, Term},
    stdlib::{
        collection::{vector::VectorTerm, CollectionTerm},
        value::{SymbolId, ValueTerm},
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SerializedTerm {
    Value(ValueTerm),
    Object(SerializedObjectTerm),
    List(SerializedListTerm),
}
impl SerializedTerm {
    pub fn value(value: ValueTerm) -> Self {
        Self::Value(value)
    }
    pub fn object(entries: impl IntoIterator<Item = (String, SerializedTerm)>) -> Self {
        Self::Object(SerializedObjectTerm::new(entries))
    }
    pub fn list(items: impl IntoIterator<Item = SerializedTerm>) -> Self {
        Self::List(SerializedListTerm::new(items))
    }
    pub fn symbol(value: SymbolId) -> Self {
        Self::Value(ValueTerm::Symbol(value))
    }
    pub fn null() -> Self {
        Self::Value(ValueTerm::Null)
    }
    pub fn boolean(value: bool) -> Self {
        Self::Value(ValueTerm::Boolean(value))
    }
    pub fn int(value: i32) -> Self {
        Self::Value(ValueTerm::Int(value))
    }
    pub fn float(value: f64) -> Self {
        Self::Value(ValueTerm::Float(value))
    }
    pub fn string(value: String) -> Self {
        Self::Value(ValueTerm::String(value))
    }
    pub fn stringify(&self) -> String {
        stringify(self)
    }
    pub fn deserialize(&self) -> Expression {
        deserialize(self)
    }
}
impl fmt::Display for SerializedTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", stringify(self))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SerializedObjectTerm {
    entries: Vec<(String, SerializedTerm)>,
}
impl SerializedObjectTerm {
    pub fn new(entries: impl IntoIterator<Item = (String, SerializedTerm)>) -> Self {
        Self {
            entries: entries.into_iter().collect(),
        }
    }
    pub fn entries(&self) -> &[(String, SerializedTerm)] {
        &self.entries
    }
}
impl fmt::Display for SerializedObjectTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", stringify_object(self))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SerializedListTerm {
    items: Vec<SerializedTerm>,
}
impl SerializedListTerm {
    pub fn new(items: impl IntoIterator<Item = SerializedTerm>) -> Self {
        Self {
            items: items.into_iter().collect(),
        }
    }
    pub fn items(&self) -> &[SerializedTerm] {
        &self.items
    }
}
impl fmt::Display for SerializedListTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", stringify_list(self))
    }
}

pub fn serialize(term: &Term) -> Result<SerializedTerm, String> {
    match term {
        Term::Value(value) => Ok(SerializedTerm::Value(value.clone())),
        Term::Struct(value) => serialize_struct_term(value),
        Term::Collection(value) => match value {
            CollectionTerm::Vector(value) => serialize_vector_term(value),
            _ => Err(format!("Unable to serialize collection: {}", value)),
        },
        _ => Err(format!("Unable to serialize value: {}", term)),
    }
}

fn serialize_struct_term(value: &StructTerm) -> Result<SerializedTerm, String> {
    match value.prototype() {
        None => {
            let items = value
                .fields()
                .iter()
                .map(|value| serialize(value.value()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(SerializedTerm::List(SerializedListTerm::new(items)))
        }
        Some(prototype) => {
            let entries = prototype
                .keys()
                .iter()
                .zip(value.fields())
                .map(|(key, value)| {
                    let key = match key {
                        ValueTerm::String(value) => Ok(value.clone()),
                        _ => Err(format!("Unable to serialize object key: {}", value)),
                    }?;
                    let value = serialize(value.value())?;
                    Ok((key, value))
                })
                .collect::<Result<Vec<_>, String>>()?;
            Ok(SerializedTerm::Object(SerializedObjectTerm::new(entries)))
        }
    }
}

fn serialize_vector_term(value: &VectorTerm) -> Result<SerializedTerm, String> {
    let items = value
        .items()
        .iter()
        .map(|item| serialize(item.value()))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(SerializedTerm::List(SerializedListTerm::new(items)))
}

fn stringify(term: &SerializedTerm) -> String {
    match term {
        SerializedTerm::Value(value) => stringify_value(value),
        SerializedTerm::Object(value) => stringify_object(value),
        SerializedTerm::List(value) => stringify_list(value),
    }
}

fn stringify_value(value: &ValueTerm) -> String {
    match value {
        ValueTerm::Symbol(id) => format!("Symbol({})", id),
        ValueTerm::Null => String::from("null"),
        ValueTerm::Boolean(value) => format!("{}", value),
        ValueTerm::Int(value) => format!("{}", value),
        ValueTerm::Float(value) => format!("{}", value),
        ValueTerm::String(value) => quote_string(value),
    }
}

fn stringify_object(value: &SerializedObjectTerm) -> String {
    if value.entries.is_empty() {
        String::from("{}")
    } else {
        format!(
            "{{ {} }}",
            value
                .entries
                .iter()
                .map(|(key, value)| format!("{}: {}", quote_string(key), value))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn stringify_list(value: &SerializedListTerm) -> String {
    format!(
        "[{}]",
        value
            .items
            .iter()
            .map(|value| format!("{}", value))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn deserialize(value: &SerializedTerm) -> Expression {
    match value {
        SerializedTerm::Value(value) => Expression::new(Term::Value(value.clone())),
        SerializedTerm::Object(value) => deserialize_object(value),
        SerializedTerm::List(value) => deserialize_list(value),
    }
}

fn deserialize_object(value: &SerializedObjectTerm) -> Expression {
    let (keys, values): (Vec<_>, Vec<_>) = value
        .entries()
        .iter()
        .map(|(key, value)| (ValueTerm::String(key.clone()), deserialize(value)))
        .unzip();
    Expression::new(Term::Struct(StructTerm::new(
        Some(StructPrototype::new(keys)),
        values,
    )))
}

fn deserialize_list(value: &SerializedListTerm) -> Expression {
    let items = value.items().iter().map(deserialize);
    Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
        items,
    ))))
}

fn quote_string(value: &str) -> String {
    let mut result = String::with_capacity(value.len() + 2);
    result.push('"');
    for current in value.chars() {
        let escape_char = match current {
            '\\' | '"' | '\'' => Some(current),
            '\n' | '\u{2028}' | '\u{2029}' => Some('n'),
            '\r' => Some('r'),
            _ => None,
        };
        match escape_char {
            Some(escaped) => {
                result.push('\\');
                result.push(escaped);
            }
            None => {
                result.push(current);
            }
        }
    }
    result.push('"');
    result
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{Expression, StructPrototype, StructTerm, Term},
        serialize::{SerializedListTerm, SerializedObjectTerm, SerializedTerm},
        stdlib::{
            collection::{vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    use super::{serialize, stringify};

    #[test]
    fn serialize_primitives() {
        let result = serialize(&Term::Value(ValueTerm::Symbol(3)));
        assert_eq!(result, Ok(SerializedTerm::Value(ValueTerm::Symbol(3))));
        let result = serialize(&Term::Value(ValueTerm::Null));
        assert_eq!(result, Ok(SerializedTerm::Value(ValueTerm::Null)));
        let result = serialize(&Term::Value(ValueTerm::Boolean(true)));
        assert_eq!(result, Ok(SerializedTerm::Value(ValueTerm::Boolean(true))));
        let result = serialize(&Term::Value(ValueTerm::Int(3)));
        assert_eq!(result, Ok(SerializedTerm::Value(ValueTerm::Int(3))));
        let result = serialize(&Term::Value(ValueTerm::Float(3.142)));
        assert_eq!(result, Ok(SerializedTerm::Value(ValueTerm::Float(3.142))));
        let result = serialize(&Term::Value(ValueTerm::String(StringValue::from("foo"))));
        assert_eq!(
            result,
            Ok(SerializedTerm::Value(ValueTerm::String(StringValue::from(
                "foo"
            ))))
        );
    }

    #[test]
    fn serialize_lists() {
        let result = serialize(&Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            Vec::new(),
        ))));
        assert_eq!(
            result,
            Ok(SerializedTerm::List(SerializedListTerm::new(Vec::new())))
        );
        let result = serialize(&Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ],
        ))));
        assert_eq!(
            result,
            Ok(SerializedTerm::List(SerializedListTerm::new(vec![
                SerializedTerm::Value(ValueTerm::Int(3)),
                SerializedTerm::Value(ValueTerm::Int(4)),
                SerializedTerm::Value(ValueTerm::Int(5)),
            ]))),
        );
    }

    #[test]
    fn serialize_objects() {
        let result = serialize(&Term::Struct(StructTerm::new(
            Some(StructPrototype::new(Vec::new())),
            Vec::new(),
        )));
        assert_eq!(result, Ok(SerializedTerm::object(Vec::new())));
        let result = serialize(&Term::Struct(StructTerm::new(
            Some(StructPrototype::new(vec![
                ValueTerm::String(StringValue::from("first")),
                ValueTerm::String(StringValue::from("second")),
                ValueTerm::String(StringValue::from("third")),
            ])),
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ],
        )));
        assert_eq!(
            result,
            Ok(SerializedTerm::object(vec![
                (
                    String::from("first"),
                    SerializedTerm::Value(ValueTerm::Int(3))
                ),
                (
                    String::from("second"),
                    SerializedTerm::Value(ValueTerm::Int(4))
                ),
                (
                    String::from("third"),
                    SerializedTerm::Value(ValueTerm::Int(5))
                ),
            ]))
        );
    }

    #[test]
    fn stringify_primitives() {
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Symbol(3))),
            String::from("Symbol(3)"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Null)),
            String::from("null"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Boolean(false))),
            String::from("false"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Boolean(true))),
            String::from("true"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Int(0))),
            String::from("0"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Int(3))),
            String::from("3"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Int(-0))),
            String::from("0"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Int(-3))),
            String::from("-3"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Float(0.0))),
            String::from("0"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Float(3.142))),
            String::from("3.142"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Float(-0.0))),
            String::from("0"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::Float(-3.142))),
            String::from("-3.142"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::String(
                StringValue::from("")
            ))),
            String::from("\"\""),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::String(
                StringValue::from("foo")
            ))),
            String::from("\"foo\""),
        );
        assert_eq!(
            stringify(&SerializedTerm::Value(ValueTerm::String(
                StringValue::from("\"\'\n\r")
            ))),
            String::from("\"\\\"\\\'\\n\\r\""),
        );
    }

    #[test]
    fn stringify_lists() {
        assert_eq!(
            stringify(&SerializedTerm::List(SerializedListTerm::new(Vec::new()))),
            String::from("[]")
        );
        assert_eq!(
            stringify(&SerializedTerm::List(SerializedListTerm::new(vec![
                SerializedTerm::value(ValueTerm::Int(3)),
                SerializedTerm::value(ValueTerm::Int(4)),
                SerializedTerm::value(ValueTerm::Int(5)),
            ]))),
            String::from("[3, 4, 5]"),
        );
    }

    #[test]
    fn stringify_objects() {
        assert_eq!(
            stringify(&SerializedTerm::Object(SerializedObjectTerm::new(
                Vec::new()
            ))),
            String::from("{}")
        );
        assert_eq!(
            stringify(&SerializedTerm::Object(SerializedObjectTerm::new(vec![
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
            String::from("{ \"first\": 3, \"second\": 4, \"third\": 5 }"),
        );
        assert_eq!(
            stringify(&SerializedTerm::Object(SerializedObjectTerm::new(vec![(
                String::from("\"\'\n\r"),
                SerializedTerm::value(ValueTerm::Int(3)),
            ),]))),
            String::from("{ \"\\\"\\\'\\n\\r\": 3 }"),
        );
    }
}
