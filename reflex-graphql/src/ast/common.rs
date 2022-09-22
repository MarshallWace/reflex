// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::ast::position::Pos;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Directive {
    pub position: Pos,
    pub name: String,
    pub arguments: Vec<(String, Value)>,
}
impl<'a> From<&'a graphql_parser::schema::Directive<'static, String>> for Directive {
    fn from(value: &'a graphql_parser::schema::Directive<'static, String>) -> Self {
        let graphql_parser::schema::Directive {
            position,
            name,
            arguments,
        } = value;
        Self {
            position: position.into(),
            name: name.into(),
            arguments: arguments
                .iter()
                .map(|(key, value)| (key.into(), value.into()))
                .collect(),
        }
    }
}
impl From<Directive> for graphql_parser::schema::Directive<'static, String> {
    fn from(value: Directive) -> Self {
        let Directive {
            position,
            name,
            arguments,
        } = value;
        Self {
            position: position.into(),
            name: name.into(),
            arguments: arguments
                .into_iter()
                .map(|(key, value)| (key.into(), value.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Number(pub(crate) i64);
impl Number {
    pub fn as_i64(&self) -> Option<i64> {
        let Self(inner) = self;
        Some(*inner)
    }
}
impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self(value as i64)
    }
}
impl<'a> From<&'a graphql_parser::query::Number> for Number {
    fn from(value: &'a graphql_parser::query::Number) -> Self {
        Self(value.as_i64().unwrap())
    }
}
impl From<Number> for graphql_parser::query::Number {
    fn from(value: Number) -> Self {
        let Number(value) = value;
        (value as i32).into()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Variable(String),
    Int(Number),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
    Enum(String),
    List(Vec<Value>),
    Object(BTreeMap<String, Value>),
}
impl Eq for Value {}
impl<'a> From<&'a graphql_parser::query::Value<'static, String>> for Value {
    fn from(value: &'a graphql_parser::query::Value<'static, String>) -> Self {
        match value {
            graphql_parser::query::Value::Variable(inner) => Self::Variable(inner.into()),
            graphql_parser::query::Value::Int(inner) => Self::Int(inner.into()),
            graphql_parser::query::Value::Float(inner) => Self::Float(*inner),
            graphql_parser::query::Value::String(inner) => Self::String(inner.clone()),
            graphql_parser::query::Value::Boolean(inner) => Self::Boolean(*inner),
            graphql_parser::query::Value::Null => Self::Null,
            graphql_parser::query::Value::Enum(inner) => Self::Enum(inner.into()),
            graphql_parser::query::Value::List(inner) => {
                Self::List(inner.iter().map(|value| value.into()).collect())
            }
            graphql_parser::query::Value::Object(inner) => Self::Object(
                inner
                    .iter()
                    .map(|value| (value.0.into(), value.1.into()))
                    .collect(),
            ),
        }
    }
}
impl From<Value> for graphql_parser::query::Value<'static, String> {
    fn from(value: Value) -> Self {
        match value {
            Value::Variable(inner) => Self::Variable(inner.into()),
            Value::Int(inner) => Self::Int(inner.into()),
            Value::Float(inner) => Self::Float(inner.into()),
            Value::String(inner) => Self::String(inner.into()),
            Value::Boolean(inner) => Self::Boolean(inner.into()),
            Value::Null => Self::Null,
            Value::Enum(inner) => Self::Enum(inner.into()),
            Value::List(inner) => Self::List(inner.into_iter().map(|value| value.into()).collect()),
            Value::Object(inner) => Self::Object(
                inner
                    .into_iter()
                    .map(|value| (value.0.into(), value.1.into()))
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    NamedType(String),
    ListType(Box<Type>),
    NonNullType(Box<Type>),
}
impl<'a> From<&'a graphql_parser::schema::Type<'static, String>> for Type {
    fn from(value: &'a graphql_parser::schema::Type<'static, String>) -> Self {
        match value {
            graphql_parser::schema::Type::NamedType(inner) => Self::NamedType(inner.into()),
            graphql_parser::schema::Type::ListType(inner) => {
                Self::ListType(Box::new(inner.as_ref().into()))
            }
            graphql_parser::schema::Type::NonNullType(inner) => {
                Self::NonNullType(Box::new(inner.as_ref().into()))
            }
        }
    }
}
impl From<Type> for graphql_parser::schema::Type<'static, String> {
    fn from(value: Type) -> Self {
        match value {
            Type::NamedType(inner) => Self::NamedType(inner.into()),
            Type::ListType(inner) => Self::ListType(Box::new(inner.as_ref().clone().into())),
            Type::NonNullType(inner) => Self::NonNullType(Box::new(inner.as_ref().clone().into())),
        }
    }
}
