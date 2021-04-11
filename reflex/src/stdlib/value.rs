// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::hash::{
    hash_bool, hash_f64, hash_i32, hash_sequence, hash_string, hash_u32, prefix_hash, HashId,
    Hashable,
};

use super::builtin::query::QueryShape;

pub type SymbolId = u32;
pub type IntValue = i32;
pub type FloatValue = f64;
pub type StringValue = String;

#[derive(PartialEq, Clone)]
pub enum ValueTerm {
    Symbol(SymbolId),
    Boolean(bool),
    Int(IntValue),
    Float(FloatValue),
    String(StringValue),
    Array(ArrayValue),
    QueryShape(QueryShape),
}
impl Hashable for ValueTerm {
    fn hash(&self) -> HashId {
        match self {
            Self::Symbol(value) => prefix_hash(0, hash_u32(*value)),
            Self::Boolean(value) => prefix_hash(1, hash_bool(*value)),
            Self::Int(value) => prefix_hash(2, hash_i32(*value)),
            Self::Float(value) => prefix_hash(3, hash_f64(*value)),
            Self::String(value) => prefix_hash(4, hash_string(value)),
            Self::Array(value) => prefix_hash(5, value.hash()),
            Self::QueryShape(value) => prefix_hash(6, value.hash()),
        }
    }
}
impl fmt::Display for ValueTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol(value) => write!(f, "<symbol:{}>", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{:?}", value),
            Self::Float(value) => write!(f, "{:?}", value),
            Self::String(value) => write!(f, "{:?}", value),
            Self::Array(value) => fmt::Display::fmt(value, f),
            Self::QueryShape(value) => fmt::Display::fmt(value, f),
        }
    }
}
impl fmt::Debug for ValueTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(PartialEq, Clone)]
pub struct ArrayValue {
    values: Vec<ValueTerm>,
}
impl Hashable for ArrayValue {
    fn hash(&self) -> HashId {
        hash_sequence(self.values.iter().map(|value| value.hash()))
    }
}
impl ArrayValue {
    pub fn new(values: Vec<ValueTerm>) -> Self {
        Self { values }
    }
}
impl fmt::Display for ArrayValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.values
                .iter()
                .map(|value| format!("{}", value))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl fmt::Debug for ArrayValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
