// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{cmp::Eq, fmt, hash::Hash};

use crate::hash::HashId;

pub type SymbolId = usize;
pub type IntValue = i32;
pub type FloatValue = f64;
pub type StringValue = String;

#[derive(PartialEq, Clone)]
pub enum ValueTerm {
    Hash(HashId),
    Symbol(SymbolId),
    Null,
    Boolean(bool),
    Int(IntValue),
    Float(FloatValue),
    String(StringValue),
}
impl Hash for ValueTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Hash(id) => {
                state.write_u8(0);
                id.hash(state);
            }
            Self::Symbol(id) => {
                state.write_u8(1);
                id.hash(state);
            }
            Self::Null => state.write_u8(2),
            Self::Boolean(value) => {
                state.write_u8(3);
                value.hash(state);
            }
            Self::Int(value) => {
                state.write_u8(4);
                value.hash(state);
            }
            Self::Float(value) => {
                state.write_u8(5);
                state.write(&value.to_be_bytes())
            }
            Self::String(value) => {
                state.write_u8(6);
                value.hash(state);
            }
        }
    }
}
impl Eq for ValueTerm {}
impl fmt::Display for ValueTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Hash(value) => write!(f, "<hash:{}>", value),
            Self::Symbol(value) => write!(f, "<symbol:{}>", value),
            Self::Null => write!(f, "<null>"),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{:?}", value),
            Self::Float(value) => match as_integer(*value) {
                Some(value) => write!(f, "{}.0", value),
                None => write!(f, "{:?}", value),
            },
            Self::String(value) => write!(f, "{:?}", value),
        }
    }
}
impl fmt::Debug for ValueTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub fn is_integer(value: f64) -> bool {
    as_integer(value).is_some()
}

pub fn as_integer(value: f64) -> Option<i32> {
    let int_value = value as i32;
    if value == int_value as f64 {
        Some(int_value)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::stdlib::value::ValueTerm;

    #[test]
    fn format_float_values() {
        assert_eq!(format!("{}", ValueTerm::Float(3.142)), "3.142");
        assert_eq!(format!("{}", ValueTerm::Float(3.0)), "3.0");
        assert_eq!(format!("{}", ValueTerm::Float(0.0)), "0.0");
        assert_eq!(format!("{}", ValueTerm::Float(-0.0)), "0.0");
        assert_eq!(format!("{}", ValueTerm::Float(-3.0)), "-3.0");
        assert_eq!(format!("{}", ValueTerm::Float(-3.142)), "-3.142");
    }
}
