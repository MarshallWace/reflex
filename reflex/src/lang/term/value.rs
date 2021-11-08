// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{cmp::Eq, collections::HashSet, hash::Hash, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, Expression, ExpressionFactory, GraphNode, HeapAllocator, SerializeJson,
        StackOffset, StringValue, VarArgs,
    },
    hash::HashId,
};

pub type SymbolId = usize;
pub type IntValue = i32;
pub type FloatValue = f64;

#[derive(PartialEq, Clone)]
pub enum ValueTerm<TString: StringValue> {
    Null,
    Boolean(bool),
    Int(IntValue),
    Float(FloatValue),
    String(TString),
    Symbol(SymbolId),
    Hash(HashId),
}
impl<TString: StringValue> Eq for ValueTerm<TString> {}
impl<TString: StringValue> Hash for ValueTerm<TString> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Null => state.write_u8(0),
            Self::Boolean(value) => {
                state.write_u8(1);
                value.hash(state);
            }
            Self::Int(value) => {
                state.write_u8(2);
                value.hash(state);
            }
            Self::Float(value) => {
                state.write_u8(3);
                state.write(&value.to_be_bytes())
            }
            Self::String(value) => {
                state.write_u8(4);
                value.hash(state);
            }
            Self::Hash(id) => {
                state.write_u8(5);
                id.hash(state);
            }
            Self::Symbol(id) => {
                state.write_u8(6);
                id.hash(state);
            }
        }
    }
}
impl<TString: StringValue> ValueTerm<TString> {
    pub fn match_null(&self) -> Option<()> {
        match self {
            ValueTerm::Null => Some(()),
            _ => None,
        }
    }
    pub fn match_boolean(&self) -> Option<bool> {
        match self {
            ValueTerm::Boolean(value) => Some(*value),
            _ => None,
        }
    }
    pub fn match_int(&self) -> Option<IntValue> {
        match self {
            ValueTerm::Int(value) => Some(*value),
            _ => None,
        }
    }
    pub fn match_float(&self) -> Option<FloatValue> {
        match self {
            ValueTerm::Float(value) => Some(*value),
            _ => None,
        }
    }
    pub fn match_string(&self) -> Option<&TString> {
        match self {
            ValueTerm::String(value) => Some(value),
            _ => None,
        }
    }
    pub fn match_symbol(&self) -> Option<SymbolId> {
        match self {
            ValueTerm::Symbol(value) => Some(*value),
            _ => None,
        }
    }
    pub fn match_hash(&self) -> Option<HashId> {
        match self {
            ValueTerm::Hash(value) => Some(*value),
            _ => None,
        }
    }
}
impl<TString: StringValue> GraphNode for ValueTerm<TString> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
}
impl<T: Expression + Compile<T>> Compile<T> for ValueTerm<T::String> {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(match self {
            Self::Hash(value) => Instruction::PushHash { value: *value },
            Self::Symbol(value) => Instruction::PushSymbol { value: *value },
            Self::Null => Instruction::PushNull,
            Self::Boolean(value) => Instruction::PushBoolean { value: *value },
            Self::Int(value) => Instruction::PushInt { value: *value },
            Self::Float(value) => Instruction::PushFloat { value: *value },
            Self::String(value) => Instruction::PushString {
                value: value.as_str().into(),
            },
        })))
    }
}
impl<TString: StringValue> std::fmt::Display for ValueTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hash(value) => write!(f, "<hash:{}>", value),
            Self::Symbol(value) => write!(f, "<symbol:{}>", value),
            Self::Null => write!(f, "null"),
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
impl<TString: StringValue> std::fmt::Debug for ValueTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl<TString: StringValue> SerializeJson for ValueTerm<TString> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            ValueTerm::Null => Ok(serde_json::Value::Null),
            ValueTerm::Boolean(boolean) => Ok(serde_json::Value::Bool(*boolean)),
            ValueTerm::Int(integer) => Ok(serde_json::Value::Number((*integer).into())),
            ValueTerm::String(string) => Ok(serde_json::Value::String(string.as_str().to_owned())),
            ValueTerm::Float(float) => match serde_json::Number::from_f64(*float) {
                Some(number) => Ok(serde_json::Value::Number(number)),
                None => Err(format!(
                    "Unable to serialize float as it is NaN or infinite: {}",
                    self
                )),
            },
            _ => Err(format!("Unable to serialize term: {}", self)),
        }
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
