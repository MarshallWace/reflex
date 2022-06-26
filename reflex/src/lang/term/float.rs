// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, Expression, ExpressionFactory, GraphNode, HeapAllocator, SerializeJson,
        StackOffset, VarArgs,
    },
};

pub type FloatValue = f64;

#[derive(PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct FloatTerm {
    pub value: FloatValue,
}
impl Eq for FloatTerm {}
impl std::hash::Hash for FloatTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&self.value.to_le_bytes())
    }
}
impl GraphNode for FloatTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn count_variable_usages(&self, _offset: StackOffset) -> usize {
        0
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
    fn is_complex(&self) -> bool {
        false
    }
}
impl<T: Expression + Compile<T>> Compile<T> for FloatTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushFloat {
            value: self.value,
        })))
    }
}
impl std::fmt::Display for FloatTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match as_integer(self.value) {
            Some(value) => write!(f, "{}.0", value),
            None => write!(f, "{}", self.value),
        }
    }
}
impl std::fmt::Debug for FloatTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl SerializeJson for FloatTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match serde_json::Number::from_f64(self.value) {
            Some(number) => Ok(serde_json::Value::Number(number)),
            None => Err(format!(
                "Unable to serialize float non-finite float as JSON value: {}",
                self
            )),
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
