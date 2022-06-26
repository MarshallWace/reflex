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

pub type IntValue = i32;

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct IntTerm {
    pub value: IntValue,
}
impl GraphNode for IntTerm {
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
impl<T: Expression + Compile<T>> Compile<T> for IntTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushInt {
            value: self.value,
        })))
    }
}
impl std::fmt::Display for IntTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl std::fmt::Debug for IntTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl SerializeJson for IntTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Ok(serde_json::Value::Number(self.value.into()))
    }
}
