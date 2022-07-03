// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, Expression, ExpressionFactory, GraphNode, HeapAllocator, SerializeJson,
        StackOffset, StringValue, VarArgs,
    },
};

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize, Hash)]
pub struct StringTerm<TString: StringValue> {
    pub value: TString,
}
impl<TString: StringValue> GraphNode for StringTerm<TString> {
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
impl<T: Expression + Compile<T>> Compile<T> for StringTerm<T::String> {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushString {
            value: String::from(self.value.as_str()),
        })))
    }
}
impl<TString: StringValue> std::fmt::Display for StringTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
impl<TString: StringValue> std::fmt::Debug for StringTerm<TString> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
impl<TString: StringValue> SerializeJson for StringTerm<TString> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Ok(serde_json::Value::String(String::from(self.value.as_str())))
    }
}
