// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, InstructionPointer, Program},
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, SerializeJson, StackOffset, VarArgs,
    },
    hash::HashId,
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct CompiledFunctionTerm {
    address: InstructionPointer,
    hash: HashId,
    required_args: StackOffset,
    optional_args: StackOffset,
}
impl CompiledFunctionTerm {
    pub fn new(
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> Self {
        Self {
            address,
            hash,
            required_args,
            optional_args,
        }
    }
    pub fn address(&self) -> InstructionPointer {
        self.address
    }
    pub fn hash(&self) -> HashId {
        self.hash
    }
    pub fn required_args(&self) -> StackOffset {
        self.required_args
    }
    pub fn optional_args(&self) -> StackOffset {
        self.optional_args
    }
}
impl GraphNode for CompiledFunctionTerm {
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
impl<T: Expression> Applicable<T> for CompiledFunctionTerm {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::lazy(self.required_args, self.optional_args, false))
    }
    fn apply(
        &self,
        _args: impl ExactSizeIterator<Item = T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Err(format!(
            "Compiled functions cannot be invoked directly: {}",
            self,
        ))
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
}
impl<T: Expression + Compile<T>> Compile<T> for CompiledFunctionTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushFunction {
            target: self.address,
            hash: self.hash(),
        })))
    }
}
impl std::fmt::Display for CompiledFunctionTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<compiled:{:x}>", self.address)
    }
}

impl SerializeJson for CompiledFunctionTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
