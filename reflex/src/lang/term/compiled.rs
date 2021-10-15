// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{
        Compile, Compiler, Instruction, InstructionPointer, NativeFunctionRegistry, Program,
    },
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, StackOffset, VarArgs,
    },
    hash::HashId,
};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct CompiledFunctionTerm {
    hash: HashId,
    address: InstructionPointer,
    num_args: StackOffset,
    variadic: bool,
}
impl CompiledFunctionTerm {
    pub fn new(
        hash: HashId,
        address: InstructionPointer,
        num_args: StackOffset,
        variadic: bool,
    ) -> Self {
        Self {
            hash,
            address,
            num_args,
            variadic,
        }
    }
    pub fn address(&self) -> InstructionPointer {
        self.address
    }
    pub fn num_args(&self) -> StackOffset {
        self.num_args
    }
    pub fn variadic(&self) -> bool {
        self.variadic
    }
}
impl GraphNode for CompiledFunctionTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression> Applicable<T> for CompiledFunctionTerm {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(
            0,
            self.num_args,
            if self.variadic {
                Some(VarArgs::Lazy)
            } else {
                None
            },
        ))
    }
    fn apply(
        &self,
        _args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Err(format!(
            "Compiled functions cannot be invoked directly: {}",
            self,
        ))
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
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        Ok((
            Program::new(once(Instruction::PushFunction {
                target: self.address,
            })),
            NativeFunctionRegistry::default(),
        ))
    }
}
impl std::fmt::Display for CompiledFunctionTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<compiled:{:x}>", self.address)
    }
}
