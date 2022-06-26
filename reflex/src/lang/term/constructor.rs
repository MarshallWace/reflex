// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        Applicable, Arity, DependencyList, EvaluationCache, Expression, ExpressionFactory,
        GraphNode, HeapAllocator, SerializeJson, StackOffset, StructPrototype, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ConstructorTerm {
    prototype: StructPrototype,
}
impl ConstructorTerm {
    pub fn new(prototype: StructPrototype) -> Self {
        Self { prototype }
    }
    pub fn prototype(&self) -> &StructPrototype {
        &self.prototype
    }
}
impl GraphNode for ConstructorTerm {
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
impl<T: Expression> Applicable<T> for ConstructorTerm {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::lazy(0, self.prototype().keys().len(), false))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_struct_term(
            allocator.clone_struct_prototype(&self.prototype),
            allocator.create_list(args),
        ))
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
}
impl<T: Expression + Compile<T>> Compile<T> for ConstructorTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushConstructor {
            prototype: allocator.clone_struct_prototype(&self.prototype),
        })))
    }
}
impl std::fmt::Display for ConstructorTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<constructor:{{{}}}>",
            self.prototype
                .keys()
                .iter()
                .map(|key| format!("{}", key))
                .collect::<Vec<_>>()
                .join(","),
        )
    }
}
impl SerializeJson for ConstructorTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
