// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, DynamicState, Evaluate, EvaluationCache, EvaluationResult, Expression,
        ExpressionFactory, GraphNode, HeapAllocator, Rewritable, SerializeJson, Signal,
        StackOffset, Substitutions, VarArgs,
    },
};

use super::compile_signal;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectTerm<T: Expression> {
    condition: Signal<T>,
}
impl<T: Expression> EffectTerm<T> {
    pub fn new(condition: Signal<T>) -> Self {
        Self { condition }
    }
    pub fn condition(&self) -> &Signal<T> {
        &self.condition
    }
}
impl<T: Expression> GraphNode for EffectTerm<T> {
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
        DependencyList::of(self.condition.id())
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        true
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
    fn is_complex(&self) -> bool {
        false
    }
}
impl<T: Expression> Evaluate<T> for EffectTerm<T> {
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        Some(EvaluationResult::new(
            match state.get(&self.condition.id()) {
                Some(value) => value.clone(),
                None => factory
                    .create_signal_term(allocator.create_signal_list(once(self.condition.clone()))),
            },
            DependencyList::of(self.condition.id()),
        ))
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for EffectTerm<T> {
    fn substitute_static(
        &self,
        _substitutions: &Substitutions<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
    fn substitute_dynamic(
        &self,
        _deep: bool,
        state: &impl DynamicState<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        state.get(&self.condition.id()).cloned()
    }
    fn hoist_free_variables(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        None
    }
    fn normalize(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
}
impl<T: Expression + Compile<T>> Compile<T> for EffectTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        let compiled_condition =
            compile_signal(&self.condition, stack_offset, factory, allocator, compiler)?;
        let mut result = compiled_condition;
        result.push(Instruction::LoadEffect);
        Ok(result)
    }
}
impl<T: Expression> std::fmt::Display for EffectTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<effect:{}>", self.condition)
    }
}
impl<T: Expression> SerializeJson for EffectTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
