// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    compiler::{Compile, Compiler, Program},
    core::{
        Applicable, Arity, DependencyList, DynamicState, Evaluate, EvaluationCache,
        EvaluationResult, Expression, ExpressionFactory, GraphNode, HeapAllocator, Reducible,
        Rewritable, SerializeJson, StackOffset, Substitutions, VarArgs,
    },
    hash::HashId,
};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::hash::Hash;
use std::sync::Arc;

#[derive(Eq, Clone, Copy, Deserialize, Serialize)]
pub struct CachedExpression<T: Expression> {
    hash: HashId,
    capture_depth: StackOffset,
    has_dynamic_dependencies_shallow: bool,
    has_dynamic_dependencies_deep: bool,
    value: T,
}
impl<T: Expression> CachedExpression<T> {
    pub fn new(value: T) -> Self {
        Self {
            hash: value.id(),
            capture_depth: value.capture_depth(),
            has_dynamic_dependencies_shallow: value.has_dynamic_dependencies(false),
            has_dynamic_dependencies_deep: value.has_dynamic_dependencies(true),
            value,
        }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
}
impl<T: Expression> Expression for CachedExpression<T> {
    type String = T::String;
    type Builtin = T::Builtin;
    fn id(&self) -> HashId {
        self.hash
    }
}
impl<T: Expression> std::hash::Hash for CachedExpression<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
impl<T: Expression> PartialEq for CachedExpression<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl<T: Expression> GraphNode for CachedExpression<T> {
    fn capture_depth(&self) -> StackOffset {
        self.capture_depth
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        if self.has_dynamic_dependencies(deep) {
            self.value.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        if deep {
            self.has_dynamic_dependencies_deep
        } else {
            self.has_dynamic_dependencies_shallow
        }
    }
    fn is_static(&self) -> bool {
        self.value.is_static()
    }
    fn is_atomic(&self) -> bool {
        self.value.is_atomic()
    }
}
impl<T: Expression> std::fmt::Display for CachedExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl<T: Expression> std::fmt::Debug for CachedExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}
impl<T: Expression> SerializeJson for CachedExpression<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        self.value().to_json()
    }
}

impl<T: Expression + Rewritable<T>> Rewritable<T> for CachedExpression<T> {
    fn children(&self) -> Vec<&T> {
        self.value.children()
    }
    fn count_subexpression_usages(
        &self,
        expression: &T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> usize {
        self.value
            .count_subexpression_usages(expression, factory, allocator)
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if substitutions.can_skip(&self.value) {
            return None;
        }
        match cache.retrieve_static_substitution(&self.value, substitutions) {
            Some(result) => result,
            None => {
                let result = self
                    .value
                    .substitute_static(substitutions, factory, allocator, cache);
                cache.store_static_substitution(
                    &self.value,
                    substitutions,
                    result.as_ref().cloned(),
                );
                result
            }
        }
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if !self.has_dynamic_dependencies(deep) {
            return None;
        }
        match cache.retrieve_dynamic_substitution(&self.value, deep, state) {
            Some(result) => result,
            None => {
                let result = self
                    .value
                    .substitute_dynamic(deep, state, factory, allocator, cache);
                cache.store_dynamic_substitution(
                    &self.value,
                    deep,
                    state,
                    result.as_ref().cloned(),
                );
                result
            }
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        self.value.hoist_free_variables(factory, allocator)
    }

    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.value.normalize(factory, allocator, cache)
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> Reducible<T> for CachedExpression<T> {
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match cache.retrieve_reduction(&self.value) {
            Some(result) => result,
            None => {
                let result = self.value.reduce(factory, allocator, cache);
                cache.store_reduction(&self.value, result.as_ref().cloned());
                result
            }
        }
    }
}
impl<T: Expression + Applicable<T>> Applicable<T> for CachedExpression<T> {
    fn arity(&self) -> Option<Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        // TODO: Memoize function applications based on arg list hash
        self.value.apply(args, factory, allocator, cache)
    }
}
impl<T: Expression + Compile<T>> Compile<T> for CachedExpression<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>> Evaluate<T>
    for CachedExpression<T>
{
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        let result = self.value.evaluate(state, factory, allocator, cache)?;
        if result.result().id() == self.id() {
            None
        } else {
            Some(result)
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct SharedExpression<T: Expression> {
    pub(crate) value: Arc<T>,
}
impl<T: Expression> SharedExpression<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(value),
        }
    }
}
impl<T: Expression> Expression for SharedExpression<T> {
    type String = T::String;
    type Builtin = T::Builtin;
    fn id(&self) -> HashId {
        self.value.id()
    }
}
impl<T: Expression> GraphNode for SharedExpression<T> {
    fn capture_depth(&self) -> StackOffset {
        self.value.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.value.dynamic_dependencies(deep)
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.value.has_dynamic_dependencies(deep)
    }
    fn is_static(&self) -> bool {
        self.value.is_static()
    }
    fn is_atomic(&self) -> bool {
        self.value.is_atomic()
    }
}
impl<T: Expression> std::fmt::Display for SharedExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl<T: Expression> std::fmt::Debug for SharedExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}
impl<T: Expression> SerializeJson for SharedExpression<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        (*self.value).to_json()
    }
}

impl<T: Expression + Rewritable<T>> Rewritable<T> for SharedExpression<T> {
    fn children(&self) -> Vec<&T> {
        self.value.children()
    }
    fn count_subexpression_usages(
        &self,
        expression: &T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> usize {
        self.value
            .count_subexpression_usages(expression, factory, allocator)
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.value
            .substitute_static(substitutions, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.value
            .substitute_dynamic(deep, state, factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        self.value.hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.value.normalize(factory, allocator, cache)
    }
}
impl<T: Expression + Reducible<T> + Rewritable<T>> Reducible<T> for SharedExpression<T> {
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.value.reduce(factory, allocator, cache)
    }
}
impl<T: Expression + Applicable<T>> Applicable<T> for SharedExpression<T> {
    fn arity(&self) -> Option<crate::core::Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        self.value.apply(args, factory, allocator, cache)
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>> Evaluate<T>
    for SharedExpression<T>
{
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        self.value.evaluate(state, factory, allocator, cache)
    }
}
impl<T: Expression + Compile<T>> Compile<T> for SharedExpression<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
