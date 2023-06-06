// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use serde_json::Value as JsonValue;
use std::{collections::HashSet, sync::Arc};

use reflex::{
    core::{
        Applicable, Arity, CompoundNode, DependencyList, DynamicState, Eagerness, Evaluate,
        EvaluationCache, EvaluationResult, Expression, ExpressionFactory, GraphNode, HeapAllocator,
        Internable, NodeId, Reducible, Rewritable, SerializeJson, StackOffset, Substitutions,
    },
    hash::HashId,
};

#[derive(Eq, Clone, Copy)]
pub struct CachedExpression<TInner> {
    value: TInner,
    id: HashId,
    size: usize,
    capture_depth: StackOffset,
    has_dynamic_dependencies_shallow: bool,
    has_dynamic_dependencies_deep: bool,
}

impl<TInner> CachedExpression<TInner>
where
    TInner: NodeId + GraphNode,
{
    pub fn new(value: TInner) -> Self {
        Self {
            id: value.id(),
            size: value.size(),
            capture_depth: value.capture_depth(),
            has_dynamic_dependencies_shallow: value.has_dynamic_dependencies(false),
            has_dynamic_dependencies_deep: value.has_dynamic_dependencies(true),
            value,
        }
    }
}

impl<TInner> CachedExpression<TInner> {
    pub fn value(&self) -> &TInner {
        &self.value
    }
}

impl<TInner> NodeId for CachedExpression<TInner> {
    fn id(&self) -> HashId {
        self.id
    }
}

impl<TInner> std::hash::Hash for CachedExpression<TInner> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.id);
    }
}

impl<TInner> PartialEq for CachedExpression<TInner>
where
    TInner: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<TInner> GraphNode for CachedExpression<TInner>
where
    TInner: GraphNode,
{
    fn size(&self) -> usize {
        self.size
    }
    fn capture_depth(&self) -> StackOffset {
        self.capture_depth
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.value.count_variable_usages(offset)
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
    fn is_complex(&self) -> bool {
        self.value.is_complex()
    }
}

impl<TInner, TWrapper> CompoundNode<TWrapper> for CachedExpression<TInner>
where
    TInner: CompoundNode<TWrapper>,
    TWrapper: Expression,
{
    type Children<'a> = TInner::Children<'a>
        where
            TWrapper: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        TWrapper: 'a,
    {
        self.value.children()
    }
}

impl<TWrapper, TInner> Rewritable<TWrapper> for CachedExpression<TInner>
where
    TInner: GraphNode + NodeId + Rewritable<TWrapper>,
    TWrapper: Expression + Rewritable<TWrapper>,
{
    fn substitute_static(
        &self,
        substitutions: &Substitutions<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        if substitutions.can_skip(self) {
            return None;
        }
        match cache.retrieve_static_substitution(self, substitutions) {
            Some(result) => result,
            None => {
                let result =
                    self.value()
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
        state: &impl DynamicState<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        if !self.has_dynamic_dependencies(deep) {
            return None;
        }
        match cache.retrieve_dynamic_substitution(self, deep, state) {
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
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
    ) -> Option<TWrapper> {
        self.value.hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        match cache.retrieve_normalization(&self.value) {
            Some(result) => result,
            None => {
                let result = self.value.normalize(factory, allocator, cache);
                cache.store_normalization(&self.value, result.as_ref().cloned());
                result
            }
        }
    }
}

impl<TInner, TWrapper> Reducible<TWrapper> for CachedExpression<TInner>
where
    TInner: GraphNode + NodeId + Reducible<TWrapper>,
    TWrapper: Expression + Reducible<TWrapper>,
{
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
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

impl<TInner, TWrapper> Applicable<TWrapper> for CachedExpression<TInner>
where
    TInner: Applicable<TWrapper>,
    TWrapper: Expression + Applicable<TWrapper>,
{
    fn arity(&self) -> Option<Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Result<TWrapper, String> {
        self.value.apply(args, factory, allocator, cache)
    }
    fn should_parallelize(&self, args: &[TWrapper]) -> bool {
        self.value.should_parallelize(args)
    }
}

impl<TInner> Internable for CachedExpression<TInner>
where
    TInner: Internable,
{
    fn should_intern(&self, eager: Eagerness) -> bool {
        self.value().should_intern(eager)
    }
}

impl<TWrapper, TInner> Evaluate<TWrapper> for CachedExpression<TInner>
where
    TInner: GraphNode + NodeId + Evaluate<TWrapper>,
    TWrapper: Expression + Evaluate<TWrapper>,
{
    fn evaluate(
        &self,
        state: &impl DynamicState<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<EvaluationResult<TWrapper>> {
        if self.is_static() {
            return None;
        }
        match cache.retrieve_evaluation(&self.value, state) {
            Some(result) => result,
            None => {
                let result = self.value.evaluate(state, factory, allocator, cache);
                cache.store_evaluation(&self.value, state, result.as_ref().cloned());
                result
            }
        }
    }
}

impl<TInner> std::fmt::Display for CachedExpression<TInner>
where
    TInner: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

impl<TInner> std::fmt::Debug for CachedExpression<TInner>
where
    TInner: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}

impl<TInner> SerializeJson for CachedExpression<TInner>
where
    TInner: SerializeJson,
{
    fn to_json(&self) -> Result<JsonValue, String> {
        self.value().to_json()
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        if self.id == target.id {
            return Ok(None);
        }
        self.value().patch(target.value())
    }
}

impl<TInner> serde::Serialize for CachedExpression<TInner>
where
    TInner: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<'de, TInner> serde::Deserialize<'de> for CachedExpression<TInner>
where
    TInner: GraphNode + NodeId + serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(TInner::deserialize(deserializer)?))
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct SharedExpression<TInner> {
    pub value: Arc<TInner>,
}

impl<TInner> SharedExpression<TInner> {
    pub fn new(value: TInner) -> Self {
        Self {
            value: Arc::new(value),
        }
    }
    pub fn value(&self) -> &TInner {
        &self.value
    }
}

impl<TInner> NodeId for SharedExpression<TInner>
where
    TInner: NodeId,
{
    fn id(&self) -> HashId {
        self.value.id()
    }
}

impl<TInner> GraphNode for SharedExpression<TInner>
where
    TInner: GraphNode,
{
    fn size(&self) -> usize {
        self.value.size()
    }
    fn capture_depth(&self) -> StackOffset {
        self.value.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.value.count_variable_usages(offset)
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
    fn is_complex(&self) -> bool {
        self.value.is_complex()
    }
}

impl<TInner, TWrapper> CompoundNode<TWrapper> for SharedExpression<TInner>
where
    TInner: CompoundNode<TWrapper>,
    TWrapper: Expression,
{
    type Children<'a> = TInner::Children<'a>
        where
            TWrapper: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        TWrapper: 'a,
    {
        self.value.children()
    }
}

impl<TInner, TWrapper> Rewritable<TWrapper> for SharedExpression<TInner>
where
    TInner: Rewritable<TWrapper>,
    TWrapper: Expression + Rewritable<TWrapper>,
{
    fn substitute_static(
        &self,
        substitutions: &Substitutions<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        self.value
            .substitute_static(substitutions, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        self.value
            .substitute_dynamic(deep, state, factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
    ) -> Option<TWrapper> {
        self.value.hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        self.value.normalize(factory, allocator, cache)
    }
}

impl<TInner, TWrapper> Reducible<TWrapper> for SharedExpression<TInner>
where
    TInner: Reducible<TWrapper>,
    TWrapper: Expression + Reducible<TWrapper>,
{
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<TWrapper> {
        self.value.reduce(factory, allocator, cache)
    }
}

impl<TInner, TWrapper: Expression> Applicable<TWrapper> for SharedExpression<TInner>
where
    TInner: Applicable<TWrapper>,
    TWrapper: Expression + Applicable<TWrapper>,
{
    fn arity(&self) -> Option<Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Result<TWrapper, String> {
        self.value.apply(args, factory, allocator, cache)
    }
    fn should_parallelize(&self, args: &[TWrapper]) -> bool {
        self.value.should_parallelize(args)
    }
}

impl<TInner, TWrapper> Evaluate<TWrapper> for SharedExpression<TInner>
where
    TInner: Evaluate<TWrapper>,
    TWrapper: Expression + Evaluate<TWrapper>,
{
    fn evaluate(
        &self,
        state: &impl DynamicState<TWrapper>,
        factory: &impl ExpressionFactory<TWrapper>,
        allocator: &impl HeapAllocator<TWrapper>,
        cache: &mut impl EvaluationCache<TWrapper>,
    ) -> Option<EvaluationResult<TWrapper>> {
        self.value.evaluate(state, factory, allocator, cache)
    }
}

impl<TInner> Internable for SharedExpression<TInner>
where
    TInner: Internable,
{
    fn should_intern(&self, eager: Eagerness) -> bool {
        self.value().should_intern(eager)
    }
}

impl<TInner> serde::Serialize for SharedExpression<TInner>
where
    TInner: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<'de, TInner> serde::Deserialize<'de> for SharedExpression<TInner>
where
    TInner: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(TInner::deserialize(deserializer)?))
    }
}

impl<TInner> std::fmt::Display for SharedExpression<TInner>
where
    TInner: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

impl<TInner> std::fmt::Debug for SharedExpression<TInner>
where
    TInner: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}

impl<TInner> SerializeJson for SharedExpression<TInner>
where
    TInner: SerializeJson,
{
    fn to_json(&self) -> Result<JsonValue, String> {
        (*self.value).to_json()
    }

    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        (*self.value).patch(target.value())
    }
}
