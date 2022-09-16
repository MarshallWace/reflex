// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, sync::Arc};

use reflex::{
    core::{
        Applicable, Arity, CompoundNode, DependencyList, DynamicState, Eagerness, Evaluate,
        EvaluationCache, EvaluationResult, Expression, ExpressionFactory, GraphNode, HeapAllocator,
        Internable, Reducible, Rewritable, SerializeJson, StackOffset, Substitutions,
    },
    hash::HashId,
};

#[derive(Eq, Clone, Copy)]
pub struct CachedExpression<T: Expression> {
    value: T,
    hash: HashId,
    capture_depth: StackOffset,
    has_dynamic_dependencies_shallow: bool,
    has_dynamic_dependencies_deep: bool,
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
    type Signal<TWrapper: Expression> = T::Signal<TWrapper>;
    type SignalList<TWrapper: Expression> = T::SignalList<TWrapper>;
    type StructPrototype<TWrapper: Expression> = T::StructPrototype<TWrapper>;
    type ExpressionList<TWrapper: Expression> = T::ExpressionList<TWrapper>;
    type NilTerm = T::NilTerm;
    type BooleanTerm = T::BooleanTerm;
    type IntTerm = T::IntTerm;
    type FloatTerm = T::FloatTerm;
    type StringTerm<TWrapper: Expression> = T::StringTerm<TWrapper>;
    type SymbolTerm = T::SymbolTerm;
    type VariableTerm = T::VariableTerm;
    type EffectTerm<TWrapper: Expression> = T::EffectTerm<TWrapper>;
    type LetTerm<TWrapper: Expression> = T::LetTerm<TWrapper>;
    type LambdaTerm<TWrapper: Expression> = T::LambdaTerm<TWrapper>;
    type ApplicationTerm<TWrapper: Expression> = T::ApplicationTerm<TWrapper>;
    type PartialApplicationTerm<TWrapper: Expression> = T::PartialApplicationTerm<TWrapper>;
    type RecursiveTerm<TWrapper: Expression> = T::RecursiveTerm<TWrapper>;
    type BuiltinTerm<TWrapper: Expression> = T::BuiltinTerm<TWrapper>;
    type CompiledFunctionTerm = T::CompiledFunctionTerm;
    type RecordTerm<TWrapper: Expression> = T::RecordTerm<TWrapper>;
    type ConstructorTerm<TWrapper: Expression> = T::ConstructorTerm<TWrapper>;
    type ListTerm<TWrapper: Expression> = T::ListTerm<TWrapper>;
    type HashmapTerm<TWrapper: Expression> = T::HashmapTerm<TWrapper>;
    type HashsetTerm<TWrapper: Expression> = T::HashsetTerm<TWrapper>;
    type SignalTerm<TWrapper: Expression> = T::SignalTerm<TWrapper>;

    type Ref<'a, TTarget> = T::Ref<'a, TTarget>
    where
        TTarget: 'a,
        Self: 'a;

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
impl<T: Expression + CompoundNode<TWrapper>, TWrapper: Expression> CompoundNode<TWrapper>
    for CachedExpression<T>
{
    type Children<'a> = T::Children<'a>
        where
            TWrapper: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a> {
        self.value.children()
    }
}
impl<TWrapper: Expression, T: Expression> Rewritable<TWrapper> for CachedExpression<T>
where
    TWrapper: Rewritable<TWrapper>,
    T: Rewritable<TWrapper>,
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
impl<TWrapper: Expression, T: Expression> Reducible<TWrapper> for CachedExpression<T>
where
    TWrapper: Reducible<TWrapper>,
    T: Reducible<TWrapper>,
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
impl<TWrapper: Expression, T: Expression> Applicable<TWrapper> for CachedExpression<T>
where
    TWrapper: Applicable<TWrapper>,
    T: Applicable<TWrapper>,
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

impl<T: Expression + Internable> Internable for CachedExpression<T> {
    fn should_intern(&self, eager: Eagerness) -> bool {
        self.value().should_intern(eager)
    }
}

impl<TWrapper: Expression, T: Expression> Evaluate<TWrapper> for CachedExpression<T>
where
    TWrapper: Evaluate<TWrapper>,
    T: Evaluate<TWrapper>,
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
impl<T: Expression + serde::Serialize> serde::Serialize for CachedExpression<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}
impl<'de, T: Expression + serde::Deserialize<'de>> serde::Deserialize<'de> for CachedExpression<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(T::deserialize(deserializer)?))
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct SharedExpression<T: Expression> {
    pub value: Arc<T>,
}
impl<T: Expression> SharedExpression<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(value),
        }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
}
impl<T: Expression> Expression for SharedExpression<T> {
    type String = T::String;
    type Builtin = T::Builtin;
    type Signal<TWrapper: Expression> = T::Signal<TWrapper>;
    type SignalList<TWrapper: Expression> = T::SignalList<TWrapper>;
    type StructPrototype<TWrapper: Expression> = T::StructPrototype<TWrapper>;
    type ExpressionList<TWrapper: Expression> = T::ExpressionList<TWrapper>;
    type NilTerm = T::NilTerm;
    type BooleanTerm = T::BooleanTerm;
    type IntTerm = T::IntTerm;
    type FloatTerm = T::FloatTerm;
    type StringTerm<TWrapper: Expression> = T::StringTerm<TWrapper>;
    type SymbolTerm = T::SymbolTerm;
    type VariableTerm = T::VariableTerm;
    type EffectTerm<TWrapper: Expression> = T::EffectTerm<TWrapper>;
    type LetTerm<TWrapper: Expression> = T::LetTerm<TWrapper>;
    type LambdaTerm<TWrapper: Expression> = T::LambdaTerm<TWrapper>;
    type ApplicationTerm<TWrapper: Expression> = T::ApplicationTerm<TWrapper>;
    type PartialApplicationTerm<TWrapper: Expression> = T::PartialApplicationTerm<TWrapper>;
    type RecursiveTerm<TWrapper: Expression> = T::RecursiveTerm<TWrapper>;
    type BuiltinTerm<TWrapper: Expression> = T::BuiltinTerm<TWrapper>;
    type CompiledFunctionTerm = T::CompiledFunctionTerm;
    type RecordTerm<TWrapper: Expression> = T::RecordTerm<TWrapper>;
    type ConstructorTerm<TWrapper: Expression> = T::ConstructorTerm<TWrapper>;
    type ListTerm<TWrapper: Expression> = T::ListTerm<TWrapper>;
    type HashmapTerm<TWrapper: Expression> = T::HashmapTerm<TWrapper>;
    type HashsetTerm<TWrapper: Expression> = T::HashsetTerm<TWrapper>;
    type SignalTerm<TWrapper: Expression> = T::SignalTerm<TWrapper>;

    type Ref<'a, TTarget> = T::Ref<'a, TTarget>
    where
        TTarget: 'a,
        Self: 'a;

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
impl<T: Expression + CompoundNode<TWrapper>, TWrapper: Expression> CompoundNode<TWrapper>
    for SharedExpression<T>
{
    type Children<'a> = T::Children<'a>
        where
            TWrapper: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a> {
        self.value.children()
    }
}
impl<TWrapper: Expression, T: Expression> Rewritable<TWrapper> for SharedExpression<T>
where
    TWrapper: Rewritable<TWrapper>,
    T: Rewritable<TWrapper>,
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
impl<TWrapper: Expression, T: Expression> Reducible<TWrapper> for SharedExpression<T>
where
    TWrapper: Reducible<TWrapper>,
    T: Reducible<TWrapper>,
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
impl<TWrapper: Expression, T: Expression> Applicable<TWrapper> for SharedExpression<T>
where
    TWrapper: Applicable<TWrapper>,
    T: Applicable<TWrapper>,
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
impl<TWrapper: Expression, T: Expression> Evaluate<TWrapper> for SharedExpression<T>
where
    TWrapper: Evaluate<TWrapper>,
    T: Evaluate<TWrapper>,
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

impl<T: Expression + Internable> Internable for SharedExpression<T> {
    fn should_intern(&self, eager: Eagerness) -> bool {
        self.value().should_intern(eager)
    }
}

impl<T: Expression + serde::Serialize> serde::Serialize for SharedExpression<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}
impl<'de, T: Expression + serde::Deserialize<'de>> serde::Deserialize<'de> for SharedExpression<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(T::deserialize(deserializer)?))
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
