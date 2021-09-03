// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use im::OrdSet;
use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    hash::{Hash, Hasher},
    iter::{once, FromIterator},
};

use crate::lang::*;
use crate::{
    compiler::InstructionPointer,
    hash::{hash_object, HashId},
};

pub trait Expression:
    GraphNode
    + std::hash::Hash
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::clone::Clone
    + std::fmt::Debug
    + std::fmt::Display
    + serde::Serialize
where
    Self: std::marker::Sized,
{
    type String: StringValue;
    fn id(&self) -> HashId;
}

pub trait GraphNode {
    fn capture_depth(&self) -> StackOffset;
    fn free_variables(&self) -> HashSet<StackOffset>;
    fn dynamic_dependencies(&self) -> DependencyList;
    /** Weak head normal form - outer term is fully evaluated, sub-expressions may contain dynamic values */
    fn is_static(&self) -> bool;
    /** Term is fully evaluated and contains no sub-expressions */
    fn is_atomic(&self) -> bool;
}

pub trait Rewritable<T: Expression + Rewritable<T>> {
    fn subexpressions(&self) -> Vec<&T>;
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
    fn substitute_dynamic(
        &self,
        state: &DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T>;
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
}

pub trait Reducible<T: Expression + Rewritable<T>> {
    fn is_reducible(&self) -> bool;
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
}

pub trait Applicable<T: Expression> {
    fn arity(&self) -> Option<Arity>;
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>;
}

pub trait Evaluate<T: Expression> {
    fn evaluate(
        &self,
        state: &DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>>;
}

pub trait Iterable {
    fn is_empty(&self) -> bool;
}

#[derive(Eq, PartialEq, Debug)]
pub struct ExpressionList<T: Expression> {
    hash: HashId,
    items: Vec<T>,
}
impl<T: Expression> std::hash::Hash for ExpressionList<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
impl<T: Expression> ExpressionList<T> {
    pub fn new(items: impl IntoIterator<Item = T>) -> Self {
        let items = items.into_iter().collect::<Vec<_>>();
        Self {
            hash: hash_object(&items),
            items,
        }
    }
    pub fn id(&self) -> HashId {
        self.hash
    }
    pub fn get(&self, index: usize) -> Option<&T> {
        self.items.get(index)
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> + DoubleEndedIterator<Item = &T> {
        self.items.iter()
    }
    pub fn as_slice(&self) -> &[T] {
        self.items.as_slice()
    }
}
impl<T: Expression, I: std::slice::SliceIndex<[T]>> std::ops::Index<I> for ExpressionList<T> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.items[index]
    }
}
impl<'a, T: Expression> IntoIterator for ExpressionList<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}
impl<'a, T: Expression> IntoIterator for &'a ExpressionList<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}
impl<T: Expression> GraphNode for ExpressionList<T> {
    fn capture_depth(&self) -> StackOffset {
        self.items
            .iter()
            .map(|term| term.capture_depth())
            .fold(0, |acc, depth| acc.max(depth))
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.items.iter().fold(HashSet::new(), |mut results, term| {
            results.extend(term.free_variables());
            results
        })
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.items
            .iter()
            .fold(DependencyList::empty(), |acc, term| {
                acc.union(term.dynamic_dependencies())
            })
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.items.is_empty()
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct SignalList<T: Expression> {
    signals: BTreeSet<Signal<T>>,
}
impl<T: Expression> SignalList<T> {
    pub fn new(signals: impl IntoIterator<Item = Signal<T>>) -> Self {
        Self {
            signals: signals.into_iter().collect(),
        }
    }
    pub fn len(&self) -> usize {
        self.signals.len()
    }
    pub fn is_empty(&self) -> bool {
        self.signals.is_empty()
    }
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &Signal<T>> {
        self.signals.iter()
    }
}
impl<'a, T: Expression> IntoIterator for SignalList<T> {
    type Item = Signal<T>;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.signals.into_iter()
    }
}
impl<'a, T: Expression> IntoIterator for &'a SignalList<T> {
    type Item = &'a Signal<T>;
    type IntoIter = std::collections::btree_set::Iter<'a, Signal<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.signals.iter()
    }
}

pub trait StringValue:
    std::hash::Hash
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::clone::Clone
    + std::fmt::Debug
    + std::fmt::Display
    + serde::Serialize
where
    Self: std::marker::Sized,
{
    fn as_str(&self) -> &str;
}

pub type StateToken = HashId;

pub struct DynamicState<T: Hash> {
    hash: HashId,
    values: HashMap<StateToken, T>,
}
impl<T: Hash> Hash for DynamicState<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<T: Hash> DynamicState<T> {
    pub fn new() -> Self {
        DynamicState {
            hash: DefaultHasher::new().finish(),
            values: HashMap::new(),
        }
    }
    pub fn id(&self) -> HashId {
        self.hash
    }
    pub fn has(&self, key: StateToken) -> bool {
        self.values.contains_key(&key)
    }
    pub fn get(&self, key: StateToken) -> Option<&T> {
        self.values.get(&key)
    }
    pub fn remove(&mut self, key: &StateToken) -> Option<T> {
        self.values.remove(key)
    }
    pub fn set(&mut self, key: StateToken, value: T) {
        let value_hash = hash_object(&value);
        let previous = self.values.insert(key, value);
        let has_changes = match &previous {
            Some(previous_value) => value_hash != hash_object(previous_value),
            None => true,
        };
        if has_changes {
            self.hash = {
                let mut hasher = DefaultHasher::new();
                hasher.write_u64(self.hash);
                hasher.write_u64(key);
                hasher.write_u64(value_hash);
                hasher.finish()
            }
        }
    }
    pub fn hash_values(&self, state_tokens: impl IntoIterator<Item = StateToken>) -> HashId {
        let mut hasher = DefaultHasher::new();
        for state_token in state_tokens {
            match self.get(state_token) {
                None => hasher.write_u8(0),
                Some(value) => value.hash(&mut hasher),
            }
        }
        hasher.finish()
    }
}

pub trait EvaluationCache<T: Expression> {
    fn retrieve_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
    ) -> Option<Option<T>>;
    fn store_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
        result: Option<T>,
    );
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &T,
        state: &DynamicState<T>,
    ) -> Option<Option<T>>;
    fn store_dynamic_substitution(
        &mut self,
        expression: &T,
        state: &DynamicState<T>,
        result: Option<T>,
    );
    fn retrieve_reduction(&mut self, expression: &T) -> Option<Option<T>>;
    fn store_reduction(&mut self, expression: &T, result: Option<T>);
    fn retrieve_evaluation(
        &mut self,
        expression: &T,
        state: &DynamicState<T>,
    ) -> Option<Option<EvaluationResult<T>>>;
    fn store_evaluation(
        &mut self,
        expression: &T,
        state: &DynamicState<T>,
        result: Option<EvaluationResult<T>>,
    );
    fn metrics(&self) -> Option<&EvaluationCacheMetrics> {
        None
    }
    fn clear_metrics(&mut self) -> Option<EvaluationCacheMetrics> {
        None
    }
}

#[derive(Default)]
pub struct EvaluationCacheMetrics {
    pub(crate) reductions: CacheMetrics,
    pub(crate) static_substitutions: CacheMetrics,
    pub(crate) dynamic_substitutions: CacheMetrics,
    pub(crate) evaluations: CacheMetrics,
}
impl std::fmt::Display for EvaluationCacheMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "Evaluations:           {}\nReductions:            {}\nStatic substitutions:  {}\nDynamic substitutions: {}",
            self.evaluations,
            self.reductions,
            self.static_substitutions,
            self.dynamic_substitutions,
        )
    }
}
#[derive(Default)]
pub struct CacheMetrics {
    num_cache_hits: usize,
    num_cache_misses: usize,
}
impl CacheMetrics {
    pub(crate) fn cache_hit(&mut self) {
        self.num_cache_hits = self.num_cache_hits + 1;
    }
    pub(crate) fn cache_miss(&mut self) {
        self.num_cache_misses = self.num_cache_misses + 1
    }
}
impl std::fmt::Display for CacheMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} operations, {} skipped",
            self.num_cache_hits, self.num_cache_misses
        )
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct StructPrototype {
    keys: Vec<String>,
}
impl StructPrototype {
    pub fn new(keys: impl IntoIterator<Item = String>) -> Self {
        Self {
            keys: keys.into_iter().collect(),
        }
    }
    pub fn keys(&self) -> &[String] {
        &self.keys
    }
    pub fn field(&self, key: &str) -> Option<StructFieldOffset> {
        self.keys
            .iter()
            .enumerate()
            .find_map(|(offset, existing_key)| {
                if *existing_key == *key {
                    Some(offset)
                } else {
                    None
                }
            })
    }
    pub fn parse_struct<T: Expression>(
        &self,
        values: &T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        if let Some(input_values) = factory.match_struct_term(values) {
            let prototype = input_values.prototype();
            if hash_object(prototype) == hash_object(self) {
                Some(values.clone())
            } else if prototype.keys().len() >= self.keys().len() {
                let values = self
                    .keys()
                    .iter()
                    .map(|key| input_values.get(key).cloned())
                    .collect::<Option<Vec<_>>>();
                match values {
                    Some(field_values) => Some(factory.create_struct_term(
                        allocator.clone_struct_prototype(self),
                        allocator.create_list(field_values),
                    )),
                    None => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}
impl std::fmt::Display for StructPrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.keys.iter().cloned().collect::<Vec<_>>().join(",")
        )
    }
}

pub type StructFieldOffset = usize;
pub type StackOffset = usize;

pub trait ExpressionFactory<T: Expression> {
    fn create_value_term(&self, value: ValueTerm<T::String>) -> T;
    fn create_static_variable_term(&self, offset: StackOffset) -> T;
    fn create_dynamic_variable_term(&self, state_token: StateToken, fallback: T) -> T;
    fn create_let_term(&self, initializer: T, body: T) -> T;
    fn create_lambda_term(&self, num_args: StackOffset, body: T) -> T;
    fn create_application_term(&self, target: T, args: ExpressionList<T>) -> T;
    fn create_partial_application_term(&self, target: T, args: ExpressionList<T>) -> T;
    fn create_recursive_term(&self, factory: T) -> T;
    fn create_builtin_term(&self, target: BuiltinTerm) -> T;
    fn create_native_function_term(&self, target: NativeFunction<T>) -> T;
    fn create_compiled_function_term(
        &self,
        hash: HashId,
        address: InstructionPointer,
        num_args: StackOffset,
    ) -> T;
    fn create_tuple_term(&self, fields: ExpressionList<T>) -> T;
    fn create_struct_term(&self, prototype: StructPrototype, fields: ExpressionList<T>) -> T;
    fn create_constructor_term(&self, prototype: StructPrototype, eager: VarArgs) -> T;
    fn create_vector_term(&self, items: ExpressionList<T>) -> T;
    fn create_hashmap_term(&self, keys: ExpressionList<T>, values: ExpressionList<T>) -> T;
    fn create_hashset_term(&self, values: ExpressionList<T>) -> T;
    fn create_signal_term(&self, signals: SignalList<T>) -> T;
    fn create_signal_transformer_term(&self, transform: T) -> T;

    fn match_value_term<'a>(&self, expression: &'a T) -> Option<&'a ValueTerm<T::String>>;
    fn match_static_variable_term<'a>(&self, expression: &'a T) -> Option<&'a StaticVariableTerm>;
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a DynamicVariableTerm<T>>;
    fn match_application_term<'a>(&self, expression: &'a T) -> Option<&'a ApplicationTerm<T>>;
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a PartialApplicationTerm<T>>;
    fn match_let_term<'a>(&self, expression: &'a T) -> Option<&'a LetTerm<T>>;
    fn match_lambda_term<'a>(&self, expression: &'a T) -> Option<&'a LambdaTerm<T>>;
    fn match_builtin_term<'a>(&self, expression: &'a T) -> Option<&'a BuiltinTerm>;
    fn match_native_function_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a NativeFunctionTerm<T>>;
    fn match_compiled_function_term<'a>(&self, target: &'a T) -> Option<&'a CompiledFunctionTerm>;
    fn match_tuple_term<'a>(&self, expression: &'a T) -> Option<&'a TupleTerm<T>>;
    fn match_struct_term<'a>(&self, expression: &'a T) -> Option<&'a StructTerm<T>>;
    fn match_constructor_term<'a>(&self, expression: &'a T) -> Option<&'a ConstructorTerm>;
    fn match_vector_term<'a>(&self, expression: &'a T) -> Option<&'a VectorTerm<T>>;
    fn match_hashmap_term<'a>(&self, expression: &'a T) -> Option<&'a HashMapTerm<T>>;
    fn match_hashset_term<'a>(&self, expression: &'a T) -> Option<&'a HashSetTerm<T>>;
    fn match_signal_term<'a>(&self, expression: &'a T) -> Option<&'a SignalTerm<T>>;
    fn match_signal_transformer_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a SignalTransformerTerm<T>>;
}

pub trait HeapAllocator<T: Expression> {
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> ExpressionList<T>;
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T>;
    fn create_sized_list(
        &self,
        size: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T>;
    fn create_empty_list(&self) -> ExpressionList<T>;
    fn create_unit_list(&self, value: T) -> ExpressionList<T>;
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T>;
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T>;
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T>;
    fn create_signal_list(&self, signals: impl IntoIterator<Item = Signal<T>>) -> SignalList<T>;
    fn create_struct_prototype(
        &self,
        keys: impl IntoIterator<Item = String, IntoIter = impl ExactSizeIterator<Item = String>>,
    ) -> StructPrototype;
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype;
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T>;
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T>;
    fn create_string(&self, value: String) -> T::String;
    fn clone_string(&self, value: &T::String) -> T::String;
    fn as_object(&self) -> &dyn NativeAllocator<T>;
}

impl<'_self, T: Expression> ExpressionFactory<T> for &'_self dyn ExpressionFactory<T> {
    fn create_value_term(&self, value: ValueTerm<T::String>) -> T {
        ExpressionFactory::<T>::create_value_term(*self, value)
    }
    fn create_static_variable_term(&self, offset: StackOffset) -> T {
        ExpressionFactory::<T>::create_static_variable_term(*self, offset)
    }
    fn create_dynamic_variable_term(&self, state_token: StateToken, fallback: T) -> T {
        ExpressionFactory::<T>::create_dynamic_variable_term(*self, state_token, fallback)
    }
    fn create_let_term(&self, initializer: T, body: T) -> T {
        ExpressionFactory::<T>::create_let_term(*self, initializer, body)
    }
    fn create_lambda_term(&self, num_args: StackOffset, body: T) -> T {
        ExpressionFactory::<T>::create_lambda_term(*self, num_args, body)
    }
    fn create_application_term(&self, target: T, args: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_application_term(*self, target, args)
    }
    fn create_partial_application_term(&self, target: T, args: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_partial_application_term(*self, target, args)
    }
    fn create_recursive_term(&self, factory: T) -> T {
        ExpressionFactory::<T>::create_recursive_term(*self, factory)
    }
    fn create_builtin_term(&self, target: BuiltinTerm) -> T {
        ExpressionFactory::<T>::create_builtin_term(*self, target)
    }
    fn create_native_function_term(&self, target: NativeFunction<T>) -> T {
        ExpressionFactory::<T>::create_native_function_term(*self, target)
    }
    fn create_compiled_function_term(
        &self,
        hash: HashId,
        address: InstructionPointer,
        num_args: StackOffset,
    ) -> T {
        ExpressionFactory::<T>::create_compiled_function_term(*self, hash, address, num_args)
    }
    fn create_tuple_term(&self, fields: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_tuple_term(*self, fields)
    }
    fn create_struct_term(&self, prototype: StructPrototype, fields: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_struct_term(*self, prototype, fields)
    }
    fn create_constructor_term(&self, prototype: StructPrototype, eager: VarArgs) -> T {
        ExpressionFactory::<T>::create_constructor_term(*self, prototype, eager)
    }
    fn create_vector_term(&self, items: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_vector_term(*self, items)
    }
    fn create_hashmap_term(&self, keys: ExpressionList<T>, values: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_hashmap_term(*self, keys, values)
    }
    fn create_hashset_term(&self, values: ExpressionList<T>) -> T {
        ExpressionFactory::<T>::create_hashset_term(*self, values)
    }
    fn create_signal_term(&self, signals: SignalList<T>) -> T {
        ExpressionFactory::<T>::create_signal_term(*self, signals)
    }
    fn create_signal_transformer_term(&self, transform: T) -> T {
        ExpressionFactory::<T>::create_signal_transformer_term(*self, transform)
    }

    fn match_value_term<'a>(&self, expression: &'a T) -> Option<&'a ValueTerm<T::String>> {
        ExpressionFactory::<T>::match_value_term(*self, expression)
    }
    fn match_static_variable_term<'a>(&self, expression: &'a T) -> Option<&'a StaticVariableTerm> {
        ExpressionFactory::<T>::match_static_variable_term(*self, expression)
    }
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a DynamicVariableTerm<T>> {
        ExpressionFactory::<T>::match_dynamic_variable_term(*self, expression)
    }
    fn match_application_term<'a>(&self, expression: &'a T) -> Option<&'a ApplicationTerm<T>> {
        ExpressionFactory::<T>::match_application_term(*self, expression)
    }
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a PartialApplicationTerm<T>> {
        ExpressionFactory::<T>::match_partial_application_term(*self, expression)
    }
    fn match_let_term<'a>(&self, expression: &'a T) -> Option<&'a LetTerm<T>> {
        ExpressionFactory::<T>::match_let_term(*self, expression)
    }
    fn match_lambda_term<'a>(&self, expression: &'a T) -> Option<&'a LambdaTerm<T>> {
        ExpressionFactory::<T>::match_lambda_term(*self, expression)
    }
    fn match_builtin_term<'a>(&self, expression: &'a T) -> Option<&'a BuiltinTerm> {
        ExpressionFactory::<T>::match_builtin_term(*self, expression)
    }
    fn match_native_function_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a NativeFunctionTerm<T>> {
        ExpressionFactory::<T>::match_native_function_term(*self, expression)
    }
    fn match_compiled_function_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a CompiledFunctionTerm> {
        ExpressionFactory::<T>::match_compiled_function_term(*self, expression)
    }
    fn match_tuple_term<'a>(&self, expression: &'a T) -> Option<&'a TupleTerm<T>> {
        ExpressionFactory::<T>::match_tuple_term(*self, expression)
    }
    fn match_struct_term<'a>(&self, expression: &'a T) -> Option<&'a StructTerm<T>> {
        ExpressionFactory::<T>::match_struct_term(*self, expression)
    }
    fn match_constructor_term<'a>(&self, expression: &'a T) -> Option<&'a ConstructorTerm> {
        ExpressionFactory::<T>::match_constructor_term(*self, expression)
    }
    fn match_vector_term<'a>(&self, expression: &'a T) -> Option<&'a VectorTerm<T>> {
        ExpressionFactory::<T>::match_vector_term(*self, expression)
    }
    fn match_hashmap_term<'a>(&self, expression: &'a T) -> Option<&'a HashMapTerm<T>> {
        ExpressionFactory::<T>::match_hashmap_term(*self, expression)
    }
    fn match_hashset_term<'a>(&self, expression: &'a T) -> Option<&'a HashSetTerm<T>> {
        ExpressionFactory::<T>::match_hashset_term(*self, expression)
    }
    fn match_signal_term<'a>(&self, expression: &'a T) -> Option<&'a SignalTerm<T>> {
        ExpressionFactory::<T>::match_signal_term(*self, expression)
    }
    fn match_signal_transformer_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a SignalTransformerTerm<T>> {
        ExpressionFactory::<T>::match_signal_transformer_term(*self, expression)
    }
}

pub trait NativeAllocator<T: Expression> {
    fn create_list(&self, expressions: Vec<T>) -> ExpressionList<T>;
    fn create_unsized_list(&self, expressions: Vec<T>) -> ExpressionList<T>;
    fn create_sized_list(&self, size: usize, expressions: Vec<T>) -> ExpressionList<T>;
    fn create_empty_list(&self) -> ExpressionList<T>;
    fn create_unit_list(&self, value: T) -> ExpressionList<T>;
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T>;
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T>;
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T>;
    fn create_signal_list(&self, signals: Vec<Signal<T>>) -> SignalList<T>;
    fn create_struct_prototype(&self, keys: Vec<String>) -> StructPrototype;
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype;
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T>;
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T>;
    fn create_string(&self, value: String) -> T::String;
    fn clone_string(&self, value: &T::String) -> T::String;
}

impl<T: Expression> HeapAllocator<T> for &dyn NativeAllocator<T> {
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> ExpressionList<T> {
        NativeAllocator::<T>::create_list(*self, expressions.into_iter().collect())
    }
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> ExpressionList<T> {
        NativeAllocator::<T>::create_unsized_list(*self, expressions.into_iter().collect())
    }
    fn create_sized_list(
        &self,
        size: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> ExpressionList<T> {
        NativeAllocator::<T>::create_sized_list(*self, size, expressions.into_iter().collect())
    }
    fn create_empty_list(&self) -> ExpressionList<T> {
        NativeAllocator::<T>::create_empty_list(*self)
    }
    fn create_unit_list(&self, value: T) -> ExpressionList<T> {
        NativeAllocator::<T>::create_unit_list(*self, value)
    }
    fn create_pair(&self, left: T, right: T) -> ExpressionList<T> {
        NativeAllocator::<T>::create_pair(*self, left, right)
    }
    fn create_triple(&self, first: T, second: T, third: T) -> ExpressionList<T> {
        NativeAllocator::<T>::create_triple(*self, first, second, third)
    }
    fn clone_list(&self, expressions: &ExpressionList<T>) -> ExpressionList<T> {
        NativeAllocator::<T>::clone_list(*self, expressions)
    }
    fn create_signal_list(&self, signals: impl IntoIterator<Item = Signal<T>>) -> SignalList<T> {
        NativeAllocator::<T>::create_signal_list(*self, signals.into_iter().collect())
    }
    fn create_struct_prototype(
        &self,
        keys: impl IntoIterator<Item = String, IntoIter = impl ExactSizeIterator<Item = String>>,
    ) -> StructPrototype {
        NativeAllocator::<T>::create_struct_prototype(*self, keys.into_iter().collect())
    }
    fn clone_struct_prototype(&self, prototype: &StructPrototype) -> StructPrototype {
        NativeAllocator::<T>::clone_struct_prototype(*self, prototype)
    }
    fn create_signal(&self, signal_type: SignalType, args: ExpressionList<T>) -> Signal<T> {
        NativeAllocator::<T>::create_signal(*self, signal_type, args)
    }
    fn clone_signal(&self, signal: &Signal<T>) -> Signal<T> {
        NativeAllocator::<T>::clone_signal(*self, signal)
    }
    fn create_string(&self, value: String) -> T::String {
        NativeAllocator::<T>::create_string(*self, value)
    }
    fn clone_string(&self, value: &T::String) -> T::String {
        NativeAllocator::<T>::clone_string(*self, value)
    }
    fn as_object(&self) -> &dyn NativeAllocator<T> {
        *self
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Arity {
    eager: usize,
    lazy: usize,
    variadic: Option<VarArgs>,
}
impl Arity {
    pub fn from(eager: usize, lazy: usize, variadic: Option<VarArgs>) -> Self {
        Self {
            eager,
            lazy,
            variadic,
        }
    }
    pub fn eager(&self) -> usize {
        self.eager
    }
    pub fn lazy(&self) -> usize {
        self.lazy
    }
    pub fn variadic(&self) -> Option<VarArgs> {
        self.variadic
    }
    pub fn required(&self) -> usize {
        self.eager + self.lazy
    }
}
impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.eager, self.lazy)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum VarArgs {
    Eager,
    Lazy,
}

pub type SignalId = HashId;

#[derive(Eq, PartialEq, Debug)]
pub struct Signal<T: Expression> {
    hash: HashId,
    signal_type: SignalType,
    args: ExpressionList<T>,
}
impl<T: Expression> Signal<T> {
    pub(crate) fn new(signal_type: SignalType, args: ExpressionList<T>) -> Self {
        let hash = {
            let mut hasher = DefaultHasher::new();
            signal_type.hash(&mut hasher);
            for arg in args.iter() {
                arg.hash(&mut hasher);
            }
            hasher.finish()
        };
        Self {
            hash,
            signal_type,
            args,
        }
    }
    pub fn id(&self) -> SignalId {
        self.hash
    }
    pub fn signal_type(&self) -> &SignalType {
        &self.signal_type
    }
    pub fn args(&self) -> &ExpressionList<T> {
        &self.args
    }
    pub fn is_type(&self, signal_type: &SignalType) -> bool {
        &self.signal_type == signal_type
    }
}
impl<T: Expression> Hash for Signal<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<T: Expression> Ord for Signal<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.cmp(&other.hash)
    }
}
impl<T: Expression> PartialOrd for Signal<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hash.partial_cmp(&other.hash)
    }
}
impl<T: Expression> std::fmt::Display for Signal<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<signal:{}:{}>",
            self.signal_type,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SignalType {
    Error,
    Pending,
    Custom(String),
}
impl std::fmt::Display for SignalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Substitutions<'a, T: Expression> {
    entries: Option<&'a [(StackOffset, T)]>,
    scope_offset: Option<ScopeOffset>,
    min_depth: StackOffset,
    offset: StackOffset,
}
#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
pub(crate) enum ScopeOffset {
    Wrap(usize),
    Unwrap(usize),
}
impl<'a, T: Expression + Rewritable<T>> Substitutions<'a, T> {
    pub(crate) fn named(
        entries: &'a [(StackOffset, T)],
        scope_offset: Option<ScopeOffset>,
    ) -> Self {
        Self {
            entries: Some(entries),
            scope_offset,
            min_depth: entries
                .iter()
                .map(|(offset, _)| offset)
                .fold(StackOffset::MAX, |acc, offset| acc.min(*offset)),
            offset: 0,
        }
    }
    pub(crate) fn wrap(depth: StackOffset) -> Self {
        Self {
            entries: None,
            scope_offset: Some(ScopeOffset::Wrap(depth)),
            min_depth: 0,
            offset: 0,
        }
    }
    #[allow(dead_code)]
    pub(crate) fn unwrap(depth: StackOffset) -> Self {
        Self {
            entries: None,
            scope_offset: Some(ScopeOffset::Unwrap(depth)),
            min_depth: 0,
            offset: 0,
        }
    }
    pub(crate) fn offset(&self, offset: StackOffset) -> Self {
        Self {
            entries: self.entries.as_ref().copied(),
            scope_offset: self.scope_offset.as_ref().copied(),
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    pub(crate) fn can_skip(&self, expression: &T) -> bool {
        let capture_depth = expression.capture_depth();
        if capture_depth == 0 {
            true
        } else {
            expression.capture_depth() - 1 < self.min_depth + self.offset
        }
    }
    pub(crate) fn get(
        &self,
        offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if offset < self.offset {
            return None;
        }
        let target_offset = offset - self.offset;
        let replacement = self.entries.and_then(|entries| {
            entries.iter().find_map(|(offset, replacement)| {
                if *offset == target_offset {
                    Some(replacement)
                } else {
                    None
                }
            })
        });
        match replacement {
            Some(replacement) => Some(
                replacement
                    .substitute_static(&Substitutions::wrap(self.offset), factory, allocator, cache)
                    .unwrap_or_else(|| replacement.clone()),
            ),
            None => match &self.scope_offset {
                Some(scope_offset) => {
                    let target_offset = match scope_offset {
                        ScopeOffset::Wrap(scope_offset) => offset + scope_offset,
                        ScopeOffset::Unwrap(scope_offset) => offset - scope_offset,
                    };
                    if target_offset != offset {
                        Some(factory.create_static_variable_term(target_offset))
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DependencyList {
    state_tokens: Option<OrdSet<StateToken>>,
}
impl DependencyList {
    pub fn empty() -> Self {
        Self { state_tokens: None }
    }
    pub fn of(state_token: StateToken) -> Self {
        Self {
            state_tokens: Some(OrdSet::unit(state_token)),
        }
    }
    pub fn from(state_tokens: impl IntoIterator<Item = StateToken>) -> Self {
        let dependencies = state_tokens.into_iter().collect::<OrdSet<_>>();
        Self {
            state_tokens: if dependencies.is_empty() {
                None
            } else {
                Some(dependencies)
            },
        }
    }
    pub fn len(&self) -> usize {
        match &self.state_tokens {
            None => 0,
            Some(dependencies) => dependencies.len(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.state_tokens.is_none()
    }
    pub fn contains(&self, entries: &BTreeSet<StateToken>) -> bool {
        match &self.state_tokens {
            None => false,
            Some(dependencies) => dependencies
                .iter()
                .any(|dependency| entries.contains(dependency)),
        }
    }
    pub fn insert(&mut self, state_token: StateToken) {
        if let Some(state_tokens) = &mut self.state_tokens {
            state_tokens.insert(state_token);
        } else {
            self.state_tokens = Some(OrdSet::unit(state_token));
        }
    }
    pub fn extend(&mut self, state_tokens: impl IntoIterator<Item = StateToken>) {
        if let Some(existing) = &mut self.state_tokens {
            for state_token in state_tokens {
                existing.insert(state_token);
            }
        } else {
            let state_tokens = OrdSet::from_iter(state_tokens);
            if !state_tokens.is_empty() {
                self.state_tokens = Some(state_tokens);
            }
        }
    }
    pub fn union(self, other: Self) -> Self {
        match self.state_tokens {
            None => other,
            Some(existing) => Self {
                state_tokens: Some(match other.state_tokens {
                    Some(other) => existing.union(other),
                    None => existing,
                }),
            },
        }
    }
    pub fn iter(&self) -> DependencyListIterator {
        match &self.state_tokens {
            Some(dependencies) => DependencyListIterator::Some(dependencies.iter()),
            None => DependencyListIterator::None,
        }
    }
}
impl<'a> IntoIterator for &'a DependencyList {
    type Item = StateToken;
    type IntoIter = DependencyListIterator<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
pub enum DependencyListIterator<'a> {
    Some(im::ordset::Iter<'a, StateToken>),
    None,
}
impl<'a> Iterator for DependencyListIterator<'a> {
    type Item = StateToken;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Some(iter) => iter.next().copied(),
            Self::None => None,
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
}
impl<'a> ExactSizeIterator for DependencyListIterator<'a> {
    fn len(&self) -> usize {
        match self {
            Self::Some(items) => items.len(),
            Self::None => 0,
        }
    }
}

pub(crate) fn match_typed_expression_list<'a, T: Expression + 'a, V, E>(
    expressions: impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
    matcher: impl Fn(&'a T) -> Option<V>,
    err: impl Fn(&'a T) -> E,
) -> Result<impl IntoIterator<Item = V, IntoIter = impl ExactSizeIterator<Item = V>>, E> {
    expressions
        .into_iter()
        .map(|item| matcher(item).ok_or_else(|| err(item)))
        // TODO: Avoid intermediate allocation
        .collect::<Result<Vec<_>, _>>()
}

pub(crate) fn transform_expression_list<T: Expression>(
    expressions: &ExpressionList<T>,
    allocator: &impl HeapAllocator<T>,
    mut transform: impl FnMut(&T) -> Option<T>,
) -> Option<ExpressionList<T>> {
    let mut result: Option<Vec<T>> = None;
    for (index, expression) in expressions.iter().enumerate() {
        let replaced = transform(expression);
        match replaced {
            None => {}
            Some(replaced) => match result {
                Some(ref mut results) => {
                    results[index] = replaced;
                }
                None => {
                    result = Some(
                        expressions
                            .iter()
                            .take(index)
                            .cloned()
                            .chain(once(replaced))
                            .chain(expressions.iter().skip(index + 1).cloned())
                            .collect::<Vec<_>>(),
                    );
                }
            },
        }
    }
    match result {
        None => None,
        Some(result) => Some(allocator.create_list(result)),
    }
}

pub fn evaluate<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>>(
    expression: &T,
    state: &DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> EvaluationResult<T> {
    expression
        .evaluate(state, factory, allocator, cache)
        .unwrap_or_else(|| EvaluationResult::new(expression.clone(), DependencyList::empty()))
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluationResult<T: Expression> {
    result: T,
    dependencies: DependencyList,
}
impl<T: Expression> EvaluationResult<T> {
    pub fn new(result: T, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result,
            dependencies,
        }
    }
    pub fn result(&self) -> &T {
        &self.result
    }
    pub fn dependencies(&self) -> &DependencyList {
        &self.dependencies
    }
    pub fn into_parts(self) -> (T, DependencyList) {
        (self.result, self.dependencies)
    }
}

#[cfg(test)]
mod tests {
    use std::iter::once;

    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        hash::{hash_object, HashId},
        lang::{term::NativeFunction, BuiltinTerm, TermFactory, ValueTerm},
        parser::sexpr::parse,
    };

    use super::{
        evaluate, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
        ExpressionFactory, ExpressionList, GraphNode, HeapAllocator, NativeAllocator, Rewritable,
        Signal, SignalType,
    };

    fn create_error_signal_term<T: Expression>(
        message: impl Into<String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T {
        factory.create_signal_term(
            allocator.create_signal_list(once(create_error_signal(message, factory, allocator))),
        )
    }

    fn create_error_signal<T: Expression>(
        message: impl Into<String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Signal<T> {
        allocator.create_signal(
            SignalType::Error,
            allocator.create_unit_list(
                factory
                    .create_value_term(ValueTerm::String(allocator.create_string(message.into()))),
            ),
        )
    }

    #[test]
    fn value_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("3", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn lambda_optimizations() {
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(lambda (foo) foo)", &factory, &allocator).unwrap();
        let result = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(result, None);

        let expression = parse("(lambda (foo bar) (+ foo bar))", &factory, &allocator).unwrap();
        let normalized = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+", &factory, &allocator).unwrap()));

        let expression = parse("(lambda (foo bar baz) (+ foo bar))", &factory, &allocator).unwrap();
        let normalized = expression.normalize(&factory, &allocator, &mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+", &factory, &allocator).unwrap()));
    }

    #[test]
    fn lambda_application_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("((lambda (foo bar) (+ foo bar)) 3 4)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("((lambda (foo bar) (((lambda (foo bar) (lambda (foo bar) (+ foo bar))) foo bar) foo bar)) 3 4)", &factory, &allocator)
                .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn invalid_function_applications() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term("Expected 1 argument, received 0", &factory, &allocator),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "((lambda (first second third) first) 3)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term("Expected 3 arguments, received 1", &factory, &allocator),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "((lambda (first second third) first) 3 4)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term("Expected 3 arguments, received 2", &factory, &allocator),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("+", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_builtin_term(BuiltinTerm::Add),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_application_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ 1 2)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn nested_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4 + 5)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn signal_short_circuiting() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((/ 3 0) 4 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<builtin:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) 4)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<builtin:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ 3 (/ 4 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<builtin:Divide>: Division by zero: 4 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 4 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_signal_term(HeapAllocator::create_signal_list(
                    &allocator,
                    vec![
                        create_error_signal(
                            "<builtin:Divide>: Division by zero: 3 / 0",
                            &factory,
                            &allocator,
                        ),
                        create_error_signal(
                            "<builtin:Divide>: Division by zero: 4 / 0",
                            &factory,
                            &allocator,
                        ),
                    ]
                )),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 3 0))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<builtin:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (+ (/ 3 0) 4) 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<builtin:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(let () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(let ((foo 3) (bar 4)) (+ foo bar))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(let ((first 3)) (let ((second 4)) first))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn letrec_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(letrec () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo 3) (bar 4)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((first 3)) (let ((second 4)) first))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4)),
                DependencyList::empty(),
            ),
        );

        let expression = parse(
            "(letrec ((foo 3) (bar foo)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo bar) (bar 3)) (+ foo bar))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo 3) (bar (+ foo 1))) bar)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 1)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo (+ bar 1)) (bar 3)) foo)",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 1)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(5 * 4 * 3 * 2 * 1)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-even? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Boolean(false)),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Boolean(true)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo (cons 3 foo))) (car (cdr (cdr (cdr foo)))))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );
        let expression = parse(
            "(letrec ((foo (cons 1 bar)) (bar (cons 2 foo))) (car (cdr (cdr (cdr foo)))))",
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(2)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn native_functions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();

        struct Constant {}
        impl Constant {
            fn name() -> Option<&'static str> {
                Some("Constant")
            }
            fn uid() -> HashId {
                hash_object(&std::any::TypeId::of::<Self>())
            }
            fn arity() -> Arity {
                Arity::from(0, 0, None)
            }
            fn apply<T: Expression>(
                _args: ExpressionList<T>,
                factory: &dyn ExpressionFactory<T>,
                _allocator: &dyn NativeAllocator<T>,
            ) -> Result<T, String> {
                Ok(factory.create_value_term(ValueTerm::Int(3)))
            }
        }

        let expression = factory.create_application_term(
            factory.create_native_function_term(NativeFunction::new(
                Constant::uid(),
                Constant::name(),
                Constant::arity(),
                Constant::apply,
            )),
            HeapAllocator::create_empty_list(&allocator),
        );
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3)),
                DependencyList::empty(),
            ),
        );

        struct Add3 {}
        impl Add3 {
            fn name() -> Option<&'static str> {
                Some("Add3")
            }
            fn uid() -> HashId {
                hash_object(&std::any::TypeId::of::<Self>())
            }
            fn arity() -> Arity {
                Arity::from(0, 0, None)
            }
            fn apply<T: Expression>(
                args: ExpressionList<T>,
                factory: &dyn ExpressionFactory<T>,
                _allocator: &dyn NativeAllocator<T>,
            ) -> Result<T, String> {
                let mut args = args.into_iter();
                let first = args.next().unwrap();
                let second = args.next().unwrap();
                let third = args.next().unwrap();
                match (
                    factory.match_value_term(&first),
                    factory.match_value_term(&second),
                    factory.match_value_term(&third),
                ) {
                    (
                        Some(ValueTerm::Int(first)),
                        Some(ValueTerm::Int(second)),
                        Some(ValueTerm::Int(third)),
                    ) => Ok(factory.create_value_term(ValueTerm::Int(first + second + third))),
                    _ => Err(String::from("Invalid arguments")),
                }
            }
        }

        let expression = factory.create_application_term(
            factory.create_native_function_term(NativeFunction::new(
                Add3::uid(),
                Add3::name(),
                Add3::arity(),
                Add3::apply,
            )),
            HeapAllocator::create_list(
                &allocator,
                vec![
                    factory.create_value_term(ValueTerm::Int(3)),
                    factory.create_value_term(ValueTerm::Int(4)),
                    factory.create_value_term(ValueTerm::Int(5)),
                ],
            ),
        );
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(3 + 4 + 5)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn partial_evaluation() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let expression = parse(
            "((lambda (foo) ((lambda (bar) (foo 3)) #f)) (lambda (foo) #t))",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_value_term(ValueTerm::Boolean(true)))
        );

        let expression = parse(
            "
            (let ((identity (lambda (value) value)))
                (let ((identity2 (lambda (value) (identity value))))
                    ((lambda (value) (identity2 (* value 2)))
                      3)))
        ",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_value_term(ValueTerm::Int(6)))
        );
    }
}
