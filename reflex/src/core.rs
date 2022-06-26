// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::convert::TryFrom;
use std::rc::Rc;
use std::sync::Arc;
use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashSet},
    hash::{Hash, Hasher},
    iter::{once, repeat, FromIterator},
};

use fnv::FnvHashMap;
use im::OrdSet;
use serde::{Deserialize, Serialize};
pub use uuid::{uuid, Uuid};

use crate::lang::*;
use crate::{
    compiler::InstructionPointer,
    hash::{hash_object, HashId},
};

pub trait Expression:
    GraphNode
    + SerializeJson
    + std::hash::Hash
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::clone::Clone
    + std::fmt::Debug
    + std::fmt::Display
where
    Self: std::marker::Sized,
{
    type String: StringValue;
    type Builtin: Builtin;
    fn id(&self) -> HashId;
}

pub trait Builtin:
    Uid
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::fmt::Debug
    + std::hash::Hash
    + std::marker::Copy
    + TryFrom<Uuid, Error = ()>
    + std::fmt::Display
{
    fn arity<T: Expression<Builtin = Self>>(&self) -> Arity;
    fn apply<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>;
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(&self, args: &[T])
        -> bool;
}

pub trait Uid {
    fn uid(&self) -> Uuid;
}

pub trait GraphNode {
    fn capture_depth(&self) -> StackOffset;
    fn free_variables(&self) -> HashSet<StackOffset>;
    fn count_variable_usages(&self, offset: StackOffset) -> usize;
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList;
    fn has_dynamic_dependencies(&self, deep: bool) -> bool;
    /// Weak head normal form - outer term is fully evaluated, sub-expressions may contain dynamic values
    fn is_static(&self) -> bool;
    /// Term is fully evaluated and contains no dynamic sub-expressions
    fn is_atomic(&self) -> bool;
    /// Term contains sub-expressions
    fn is_complex(&self) -> bool;
}

pub trait CompoundNode<'a, T: Expression + 'a> {
    type Children: Iterator<Item = &'a T>;
    fn children(&'a self) -> Self::Children;
}

pub trait Rewritable<T: Expression> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
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

pub trait Reducible<T: Expression> {
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
    fn should_parallelize(&self, args: &[T]) -> bool;
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>;
}

pub trait Evaluate<T: Expression> {
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>>;
}

pub trait SerializeJson {
    fn to_json(&self) -> Result<serde_json::Value, String>;
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum ArgType {
    /** Evaluate argument before function application, short-circuiting any signals encountered during evaluation */
    Strict,
    /** Evaluate argument before function application, passing signals into function body */
    Eager,
    /** Pass argument directly into function body without evaluating */
    Lazy,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct FunctionArity<const REQUIRED: usize, const OPTIONAL: usize> {
    pub required: [ArgType; REQUIRED],
    pub optional: [ArgType; OPTIONAL],
    pub variadic: Option<ArgType>,
}
impl<const REQUIRED: usize, const OPTIONAL: usize> FunctionArity<REQUIRED, OPTIONAL> {
    pub fn required(&self) -> &[ArgType] {
        &self.required
    }
    pub fn optional(&self) -> &[ArgType] {
        &self.optional
    }
    pub fn variadic(&self) -> Option<ArgType> {
        self.variadic
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Arity {
    /** Homogeneous arity arguments all have the same eagerness requirements */
    Homogeneous(HomogeneousArity),
    /** Heterogeneous arity arguments can specify a mixture of eagerness requirements */
    Heterogeneous(HeterogeneousArity),
}
impl<const R: usize, const O: usize> From<&'static FunctionArity<R, O>> for Arity {
    fn from(definition: &'static FunctionArity<R, O>) -> Self {
        Self::Heterogeneous(HeterogeneousArity::from(definition))
    }
}
impl Arity {
    pub fn strict(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Strict,
            required,
            optional,
            variadic,
        })
    }
    pub fn eager(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Eager,
            required,
            optional,
            variadic,
        })
    }
    pub fn lazy(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Lazy,
            required,
            optional,
            variadic,
        })
    }
    pub fn required(&self) -> impl ExactSizeIterator<Item = ArgType> {
        match self {
            Self::Homogeneous(arity) => PositionalArityIterator::Homogeneous(arity.required()),
            Self::Heterogeneous(arity) => PositionalArityIterator::Heterogeneous(arity.required()),
        }
    }
    pub fn optional(&self) -> impl ExactSizeIterator<Item = ArgType> {
        match self {
            Self::Homogeneous(arity) => PositionalArityIterator::Homogeneous(arity.optional()),
            Self::Heterogeneous(arity) => PositionalArityIterator::Heterogeneous(arity.optional()),
        }
    }
    pub fn variadic(&self) -> Option<ArgType> {
        match self {
            Self::Homogeneous(arity) => arity.variadic(),
            Self::Heterogeneous(arity) => arity.variadic(),
        }
    }
    pub fn partial(&self, offset: usize) -> Self {
        match self {
            Self::Homogeneous(arity) => Self::Homogeneous(arity.partial(offset)),
            Self::Heterogeneous(arity) => Self::Heterogeneous(arity.partial(offset)),
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = ArgType> {
        self.required()
            .chain(self.optional())
            .chain(self.variadic().into_iter().flat_map(repeat))
    }
}
impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Homogeneous(arity) => std::fmt::Display::fmt(arity, f),
            Self::Heterogeneous(arity) => std::fmt::Display::fmt(arity, f),
        }
    }
}
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct HomogeneousArity {
    arity_type: ArgType,
    required: usize,
    optional: usize,
    variadic: bool,
}
impl HomogeneousArity {
    fn required(&self) -> HomogeneousArityIterator {
        HomogeneousArityIterator(self.required)
    }
    fn optional(&self) -> HomogeneousArityIterator {
        HomogeneousArityIterator(self.optional)
    }
    fn variadic(&self) -> Option<ArgType> {
        match self.variadic {
            true => Some(self.arity_type),
            false => None,
        }
    }
    fn partial(&self, offset: usize) -> Self {
        Self {
            arity_type: self.arity_type,
            required: self.required.saturating_sub(offset),
            optional: self
                .optional
                .saturating_sub(offset.saturating_sub(self.required)),
            variadic: self.variadic,
        }
    }
}
impl std::fmt::Display for HomogeneousArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.required + self.optional,
            if self.variadic {
                String::from("+")
            } else {
                String::from("")
            }
        )
    }
}
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct HeterogeneousArity {
    required: &'static [ArgType],
    optional: &'static [ArgType],
    variadic: Option<ArgType>,
}
impl<'a, const R: usize, const O: usize> From<&'static FunctionArity<R, O>> for HeterogeneousArity {
    fn from(definition: &'static FunctionArity<R, O>) -> Self {
        Self {
            required: definition.required(),
            optional: definition.optional(),
            variadic: definition.variadic(),
        }
    }
}
impl HeterogeneousArity {
    fn required(&self) -> HeterogeneousArityIterator {
        HeterogeneousArityIterator(self.required, 0)
    }
    fn optional(&self) -> HeterogeneousArityIterator {
        HeterogeneousArityIterator(self.optional, 0)
    }
    fn variadic(&self) -> Option<ArgType> {
        self.variadic
    }
    fn partial(&self, offset: usize) -> Self {
        Self {
            required: &self.required[offset.min(self.required.len())..],
            optional: &self.optional[offset
                .saturating_sub(self.required.len())
                .min(self.optional.len())..],
            variadic: self.variadic,
        }
    }
}
impl std::fmt::Display for HeterogeneousArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.required.len() + self.optional.len(),
            if self.variadic.is_some() {
                String::from("+")
            } else {
                String::from("")
            }
        )
    }
}
pub enum PositionalArityIterator {
    Homogeneous(HomogeneousArityIterator),
    Heterogeneous(HeterogeneousArityIterator),
}
impl Iterator for PositionalArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Homogeneous(iter) => iter.next(),
            Self::Heterogeneous(iter) => iter.next(),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Homogeneous(iter) => iter.size_hint(),
            Self::Heterogeneous(iter) => iter.size_hint(),
        }
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            Self::Homogeneous(iter) => iter.count(),
            Self::Heterogeneous(iter) => iter.count(),
        }
    }
}
impl ExactSizeIterator for PositionalArityIterator {
    fn len(&self) -> usize {
        match self {
            Self::Homogeneous(iter) => iter.len(),
            Self::Heterogeneous(iter) => iter.len(),
        }
    }
}
pub struct HomogeneousArityIterator(usize);
impl Iterator for HomogeneousArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        let remaining = self.0;
        if remaining == 0 {
            None
        } else {
            self.0 -= 1;
            Some(ArgType::Lazy)
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let len = ExactSizeIterator::len(&self);
        len
    }
}
impl ExactSizeIterator for HomogeneousArityIterator {
    fn len(&self) -> usize {
        let remaining = self.0;
        remaining
    }
}
pub struct HeterogeneousArityIterator(&'static [ArgType], usize);
impl Iterator for HeterogeneousArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        let args = self.0;
        let index = self.1;
        let arg = args.get(index)?;
        self.1 += 1;
        Some(*arg)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let len = ExactSizeIterator::len(&self);
        len
    }
}
impl ExactSizeIterator for HeterogeneousArityIterator {
    fn len(&self) -> usize {
        let args = self.0;
        let index = self.1;
        let remaining = args.len() - index;
        remaining
    }
}

pub type ExpressionListSlice<'a, T> = std::slice::Iter<'a, T>;

#[derive(Eq, PartialEq, Clone, Debug)]
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
    pub fn iter<'a>(&'a self) -> ExpressionListSlice<'a, T> {
        self.items.iter()
    }
    pub fn as_slice(&self) -> &[T] {
        self.items.as_slice()
    }
    pub fn into_values(self) -> Vec<T> {
        self.items
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
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.items.iter().fold(0, |results, term| {
            results + term.count_variable_usages(offset)
        })
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.items
            .iter()
            .fold(DependencyList::empty(), |acc, term| {
                acc.union(term.dynamic_dependencies(deep))
            })
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.items
            .iter()
            .any(|term| term.has_dynamic_dependencies(deep))
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.items.iter().all(|item| item.is_atomic())
    }
    fn is_complex(&self) -> bool {
        true
    }
}
impl<T: Expression> serde::Serialize for ExpressionList<T>
where
    T: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedExpressionList::from(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for ExpressionList<T>
where
    T: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedExpressionList::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedExpressionList<T: Expression>(Vec<T>);
impl<'a, T: Expression> From<&'a ExpressionList<T>> for SerializedExpressionList<T> {
    fn from(value: &'a ExpressionList<T>) -> Self {
        let ExpressionList { items, hash: _ } = value;
        SerializedExpressionList(items.iter().cloned().collect())
    }
}
impl<T: Expression> From<SerializedExpressionList<T>> for ExpressionList<T> {
    fn from(value: SerializedExpressionList<T>) -> Self {
        let SerializedExpressionList(items) = value;
        Self::new(items)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
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
impl<T: Expression> serde::Serialize for SignalList<T>
where
    T: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedSignalList::from(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for SignalList<T>
where
    T: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedSignalList::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedSignalList<T: Expression>(Vec<Signal<T>>);
impl<'a, T: Expression> From<&'a SignalList<T>> for SerializedSignalList<T> {
    fn from(value: &'a SignalList<T>) -> Self {
        let SignalList { signals } = value;
        SerializedSignalList(signals.iter().cloned().collect())
    }
}
impl<T: Expression> From<SerializedSignalList<T>> for SignalList<T> {
    fn from(value: SerializedSignalList<T>) -> Self {
        let SerializedSignalList(signals) = value;
        Self::new(signals)
    }
}

pub trait StringValue:
    From<String>
    + for<'a> From<&'a str>
    + std::hash::Hash
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::clone::Clone
    + std::fmt::Debug
    + std::fmt::Display
where
    Self: std::marker::Sized,
{
    fn as_str(&self) -> &str;
    fn from_static(_self: Option<Self>, value: &'static str) -> Self;
}
impl StringValue for String {
    fn as_str(&self) -> &str {
        self.as_str()
    }
    fn from_static(_self: Option<Self>, value: &'static str) -> Self {
        Self::from(value)
    }
}

pub type StateToken = HashId;

pub trait DynamicState<T: Hash> {
    fn id(&self) -> HashId;
    fn has(&self, key: &StateToken) -> bool;
    fn get(&self, key: &StateToken) -> Option<&T>;
}

pub struct StateCache<T: Hash> {
    hash: HashId,
    values: FnvHashMap<StateToken, T>,
}
impl<T: Hash> Default for StateCache<T> {
    fn default() -> Self {
        Self {
            hash: DefaultHasher::new().finish(),
            values: FnvHashMap::default(),
        }
    }
}
impl<T: Hash> Hash for StateCache<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<T: Hash> DynamicState<T> for StateCache<T> {
    fn id(&self) -> HashId {
        self.hash
    }
    fn has(&self, key: &StateToken) -> bool {
        self.values.contains_key(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        self.values.get(key)
    }
}
impl<T: Hash> StateCache<T> {
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
    pub fn remove(&mut self, key: &StateToken) -> Option<T> {
        let result = self.values.remove(key);
        if result.is_some() {
            let mut hasher = DefaultHasher::new();
            hasher.write_u64(self.hash);
            hasher.write_u64(*key);
            hasher.write_u8(0);
            self.hash = hasher.finish();
        }
        result
    }
    pub fn extend(&mut self, entries: impl IntoIterator<Item = (StateToken, T)>) {
        let mut hasher = DefaultHasher::new();
        hasher.write_u64(self.hash);
        for (key, value) in entries {
            hasher.write_u64(key);
            hasher.write_u64(hash_object(&value));
            self.set(key, value);
        }
        self.hash = hasher.finish();
    }
    pub fn gc(&mut self, retained_keys: &DependencyList) {
        let mut hasher = DefaultHasher::new();
        hasher.write_u64(self.hash);
        self.values.retain(|key, _| {
            if retained_keys.contains(*key) {
                true
            } else {
                hasher.write_u64(*key);
                hasher.write_u8(0);
                false
            }
        });
        self.values.shrink_to_fit();
        self.hash = hasher.finish();
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
}
impl<T: Hash> FromIterator<(StateToken, T)> for StateCache<T> {
    fn from_iter<I: IntoIterator<Item = (StateToken, T)>>(iter: I) -> Self {
        let mut hasher = DefaultHasher::new();
        let mut values = FnvHashMap::default();
        for (key, value) in iter {
            hasher.write_u64(key);
            std::hash::Hash::hash(&value, &mut hasher);
            values.insert(key, value);
        }
        Self {
            hash: hasher.finish(),
            values,
        }
    }
}
impl<T: Hash> DynamicState<T> for Rc<StateCache<T>> {
    fn id(&self) -> HashId {
        (&**self).id()
    }
    fn has(&self, key: &StateToken) -> bool {
        (&**self).has(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        (&**self).get(key)
    }
}
impl<T: Hash> DynamicState<T> for Arc<StateCache<T>> {
    fn id(&self) -> HashId {
        (&**self).id()
    }
    fn has(&self, key: &StateToken) -> bool {
        (&**self).has(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        (&**self).get(key)
    }
}

pub fn hash_state_values<T: Expression>(
    state: &impl DynamicState<T>,
    state_tokens: impl IntoIterator<Item = StateToken>,
) -> HashId {
    let mut hasher = DefaultHasher::new();
    for state_token in state_tokens {
        match state.get(&state_token) {
            None => hasher.write_u8(0),
            Some(value) => value.hash(&mut hasher),
        }
    }
    hasher.finish()
}

pub trait EvaluationCache<T: Expression> {
    fn retrieve_static_substitution(
        &mut self,
        expression: &impl Expression,
        substitutions: &Substitutions<T>,
    ) -> Option<Option<T>>;
    fn store_static_substitution(
        &mut self,
        expression: &impl Expression,
        substitutions: &Substitutions<T>,
        result: Option<T>,
    );
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &impl Expression,
        deep: bool,
        state: &impl DynamicState<T>,
    ) -> Option<Option<T>>;
    fn store_dynamic_substitution(
        &mut self,
        expression: &impl Expression,
        deep: bool,
        state: &impl DynamicState<T>,
        result: Option<T>,
    );
    fn retrieve_reduction(&mut self, expression: &impl Expression) -> Option<Option<T>>;
    fn store_reduction(&mut self, expression: &impl Expression, result: Option<T>);
    fn retrieve_normalization(&mut self, expression: &impl Expression) -> Option<Option<T>>;
    fn store_normalization(&mut self, expression: &impl Expression, result: Option<T>);
    fn retrieve_evaluation(
        &mut self,
        expression: &impl Expression,
        state: &impl DynamicState<T>,
    ) -> Option<Option<EvaluationResult<T>>>;
    fn store_evaluation(
        &mut self,
        expression: &impl Expression,
        state: &impl DynamicState<T>,
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

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
        if let Some(input_values) = factory.match_record_term(values) {
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
                    Some(field_values) => Some(factory.create_record_term(
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
    fn create_nil_term(&self) -> T;
    fn create_boolean_term(&self, value: bool) -> T;
    fn create_int_term(&self, value: IntValue) -> T;
    fn create_float_term(&self, value: FloatValue) -> T;
    fn create_string_term(&self, value: T::String) -> T;
    fn create_symbol_term(&self, value: SymbolId) -> T;
    fn create_static_variable_term(&self, offset: StackOffset) -> T;
    fn create_dynamic_variable_term(&self, state_token: StateToken, fallback: T) -> T;
    fn create_let_term(&self, initializer: T, body: T) -> T;
    fn create_lambda_term(&self, num_args: StackOffset, body: T) -> T;
    fn create_application_term(&self, target: T, args: ExpressionList<T>) -> T;
    fn create_partial_application_term(&self, target: T, args: ExpressionList<T>) -> T;
    fn create_recursive_term(&self, factory: T) -> T;
    fn create_builtin_term(&self, target: impl Into<T::Builtin>) -> T;
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> T;
    fn create_record_term(&self, prototype: StructPrototype, fields: ExpressionList<T>) -> T;
    fn create_constructor_term(&self, prototype: StructPrototype) -> T;
    fn create_list_term(&self, items: ExpressionList<T>) -> T;
    fn create_hashmap_term(&self, keys: ExpressionList<T>, values: ExpressionList<T>) -> T;
    fn create_hashset_term(&self, values: ExpressionList<T>) -> T;
    fn create_signal_term(&self, signals: SignalList<T>) -> T;

    fn match_nil_term<'a>(&self, expression: &'a T) -> Option<&'a NilTerm>;
    fn match_boolean_term<'a>(&self, expression: &'a T) -> Option<&'a BooleanTerm>;
    fn match_int_term<'a>(&self, expression: &'a T) -> Option<&'a IntTerm>;
    fn match_float_term<'a>(&self, expression: &'a T) -> Option<&'a FloatTerm>;
    fn match_string_term<'a>(&self, expression: &'a T) -> Option<&'a StringTerm<T::String>>;
    fn match_symbol_term<'a>(&self, expression: &'a T) -> Option<&'a SymbolTerm>;
    fn match_static_variable_term<'a>(&self, expression: &'a T) -> Option<&'a StaticVariableTerm>;
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a DynamicVariableTerm<T>>;
    fn match_let_term<'a>(&self, expression: &'a T) -> Option<&'a LetTerm<T>>;
    fn match_lambda_term<'a>(&self, expression: &'a T) -> Option<&'a LambdaTerm<T>>;
    fn match_application_term<'a>(&self, expression: &'a T) -> Option<&'a ApplicationTerm<T>>;
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a PartialApplicationTerm<T>>;
    fn match_recursive_term<'a>(&self, expression: &'a T) -> Option<&'a RecursiveTerm<T>>;
    fn match_builtin_term<'a>(&self, expression: &'a T) -> Option<&'a BuiltinTerm<T>>;
    fn match_compiled_function_term<'a>(&self, target: &'a T) -> Option<&'a CompiledFunctionTerm>;
    fn match_record_term<'a>(&self, expression: &'a T) -> Option<&'a RecordTerm<T>>;
    fn match_constructor_term<'a>(&self, expression: &'a T) -> Option<&'a ConstructorTerm>;
    fn match_list_term<'a>(&self, expression: &'a T) -> Option<&'a ListTerm<T>>;
    fn match_hashmap_term<'a>(&self, expression: &'a T) -> Option<&'a HashMapTerm<T>>;
    fn match_hashset_term<'a>(&self, expression: &'a T) -> Option<&'a HashSetTerm<T>>;
    fn match_signal_term<'a>(&self, expression: &'a T) -> Option<&'a SignalTerm<T>>;
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
    fn create_string(&self, value: impl Into<T::String>) -> T::String;
    fn create_static_string(&self, value: &'static str) -> T::String;
    fn clone_string(&self, value: &T::String) -> T::String;
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize)]
pub enum VarArgs {
    Eager,
    Lazy,
}

pub type SignalId = HashId;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Signal<T: Expression> {
    hash: HashId,
    signal_type: SignalType,
    args: ExpressionList<T>,
}
impl<T: Expression> Signal<T> {
    pub fn new(signal_type: SignalType, args: ExpressionList<T>) -> Self {
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
impl<T: Expression> serde::Serialize for Signal<T>
where
    T: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Into::<SerializedSignal<T>>::into(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for Signal<T>
where
    T: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok((SerializedSignal::<T>::deserialize(deserializer)?).into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedSignal<T: Expression> {
    signal_type: SerializedSignalType,
    custom_type: Option<String>,
    args: ExpressionList<T>,
}
impl<'a, T: Expression> Into<SerializedSignal<T>> for &'a Signal<T> {
    fn into(self) -> SerializedSignal<T> {
        let (signal_type, custom_type) = serialize_signal_type(&self.signal_type);
        let args = self.args.clone();
        SerializedSignal {
            signal_type,
            custom_type,
            args,
        }
    }
}
impl<T: Expression> Into<Signal<T>> for SerializedSignal<T> {
    fn into(self) -> Signal<T> {
        let SerializedSignal {
            signal_type,
            custom_type,
            args,
        } = self;
        let signal_type = deserialize_signal_type(signal_type, custom_type);
        Signal::new(signal_type, args)
    }
}
#[derive(Debug, Serialize, Deserialize)]
enum SerializedSignalType {
    Error,
    Pending,
    Custom,
}
fn serialize_signal_type(signal_type: &SignalType) -> (SerializedSignalType, Option<String>) {
    match signal_type {
        SignalType::Pending => (SerializedSignalType::Pending, None),
        SignalType::Error => (SerializedSignalType::Error, None),
        SignalType::Custom(custom_type) => {
            (SerializedSignalType::Custom, Some(custom_type.clone()))
        }
    }
}
fn deserialize_signal_type(
    signal_type: SerializedSignalType,
    custom_type: Option<String>,
) -> SignalType {
    match signal_type {
        SerializedSignalType::Pending => SignalType::Pending,
        SerializedSignalType::Error => SignalType::Error,
        SerializedSignalType::Custom => SignalType::Custom(custom_type.unwrap_or(String::new())),
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
enum SubstitutionPatterns<'a, T: Expression> {
    Variable(&'a Vec<(StackOffset, T)>, Option<ScopeOffset>),
    #[allow(dead_code)]
    Offset(ScopeOffset),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Substitutions<'a, T: Expression> {
    entries: SubstitutionPatterns<'a, T>,
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
        entries: &'a Vec<(StackOffset, T)>,
        scope_offset: Option<ScopeOffset>,
    ) -> Self {
        Self {
            entries: SubstitutionPatterns::Variable(entries, scope_offset),
            min_depth: entries
                .iter()
                .map(|(offset, _)| offset)
                .fold(StackOffset::MAX, |acc, offset| acc.min(*offset)),
            offset: 0,
        }
    }
    pub fn increase_scope_offset(depth: StackOffset, min_depth: StackOffset) -> Self {
        Self {
            entries: SubstitutionPatterns::Offset(ScopeOffset::Wrap(depth)),
            min_depth,
            offset: 0,
        }
    }
    pub fn decrease_scope_offset(depth: StackOffset, min_depth: StackOffset) -> Self {
        Self {
            entries: SubstitutionPatterns::Offset(ScopeOffset::Unwrap(depth)),
            min_depth,
            offset: 0,
        }
    }
    pub(crate) fn offset(&self, offset: StackOffset) -> Self {
        Self {
            entries: self.entries.clone(),
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    pub(crate) fn can_skip(&self, expression: &impl Expression) -> bool {
        let capture_depth = expression.capture_depth();
        if capture_depth == 0 {
            true
        } else {
            expression.capture_depth() - 1 < self.min_depth + self.offset
        }
    }
    pub(crate) fn substitute_variable(
        &self,
        offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if offset < self.min_depth + self.offset {
            return None;
        }
        let (entries, scope_offset) = match self.entries {
            SubstitutionPatterns::Variable(entries, scope_offset) => {
                Some((Some(entries), scope_offset))
            }
            SubstitutionPatterns::Offset(scope_offset) => Some((None, Some(scope_offset))),
        }?;

        let target_offset = offset - self.offset;
        let replacement = entries.and_then(|entries| {
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
                    .substitute_static(
                        &Substitutions::increase_scope_offset(self.offset, 0),
                        factory,
                        allocator,
                        cache,
                    )
                    .unwrap_or_else(|| replacement.clone()),
            ),
            None => match scope_offset {
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

#[derive(Default, Hash, Eq, PartialEq, Clone, Debug)]
pub struct DependencyList {
    state_tokens: OrdSet<StateToken>,
}
impl Extend<StateToken> for DependencyList {
    fn extend<T: IntoIterator<Item = StateToken>>(&mut self, state_tokens: T) {
        if self.state_tokens.is_empty() {
            self.state_tokens = OrdSet::from_iter(state_tokens);
        } else {
            for state_token in state_tokens {
                self.state_tokens.insert(state_token);
            }
        }
    }
}
impl Extend<DependencyList> for DependencyList {
    fn extend<T: IntoIterator<Item = DependencyList>>(&mut self, values: T) {
        for values in values {
            self.extend(values)
        }
    }
}
impl DependencyList {
    pub fn empty() -> Self {
        Self::default()
    }
    pub fn of(state_token: StateToken) -> Self {
        Self {
            state_tokens: OrdSet::unit(state_token),
        }
    }
    pub fn len(&self) -> usize {
        self.state_tokens.len()
    }
    pub fn is_empty(&self) -> bool {
        self.state_tokens.is_empty()
    }
    pub fn contains(&self, state_token: StateToken) -> bool {
        self.state_tokens.contains(&state_token)
    }
    pub fn intersects(&self, entries: &BTreeSet<StateToken>) -> bool {
        if self.state_tokens.is_empty() {
            false
        } else {
            self.state_tokens
                .iter()
                .any(|dependency| entries.contains(dependency))
        }
    }
    pub fn insert(&mut self, state_token: StateToken) {
        self.state_tokens.insert(state_token);
    }
    pub fn union(self, other: Self) -> Self {
        if self.is_empty() {
            other
        } else if other.is_empty() {
            self
        } else {
            Self {
                state_tokens: self.state_tokens.union(other.state_tokens),
            }
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = StateToken> + ExactSizeIterator + '_ {
        self.state_tokens.iter().copied()
    }
}
impl FromIterator<StateToken> for DependencyList {
    fn from_iter<T: IntoIterator<Item = StateToken>>(iter: T) -> Self {
        Self {
            state_tokens: iter.into_iter().collect::<OrdSet<_>>(),
        }
    }
}
impl IntoIterator for DependencyList {
    type Item = StateToken;
    type IntoIter = im::ordset::ConsumingIter<StateToken>;
    fn into_iter(self) -> Self::IntoIter {
        self.state_tokens.into_iter()
    }
}
impl<'a> IntoIterator for &'a DependencyList {
    type Item = StateToken;
    type IntoIter = std::iter::Copied<im::ordset::Iter<'a, StateToken>>;
    fn into_iter(self) -> Self::IntoIter {
        self.state_tokens.iter().copied()
    }
}
impl serde::Serialize for DependencyList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedDependencyList::from(self).serialize(serializer)
    }
}
impl<'de> serde::Deserialize<'de> for DependencyList {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedDependencyList::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedDependencyList(Vec<StateToken>);
impl<'a> From<&'a DependencyList> for SerializedDependencyList {
    fn from(value: &'a DependencyList) -> Self {
        let DependencyList { state_tokens } = value;
        SerializedDependencyList(state_tokens.iter().cloned().collect())
    }
}
impl From<SerializedDependencyList> for DependencyList {
    fn from(value: SerializedDependencyList) -> Self {
        let SerializedDependencyList(state_tokens) = value;
        Self::from_iter(state_tokens)
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
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> EvaluationResult<T> {
    expression
        .evaluate(state, factory, allocator, cache)
        .unwrap_or_else(|| EvaluationResult::new(expression.clone(), DependencyList::empty()))
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
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
    pub fn with_dependencies(self, dependencies: DependencyList) -> Self {
        Self {
            result: self.result,
            dependencies: self.dependencies.union(dependencies),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter::once;

    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, ArgType, DependencyList, EvaluationResult, Expression, ExpressionFactory,
            FunctionArity, GraphNode, HeapAllocator, Rewritable, Signal, SignalType, StateCache,
        },
        lang::SharedTermFactory,
        parser::sexpr::parse,
        stdlib::Stdlib,
    };

    fn create_error_signal_term<T: Expression>(
        message: impl Into<T::String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T {
        factory.create_signal_term(
            allocator.create_signal_list(once(create_error_signal(message, factory, allocator))),
        )
    }

    fn create_error_signal<T: Expression>(
        message: impl Into<T::String>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Signal<T> {
        allocator.create_signal(
            SignalType::Error,
            allocator
                .create_unit_list(factory.create_string_term(allocator.create_string(message))),
        )
    }

    #[test]
    fn value_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("3", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),)
        );
    }

    #[test]
    fn lambda_optimizations() {
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
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
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression =
            parse("((lambda (foo bar) (+ foo bar)) 3 4)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
        let expression =
            parse("((lambda (foo bar) (((lambda (foo bar) (lambda (foo bar) (+ foo bar))) foo bar) foo bar)) 3 4)", &factory, &allocator)
                .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
    }

    #[test]
    fn invalid_function_applications() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((lambda (foo) foo))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<function:1>: Expected 1 argument, received 0",
                    &factory,
                    &allocator
                ),
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
                create_error_signal_term(
                    "<function:3>: Expected 3 arguments, received 1",
                    &factory,
                    &allocator
                ),
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
                create_error_signal_term(
                    "<function:3>: Expected 3 arguments, received 2",
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("+", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_builtin_term(Stdlib::Add),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_application_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ 1 2)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
    }

    #[test]
    fn nested_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4 + 5), DependencyList::empty(),),
        );
    }

    #[test]
    fn signal_short_circuiting() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("((/ 3 0) 4 5)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_error_signal_term(
                    "<stdlib:Divide>: Division by zero: 3 / 0",
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
                    "<stdlib:Divide>: Division by zero: 3 / 0",
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
                    "<stdlib:Divide>: Division by zero: 4 / 0",
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
                            "<stdlib:Divide>: Division by zero: 3 / 0",
                            &factory,
                            &allocator,
                        ),
                        create_error_signal(
                            "<stdlib:Divide>: Division by zero: 4 / 0",
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
                    "<stdlib:Divide>: Division by zero: 3 / 0",
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
                    "<stdlib:Divide>: Division by zero: 3 / 0",
                    &factory,
                    &allocator,
                ),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(let () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(let ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression =
            parse("(let ((foo 3) (bar 4)) (+ foo bar))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
        );
    }

    #[test]
    fn letrec_expressions() {
        let state = StateCache::default();
        let mut cache = SubstitutionCache::new();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse("(letrec () 3)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((foo 3)) foo)", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_int_term(3 + 4), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3 + 3), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3 + 3), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3 + 1), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3 + 1), DependencyList::empty(),),
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
                factory.create_int_term(5 * 4 * 3 * 2 * 1),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-even? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_boolean_term(false), DependencyList::empty(),),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))", &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(factory.create_boolean_term(true), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(3), DependencyList::empty(),),
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
            EvaluationResult::new(factory.create_int_term(2), DependencyList::empty(),),
        );
    }

    #[test]
    fn partial_evaluation() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();
        let expression = parse(
            "((lambda (foo) ((lambda (bar) (foo 3)) #f)) (lambda (foo) #t))",
            &factory,
            &allocator,
        )
        .unwrap();
        assert_eq!(
            expression.normalize(&factory, &allocator, &mut SubstitutionCache::new()),
            Some(factory.create_boolean_term(true))
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
            Some(factory.create_int_term(6))
        );
    }

    #[test]
    fn arity() {
        trait MyApplicable {
            fn required_args(&self) -> &[ArgType];
            fn optional_args(&self) -> &[ArgType];
            fn variadic_args(&self) -> Option<ArgType>;
        }
        const FOO_ARITY: FunctionArity<3, 1> = FunctionArity {
            required: [ArgType::Strict, ArgType::Lazy, ArgType::Lazy],
            optional: [ArgType::Strict],
            variadic: None,
        };
        struct Foo {}
        impl MyApplicable for Foo {
            fn required_args(&self) -> &[ArgType] {
                &FOO_ARITY.required()
            }
            fn optional_args(&self) -> &[ArgType] {
                &FOO_ARITY.optional()
            }
            fn variadic_args(&self) -> Option<ArgType> {
                FOO_ARITY.variadic()
            }
        }
        let expression = Foo {};
        assert_eq!(expression.required_args().len(), 3);
        assert_eq!(expression.optional_args().len(), 1);
        assert_eq!(
            expression
                .required_args()
                .iter()
                .map(|arg| match arg {
                    ArgType::Strict => String::from("strict"),
                    ArgType::Eager => String::from("eager"),
                    ArgType::Lazy => String::from("lazy"),
                })
                .collect::<String>(),
            "strictlazylazy"
        );
        struct Bar {
            offset: usize,
        }
        impl MyApplicable for Bar {
            fn required_args(&self) -> &[ArgType] {
                &FOO_ARITY.required()[self.offset.min(FOO_ARITY.required().len())..]
            }
            fn optional_args(&self) -> &[ArgType] {
                &FOO_ARITY.optional()[self.offset.saturating_sub(FOO_ARITY.required().len())..]
            }
            fn variadic_args(&self) -> Option<ArgType> {
                FOO_ARITY.variadic()
            }
        }
        let expression = Bar { offset: 4 };
        assert_eq!(expression.required_args().len(), 0);
        assert_eq!(expression.optional_args().len(), 0);
        assert_eq!(
            expression
                .required_args()
                .iter()
                .map(|arg| match arg {
                    ArgType::Strict => String::from("strict"),
                    ArgType::Eager => String::from("eager"),
                    ArgType::Lazy => String::from("lazy"),
                })
                .collect::<String>(),
            ""
        );
    }
}
