// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(generic_associated_types)]

use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashSet},
    hash::{Hash, Hasher},
};

use reflex::{
    core::{
        ConditionListType, ConditionType, DependencyList, Expression, ExpressionListType,
        GraphNode, SignalType, StackOffset, StateToken, StructPrototypeType, TermHash,
    },
    hash::{hash_object, HashId},
};
use serde::{Deserialize, Serialize};

pub mod allocator;
pub mod expression;
mod factory;
pub mod term;

pub use self::factory::*;

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
impl<T: Expression> TermHash for ExpressionList<T> {}
impl<T: Expression> ExpressionListType<T> for ExpressionList<T> {
    type Iterator<'a> = std::slice::Iter<'a, T> where T: 'a, Self: 'a;
    fn len(&self) -> usize {
        self.items.len()
    }
    fn get(&self, index: usize) -> Option<&T> {
        self.items.get(index)
    }
    fn iter<'a>(&'a self) -> Self::Iterator<'a> {
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
impl<T: Expression> std::fmt::Display for ExpressionList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.items
                .iter()
                .map(|value| format!("{}", value))
                .collect::<Vec<_>>()
                .join(",")
        )
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
        Into::<SerializedExpressionList<T>>::into(self).serialize(serializer)
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
        Ok(SerializedExpressionList::<T>::deserialize(deserializer)?.into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedExpressionList<T: Expression>(Vec<T>);
impl<'a, T: Expression> Into<SerializedExpressionList<T>> for &'a ExpressionList<T> {
    fn into(self) -> SerializedExpressionList<T> {
        let ExpressionList { items, .. } = self.clone();
        SerializedExpressionList(items)
    }
}
impl<T: Expression> Into<ExpressionList<T>> for SerializedExpressionList<T> {
    fn into(self) -> ExpressionList<T> {
        let SerializedExpressionList(items) = self;
        ExpressionList::new(items)
    }
}

pub type ExpressionListSlice<'a, T> = std::slice::Iter<'a, T>;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SignalList<T: Expression> {
    signals: BTreeSet<T::Signal>,
}
impl<T: Expression> SignalList<T>
where
    T::Signal: Ord,
{
    pub fn new(signals: impl IntoIterator<Item = T::Signal>) -> Self {
        Self {
            signals: signals.into_iter().collect(),
        }
    }
}
impl<T: Expression> TermHash for SignalList<T> {}
impl<T: Expression> ConditionListType<T> for SignalList<T> {
    type Iterator<'a> = std::collections::btree_set::Iter<'a, T::Signal> where T::Signal: 'a, Self: 'a;
    fn len(&self) -> usize {
        self.signals.len()
    }
    fn iter<'a>(&'a self) -> Self::Iterator<'a> {
        self.signals.iter()
    }
}
impl<T: Expression> std::fmt::Display for SignalList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.signals
                .iter()
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl<T: Expression> serde::Serialize for SignalList<T>
where
    T: serde::Serialize,
    T::Signal: Ord,
    T::Signal: Serialize,
    for<'de> T::Signal: Deserialize<'de>,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Into::<SerializedSignalList<T>>::into(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for SignalList<T>
where
    T: serde::Deserialize<'de>,
    T::Signal: Ord,
    T::Signal: Serialize,
    for<'a> T::Signal: Deserialize<'a>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(SerializedSignalList::<T>::deserialize(deserializer)?.into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedSignalList<T: Expression>(Vec<T::Signal>)
where
    T::Signal: Ord,
    T::Signal: Serialize,
    for<'a> T::Signal: Deserialize<'a>;
impl<'a, T: Expression> Into<SerializedSignalList<T>> for &'a SignalList<T>
where
    T::Signal: Ord,
    T::Signal: Serialize,
    for<'de> T::Signal: Deserialize<'de>,
{
    fn into(self) -> SerializedSignalList<T> {
        SerializedSignalList(self.signals.iter().cloned().collect())
    }
}
impl<T: Expression> Into<SignalList<T>> for SerializedSignalList<T>
where
    T::Signal: Ord,
    T::Signal: Serialize,
    for<'de> T::Signal: Deserialize<'de>,
{
    fn into(self) -> SignalList<T> {
        let SerializedSignalList(signals) = self;
        SignalList::new(signals)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Signal<T: Expression> {
    hash: HashId,
    signal_type: SignalType,
    args: T::ExpressionList,
}
impl<T: Expression> Signal<T> {
    pub fn new(signal_type: SignalType, args: T::ExpressionList) -> Self {
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
    pub fn is_type(&self, signal_type: &SignalType) -> bool {
        &self.signal_type == signal_type
    }
}
impl<T: Expression> TermHash for Signal<T> {}
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
impl<T: Expression> ConditionType<T> for Signal<T> {
    fn id(&self) -> StateToken {
        self.hash
    }
    fn signal_type(&self) -> &SignalType {
        &self.signal_type
    }
    fn args(&self) -> &T::ExpressionList {
        &self.args
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
    T::ExpressionList: serde::Serialize,
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
    T::ExpressionList: serde::Deserialize<'de>,
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
    args: T::ExpressionList,
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

pub type StructFieldOffset = usize;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct StructPrototype<T: Expression> {
    keys: T::ExpressionList,
}
impl<T: Expression> StructPrototype<T> {
    pub fn new(keys: T::ExpressionList) -> Self {
        Self { keys }
    }
    pub fn field(&self, key: &T) -> Option<StructFieldOffset> {
        self.keys
            .iter()
            .enumerate()
            .find_map(|(offset, existing_key)| {
                if existing_key == key {
                    Some(offset)
                } else {
                    None
                }
            })
    }
}
impl<T: Expression> TermHash for StructPrototype<T> {}
impl<T: Expression> StructPrototypeType<T> for StructPrototype<T> {
    fn keys(&self) -> &T::ExpressionList {
        &self.keys
    }
}
impl<T: Expression> std::fmt::Display for StructPrototype<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.keys
                .iter()
                .map(|key| format!("{}", key))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
