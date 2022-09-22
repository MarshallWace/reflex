// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{BTreeSet, HashSet},
    hash::{Hash, Hasher},
};

use reflex::{
    core::{
        ConditionListType, ConditionType, DependencyList, Expression, ExpressionListType,
        GraphNode, IntoRefTypeIterator, RefType, SignalType, StackOffset, StateToken,
        StructPrototypeType,
    },
    hash::{hash_iter, hash_object, FnvHasher, HashId},
};
use serde::{Deserialize, Serialize};

pub mod allocator;
pub mod expression;
mod factory;
pub mod term;

pub use self::factory::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ExpressionList<T: Expression> {
    id: HashId,
    items: Vec<T>,
}
impl<T: Expression> std::hash::Hash for ExpressionList<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl<T: Expression> ExpressionList<T> {
    pub fn new(items: impl IntoIterator<Item = T>) -> Self {
        let items = items.into_iter().collect::<Vec<_>>();
        Self {
            id: hash_object(&items.iter().map(|val| val.id()).collect::<Vec<_>>()),
            items,
        }
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
    pub fn as_slice(&self) -> &[T] {
        self.items.as_slice()
    }
    pub fn into_values(self) -> Vec<T> {
        self.items
    }
}

impl<T: Expression> ExpressionListType<T> for ExpressionList<T> {
    type Iterator<'a> = IntoRefTypeIterator<T, T::Ref<'a, T>, std::slice::Iter<'a, T>> where T: 'a, Self: 'a;
    fn id(&self) -> HashId {
        self.id
    }
    fn len(&self) -> usize {
        self.items.len()
    }
    fn get<'a>(&'a self, index: usize) -> Option<T::Ref<'a, T>>
    where
        T: 'a,
    {
        self.items.get(index).map(|item| item.into())
    }
    fn iter<'a>(&'a self) -> Self::Iterator<'a> {
        IntoRefTypeIterator::new(self.items.iter())
    }
}
impl<T: Expression> GraphNode for ExpressionList<T> {
    fn capture_depth(&self) -> StackOffset {
        self.items
            .iter()
            .map(|term| term.capture_depth())
            .max()
            .unwrap_or_default()
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct SignalList<T: Expression> {
    id: HashId,
    signals: BTreeSet<T::Signal<T>>,
}

impl<T: Expression> Hash for SignalList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl<T: Expression> SignalList<T>
where
    T::Signal<T>: Ord,
{
    pub fn new(signals: impl IntoIterator<Item = T::Signal<T>>) -> Self {
        let signals = signals.into_iter().collect::<BTreeSet<_>>();
        Self {
            id: hash_iter(signals.iter().map(|signal| signal.id())),
            signals,
        }
    }
}
impl<T: Expression> ConditionListType<T> for SignalList<T> {
    type Iterator<'a> = IntoRefTypeIterator<T::Signal<T>, T::Ref<'a, T::Signal<T>>, std::collections::btree_set::Iter<'a, T::Signal<T>>>
        where
            T::Signal<T>: 'a,
            Self: 'a;
    fn len(&self) -> usize {
        self.signals.len()
    }
    fn iter<'a>(&'a self) -> Self::Iterator<'a> {
        IntoRefTypeIterator::new(self.signals.iter())
    }
    fn id(&self) -> u64 {
        self.id
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
    T::Signal<T>: Ord,
    T::Signal<T>: Serialize,
    for<'de> T::Signal<T>: Deserialize<'de>,
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
    T::Signal<T>: Ord,
    T::Signal<T>: Serialize,
    for<'a> T::Signal<T>: Deserialize<'a>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(SerializedSignalList::<T>::deserialize(deserializer)?.into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedSignalList<T: Expression>(Vec<T::Signal<T>>)
where
    T::Signal<T>: Ord,
    T::Signal<T>: Serialize,
    for<'a> T::Signal<T>: Deserialize<'a>;
impl<'a, T: Expression> Into<SerializedSignalList<T>> for &'a SignalList<T>
where
    T::Signal<T>: Ord,
    T::Signal<T>: Serialize,
    for<'de> T::Signal<T>: Deserialize<'de>,
{
    fn into(self) -> SerializedSignalList<T> {
        SerializedSignalList(self.signals.iter().cloned().collect())
    }
}
impl<T: Expression> Into<SignalList<T>> for SerializedSignalList<T>
where
    T::Signal<T>: Ord,
    T::Signal<T>: Serialize,
    for<'de> T::Signal<T>: Deserialize<'de>,
{
    fn into(self) -> SignalList<T> {
        let SerializedSignalList(signals) = self;
        SignalList::new(signals)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Signal<T: Expression> {
    id: HashId,
    signal_type: SignalType,
    args: T::ExpressionList<T>,
}
impl<T: Expression> Signal<T> {
    pub fn new(signal_type: SignalType, args: T::ExpressionList<T>) -> Self {
        let hash = {
            let mut hasher = FnvHasher::default();
            signal_type.hash(&mut hasher);
            for arg in args.iter().map(|item| item.as_deref()) {
                arg.id().hash(&mut hasher)
            }
            hasher.finish()
        };
        Self {
            id: hash,
            signal_type,
            args,
        }
    }
    pub fn is_type(&self, signal_type: &SignalType) -> bool {
        &self.signal_type == signal_type
    }
}
impl<T: Expression> Hash for Signal<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}
impl<T: Expression> Ord for Signal<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}
impl<T: Expression> PartialOrd for Signal<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}
impl<T: Expression> ConditionType<T> for Signal<T> {
    fn id(&self) -> StateToken {
        self.id
    }
    fn signal_type(&self) -> SignalType {
        // FIXME: Prevent unnecessary cloning of signal type
        self.signal_type.clone()
    }
    fn args<'a>(&'a self) -> T::Ref<'a, T::ExpressionList<T>>
    where
        T::ExpressionList<T>: 'a,
        T: 'a,
    {
        (&self.args).into()
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
                .map(|item| item.as_deref())
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl<T: Expression> serde::Serialize for Signal<T>
where
    T: serde::Serialize,
    T::ExpressionList<T>: serde::Serialize,
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
    T::ExpressionList<T>: serde::Deserialize<'de>,
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
    args: T::ExpressionList<T>,
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
    keys: T::ExpressionList<T>,
}
impl<T: Expression> StructPrototype<T> {
    pub fn new(keys: T::ExpressionList<T>) -> Self {
        Self { keys }
    }
    pub fn field(&self, key: &T) -> Option<StructFieldOffset> {
        self.keys
            .iter()
            .map(|item| item.as_deref())
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
impl<T: Expression> StructPrototypeType<T> for StructPrototype<T> {
    fn keys<'a>(&'a self) -> T::Ref<'a, T::ExpressionList<T>>
    where
        T::ExpressionList<T>: 'a,
        T: 'a,
    {
        (&self.keys).into()
    }
}
impl<T: Expression> std::fmt::Display for StructPrototype<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.keys
                .iter()
                .map(|item| item.as_deref())
                .map(|key| format!("{}", key))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}