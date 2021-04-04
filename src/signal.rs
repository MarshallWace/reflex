// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt,
    rc::Rc,
};

use crate::{
    expression::{Expression, NodeType},
    hash::{combine_hashes, hash_sequence, hash_string},
};

pub type SignalName = &'static str;

#[derive(Clone)]
pub struct Signal<T: NodeType<T>> {
    hash: u32,
    name: SignalName,
    args: Vec<Expression<T>>,
}
impl<T: NodeType<T>> Signal<T> {
    pub fn new(name: SignalName, args: Vec<Expression<T>>) -> Self {
        Signal {
            hash: hash_signal(name, &args),
            name,
            args,
        }
    }
    pub fn hash(&self) -> u32 {
        self.hash
    }
    pub fn name(&self) -> SignalName {
        self.name
    }
    pub fn args(&self) -> &[Expression<T>] {
        &self.args
    }
    pub fn is(&self, name: SignalName) -> bool {
        self.name == name
    }
}
impl<T: NodeType<T>> PartialEq for Signal<T> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}
impl<T: NodeType<T>> fmt::Display for Signal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<signal:{}:{}>", self.name, self.hash)
    }
}
impl<T: NodeType<T>> fmt::Debug for Signal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CombinedSignal<T: NodeType<T>> {
    entries: HashMap<SignalName, HashMap<u32, Signal<T>>>,
}
impl<T: NodeType<T>> CombinedSignal<T> {
    pub fn join(left: Signal<T>, right: Signal<T>) -> Self {
        let left_name = left.name();
        let right_name = right.name();
        let entries = if left_name == right_name {
            let mut entries = HashMap::with_capacity(1);
            let mut grouped_entries = HashMap::with_capacity(2);
            grouped_entries.insert(left.hash(), left);
            grouped_entries.insert(right.hash(), right);
            entries.insert(left_name, grouped_entries);
            entries
        } else {
            let mut entries = HashMap::with_capacity(2);
            let mut left_entries = HashMap::with_capacity(1);
            let mut right_entries = HashMap::with_capacity(1);
            left_entries.insert(left.hash(), left);
            right_entries.insert(right.hash(), right);
            entries.insert(left_name, left_entries);
            entries.insert(right_name, right_entries);
            entries
        };
        CombinedSignal { entries }
    }
    pub fn append(self, signal: Signal<T>) -> Self {
        let signal_name = signal.name();
        let hash = signal.hash();
        let CombinedSignal { mut entries } = self;
        match entries.get_mut(signal_name) {
            Some(existing_entries) => match existing_entries.get(&hash) {
                Some(_) => {}
                None => {
                    existing_entries.insert(hash, signal);
                }
            },
            None => {
                let mut grouped_entries = HashMap::with_capacity(1);
                grouped_entries.insert(hash, signal);
                entries.insert(signal_name, grouped_entries);
            }
        };
        CombinedSignal { entries }
    }
    pub fn extend(self, other: CombinedSignal<T>) -> Self {
        let CombinedSignal { mut entries } = self;
        let CombinedSignal {
            entries: other_entries,
        } = other;
        for (signal_name, signal_entries) in other_entries {
            match entries.entry(signal_name) {
                Entry::Occupied(mut entry) => entry.get_mut().extend(signal_entries),
                Entry::Vacant(entry) => {
                    entry.insert(signal_entries);
                }
            }
        }
        CombinedSignal { entries }
    }
    pub fn contains(&self, name: SignalName) -> bool {
        self.entries.contains_key(&name)
    }
    pub fn remove(mut self, name: SignalName) -> (Option<Vec<Signal<T>>>, Option<SignalResult<T>>) {
        match self.entries.remove(&name) {
            Some(entry) => {
                let removed_entries = entry.into_iter().map(|(_, signal)| signal).collect();
                let remaining_entries = match self.entries.len() {
                    0 => None,
                    1 if self.entries.values().next().unwrap().len() == 1 => {
                        let signal_name = *self.entries.keys().next().unwrap();
                        let mut grouped_entries = self.entries.remove(signal_name).unwrap();
                        let signal_id = *grouped_entries.keys().next().unwrap();
                        let entry = grouped_entries.remove(&signal_id).unwrap();
                        Some(SignalResult::Single(entry))
                    }
                    _ => Some(SignalResult::Multiple(CombinedSignal {
                        entries: self.entries,
                    })),
                };
                (Some(removed_entries), remaining_entries)
            }
            None => (
                None,
                Some(SignalResult::Multiple(CombinedSignal {
                    entries: self.entries,
                })),
            ),
        }
    }
}
impl<T: NodeType<T>> fmt::Display for CombinedSignal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<signalset:{}>",
            self.entries
                .values()
                .flat_map(|entries| entries.values())
                .map(|item| format!("{}", item))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SignalResult<T: NodeType<T>> {
    Single(Signal<T>),
    Multiple(CombinedSignal<T>),
}
impl<T: NodeType<T>> SignalResult<T> {
    pub fn single(signal: Signal<T>) -> Self {
        SignalResult::Single(signal)
    }
    pub fn multiple(signals: CombinedSignal<T>) -> Self {
        SignalResult::Multiple(signals)
    }
    pub fn join(left: Signal<T>, right: Signal<T>) -> Self {
        if left.hash() == right.hash() {
            SignalResult::Single(left)
        } else {
            SignalResult::Multiple(CombinedSignal::join(left, right))
        }
    }
    pub fn append(self, signal: Signal<T>) -> Self {
        match self {
            SignalResult::Single(existing) => SignalResult::join(existing, signal),
            SignalResult::Multiple(existing) => SignalResult::Multiple(existing.append(signal)),
        }
    }
    pub fn combine(self, other: SignalResult<T>) -> Self {
        match self {
            SignalResult::Single(existing) => match other {
                SignalResult::Single(other) => SignalResult::join(existing, other),
                SignalResult::Multiple(other) => SignalResult::multiple(other.append(existing)),
            },
            SignalResult::Multiple(existing) => match other {
                SignalResult::Single(other) => SignalResult::multiple(existing.append(other)),
                SignalResult::Multiple(other) => SignalResult::multiple(existing.extend(other)),
            },
        }
    }
    pub fn contains(&self, name: SignalName) -> bool {
        match self {
            SignalResult::Single(signal) => signal.name() == name,
            SignalResult::Multiple(signal) => signal.contains(name),
        }
    }
}
impl<T: NodeType<T>> fmt::Display for SignalResult<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SignalResult::Single(signal) => fmt::Display::fmt(signal, f),
            SignalResult::Multiple(signal) => fmt::Display::fmt(signal, f),
        }
    }
}

#[derive(Clone)]
pub struct EffectHandler<T: NodeType<T>> {
    id: u32,
    handler: Rc<dyn (Fn(&[&[Expression<T>]]) -> Expression<T>)>,
}
impl<T: NodeType<T>> EffectHandler<T> {
    pub fn new(id: u32, handler: impl Fn(&[&[Expression<T>]]) -> Expression<T> + 'static) -> Self {
        EffectHandler {
            id,
            handler: Rc::new(handler),
        }
    }
    pub fn handle(&self, signals: &[&[Expression<T>]]) -> Expression<T> {
        (*self.handler)(signals)
    }
}
impl<T: NodeType<T>> PartialEq for EffectHandler<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T: NodeType<T>> fmt::Debug for EffectHandler<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<effect:{}>", self.id)
    }
}

pub fn hash_signal<T: NodeType<T>>(name: SignalName, args: &[Expression<T>]) -> u32 {
    combine_hashes(
        hash_string(&name),
        hash_sequence(args.iter().map(|arg| arg.hash())),
    )
}
