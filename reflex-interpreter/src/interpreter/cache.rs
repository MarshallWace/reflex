// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    iter::{once, FromIterator},
};

use reflex::{
    core::{hash_state_values, DynamicState, EvaluationResult, Expression},
    hash::{HashId, IntMap},
};
use tracing::trace;

pub struct GcMetrics {
    pub purged: usize,
    pub remaining: usize,
}
impl std::fmt::Display for GcMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} purged, {} remaining", self.purged, self.remaining)
    }
}

pub trait InterpreterCache<T: Expression> {
    fn retrieve_result(
        &self,
        key: HashId,
        state: &impl DynamicState<T>,
    ) -> Option<EvaluationResult<T>>;
    fn contains(&self, key: HashId, state: &impl DynamicState<T>) -> bool;
    fn len(&self) -> usize;
}
pub trait MutableInterpreterCache<T: Expression>: InterpreterCache<T> {
    fn insert(&mut self, entry: InterpreterCacheEntry<T>);
    fn update_state_hash(&mut self, key: HashId, state: &impl DynamicState<T>);
    fn extend(&mut self, entries: impl IntoIterator<Item = InterpreterCacheEntry<T>>);
}

#[derive(Eq)]
pub struct InterpreterCacheEntry<T: Expression> {
    cache_key: HashId,
    result: EvaluationResult<T>,
    state_id: usize,
    overall_state_hash: HashId,
    minimal_state_hash: HashId,
    children: Vec<HashId>,
}
impl<T: Expression> PartialEq for InterpreterCacheEntry<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cache_key == other.cache_key
            && self.result == other.result
            && self.overall_state_hash == other.overall_state_hash
            && self.minimal_state_hash == other.minimal_state_hash
    }
}
impl<T: Expression> std::hash::Hash for InterpreterCacheEntry<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.cache_key.hash(state);
        self.overall_state_hash.hash(state);
        self.minimal_state_hash.hash(state);
    }
}
impl<T: Expression> InterpreterCacheEntry<T> {
    pub fn new(
        cache_key: HashId,
        result: EvaluationResult<T>,
        state_id: usize,
        state: &impl DynamicState<T>,
        children: Vec<HashId>,
    ) -> Self {
        let (overall_state_hash, minimal_state_hash) = if result.dependencies().is_empty() {
            (0, 0)
        } else {
            (state.id(), hash_state_values(state, result.dependencies()))
        };
        Self {
            cache_key,
            state_id,
            overall_state_hash,
            minimal_state_hash,
            result,
            children,
        }
    }
    pub fn cache_key(&self) -> HashId {
        self.cache_key
    }
    pub fn result(&self) -> &EvaluationResult<T> {
        &self.result
    }
    pub fn state_id(&self) -> usize {
        self.state_id
    }
    pub fn state_hash(&self) -> HashId {
        self.overall_state_hash
    }
    pub fn children(&self) -> &[HashId] {
        &self.children
    }
    pub fn into_result(self) -> EvaluationResult<T> {
        self.result
    }
}
impl<T: Expression> std::fmt::Debug for InterpreterCacheEntry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n {}: {}",
            self.cache_key,
            self.children
                .iter()
                .map(|child| format!("\n  {}", child))
                .collect::<String>()
        )
    }
}

struct GcWrapper<T> {
    value: T,
    marked: bool,
}
fn gc<'a, T: Expression>(
    cache_entries: &mut IntMap<HashId, GcWrapper<InterpreterCacheEntry<T>>>,
    roots: impl IntoIterator<Item = HashId>,
) -> GcMetrics {
    let existing_size = cache_entries.len();
    // Mark any values accessible from the given roots
    for key in roots {
        mark_gc_nodes(cache_entries, VecDeque::from_iter(once(key)));
    }
    // Sweep out unmarked values, resetting the GC marker for any marked values
    cache_entries.retain(|_key, value| {
        if value.marked {
            value.marked = false;
            true
        } else {
            false
        }
    });
    cache_entries.shrink_to_fit();
    let num_remaining = cache_entries.len();
    let num_disposed = existing_size - num_remaining;
    GcMetrics {
        purged: num_disposed,
        remaining: num_remaining,
    }
}
fn mark_gc_nodes<T: Expression>(
    cache_entries: &mut IntMap<HashId, GcWrapper<InterpreterCacheEntry<T>>>,
    mut keys: VecDeque<HashId>,
) {
    while let Some(key) = keys.pop_front() {
        // Determine whether the given key exists in the store
        if let Some(wrapper) = cache_entries.get_mut(&key) {
            // If the key has not yet been marked, mark the node and all its descendants
            if !wrapper.marked {
                wrapper.marked = true;
                keys.extend(wrapper.value.children.iter());
            }
        }
    }
}

pub struct DefaultInterpreterCache<T: Expression> {
    cache: IntMap<HashId, GcWrapper<InterpreterCacheEntry<T>>>,
}
impl<T: Expression> Default for DefaultInterpreterCache<T> {
    fn default() -> Self {
        Self {
            cache: IntMap::default(),
        }
    }
}
impl<T: Expression> DefaultInterpreterCache<T> {
    pub fn apply_updates(
        &mut self,
        entries: impl IntoIterator<Item = InterpreterCacheEntry<T>>,
        state_id: usize,
        latest_state_id: usize,
    ) {
        let is_latest_state = state_id == latest_state_id;
        if is_latest_state {
            self.extend(entries)
        } else {
            for entry in entries {
                let is_newer = match self.cache.get(&entry.cache_key()) {
                    None => true,
                    Some(wrapper) => state_id > wrapper.value.state_id,
                };
                if is_newer {
                    self.insert(entry);
                }
            }
        }
    }
    pub fn gc(&mut self, roots: impl IntoIterator<Item = HashId>) -> GcMetrics {
        gc(&mut self.cache, roots)
    }
    pub fn len(&self) -> usize {
        self.cache.len()
    }
    pub fn size(&self) -> usize {
        self.cache
            .values()
            .map(|x| x.value.result.result().size())
            .sum()
    }
}
impl<T: Expression> InterpreterCache<T> for DefaultInterpreterCache<T> {
    fn retrieve_result(
        &self,
        key: HashId,
        state: &impl DynamicState<T>,
    ) -> Option<EvaluationResult<T>> {
        self.cache.get(&key).and_then(|wrapper| {
            let entry = &wrapper.value;
            let state_dependencies = entry.result.dependencies();
            if state_dependencies.is_empty()
                || entry.overall_state_hash == 0
                || entry.overall_state_hash == state.id()
                || entry.minimal_state_hash == hash_state_values(state, state_dependencies)
            {
                Some(entry.result.clone())
            } else {
                None
            }
        })
    }
    fn contains(&self, key: HashId, state: &impl DynamicState<T>) -> bool {
        self.cache
            .get(&key)
            .map(|wrapper| {
                let entry = &wrapper.value;
                entry.overall_state_hash == 0
                    || entry.overall_state_hash == state.id()
                    || entry.minimal_state_hash
                        == hash_state_values(state, entry.result.dependencies())
            })
            .unwrap_or(false)
    }
    fn len(&self) -> usize {
        self.cache.len()
    }
}
impl<T: Expression> MutableInterpreterCache<T> for DefaultInterpreterCache<T> {
    fn insert(&mut self, entry: InterpreterCacheEntry<T>) {
        match self.cache.entry(entry.cache_key) {
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.insert(GcWrapper {
                    marked: occupied_entry.get().marked,
                    value: entry,
                });
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(GcWrapper {
                    marked: false,
                    value: entry,
                });
            }
        }
    }
    fn update_state_hash(&mut self, key: HashId, state: &impl DynamicState<T>) {
        match self.cache.get_mut(&key) {
            Some(wrapper) => {
                let entry = &mut wrapper.value;
                entry.overall_state_hash = state.id()
            }
            None => {
                panic!("Cache entry not found: {}", key);
            }
        }
    }
    fn extend(&mut self, entries: impl IntoIterator<Item = InterpreterCacheEntry<T>>) {
        for entry in entries {
            self.insert(entry)
        }
    }
}

pub struct LocalCacheEntries<T: Expression> {
    state_hash: HashId,
    entries: IntMap<HashId, InterpreterCacheEntry<T>>,
}
impl<T: Expression> Default for LocalCacheEntries<T> {
    fn default() -> Self {
        Self {
            state_hash: Default::default(),
            entries: IntMap::default(),
        }
    }
}
impl<T: Expression> LocalCacheEntries<T> {
    pub fn new(state: &impl DynamicState<T>) -> Self {
        Self {
            state_hash: state.id(),
            entries: HashMap::default(),
        }
    }
}
impl<T: Expression> IntoIterator for LocalCacheEntries<T> {
    type Item = InterpreterCacheEntry<T>;
    type IntoIter = std::collections::hash_map::IntoValues<HashId, Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_values()
    }
}
impl<T: Expression> InterpreterCache<T> for LocalCacheEntries<T> {
    fn retrieve_result(
        &self,
        key: HashId,
        state: &impl DynamicState<T>,
    ) -> Option<EvaluationResult<T>> {
        let entry = if state.id() == self.state_hash {
            self.entries.get(&key).map(|entry| entry.result.clone())
        } else {
            None
        };
        trace!(cache_entries = if entry.is_some() { "hit" } else { "miss" });
        entry
    }
    fn contains(&self, key: HashId, state: &impl DynamicState<T>) -> bool {
        state.id() == self.state_hash && self.entries.contains_key(&key)
    }
    fn len(&self) -> usize {
        self.entries.len()
    }
}
impl<T: Expression> MutableInterpreterCache<T> for LocalCacheEntries<T> {
    fn insert(&mut self, entry: InterpreterCacheEntry<T>) {
        match self.entries.entry(entry.cache_key) {
            Entry::Occupied(mut occupied_entry) => {
                occupied_entry.insert(entry);
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(entry);
            }
        }
    }
    fn update_state_hash(&mut self, key: HashId, state: &impl DynamicState<T>) {
        match self.entries.get_mut(&key) {
            Some(entry) => {
                entry.overall_state_hash = state.id();
            }
            None => {
                panic!("Cache entry not found: {}", key);
            }
        }
    }
    fn extend(&mut self, entries: impl IntoIterator<Item = InterpreterCacheEntry<T>>) {
        for entry in entries {
            self.insert(entry)
        }
    }
}

pub(crate) struct MultithreadedCacheEntries<'a, T: Expression> {
    parent: Option<&'a MultithreadedCacheEntries<'a, T>>,
    entries: LocalCacheEntries<T>,
}
impl<'a, T: Expression> Default for MultithreadedCacheEntries<'a, T> {
    fn default() -> Self {
        Self {
            parent: Default::default(),
            entries: Default::default(),
        }
    }
}
impl<'a, T: Expression> MultithreadedCacheEntries<'a, T> {
    pub(crate) fn new(state: &impl DynamicState<T>) -> Self {
        Self {
            parent: None,
            entries: LocalCacheEntries::new(state),
        }
    }
    pub(crate) fn create_child(&'a self, entries: LocalCacheEntries<T>) -> Self {
        Self {
            parent: Some(self),
            entries,
        }
    }
    pub(crate) fn into_entries(self) -> LocalCacheEntries<T> {
        self.entries
    }
}
impl<'a, T: Expression> InterpreterCache<T> for MultithreadedCacheEntries<'a, T> {
    fn retrieve_result(
        &self,
        key: HashId,
        state: &impl DynamicState<T>,
    ) -> Option<EvaluationResult<T>> {
        self.entries.retrieve_result(key, state).or_else(|| {
            self.parent
                .and_then(|parent| parent.retrieve_result(key, state))
        })
    }
    fn contains(&self, key: HashId, state: &impl DynamicState<T>) -> bool {
        self.entries.contains(key, state)
            || self
                .parent
                .map(|parent| parent.contains(key, state))
                .unwrap_or(false)
    }
    fn len(&self) -> usize {
        self.entries.len() + self.parent.map(|parent| parent.len()).unwrap_or(0)
    }
}
impl<'a, T: Expression> MutableInterpreterCache<T> for MultithreadedCacheEntries<'a, T> {
    fn insert(&mut self, entry: InterpreterCacheEntry<T>) {
        self.entries.insert(entry)
    }
    fn update_state_hash(&mut self, key: HashId, state: &impl DynamicState<T>) {
        self.entries.update_state_hash(key, state);
    }
    fn extend(&mut self, entries: impl IntoIterator<Item = InterpreterCacheEntry<T>>) {
        self.entries.extend(entries)
    }
}
