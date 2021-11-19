// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::hash_map::Entry,
    iter::{once, FromIterator},
};

use fnv::FnvHashMap;
use tracing::trace;

use crate::{
    core::{hash_state_values, DynamicState, EvaluationResult, Expression},
    hash::HashId,
    DependencyCache,
};

pub struct GcMetrics {
    purged: usize,
    remaining: usize,
}
impl std::fmt::Display for GcMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} purged, {} remaining", self.purged, self.remaining)
    }
}

pub type InterpreterCacheKey = HashId;

pub trait InterpreterCache<T: Expression> {
    fn retrieve_result(
        &self,
        key: &InterpreterCacheKey,
        state: &impl DynamicState<T>,
    ) -> Option<(EvaluationResult<T>, Option<InterpreterCacheEntry<T>>)>;
    fn contains_key(&self, key: &InterpreterCacheKey) -> bool;
}
pub struct InterpreterCacheEntry<T: Expression> {
    result: EvaluationResult<T>,
    overall_state_hash: HashId,
    minimal_state_hash: HashId,
}
impl<T: Expression> InterpreterCacheEntry<T> {
    pub fn new(result: EvaluationResult<T>, state: &impl DynamicState<T>) -> Self {
        Self {
            overall_state_hash: state.id(),
            minimal_state_hash: hash_state_values(state, result.dependencies()),
            result,
        }
    }

    pub fn result(&self) -> &EvaluationResult<T> {
        &self.result
    }
}

pub trait CacheEntries<T: Expression> {
    fn insert(
        &mut self,
        key: InterpreterCacheKey,
        value: InterpreterCacheEntry<T>,
        subexpressions: Vec<InterpreterCacheKey>,
    );
    fn extend(
        &mut self,
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    );
    fn get(
        &self,
        key: &InterpreterCacheKey,
    ) -> Option<&(InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
}

pub(super) struct MultithreadedCacheEntries<'a, T: Expression> {
    parent_cache: Option<&'a MultithreadedCacheEntries<'a, T>>,
    thread_local_cache: LocalCacheEntries<T>,
}
impl<'a, T: Expression> MultithreadedCacheEntries<'a, T> {
    pub(super) fn new_top_level() -> Self {
        Self {
            parent_cache: None,
            thread_local_cache: LocalCacheEntries::default(),
        }
    }
    pub(super) fn new_from_threaded<'b: 'a>(
        shared_cache: &'a MultithreadedCacheEntries<'b, T>,
    ) -> Self {
        Self {
            parent_cache: Some(shared_cache),
            thread_local_cache: LocalCacheEntries::default(),
        }
    }
    pub(super) fn into_thread_local_entries(self) -> LocalCacheEntries<T> {
        self.thread_local_cache
    }
}
impl<'a, T: Expression> CacheEntries<T> for MultithreadedCacheEntries<'a, T> {
    fn insert(
        &mut self,
        key: InterpreterCacheKey,
        value: InterpreterCacheEntry<T>,
        subexpressions: Vec<InterpreterCacheKey>,
    ) {
        self.thread_local_cache.insert(key, value, subexpressions)
    }
    fn extend(
        &mut self,
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) {
        self.thread_local_cache.extend(entries)
    }
    fn get(
        &self,
        key: &InterpreterCacheKey,
    ) -> Option<&(InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)> {
        self.thread_local_cache
            .get(key)
            .or_else(|| self.parent_cache.map(|parent| parent.get(key)).flatten())
    }
    fn len(&self) -> usize {
        self.thread_local_cache.len() + self.parent_cache.map(|parent| parent.len()).unwrap_or(0)
    }

    fn is_empty(&self) -> bool {
        self.thread_local_cache.is_empty()
            && self
                .parent_cache
                .map(|parent| parent.is_empty())
                .unwrap_or(true)
    }
}

pub struct LocalCacheEntries<T: Expression> {
    entries: FnvHashMap<InterpreterCacheKey, (InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>,
    insertion_order: Vec<InterpreterCacheKey>,
}
impl<T: Expression> Default for LocalCacheEntries<T> {
    fn default() -> Self {
        Self {
            entries: FnvHashMap::default(),
            insertion_order: Vec::default(),
        }
    }
}
impl<T: Expression> CacheEntries<T> for LocalCacheEntries<T> {
    fn insert(
        &mut self,
        key: InterpreterCacheKey,
        value: InterpreterCacheEntry<T>,
        subexpressions: Vec<InterpreterCacheKey>,
    ) {
        match self.entries.entry(key) {
            Entry::Occupied(mut entry) => {
                let mut subexpressions = subexpressions;
                let existing_subexpressions = &entry.get().1;
                subexpressions.extend(existing_subexpressions);
                entry.insert((value, subexpressions));
            }
            Entry::Vacant(entry) => {
                self.insertion_order.push(key);
                entry.insert((value, subexpressions));
            }
        }
    }
    fn extend(
        &mut self,
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) {
        for (key, value, subexpressions) in entries {
            self.insert(key, value, subexpressions)
        }
    }
    fn get(
        &self,
        key: &InterpreterCacheKey,
    ) -> Option<&(InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)> {
        let entry = self.entries.get(key);
        trace!(cache_entries = if entry.is_some() { "hit" } else { "miss" });
        entry
    }
    fn len(&self) -> usize {
        self.entries.len()
    }
    fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}
impl<T: Expression> IntoIterator for LocalCacheEntries<T> {
    type Item = (
        InterpreterCacheKey,
        InterpreterCacheEntry<T>,
        Vec<InterpreterCacheKey>,
    );
    type IntoIter = CacheEntriesIntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        CacheEntriesIntoIter::new(self)
    }
}
pub struct CacheEntriesIntoIter<T: Expression> {
    entries: FnvHashMap<InterpreterCacheKey, (InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>,
    insertion_order: std::vec::IntoIter<InterpreterCacheKey>,
}
impl<T: Expression> CacheEntriesIntoIter<T> {
    fn new(entries: LocalCacheEntries<T>) -> Self {
        Self {
            entries: entries.entries,
            insertion_order: entries.insertion_order.into_iter(),
        }
    }
}
impl<T: Expression> Iterator for CacheEntriesIntoIter<T> {
    type Item = (
        InterpreterCacheKey,
        InterpreterCacheEntry<T>,
        Vec<InterpreterCacheKey>,
    );
    fn next(&mut self) -> Option<Self::Item> {
        self.insertion_order.next().map(move |key| {
            let (entry, subexpressions) = self.entries.remove(&key).unwrap();
            (key, entry, subexpressions)
        })
    }
}

pub struct DefaultInterpreterCache<T: Expression> {
    cache: DependencyCache<InterpreterCacheKey, InterpreterCacheEntry<T>>,
}
impl<T: Expression> Default for DefaultInterpreterCache<T> {
    fn default() -> Self {
        Self {
            cache: DependencyCache::default(),
        }
    }
}
impl<T: Expression> InterpreterCache<T> for DefaultInterpreterCache<T> {
    fn retrieve_result(
        &self,
        key: &InterpreterCacheKey,
        state: &impl DynamicState<T>,
    ) -> Option<(EvaluationResult<T>, Option<InterpreterCacheEntry<T>>)> {
        let result = self.cache.get(key).and_then(|entry| {
            let state_dependencies = entry.result.dependencies();
            if state_dependencies.is_empty() || entry.overall_state_hash == state.id() {
                Some((entry.result().clone(), None))
            } else if entry.minimal_state_hash == hash_state_values(state, state_dependencies) {
                let result = entry.result();
                Some((
                    result.clone(),
                    Some(InterpreterCacheEntry {
                        result: result.clone(),
                        overall_state_hash: state.id(),
                        minimal_state_hash: entry.minimal_state_hash,
                    }),
                ))
            } else {
                None
            }
        });
        trace!(interpreter_cache = if result.is_some() { "hit" } else { "miss" });
        result
    }
    fn contains_key(&self, key: &InterpreterCacheKey) -> bool {
        self.cache.contains_key(&key)
    }
}
impl<T: Expression> DefaultInterpreterCache<T> {
    pub fn new(
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) -> Self {
        Self {
            cache: DependencyCache::new(entries),
        }
    }
    pub fn extend(
        &mut self,
        entries: impl IntoIterator<
            Item = (
                InterpreterCacheKey,
                InterpreterCacheEntry<T>,
                Vec<InterpreterCacheKey>,
            ),
        >,
    ) {
        self.cache.extend(
            entries
                .into_iter()
                .map(|(key, value, subexpressions)| (key, value, subexpressions)),
        );
    }
    pub fn store_result(
        &mut self,
        key: InterpreterCacheKey,
        state: &impl DynamicState<T>,
        result: EvaluationResult<T>,
        children: Vec<InterpreterCacheKey>,
    ) {
        let state_dependencies = result.dependencies();
        let (overall_state_hash, minimal_state_hash) = if state_dependencies.is_empty() {
            (0, 0)
        } else {
            (state.id(), hash_state_values(state, state_dependencies))
        };
        let entry = InterpreterCacheEntry {
            result,
            overall_state_hash,
            minimal_state_hash,
        };
        self.cache.set(key, entry, children);
    }
    pub fn replace_children(
        &mut self,
        key: &InterpreterCacheKey,
        children: Vec<InterpreterCacheKey>,
    ) -> bool {
        self.cache.replace_children(key, children)
    }
    pub fn retain(&mut self, key: InterpreterCacheKey) {
        self.cache.retain(once(&key));
    }
    pub fn release(&mut self, key: InterpreterCacheKey) {
        self.cache.release(once(&key));
    }
    pub fn gc(&mut self) -> GcMetrics {
        let num_disposed = self.cache.gc::<IteratorCounter>().count();
        let num_remaining = self.cache.len();
        GcMetrics {
            purged: num_disposed,
            remaining: num_remaining,
        }
    }
}

struct IteratorCounter(usize);
impl IteratorCounter {
    fn count(&self) -> usize {
        self.0
    }
}
impl<V> FromIterator<V> for IteratorCounter {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self(iter.into_iter().fold(0, |count, _| count + 1))
    }
}
