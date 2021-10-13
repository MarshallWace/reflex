// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::hash_map::Entry,
    iter::{once, FromIterator},
};

use fnv::FnvHashMap;

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

pub struct CacheEntries<T: Expression> {
    entries: FnvHashMap<InterpreterCacheKey, (InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>,
    insertion_order: Vec<InterpreterCacheKey>,
}
impl<T: Expression> Default for CacheEntries<T> {
    fn default() -> Self {
        Self {
            entries: FnvHashMap::default(),
            insertion_order: Vec::default(),
        }
    }
}
impl<T: Expression> CacheEntries<T> {
    pub fn insert(
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
    pub fn get(
        &self,
        key: &InterpreterCacheKey,
    ) -> Option<&(InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)> {
        self.entries.get(key)
    }
    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
    pub fn iter(&self) -> CacheEntriesIter<T> {
        CacheEntriesIter::new(self)
    }
}
impl<T: Expression + 'static> IntoIterator for CacheEntries<T> {
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
pub struct CacheEntriesIter<'a, T: Expression> {
    entries:
        &'a FnvHashMap<InterpreterCacheKey, (InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>,
    insertion_order: std::slice::Iter<'a, InterpreterCacheKey>,
}
impl<'a, T: Expression> CacheEntriesIter<'a, T> {
    fn new(entries: &'a CacheEntries<T>) -> Self {
        Self {
            entries: &entries.entries,
            insertion_order: entries.insertion_order.iter(),
        }
    }
}
impl<'a, T: Expression> Iterator for CacheEntriesIter<'a, T> {
    type Item = (
        &'a InterpreterCacheKey,
        &'a InterpreterCacheEntry<T>,
        &'a Vec<InterpreterCacheKey>,
    );
    fn next(&mut self) -> Option<Self::Item> {
        self.insertion_order.next().map(move |key| {
            let (entry, subexpressions) = self.entries.get(key).unwrap();
            (key, entry, subexpressions)
        })
    }
}
pub struct CacheEntriesIntoIter<T: Expression> {
    entries: FnvHashMap<InterpreterCacheKey, (InterpreterCacheEntry<T>, Vec<InterpreterCacheKey>)>,
    insertion_order: std::vec::IntoIter<InterpreterCacheKey>,
}
impl<T: Expression> CacheEntriesIntoIter<T> {
    fn new(entries: CacheEntries<T>) -> Self {
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
        self.cache.get(key).and_then(|entry| {
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
        })
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