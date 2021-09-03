// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::{once, FromIterator};

use crate::{
    cache::hash_state_values,
    core::{DynamicState, EvaluationResult, Expression},
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

type CacheKey = HashId;

pub trait InterpreterCache<T: Expression> {
    fn retrieve_result(
        &mut self,
        key: CacheKey,
        state: &DynamicState<T>,
    ) -> Option<&EvaluationResult<T>>;
    fn store_result(
        &mut self,
        key: CacheKey,
        state: &DynamicState<T>,
        result: EvaluationResult<T>,
        children: Vec<CacheKey>,
    );
    fn retain(&mut self, key: CacheKey);
    fn release(&mut self, key: CacheKey);
    fn gc(&mut self) -> GcMetrics;
}

struct InterpreterCacheEntry<T: Expression> {
    result: EvaluationResult<T>,
    overall_state_hash: HashId,
    minimal_state_hash: HashId,
}
impl<T: Expression> InterpreterCacheEntry<T> {
    fn result(&self) -> &EvaluationResult<T> {
        &self.result
    }
}

pub struct DefaultInterpreterCache<T: Expression> {
    cache: DependencyCache<CacheKey, InterpreterCacheEntry<T>>,
}
impl<T: Expression> Default for DefaultInterpreterCache<T> {
    fn default() -> Self {
        Self {
            cache: DependencyCache::default(),
        }
    }
}
impl<T: Expression> InterpreterCache<T> for DefaultInterpreterCache<T> {
    fn retrieve_result<'a>(
        &'a mut self,
        key: CacheKey,
        state: &DynamicState<T>,
    ) -> Option<&'a EvaluationResult<T>> {
        self.cache.get_mut(&key).and_then(|entry| {
            let state_dependencies = entry.result.dependencies();
            if state_dependencies.is_empty() || entry.overall_state_hash == state.id() {
                Some(entry.result())
            } else if entry.minimal_state_hash == hash_state_values(state, state_dependencies) {
                entry.overall_state_hash = state.id();
                Some(entry.result())
            } else {
                None
            }
        })
    }
    fn store_result(
        &mut self,
        key: CacheKey,
        state: &DynamicState<T>,
        result: EvaluationResult<T>,
        children: Vec<CacheKey>,
    ) {
        let state_dependencies = result.dependencies();
        let (overall_state_hash, minimal_state_hash) = if state_dependencies.is_empty() {
            (0, 0)
        } else {
            (state.id(), hash_state_values(state, state_dependencies))
        };
        self.cache.set(
            key,
            InterpreterCacheEntry {
                result,
                overall_state_hash,
                minimal_state_hash,
            },
            Some(children),
        );
    }
    fn retain(&mut self, key: CacheKey) {
        self.cache.retain(once(&key));
    }
    fn release(&mut self, key: CacheKey) {
        self.cache.release(once(&key));
    }
    fn gc(&mut self) -> GcMetrics {
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
