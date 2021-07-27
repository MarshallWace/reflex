// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use fnv::FnvHashMap;

use crate::{
    cache::hash_state_values,
    core::{DynamicState, EvaluationResult, Expression},
    hash::HashId,
};

pub trait InterpreterCache<T: Expression> {
    fn retrieve_result(
        &self,
        key: HashId,
        state: Option<&DynamicState<T>>,
    ) -> Option<&EvaluationResult<T>>;
    fn store_result(&mut self, key: HashId, state: &DynamicState<T>, value: EvaluationResult<T>);
}

pub struct DefaultInterpreterCache<T: Expression> {
    cache: FnvHashMap<HashId, EvaluationResult<T>>,
    stateful_cache: FnvHashMap<(HashId, HashId), EvaluationResult<T>>,
}
impl<T: Expression> Default for DefaultInterpreterCache<T> {
    fn default() -> Self {
        Self {
            cache: FnvHashMap::default(),
            stateful_cache: FnvHashMap::default(),
        }
    }
}
impl<T: Expression> InterpreterCache<T> for DefaultInterpreterCache<T> {
    fn retrieve_result<'a>(
        &'a self,
        key: HashId,
        state: Option<&DynamicState<T>>,
    ) -> Option<&'a EvaluationResult<T>> {
        self.cache.get(&key).and_then(|result| {
            if result.dependencies().is_empty() {
                Some(result)
            } else if let Some(state) = state {
                let overall_state_hash = state.id();
                self.stateful_cache
                    .get(&(key, overall_state_hash))
                    .or_else(|| {
                        let minimal_state_hash = hash_state_values(state, result.dependencies());
                        self.stateful_cache.get(&(key, minimal_state_hash))
                    })
            } else {
                None
            }
        })
    }
    fn store_result(&mut self, key: HashId, state: &DynamicState<T>, value: EvaluationResult<T>) {
        if !value.dependencies().is_empty() {
            let overall_state_hash = state.id();
            self.stateful_cache
                .insert((key, overall_state_hash), value.clone());
            let minimal_state_hash = hash_state_values(state, value.dependencies());
            self.stateful_cache
                .insert((key, minimal_state_hash), value.clone());
        }
        self.cache.insert(key, value);
    }
}
