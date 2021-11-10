// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    core::{
        hash_state_values, DynamicState, EvaluationCache, EvaluationCacheMetrics, EvaluationResult,
        Expression, Reducible, Rewritable, StateToken, Substitutions,
    },
    hash::{hash_object, HashId},
};

#[derive(Default)]
pub struct NoopCache {}
impl<T: Expression> EvaluationCache<T> for NoopCache {
    fn retrieve_static_substitution(
        &mut self,
        _expression: &T,
        _substitutions: &Substitutions<T>,
    ) -> Option<Option<T>> {
        None
    }
    fn store_static_substitution(
        &mut self,
        _expression: &T,
        _substitutions: &Substitutions<T>,
        _result: Option<T>,
    ) {
    }
    fn retrieve_dynamic_substitution(
        &mut self,
        _expression: &T,
        _deep: bool,
        _state: &impl DynamicState<T>,
    ) -> Option<Option<T>> {
        None
    }
    fn store_dynamic_substitution(
        &mut self,
        _expression: &T,
        _deep: bool,
        _state: &impl DynamicState<T>,
        _result: Option<T>,
    ) {
    }
    fn retrieve_evaluation(
        &mut self,
        _expression: &T,
        _state: &impl DynamicState<T>,
    ) -> Option<Option<EvaluationResult<T>>> {
        None
    }
    fn store_evaluation(
        &mut self,
        _expression: &T,
        _state: &impl DynamicState<T>,
        _result: Option<EvaluationResult<T>>,
    ) {
    }
    fn retrieve_reduction(&mut self, _expression: &T) -> Option<Option<T>> {
        None
    }
    fn store_reduction(&mut self, _expression: &T, _result: Option<T>) {}
    fn retrieve_normalization(&mut self, _expression: &T) -> Option<Option<T>> {
        None
    }
    fn store_normalization(&mut self, _expression: &T, _result: Option<T>) {}
}

pub struct SubstitutionCache<T: Expression> {
    entries: HashMap<T, SubstitutionCacheEntry<T>>,
    metrics: EvaluationCacheMetrics,
}
impl<T: Expression> SubstitutionCache<T> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            metrics: EvaluationCacheMetrics::default(),
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> EvaluationCache<T> for SubstitutionCache<T> {
    fn retrieve_reduction(&mut self, expression: &T) -> Option<Option<T>> {
        if !expression.is_reducible() {
            return None;
        }
        let result = self
            .entries
            .get(expression)
            .map(|entry| entry.get_reduce().cloned());
        match result {
            Some(result) => {
                self.metrics.reductions.cache_hit();
                Some(result)
            }
            None => {
                self.metrics.reductions.cache_miss();
                None
            }
        }
    }
    fn store_reduction(&mut self, expression: &T, result: Option<T>) {
        if !expression.is_reducible() {
            return;
        }
        match self.entries.entry(expression.clone()) {
            Entry::Occupied(mut entry) => entry.get_mut().set_reduce(result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_reduce(expression, result));
            }
        }
    }
    fn retrieve_normalization(&mut self, expression: &T) -> Option<Option<T>> {
        self.entries
            .get(expression)
            .map(|entry| entry.get_normalize().cloned())
    }
    fn store_normalization(&mut self, expression: &T, result: Option<T>) {
        match self.entries.entry(expression.clone()) {
            Entry::Occupied(mut entry) => entry.get_mut().set_normalize(result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_normalize(expression, result));
            }
        }
    }
    fn retrieve_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
    ) -> Option<Option<T>> {
        if expression.capture_depth() == 0 {
            return None;
        }
        let result = match self.entries.get(expression) {
            Some(entry) => entry
                .get_substitute_static(substitutions)
                .map(|value| value.cloned()),
            None => None,
        };
        match result {
            Some(result) => {
                self.metrics.static_substitutions.cache_hit();
                Some(result)
            }
            None => {
                self.metrics.static_substitutions.cache_miss();
                None
            }
        }
    }
    fn store_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
        result: Option<T>,
    ) {
        if expression.capture_depth() == 0 {
            return;
        }
        match self.entries.entry(expression.clone()) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().set_substitute_static(substitutions, result)
            }
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_substitute_static(
                    expression,
                    substitutions,
                    result,
                ));
            }
        }
    }
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &T,
        deep: bool,
        state: &impl DynamicState<T>,
    ) -> Option<Option<T>> {
        if deep || !expression.has_dynamic_dependencies(deep) {
            return None;
        }
        let result = match self.entries.get(expression) {
            Some(entry) => entry
                .get_substitute_dynamic(state)
                .map(|value| value.cloned()),
            None => None,
        };
        match result {
            Some(result) => {
                self.metrics.dynamic_substitutions.cache_hit();
                Some(result)
            }
            None => {
                self.metrics.dynamic_substitutions.cache_miss();
                None
            }
        }
    }
    fn store_dynamic_substitution(
        &mut self,
        expression: &T,
        deep: bool,
        state: &impl DynamicState<T>,
        result: Option<T>,
    ) {
        if deep || !expression.has_dynamic_dependencies(deep) {
            return;
        }
        match self.entries.entry(expression.clone()) {
            Entry::Occupied(mut entry) => entry.get_mut().set_substitute_dynamic(state, result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_substitute_dynamic(
                    expression, state, result,
                ));
            }
        }
    }
    fn retrieve_evaluation(
        &mut self,
        expression: &T,
        state: &impl DynamicState<T>,
    ) -> Option<Option<EvaluationResult<T>>> {
        if expression.is_static() {
            return None;
        }
        let result = match self.entries.get(expression) {
            Some(entry) => entry.get_evaluate(state).map(|value| value.cloned()),
            None => None,
        };
        match result {
            Some(result) => {
                self.metrics.evaluations.cache_hit();
                Some(result)
            }
            None => {
                self.metrics.evaluations.cache_miss();
                None
            }
        }
    }
    fn store_evaluation(
        &mut self,
        expression: &T,
        state: &impl DynamicState<T>,
        result: Option<EvaluationResult<T>>,
    ) {
        if expression.is_static() {
            return;
        }
        match self.entries.entry(expression.clone()) {
            Entry::Occupied(mut entry) => entry.get_mut().set_evaluate(state, result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_evaluate(
                    expression, state, result,
                ));
            }
        }
    }
    fn metrics(&self) -> Option<&EvaluationCacheMetrics> {
        Some(&self.metrics)
    }
    fn clear_metrics(&mut self) -> Option<EvaluationCacheMetrics> {
        Some(std::mem::replace(
            &mut self.metrics,
            EvaluationCacheMetrics::default(),
        ))
    }
}
struct SubstitutionCacheEntry<T: Expression> {
    state_dependencies: Vec<StateToken>,
    reduce: Option<T>,
    normalize: Option<T>,
    evaluate: HashMap<HashId, Option<EvaluationResult<T>>>,
    substitute_static: HashMap<HashId, Option<T>>,
    substitute_dynamic: HashMap<HashId, Option<T>>,
}
impl<T: Expression> SubstitutionCacheEntry<T> {
    fn new(target: &T) -> Self {
        Self {
            state_dependencies: target.dynamic_dependencies(false).iter().collect(),
            reduce: None,
            normalize: None,
            evaluate: HashMap::new(),
            substitute_static: HashMap::new(),
            substitute_dynamic: HashMap::new(),
        }
    }
    fn from_reduce(target: &T, value: Option<T>) -> Self {
        let mut entry = Self::new(target);
        entry.set_reduce(value);
        entry
    }
    fn from_normalize(target: &T, value: Option<T>) -> Self {
        let mut entry = Self::new(target);
        entry.set_normalize(value);
        entry
    }
    fn from_evaluate(
        target: &T,
        state: &impl DynamicState<T>,
        value: Option<EvaluationResult<T>>,
    ) -> Self {
        let mut entry = Self::new(target);
        entry.set_evaluate(state, value);
        entry
    }
    fn from_substitute_static(
        target: &T,
        substitutions: &Substitutions<T>,
        value: Option<T>,
    ) -> Self {
        let mut entry = Self::new(target);
        entry.set_substitute_static(substitutions, value);
        entry
    }
    fn from_substitute_dynamic(target: &T, state: &impl DynamicState<T>, value: Option<T>) -> Self {
        let mut entry = Self::new(target);
        entry.set_substitute_dynamic(state, value);
        entry
    }
    fn get_reduce(&self) -> Option<&T> {
        self.reduce.as_ref()
    }
    fn set_reduce(&mut self, value: Option<T>) {
        self.reduce = value;
    }
    fn get_normalize(&self) -> Option<&T> {
        self.normalize.as_ref()
    }
    fn set_normalize(&mut self, value: Option<T>) {
        self.normalize = value;
    }
    fn get_evaluate(&self, state: &impl DynamicState<T>) -> Option<Option<&EvaluationResult<T>>> {
        let overall_state_hash = state.id();
        self.evaluate
            .get(&overall_state_hash)
            .or_else(|| {
                let minimal_state_hash =
                    hash_state_values(state, self.state_dependencies.iter().copied());
                self.evaluate.get(&minimal_state_hash)
            })
            .map(|entry| entry.as_ref())
    }
    fn set_evaluate(&mut self, state: &impl DynamicState<T>, value: Option<EvaluationResult<T>>) {
        let overall_state_hash = state.id();
        self.evaluate.insert(overall_state_hash, value.clone());
        let minimal_state_hash = hash_state_values(state, self.state_dependencies.iter().copied());
        self.evaluate.insert(minimal_state_hash, value);
    }
    fn get_substitute_static(&self, substitutions: &Substitutions<T>) -> Option<Option<&T>> {
        self.substitute_static
            .get(&hash_object(substitutions))
            .map(|entry| entry.as_ref())
    }
    fn set_substitute_static(&mut self, substitutions: &Substitutions<T>, value: Option<T>) {
        self.substitute_static
            .insert(hash_object(substitutions), value);
    }
    fn get_substitute_dynamic(&self, state: &impl DynamicState<T>) -> Option<Option<&T>> {
        let overall_state_hash = state.id();
        self.substitute_dynamic
            .get(&overall_state_hash)
            .or_else(|| {
                let minimal_state_hash =
                    hash_state_values(state, self.state_dependencies.iter().copied());
                self.substitute_dynamic.get(&minimal_state_hash)
            })
            .map(|entry| entry.as_ref())
    }
    fn set_substitute_dynamic(&mut self, state: &impl DynamicState<T>, value: Option<T>) {
        let overall_state_hash = state.id();
        self.substitute_dynamic
            .insert(overall_state_hash, value.clone());
        let minimal_state_hash = hash_state_values(state, self.state_dependencies.iter().copied());
        self.substitute_dynamic.insert(minimal_state_hash, value);
    }
}

pub struct GenerationalSubstitutionCache<T: Expression> {
    current: SubstitutionCache<T>,
    previous: Option<SubstitutionCache<T>>,
}
impl<T: Expression> GenerationalSubstitutionCache<T> {
    pub fn new() -> Self {
        Self {
            current: SubstitutionCache::new(),
            previous: None,
        }
    }
    pub fn advance(self) -> Self {
        Self {
            current: SubstitutionCache::new(),
            previous: Some(self.current),
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> EvaluationCache<T>
    for GenerationalSubstitutionCache<T>
{
    fn retrieve_reduction(&mut self, expression: &T) -> Option<Option<T>> {
        match self.current.retrieve_reduction(expression) {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => match previous.retrieve_reduction(expression) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .store_reduction(expression, Some(value.clone()));
                            Some(value)
                        }
                        None => None,
                    }),
                    None => None,
                },
                None => None,
            },
        }
    }
    fn store_reduction(&mut self, expression: &T, result: Option<T>) {
        self.current.store_reduction(expression, result);
    }
    fn retrieve_normalization(&mut self, expression: &T) -> Option<Option<T>> {
        match self.current.retrieve_normalization(expression) {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => match previous.retrieve_normalization(expression) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .store_normalization(expression, Some(value.clone()));
                            Some(value)
                        }
                        None => None,
                    }),
                    None => None,
                },
                None => None,
            },
        }
    }
    fn store_normalization(&mut self, expression: &T, result: Option<T>) {
        self.current.store_normalization(expression, result);
    }
    fn retrieve_evaluation(
        &mut self,
        expression: &T,
        state: &impl DynamicState<T>,
    ) -> Option<Option<EvaluationResult<T>>> {
        match self.current.retrieve_evaluation(expression, state) {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => match previous.retrieve_evaluation(expression, state) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .store_evaluation(expression, state, Some(value.clone()));
                            Some(value)
                        }
                        None => None,
                    }),
                    None => None,
                },
                None => None,
            },
        }
    }
    fn store_evaluation(
        &mut self,
        expression: &T,
        state: &impl DynamicState<T>,
        result: Option<EvaluationResult<T>>,
    ) {
        self.current.store_evaluation(expression, state, result);
    }
    fn retrieve_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
    ) -> Option<Option<T>> {
        match self
            .current
            .retrieve_static_substitution(expression, substitutions)
        {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => {
                    match previous.retrieve_static_substitution(expression, substitutions) {
                        Some(result) => Some(match result {
                            Some(value) => {
                                self.current.store_static_substitution(
                                    expression,
                                    substitutions,
                                    Some(value.clone()),
                                );
                                Some(value)
                            }
                            None => None,
                        }),
                        None => None,
                    }
                }
                None => None,
            },
        }
    }
    fn store_static_substitution(
        &mut self,
        expression: &T,
        substitutions: &Substitutions<T>,
        result: Option<T>,
    ) {
        self.current
            .store_static_substitution(expression, substitutions, result);
    }
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &T,
        deep: bool,
        state: &impl DynamicState<T>,
    ) -> Option<Option<T>> {
        match self
            .current
            .retrieve_dynamic_substitution(expression, deep, state)
        {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => {
                    match previous.retrieve_dynamic_substitution(expression, deep, state) {
                        Some(result) => Some(match result {
                            Some(value) => {
                                self.current.store_dynamic_substitution(
                                    expression,
                                    deep,
                                    state,
                                    Some(value.clone()),
                                );
                                Some(value)
                            }
                            None => None,
                        }),
                        None => None,
                    }
                }
                None => None,
            },
        }
    }
    fn store_dynamic_substitution(
        &mut self,
        expression: &T,
        deep: bool,
        state: &impl DynamicState<T>,
        result: Option<T>,
    ) {
        self.current
            .store_dynamic_substitution(expression, deep, state, result);
    }
}
