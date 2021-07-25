// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    core::{DynamicState, Expression, Rewritable, StateToken, Substitutions},
    hash::{hash_object, hash_state_values, HashId},
};

pub trait EvaluationCache {
    fn retrieve_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>>;
    fn store_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<Expression>,
    );
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
    ) -> Option<Option<Expression>>;
    fn store_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
        result: Option<Expression>,
    );
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>>;
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>);
}

pub struct NoopCache {}
impl NoopCache {
    pub fn new() -> Self {
        NoopCache {}
    }
}
impl EvaluationCache for NoopCache {
    fn retrieve_static_substitution(
        &mut self,
        _expression: &Expression,
        _substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        None
    }
    fn store_static_substitution(
        &mut self,
        _expression: &Expression,
        _substitutions: &Substitutions,
        _result: Option<Expression>,
    ) {
    }
    fn retrieve_dynamic_substitution(
        &mut self,
        _expression: &Expression,
        _state: &DynamicState,
    ) -> Option<Option<Expression>> {
        None
    }
    fn store_dynamic_substitution(
        &mut self,
        _expression: &Expression,
        _state: &DynamicState,
        _result: Option<Expression>,
    ) {
    }
    fn retrieve_reduction(&mut self, _expression: &Expression) -> Option<Option<Expression>> {
        None
    }
    fn store_reduction(&mut self, _expression: &Expression, _result: Option<Expression>) {}
}

pub struct SubstitutionCache {
    entries: HashMap<Expression, SubstitutionCacheEntry>,
}
impl SubstitutionCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
}
impl EvaluationCache for SubstitutionCache {
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>> {
        if !expression.is_reducible() {
            return None;
        }
        self.entries
            .get(expression)
            .map(|entry| entry.get_reduce().map(Expression::clone))
    }
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>) {
        if !expression.is_reducible() {
            return;
        }
        match self.entries.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => entry.get_mut().set_reduce(result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_reduce(expression, result));
            }
        }
    }
    fn retrieve_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        if expression.capture_depth() == 0 {
            return None;
        }
        match self.entries.get(expression) {
            Some(entry) => entry
                .get_substitute_static(substitutions)
                .map(|value| value.map(Expression::clone)),
            None => None,
        }
    }
    fn store_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<Expression>,
    ) {
        if expression.capture_depth() == 0 {
            return;
        }
        match self.entries.entry(Expression::clone(expression)) {
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
        expression: &Expression,
        state: &DynamicState,
    ) -> Option<Option<Expression>> {
        if expression.dynamic_dependencies().is_empty() {
            return None;
        }
        match self.entries.get(expression) {
            Some(entry) => entry
                .get_substitute_dynamic(state)
                .map(|value| value.map(Expression::clone)),
            None => None,
        }
    }
    fn store_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
        result: Option<Expression>,
    ) {
        if expression.dynamic_dependencies().is_empty() {
            return;
        }
        match self.entries.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => entry.get_mut().set_substitute_dynamic(state, result),
            Entry::Vacant(entry) => {
                entry.insert(SubstitutionCacheEntry::from_substitute_dynamic(
                    expression, state, result,
                ));
            }
        }
    }
}
struct SubstitutionCacheEntry {
    state_dependencies: Vec<StateToken>,
    reduce: Option<Expression>,
    substitute_static: HashMap<HashId, Option<Expression>>,
    substitute_dynamic: HashMap<HashId, Option<Expression>>,
}
impl SubstitutionCacheEntry {
    fn new(target: &Expression) -> Self {
        Self {
            state_dependencies: target.dynamic_dependencies().into_iter().collect(),
            reduce: None,
            substitute_static: HashMap::new(),
            substitute_dynamic: HashMap::new(),
        }
    }
    fn from_reduce(target: &Expression, value: Option<Expression>) -> Self {
        let mut entry = Self::new(target);
        entry.set_reduce(value);
        entry
    }
    fn from_substitute_static(
        target: &Expression,
        substitutions: &Substitutions,
        value: Option<Expression>,
    ) -> Self {
        let mut entry = Self::new(target);
        entry.set_substitute_static(substitutions, value);
        entry
    }
    fn from_substitute_dynamic(
        target: &Expression,
        state: &DynamicState,
        value: Option<Expression>,
    ) -> Self {
        let mut entry = Self::new(target);
        entry.set_substitute_dynamic(state, value);
        entry
    }
    fn get_reduce(&self) -> Option<&Expression> {
        self.reduce.as_ref()
    }
    fn set_reduce(&mut self, value: Option<Expression>) {
        self.reduce = value;
    }
    fn get_substitute_static(&self, substitutions: &Substitutions) -> Option<Option<&Expression>> {
        self.substitute_static
            .get(&hash_object(substitutions))
            .map(|entry| entry.as_ref())
    }
    fn set_substitute_static(&mut self, substitutions: &Substitutions, value: Option<Expression>) {
        self.substitute_static
            .insert(hash_object(substitutions), value);
    }
    fn get_substitute_dynamic(&self, state: &DynamicState) -> Option<Option<&Expression>> {
        let overall_state_hash = hash_object(state);
        self.substitute_dynamic
            .get(&overall_state_hash)
            .or_else(|| {
                let minimal_state_hash =
                    hash_state_values(state, self.state_dependencies.iter().copied());
                self.substitute_dynamic.get(&minimal_state_hash)
            })
            .map(|entry| entry.as_ref())
    }
    fn set_substitute_dynamic(&mut self, state: &DynamicState, value: Option<Expression>) {
        let overall_state_hash = hash_object(state);
        self.substitute_dynamic
            .insert(overall_state_hash, value.clone());
        let minimal_state_hash = hash_state_values(state, self.state_dependencies.iter().copied());
        self.substitute_dynamic.insert(minimal_state_hash, value);
    }
}

pub struct GenerationalSubstitutionCache {
    current: SubstitutionCache,
    previous: Option<SubstitutionCache>,
}
impl GenerationalSubstitutionCache {
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
impl EvaluationCache for GenerationalSubstitutionCache {
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>> {
        match self.current.retrieve_reduction(expression) {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => match previous.retrieve_reduction(expression) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .store_reduction(expression, Some(Expression::clone(&value)));
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
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>) {
        self.current.store_reduction(expression, result);
    }
    fn retrieve_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
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
                                    Some(Expression::clone(&value)),
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
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<Expression>,
    ) {
        self.current
            .store_static_substitution(expression, substitutions, result);
    }
    fn retrieve_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
    ) -> Option<Option<Expression>> {
        match self
            .current
            .retrieve_dynamic_substitution(expression, state)
        {
            Some(result) => Some(result),
            None => match &mut self.previous {
                Some(previous) => match previous.retrieve_dynamic_substitution(expression, state) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current.store_dynamic_substitution(
                                expression,
                                state,
                                Some(Expression::clone(&value)),
                            );
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
    fn store_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
        result: Option<Expression>,
    ) {
        self.current
            .store_dynamic_substitution(expression, state, result);
    }
}
