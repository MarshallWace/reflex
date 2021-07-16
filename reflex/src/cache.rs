// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    core::{DynamicState, Expression, Substitutions},
    hash::{hash_object, HashId},
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
        result: Option<&Expression>,
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
        result: Option<&Expression>,
    );
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>>;
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>);
}

pub struct GenerationalGc {
    current: CacheGeneration,
    previous: Option<CacheGeneration>,
}
impl GenerationalGc {
    pub fn new() -> Self {
        Self {
            current: CacheGeneration::new(),
            previous: None,
        }
    }
    pub fn advance(self) -> Self {
        Self {
            current: CacheGeneration::new(),
            previous: Some(self.current),
        }
    }
}
impl EvaluationCache for GenerationalGc {
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>> {
        match self.current.retrieve_reduction(expression) {
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(&value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => match previous.retrieve_reduction(expression) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .store_reduction(expression, Some(Expression::clone(&value)));
                            Some(Expression::clone(&value))
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
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(&value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => {
                    match previous.retrieve_static_substitution(expression, substitutions) {
                        Some(result) => Some(match result {
                            Some(value) => {
                                self.current.store_static_substitution(
                                    expression,
                                    substitutions,
                                    Some(Expression::clone(&value)),
                                );
                                Some(Expression::clone(&value))
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
        result: Option<&Expression>,
    ) {
        self.current.store_static_substitution(
            expression,
            substitutions,
            result.map(Expression::clone),
        );
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
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(&value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => match previous.retrieve_dynamic_substitution(expression, state) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current.store_dynamic_substitution(
                                expression,
                                state,
                                Some(Expression::clone(&value)),
                            );
                            Some(Expression::clone(&value))
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
        result: Option<&Expression>,
    ) {
        self.current
            .store_dynamic_substitution(expression, state, result.map(Expression::clone));
    }
}

struct CacheGeneration {
    entries: HashMap<Expression, CacheEntry>,
}
impl CacheGeneration {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
    fn retrieve_reduction(&self, expression: &Expression) -> Option<Option<Expression>> {
        self.entries
            .get(expression)
            .map(|entry| entry.get_reduce().map(Expression::clone))
    }
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>) {
        match self.entries.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => entry.get_mut().set_reduce(result),
            Entry::Vacant(entry) => {
                entry.insert(CacheEntry::from_reduce(result));
            }
        }
    }
    fn retrieve_static_substitution(
        &self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        match self.entries.get(expression) {
            Some(entry) => match entry.get_substitute_static(substitutions) {
                Some(value) => Some(value.map(Expression::clone)),
                None => None,
            },
            None => None,
        }
    }
    fn store_static_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<Expression>,
    ) {
        match self.entries.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().set_substitute_static(substitutions, result)
            }
            Entry::Vacant(entry) => {
                entry.insert(CacheEntry::from_substitute_static(substitutions, result));
            }
        }
    }
    fn retrieve_dynamic_substitution(
        &self,
        expression: &Expression,
        state: &DynamicState,
    ) -> Option<Option<Expression>> {
        match self.entries.get(expression) {
            Some(entry) => match entry.get_substitute_dynamic(state) {
                Some(value) => Some(value.map(Expression::clone)),
                None => None,
            },
            None => None,
        }
    }
    fn store_dynamic_substitution(
        &mut self,
        expression: &Expression,
        state: &DynamicState,
        result: Option<Expression>,
    ) {
        match self.entries.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => entry.get_mut().set_substitute_dynamic(state, result),
            Entry::Vacant(entry) => {
                entry.insert(CacheEntry::from_substitute_dynamic(state, result));
            }
        }
    }
}

struct CacheEntry {
    reduce: Option<Expression>,
    substitute_static: HashMap<HashId, Option<Expression>>,
    substitute_dynamic: HashMap<HashId, Option<Expression>>,
}
impl CacheEntry {
    fn new() -> Self {
        Self {
            reduce: None,
            substitute_static: HashMap::new(),
            substitute_dynamic: HashMap::new(),
        }
    }
    fn from_reduce(value: Option<Expression>) -> Self {
        let mut entry = Self::new();
        entry.set_reduce(value);
        entry
    }
    fn from_substitute_static(substitutions: &Substitutions, value: Option<Expression>) -> Self {
        let mut entry = Self::new();
        entry.set_substitute_static(substitutions, value);
        entry
    }
    fn from_substitute_dynamic(state: &DynamicState, value: Option<Expression>) -> Self {
        let mut entry = Self::new();
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
        self.substitute_dynamic
            .get(&hash_object(state))
            .map(|entry| entry.as_ref())
    }
    fn set_substitute_dynamic(&mut self, state: &DynamicState, value: Option<Expression>) {
        self.substitute_dynamic.insert(hash_object(state), value);
    }
}
