// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{once, FromIterator},
};

use crate::{
    core::{Expression, Substitutions},
    hash::{hash_object, HashId},
};

pub trait EvaluationCache {
    fn retrieve_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>>;
    fn store_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<&Expression>,
    );
    fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>>;
    fn store_reduction(&mut self, expression: &Expression, result: Option<&Expression>);
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
    pub fn gc(self) -> Self {
        Self {
            current: CacheGeneration::new(),
            previous: Some(self.current),
        }
    }
}
impl EvaluationCache for GenerationalGc {
    fn retrieve_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        match self
            .current
            .retrieve_substitution(expression, substitutions)
        {
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(&value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => match previous.retrieve_substitution(expression, substitutions) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current.store_substitution(
                                expression,
                                substitutions,
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
    fn store_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<&Expression>,
    ) {
        self.current
            .store_substitution(expression, substitutions, result.map(Expression::clone));
    }
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
    fn store_reduction(&mut self, expression: &Expression, result: Option<&Expression>) {
        self.current
            .store_reduction(expression, result.map(Expression::clone));
    }
}

struct CacheGeneration {
    reductions: HashMap<Expression, Option<Expression>>,
    substitutions: HashMap<Expression, HashMap<HashId, Option<Expression>>>,
}
impl CacheGeneration {
    fn new() -> Self {
        Self {
            reductions: HashMap::new(),
            substitutions: HashMap::new(),
        }
    }
    fn retrieve_reduction(&self, expression: &Expression) -> Option<Option<Expression>> {
        match self.reductions.get(expression) {
            None => None,
            Some(result) => match result {
                None => Some(None),
                Some(value) => Some(Some(Expression::clone(value))),
            },
        }
    }
    fn store_reduction(&mut self, expression: &Expression, result: Option<Expression>) {
        self.reductions
            .insert(Expression::clone(expression), result);
    }
    fn retrieve_substitution(
        &self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        match self
            .substitutions
            .get(expression)
            .and_then(|entry| entry.get(&hash_object(substitutions)))
        {
            None => None,
            Some(result) => match result {
                None => None,
                Some(value) => Some(Some(Expression::clone(value))),
            },
        }
    }
    fn store_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<Expression>,
    ) {
        match self.substitutions.entry(Expression::clone(expression)) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(hash_object(substitutions), result);
            }
            Entry::Vacant(entry) => {
                entry.insert(HashMap::from_iter(once((
                    hash_object(substitutions),
                    result,
                ))));
            }
        }
    }
}
