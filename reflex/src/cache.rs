// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashMap;

use crate::{
    core::{Expression, Substitutions},
    hash::{combine_hashes, HashId, Hashable},
};

struct CacheGeneration {
    reductions: HashMap<HashId, Option<Expression>>,
    substitutions: HashMap<HashId, Option<Expression>>,
}
impl CacheGeneration {
    fn new() -> Self {
        Self {
            reductions: HashMap::new(),
            substitutions: HashMap::new(),
        }
    }
}

pub struct EvaluationCache {
    current: CacheGeneration,
    previous: Option<CacheGeneration>,
}
impl EvaluationCache {
    pub fn new() -> Self {
        Self {
            current: CacheGeneration::new(),
            previous: None,
        }
    }
    pub fn retrieve_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
    ) -> Option<Option<Expression>> {
        let key = combine_hashes(expression.hash(), substitutions.hash());
        match self.current.substitutions.get(&key) {
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => match previous.substitutions.get(&key) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .substitutions
                                .insert(key, Some(Expression::clone(value)));
                            Some(Expression::clone(value))
                        }
                        None => None,
                    }),
                    None => None,
                },
                None => None,
            },
        }
    }
    pub fn store_substitution(
        &mut self,
        expression: &Expression,
        substitutions: &Substitutions,
        result: Option<&Expression>,
    ) {
        let key = combine_hashes(expression.hash(), substitutions.hash());
        self.current
            .substitutions
            .insert(key, result.map(Expression::clone));
    }
    pub fn retrieve_reduction(&mut self, expression: &Expression) -> Option<Option<Expression>> {
        let key = expression.hash();
        match self.current.reductions.get(&key) {
            Some(result) => Some(match result {
                Some(value) => Some(Expression::clone(value)),
                None => None,
            }),
            None => match &self.previous {
                Some(previous) => match previous.reductions.get(&key) {
                    Some(result) => Some(match result {
                        Some(value) => {
                            self.current
                                .reductions
                                .insert(key, Some(Expression::clone(value)));
                            Some(Expression::clone(value))
                        }
                        None => None,
                    }),
                    None => None,
                },
                None => None,
            },
        }
    }
    pub fn store_reduction(&mut self, expression: &Expression, result: Option<&Expression>) {
        let key = expression.hash();
        self.current
            .reductions
            .insert(key, result.map(Expression::clone));
    }
    pub fn next(self) -> Self {
        Self {
            current: CacheGeneration::new(),
            previous: Some(self.current),
        }
    }
}
