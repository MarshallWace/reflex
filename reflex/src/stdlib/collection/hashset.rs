// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, fmt, iter::once};

use crate::{
    cache::EvaluationCache,
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, optimize_multiple,
        substitute_multiple, DependencyList, Expression, Rewritable, StackOffset, Substitutions,
        Term,
    },
    hash::{hash_unordered_sequence, HashId, Hashable},
};

use super::CollectionTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct HashSetTerm {
    lookup: HashSet<HashId>,
    values: Vec<Expression>,
}
impl Hashable for HashSetTerm {
    fn hash(&self) -> HashId {
        hash_unordered_sequence(self.values.iter().map(|value| value.hash()))
    }
}
impl HashSetTerm {
    pub fn new(items: impl IntoIterator<Item = Expression>) -> Self {
        let values: Vec<_> = items.into_iter().collect();
        let lookup = values.iter().map(|value| value.hash()).collect();
        Self { lookup, values }
    }
    pub fn values(&self) -> &[Expression] {
        &self.values
    }
    pub fn has(&self, value: &Expression) -> bool {
        self.lookup.contains(&value.hash())
    }
    pub fn add(&self, value: Expression) -> Option<Expression> {
        if self.has(&value) {
            None
        } else {
            Some(Expression::new(Term::Collection(CollectionTerm::HashSet(
                Self::new(self.values.iter().map(Expression::clone).chain(once(value))),
            ))))
        }
    }
    pub fn iterate(&self) -> impl IntoIterator<Item = Expression> + ExactSizeIterator + '_ {
        self.values.iter().map(Expression::clone)
    }
    pub fn collect(entries: impl IntoIterator<Item = Expression>) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::HashSet(Self::new(
            entries,
        ))))
    }
}
impl Rewritable for HashSetTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.values)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.values)
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        substitute_multiple(&self.values, substitutions, cache).map(|updated| {
            Expression::new(Term::Collection(CollectionTerm::HashSet(Self::new(
                updated,
            ))))
        })
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        optimize_multiple(&self.values, cache).map(|updated| {
            Expression::new(Term::Collection(CollectionTerm::HashSet(Self::new(
                updated,
            ))))
        })
    }
}
impl fmt::Display for HashSetTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<hashset:{}>", self.values.len())
    }
}
