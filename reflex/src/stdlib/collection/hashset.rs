// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::BTreeSet, fmt, hash::Hash, iter::once};

use crate::{
    cache::EvaluationCache,
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, optimize_multiple, signals_multiple,
        substitute_multiple, DependencyList, Expression, Rewritable, Signal, StackOffset,
        Substitutions, Term,
    },
};

use super::CollectionTerm;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashSetTerm {
    lookup: BTreeSet<Expression>,
    values: Vec<Expression>,
}
impl Hash for HashSetTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for value in self.lookup.iter() {
            value.hash(state);
        }
    }
}
impl HashSetTerm {
    pub fn new(items: impl IntoIterator<Item = Expression>) -> Self {
        let lookup = items.into_iter().collect::<BTreeSet<_>>();
        let values = lookup
            .iter()
            .map(|value| Expression::clone(value))
            .collect();
        Self { lookup, values }
    }
    pub fn has(&self, value: &Expression) -> bool {
        self.lookup.contains(value)
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
    fn signals(&self) -> Vec<Signal> {
        signals_multiple(&self.values)
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_multiple(&self.values, substitutions, cache).map(|updated| {
            Expression::new(Term::Collection(CollectionTerm::HashSet(Self::new(
                updated,
            ))))
        })
    }
    fn optimize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        optimize_multiple(&self.values, cache).map(|updated| {
            Expression::new(Term::Collection(CollectionTerm::HashSet(Self::new(
                updated,
            ))))
        })
    }
}
impl fmt::Display for HashSetTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<hashset:{}>", self.lookup.len())
    }
}
