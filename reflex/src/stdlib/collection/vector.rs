// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    cache::EvaluationCache,
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, optimize_multiple, signals_multiple,
        substitute_multiple, DependencyList, Expression, Rewritable, Signal, StackOffset,
        Substitutions, Term,
    },
    hash::{hash_sequence, HashId, Hashable},
};

use super::CollectionTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct VectorTerm {
    items: Vec<Expression>,
}
impl Hashable for VectorTerm {
    fn hash(&self) -> HashId {
        hash_sequence(self.items.iter().map(|item| item.hash()))
    }
}
impl VectorTerm {
    pub fn new(items: impl IntoIterator<Item = Expression>) -> Self {
        Self {
            items: items.into_iter().collect(),
        }
    }
    pub fn get(&self, index: usize) -> Option<&Expression> {
        self.items.get(index)
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn items(&self) -> &[Expression] {
        &self.items
    }
    pub fn push(&self, value: Expression) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(
            self.items.iter().map(Expression::clone).chain(once(value)),
        ))))
    }
    pub fn iterate(&self) -> impl IntoIterator<Item = Expression> + ExactSizeIterator + '_ {
        self.items.iter().map(|item| Expression::clone(item))
    }
    pub fn collect(items: impl IntoIterator<Item = Expression>) -> Self {
        Self {
            items: items.into_iter().collect(),
        }
    }
}
impl Rewritable for VectorTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.items)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.items)
    }
    fn signals(&self) -> Vec<Signal> {
        signals_multiple(&self.items)
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        substitute_multiple(&self.items, substitutions, cache).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        optimize_multiple(&self.items, cache).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
}
impl fmt::Display for VectorTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<vector:{}>", self.items.len())
    }
}
