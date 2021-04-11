// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    core::{
        capture_depth_multiple, dynamic_dependencies_multiple, optimize_multiple,
        substitute_multiple, DependencyList, Expression, Rewritable, StackOffset, Substitutions,
        Term,
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
    pub fn new(items: Vec<Expression>) -> Self {
        Self { items }
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
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        substitute_multiple(&self.items, substitutions).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
    fn optimize(&self) -> Option<Expression> {
        optimize_multiple(&self.items).map(|items| {
            Expression::new(Term::Collection(CollectionTerm::Vector(Self::new(items))))
        })
    }
}
impl fmt::Display for VectorTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<vector:{}>", self.items.len())
    }
}
