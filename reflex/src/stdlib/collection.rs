// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    core::{DependencyList, Expression, Rewritable, StackOffset, Substitutions},
    hash::{prefix_hash, HashId, Hashable},
};

pub mod vector;
use vector::VectorTerm;

#[derive(PartialEq, Clone, Debug)]
pub enum CollectionTerm {
    Vector(VectorTerm),
}
impl Hashable for CollectionTerm {
    fn hash(&self) -> HashId {
        match self {
            CollectionTerm::Vector(term) => prefix_hash(0, term.hash()),
        }
    }
}
impl Rewritable for CollectionTerm {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Vector(term) => term.capture_depth(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Vector(term) => term.dynamic_dependencies(),
        }
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match self {
            Self::Vector(term) => term.substitute(substitutions),
        }
    }
    fn optimize(&self) -> Option<Expression> {
        match self {
            Self::Vector(term) => term.optimize(),
        }
    }
}
impl fmt::Display for CollectionTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CollectionTerm::Vector(term) => fmt::Display::fmt(term, f),
        }
    }
}
