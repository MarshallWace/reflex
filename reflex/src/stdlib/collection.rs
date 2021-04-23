// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    core::{DependencyList, Expression, Rewritable, StackOffset, Substitutions},
    hash::{prefix_hash, HashId, Hashable},
};

pub mod hashmap;
use hashmap::HashMapTerm;
pub mod hashset;
use hashset::HashSetTerm;
pub mod vector;
use vector::VectorTerm;

#[derive(PartialEq, Clone, Debug)]
pub enum CollectionTerm {
    HashMap(HashMapTerm),
    HashSet(HashSetTerm),
    Vector(VectorTerm),
}
impl Hashable for CollectionTerm {
    fn hash(&self) -> HashId {
        match self {
            CollectionTerm::HashMap(term) => prefix_hash(0, term.hash()),
            CollectionTerm::HashSet(term) => prefix_hash(1, term.hash()),
            CollectionTerm::Vector(term) => prefix_hash(2, term.hash()),
        }
    }
}
impl Rewritable for CollectionTerm {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::HashMap(term) => term.capture_depth(),
            Self::HashSet(term) => term.capture_depth(),
            Self::Vector(term) => term.capture_depth(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::HashMap(term) => term.dynamic_dependencies(),
            Self::HashSet(term) => term.dynamic_dependencies(),
            Self::Vector(term) => term.dynamic_dependencies(),
        }
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.substitute(substitutions),
            Self::HashSet(term) => term.substitute(substitutions),
            Self::Vector(term) => term.substitute(substitutions),
        }
    }
    fn optimize(&self) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.optimize(),
            Self::HashSet(term) => term.optimize(),
            Self::Vector(term) => term.optimize(),
        }
    }
}
impl fmt::Display for CollectionTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CollectionTerm::HashMap(term) => fmt::Display::fmt(term, f),
            CollectionTerm::HashSet(term) => fmt::Display::fmt(term, f),
            CollectionTerm::Vector(term) => fmt::Display::fmt(term, f),
        }
    }
}
