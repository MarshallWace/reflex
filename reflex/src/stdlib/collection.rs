// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    cache::EvaluationCache,
    core::{DependencyList, Expression, Rewritable, Signal, StackOffset, Substitutions},
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
    fn signals(&self) -> Vec<Signal> {
        match self {
            Self::HashMap(term) => term.signals(),
            Self::HashSet(term) => term.signals(),
            Self::Vector(term) => term.signals(),
        }
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.substitute(substitutions, cache),
            Self::HashSet(term) => term.substitute(substitutions, cache),
            Self::Vector(term) => term.substitute(substitutions, cache),
        }
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.optimize(cache),
            Self::HashSet(term) => term.optimize(cache),
            Self::Vector(term) => term.optimize(cache),
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
