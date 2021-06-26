// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    cache::EvaluationCache,
    core::{DependencyList, DynamicState, Expression, Rewritable, StackOffset, Substitutions},
};

pub mod hashmap;
use hashmap::HashMapTerm;
pub mod hashset;
use hashset::HashSetTerm;
pub mod vector;
use vector::VectorTerm;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum CollectionTerm {
    HashMap(HashMapTerm),
    HashSet(HashSetTerm),
    Vector(VectorTerm),
}
impl CollectionTerm {
    pub fn len(&self) -> usize {
        match self {
            Self::HashMap(term) => term.len(),
            Self::HashSet(term) => term.len(),
            Self::Vector(term) => term.len(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.substitute_static(substitutions, cache),
            Self::HashSet(term) => term.substitute_static(substitutions, cache),
            Self::Vector(term) => term.substitute_static(substitutions, cache),
        }
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.substitute_dynamic(state, cache),
            Self::HashSet(term) => term.substitute_dynamic(state, cache),
            Self::Vector(term) => term.substitute_dynamic(state, cache),
        }
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        match self {
            Self::HashMap(term) => term.normalize(cache),
            Self::HashSet(term) => term.normalize(cache),
            Self::Vector(term) => term.normalize(cache),
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
