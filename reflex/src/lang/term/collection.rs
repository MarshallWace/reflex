// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

pub use hashmap::*;
pub use hashset::*;
pub use vector::*;

use crate::{
    compiler::{Compile, Compiler, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Iterable, Rewritable, SerializeJson, StackOffset, Substitutions, VarArgs,
    },
};

mod hashmap;
mod hashset;
mod vector;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum CollectionTerm<T: Expression> {
    Vector(VectorTerm<T>),
    HashMap(HashMapTerm<T>),
    HashSet(HashSetTerm<T>),
}
impl<T: Expression> GraphNode for CollectionTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Vector(term) => term.capture_depth(),
            Self::HashMap(term) => term.capture_depth(),
            Self::HashSet(term) => term.capture_depth(),
        }
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        match self {
            Self::Vector(term) => term.free_variables(),
            Self::HashMap(term) => term.free_variables(),
            Self::HashSet(term) => term.free_variables(),
        }
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        match self {
            Self::Vector(term) => term.dynamic_dependencies(deep),
            Self::HashMap(term) => term.dynamic_dependencies(deep),
            Self::HashSet(term) => term.dynamic_dependencies(deep),
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        match self {
            Self::Vector(term) => term.has_dynamic_dependencies(deep),
            Self::HashMap(term) => term.has_dynamic_dependencies(deep),
            Self::HashSet(term) => term.has_dynamic_dependencies(deep),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Vector(term) => term.is_static(),
            Self::HashMap(term) => term.is_static(),
            Self::HashSet(term) => term.is_static(),
        }
    }
    fn is_atomic(&self) -> bool {
        match self {
            Self::Vector(term) => term.is_atomic(),
            Self::HashMap(term) => term.is_atomic(),
            Self::HashSet(term) => term.is_atomic(),
        }
    }
}
impl<T: Expression> Iterable for CollectionTerm<T> {
    fn is_empty(&self) -> bool {
        match self {
            Self::Vector(term) => term.is_empty(),
            Self::HashMap(term) => term.is_empty(),
            Self::HashSet(term) => term.is_empty(),
        }
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for CollectionTerm<T> {
    fn children(&self) -> Vec<&T> {
        match self {
            Self::Vector(term) => term.children(),
            Self::HashMap(term) => term.children(),
            Self::HashSet(term) => term.children(),
        }
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Vector(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::HashMap(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::HashSet(term) => term.substitute_static(substitutions, factory, allocator, cache),
        }
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Vector(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::HashMap(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::HashSet(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        match self {
            Self::Vector(term) => term.hoist_free_variables(factory, allocator),
            Self::HashMap(term) => term.hoist_free_variables(factory, allocator),
            Self::HashSet(term) => term.hoist_free_variables(factory, allocator),
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Vector(term) => term.normalize(factory, allocator, cache),
            Self::HashMap(term) => term.normalize(factory, allocator, cache),
            Self::HashSet(term) => term.normalize(factory, allocator, cache),
        }
    }
}
impl<T: Expression + Compile<T>> Compile<T> for CollectionTerm<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        match self {
            Self::Vector(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashMap(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashSet(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
        }
    }
}
impl<T: Expression> std::fmt::Display for CollectionTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vector(term) => std::fmt::Display::fmt(term, f),
            Self::HashMap(term) => std::fmt::Display::fmt(term, f),
            Self::HashSet(term) => std::fmt::Display::fmt(term, f),
        }
    }
}

impl<T: Expression> SerializeJson for CollectionTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            CollectionTerm::Vector(term) => term.to_json(),
            CollectionTerm::HashMap(term) => term.to_json(),
            CollectionTerm::HashSet(term) => term.to_json(),
        }
    }
}
