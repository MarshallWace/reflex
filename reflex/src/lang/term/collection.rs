// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashSet;

use crate::{
    compiler::{Compile, Compiler, NativeFunctionRegistry, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Iterable, Rewritable, StackOffset, Substitutions, VarArgs,
    },
};

mod hashmap;
pub use hashmap::*;
mod hashset;
pub use hashset::*;
mod vector;
pub use vector::*;

#[derive(Hash, Eq, PartialEq, Debug)]
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
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Vector(term) => term.dynamic_dependencies(),
            Self::HashMap(term) => term.dynamic_dependencies(),
            Self::HashSet(term) => term.dynamic_dependencies(),
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
    fn subexpressions(&self) -> Vec<&T> {
        match self {
            Self::Vector(term) => term.subexpressions(),
            Self::HashMap(term) => term.subexpressions(),
            Self::HashSet(term) => term.subexpressions(),
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
        state: &DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Vector(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::HashMap(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::HashSet(term) => term.substitute_dynamic(state, factory, allocator, cache),
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
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
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
impl<T: Expression> serde::Serialize for CollectionTerm<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Vector(term) => serde::Serialize::serialize(term, serializer),
            Self::HashMap(term) => serde::Serialize::serialize(term, serializer),
            Self::HashSet(term) => serde::Serialize::serialize(term, serializer),
        }
    }
}
