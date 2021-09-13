// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::HashSet,
    iter::{once, FromIterator},
};

use crate::{
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Rewritable, StackOffset, StateToken, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum VariableTerm<T: Expression> {
    Static(StaticVariableTerm),
    Dynamic(DynamicVariableTerm<T>),
}
impl<T: Expression> GraphNode for VariableTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Static(term) => term.capture_depth(),
            Self::Dynamic(term) => term.capture_depth(),
        }
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        match self {
            Self::Static(term) => term.free_variables(),
            Self::Dynamic(term) => term.free_variables(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Static(term) => term.dynamic_dependencies(),
            Self::Dynamic(term) => term.dynamic_dependencies(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Static(term) => term.is_static(),
            Self::Dynamic(term) => term.is_static(),
        }
    }
    fn is_atomic(&self) -> bool {
        match self {
            Self::Static(term) => term.is_atomic(),
            Self::Dynamic(term) => term.is_atomic(),
        }
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for VariableTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        match self {
            Self::Static(term) => term.subexpressions(),
            Self::Dynamic(term) => term.subexpressions(),
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
            Self::Static(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Dynamic(term) => term.substitute_static(substitutions, factory, allocator, cache),
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
            Self::Static(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Dynamic(term) => term.substitute_dynamic(state, factory, allocator, cache),
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        match self {
            Self::Static(term) => term.hoist_free_variables(factory, allocator),
            Self::Dynamic(term) => term.hoist_free_variables(factory, allocator),
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Static(term) => term.normalize(factory, allocator, cache),
            Self::Dynamic(term) => term.normalize(factory, allocator, cache),
        }
    }
}
impl<T: Expression + Compile<T>> Compile<T> for VariableTerm<T> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        match self {
            Self::Static(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Dynamic(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
        }
    }
}
impl<T: Expression> std::fmt::Display for VariableTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(term) => std::fmt::Display::fmt(term, f),
            Self::Dynamic(term) => std::fmt::Display::fmt(term, f),
        }
    }
}
impl<T: Expression> serde::Serialize for VariableTerm<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Static(term) => serde::Serialize::serialize(term, serializer),
            Self::Dynamic(term) => serde::Serialize::serialize(term, serializer),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct StaticVariableTerm {
    offset: StackOffset,
}
impl StaticVariableTerm {
    pub fn new(offset: StackOffset) -> Self {
        Self { offset }
    }
    pub fn offset(&self) -> StackOffset {
        self.offset
    }
}
impl GraphNode for StaticVariableTerm {
    fn capture_depth(&self) -> StackOffset {
        self.offset + 1
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::from_iter(once(self.offset))
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for StaticVariableTerm {
    fn subexpressions(&self) -> Vec<&T> {
        Vec::new()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        substitutions.get(self.offset, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        _state: &DynamicState<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
    fn hoist_free_variables(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        None
    }
    fn normalize(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
}
impl<T: Expression + Compile<T>> Compile<T> for StaticVariableTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        Ok((
            Program::new(once(Instruction::PushStatic {
                offset: self.offset + stack_offset,
            })),
            NativeFunctionRegistry::default(),
        ))
    }
}
impl std::fmt::Display for StaticVariableTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<static:{}>", self.offset)
    }
}
impl serde::Serialize for StaticVariableTerm {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct DynamicVariableTerm<T: Expression> {
    state_token: StateToken,
    fallback: T,
}
impl<T: Expression> DynamicVariableTerm<T> {
    pub fn new(state_token: StateToken, fallback: T) -> Self {
        Self {
            state_token,
            fallback,
        }
    }
    pub fn state_token(&self) -> StateToken {
        self.state_token
    }
    pub fn fallback(&self) -> &T {
        &self.fallback
    }
}
impl<T: Expression> GraphNode for DynamicVariableTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::of(self.state_token)
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for DynamicVariableTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        Vec::new()
    }
    fn substitute_static(
        &self,
        _substitutions: &Substitutions<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        Some(
            state
                .get(self.state_token)
                .unwrap_or(&self.fallback)
                .clone(),
        )
    }
    fn hoist_free_variables(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        None
    }
    fn normalize(
        &self,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        None
    }
}
impl<T: Expression + Compile<T>> Compile<T> for DynamicVariableTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let (compiled_fallback, native_functions) =
            self.fallback
                .compile(VarArgs::Lazy, stack_offset, factory, allocator, compiler)?;
        let mut result = compiled_fallback;
        result.push(Instruction::PushDynamic {
            state_token: self.state_token,
        });
        Ok((result, native_functions))
    }
}
impl<T: Expression> std::fmt::Display for DynamicVariableTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<dynamic:{}:{}>", self.state_token, self.fallback)
    }
}
impl<T: Expression> serde::Serialize for DynamicVariableTerm<T> {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
    }
}
