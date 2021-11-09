// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    iter::{once, FromIterator},
};

use crate::{
    compiler::{Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, DynamicState, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, Rewritable, SerializeJson, StackOffset, StateToken, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        match self {
            Self::Static(term) => term.dynamic_dependencies(deep),
            Self::Dynamic(term) => term.dynamic_dependencies(deep),
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        match self {
            Self::Static(term) => term.has_dynamic_dependencies(deep),
            Self::Dynamic(term) => term.has_dynamic_dependencies(deep),
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
    fn children(&self) -> Vec<&T> {
        match self {
            Self::Static(term) => term.children(),
            Self::Dynamic(term) => term.children(),
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
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Static(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Dynamic(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
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
    ) -> Result<Program, String> {
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

impl<T: Expression> SerializeJson for VariableTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            Self::Static(term) => term.to_json(),
            Self::Dynamic(term) => term.to_json(),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for StaticVariableTerm {
    fn children(&self) -> Vec<&T> {
        Vec::new()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        substitutions.substitute_variable(self.offset, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        _deep: bool,
        _state: &impl DynamicState<T>,
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
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushStatic {
            offset: self.offset + stack_offset,
        })))
    }
}
impl std::fmt::Display for StaticVariableTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<static:{}>", self.offset)
    }
}
impl SerializeJson for StaticVariableTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::of(self.state_token)
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        true
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for DynamicVariableTerm<T> {
    fn children(&self) -> Vec<&T> {
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
        _deep: bool,
        state: &impl DynamicState<T>,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        Some(
            state
                .get(&self.state_token)
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
    ) -> Result<Program, String> {
        let compiled_fallback =
            self.fallback
                .compile(VarArgs::Lazy, stack_offset, factory, allocator, compiler)?;
        let mut result = compiled_fallback;
        result.push(Instruction::PushDynamic {
            state_token: self.state_token,
        });
        Ok(result)
    }
}
impl<T: Expression> std::fmt::Display for DynamicVariableTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<dynamic:{}:{}>", self.state_token, self.fallback)
    }
}

impl<T: Expression> SerializeJson for DynamicVariableTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
