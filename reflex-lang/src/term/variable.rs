// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::HashSet,
    iter::{once, FromIterator},
};

use serde::{Deserialize, Serialize};

use reflex::core::{
    DependencyList, DynamicState, Eagerness, EvaluationCache, Expression, ExpressionFactory,
    GraphNode, HeapAllocator, Internable, Rewritable, SerializeJson, StackOffset, Substitutions,
    VariableTermType,
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct VariableTerm {
    offset: StackOffset,
}
impl VariableTerm {
    pub fn new(offset: StackOffset) -> Self {
        Self { offset }
    }
}
impl VariableTermType for VariableTerm {
    fn offset(&self) -> StackOffset {
        self.offset
    }
}
impl GraphNode for VariableTerm {
    fn size(&self) -> usize {
        1
    }
    fn capture_depth(&self) -> StackOffset {
        self.offset + 1
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::from_iter(once(self.offset))
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        if offset == self.offset {
            1
        } else {
            0
        }
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
    fn is_complex(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for VariableTerm {
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

impl Internable for VariableTerm {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        false
    }
}

impl std::fmt::Display for VariableTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<static:{}>", self.offset)
    }
}
impl SerializeJson for VariableTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}
