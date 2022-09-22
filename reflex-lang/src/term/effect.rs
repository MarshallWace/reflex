// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};

use reflex::core::{
    ConditionType, DependencyList, DynamicState, Eagerness, EffectTermType, Evaluate,
    EvaluationCache, EvaluationResult, Expression, ExpressionFactory, GraphNode, HeapAllocator,
    Internable, Rewritable, SerializeJson, StackOffset, Substitutions,
};

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct EffectTerm<T: Expression> {
    condition: T::Signal<T>,
}

impl<T: Expression> std::hash::Hash for EffectTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.condition.id().hash(state);
    }
}
impl<T: Expression> EffectTermType<T> for EffectTerm<T> {
    fn condition<'a>(&'a self) -> T::Ref<'a, T::Signal<T>>
    where
        T::Signal<T>: 'a,
        T: 'a,
    {
        (&self.condition).into()
    }
}

impl<T: Expression> EffectTerm<T> {
    pub fn new(condition: T::Signal<T>) -> Self {
        Self { condition }
    }
}
impl<T: Expression> GraphNode for EffectTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn count_variable_usages(&self, _offset: StackOffset) -> usize {
        0
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::of(self.condition.id())
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
    fn is_complex(&self) -> bool {
        false
    }
}
impl<T: Expression> Evaluate<T> for EffectTerm<T> {
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        Some(EvaluationResult::new(
            match state.get(&self.condition.id()) {
                Some(value) => value.clone(),
                None => factory
                    .create_signal_term(allocator.create_signal_list(once(self.condition.clone()))),
            },
            DependencyList::of(self.condition.id()),
        ))
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for EffectTerm<T> {
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
        state.get(&self.condition.id()).cloned()
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

impl<T: Expression> Internable for EffectTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        false
    }
}

impl<T: Expression> std::fmt::Display for EffectTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<effect:{}>", self.condition)
    }
}
impl<T: Expression> SerializeJson for EffectTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}