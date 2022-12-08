// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    CompoundNode, DependencyList, DynamicState, Eagerness, EvaluationCache, Expression,
    ExpressionFactory, GraphNode, HeapAllocator, Internable, LetTermType, Reducible, Rewritable,
    ScopeOffset, SerializeJson, StackOffset, Substitutions,
};

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct LetTerm<T: Expression> {
    initializer: T,
    body: T,
}

impl<T: Expression> std::hash::Hash for LetTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.initializer.id().hash(state);
        self.body.id().hash(state);
    }
}

impl<T: Expression> LetTerm<T> {
    pub fn new(initializer: T, body: T) -> Self {
        Self { initializer, body }
    }
}
impl<'term, T: Expression + 'term> LetTermType<T> for LetTerm<T> {
    fn initializer<'a>(&'a self) -> T::Ref<'a, T>
    where
        T: 'a,
    {
        (&self.initializer).into()
    }
    fn body<'a>(&'a self) -> T::Ref<'a, T>
    where
        T: 'a,
    {
        (&self.body).into()
    }
}
impl<T: Expression> GraphNode for LetTerm<T> {
    fn size(&self) -> usize {
        1 + self.initializer.size() + self.body.size()
    }
    fn capture_depth(&self) -> StackOffset {
        self.initializer
            .capture_depth()
            .max(self.body.capture_depth().saturating_sub(1))
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.initializer
            .free_variables()
            .into_iter()
            .chain(self.body.free_variables().into_iter().filter_map(|offset| {
                if offset == 0 {
                    None
                } else {
                    Some(offset - 1)
                }
            }))
            .collect()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.body.count_variable_usages(offset + 1)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.initializer
            .dynamic_dependencies(deep)
            .union(self.body.dynamic_dependencies(deep))
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.initializer.has_dynamic_dependencies(deep) || self.body.has_dynamic_dependencies(deep)
    }
    fn is_static(&self) -> bool {
        false
    }
    fn is_atomic(&self) -> bool {
        false
    }
    fn is_complex(&self) -> bool {
        true
    }
}
impl<T: Expression> CompoundNode<T> for LetTerm<T> {
    type Children<'a> = std::iter::Chain<std::iter::Once<T::Ref<'a, T>>, std::iter::Once<T::Ref<'a, T>>>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a> {
        once((&self.initializer).into()).chain(once((&self.body).into()))
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T>> Rewritable<T> for LetTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let initializer =
            self.initializer
                .substitute_static(substitutions, factory, allocator, cache);
        let body = self
            .body
            .substitute_static(&substitutions.offset(1), factory, allocator, cache);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
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
        let initializer = self
            .initializer
            .substitute_dynamic(deep, state, factory, allocator, cache);
        let body = self
            .body
            .substitute_dynamic(deep, state, factory, allocator, cache);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let initializer = self.initializer.hoist_free_variables(factory, allocator);
        let body = self.body.hoist_free_variables(factory, allocator);
        if initializer.is_none() && body.is_none() {
            return None;
        } else {
            Some(factory.create_let_term(
                initializer.unwrap_or_else(|| self.initializer.clone()),
                body.unwrap_or_else(|| self.body.clone()),
            ))
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let normalized_initializer = self.initializer.normalize(factory, allocator, cache);
        let normalized_body = self.body.normalize(factory, allocator, cache);
        let initializer = normalized_initializer.as_ref().unwrap_or(&self.initializer);
        let body = normalized_body.as_ref().unwrap_or(&self.body);
        let can_inline_initializer =
            !initializer.is_complex() || body.count_variable_usages(0) <= 1;
        if can_inline_initializer {
            body.substitute_static(
                &Substitutions::named(
                    &vec![(0, initializer.clone())],
                    Some(ScopeOffset::Unwrap(1)),
                ),
                factory,
                allocator,
                cache,
            )
            .map(|result| {
                result
                    .normalize(factory, allocator, cache)
                    .unwrap_or(result)
            })
            .or_else(|| normalized_body.or_else(|| Some(self.body.clone())))
        } else if normalized_initializer.is_some() || normalized_body.is_some() {
            Some(factory.create_let_term(
                normalized_initializer.unwrap_or_else(|| self.initializer.clone()),
                normalized_body.unwrap_or_else(|| self.body.clone()),
            ))
        } else {
            None
        }
    }
}
impl<T: Expression + Rewritable<T>> Reducible<T> for LetTerm<T> {
    fn is_reducible(&self) -> bool {
        true
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if self.body.capture_depth() == 0 {
            Some(self.body.clone())
        } else {
            let substitutions = vec![(0, self.initializer.clone())];
            let substitutions = Substitutions::named(&substitutions, Some(ScopeOffset::Unwrap(1)));
            self.body
                .substitute_static(&substitutions, factory, allocator, cache)
        }
    }
}

impl<T: Expression> Internable for LetTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        self.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for LetTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<let:{}:{}>", self.initializer, self.body)
    }
}
impl<T: Expression> SerializeJson for LetTerm<T> {
    fn to_json(&self) -> Result<JsonValue, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        Err(format!(
            "Unable to create patch for terms: {}, {}",
            self, target
        ))
    }
}
