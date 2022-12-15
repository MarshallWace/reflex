// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::once};

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::core::{
    transform_expression_list, CompoundNode, DependencyList, DynamicState, Eagerness,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListIter, ExpressionListType,
    GraphNode, HeapAllocator, Internable, ListTermType, RefType, Rewritable, SerializeJson,
    StackOffset, Substitutions,
};
use reflex_utils::json;

#[derive(Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ListTerm<T: Expression> {
    items: T::ExpressionList<T>,
}
impl<T: Expression> ListTerm<T> {
    pub fn new(items: T::ExpressionList<T>) -> Self {
        Self { items }
    }
}

impl<T: Expression> std::hash::Hash for ListTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.items.id().hash(state);
    }
}

impl<T: Expression> ListTermType<T> for ListTerm<T> {
    fn items<'a>(&'a self) -> T::ExpressionListRef<'a, T>
    where
        T::ExpressionList<T>: 'a,
        T: 'a,
    {
        (&self.items).into()
    }
}
impl<T: Expression> GraphNode for ListTerm<T> {
    fn size(&self) -> usize {
        1 + self.items.size()
    }
    fn capture_depth(&self) -> StackOffset {
        self.items.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.items.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.items.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        if deep {
            self.items.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        if deep {
            self.items.has_dynamic_dependencies(deep)
        } else {
            false
        }
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.items.is_atomic()
    }
    fn is_complex(&self) -> bool {
        true
    }
}
impl<T: Expression> CompoundNode<T> for ListTerm<T> {
    type Children<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        T: 'a,
    {
        self.items.iter()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for ListTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.items, allocator, |item| {
            item.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|items| factory.create_list_term(items))
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if deep {
            transform_expression_list(&self.items, allocator, |item| {
                item.substitute_dynamic(deep, state, factory, allocator, cache)
            })
            .map(|items| factory.create_list_term(items))
        } else {
            None
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        transform_expression_list(&self.items, allocator, |item| {
            item.hoist_free_variables(factory, allocator)
        })
        .map(|items| factory.create_list_term(items))
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.items, allocator, |item| {
            item.normalize(factory, allocator, cache)
        })
        .map(|items| factory.create_list_term(items))
    }
}

impl<T: Expression> Internable for ListTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        self.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for ListTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_displayed_items = 100;
        let items = &self.items;
        let num_items = items.len();
        write!(
            f,
            "[{}]",
            if num_items <= max_displayed_items {
                items
                    .iter()
                    .map(|item| item.as_deref())
                    .map(|item| format!("{}", item))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                items
                    .iter()
                    .map(|item| item.as_deref())
                    .take(max_displayed_items - 1)
                    .map(|item| format!("{}", item))
                    .chain(once(format!(
                        "...{} more items",
                        num_items - (max_displayed_items - 1)
                    )))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        )
    }
}

impl<T: Expression + SerializeJson + Clone> SerializeJson for ListTerm<T> {
    fn to_json(&self) -> Result<JsonValue, String> {
        self.items
            .iter()
            .map(|item| item.as_deref())
            .map(|key| key.to_json())
            .collect::<Result<Vec<_>, String>>()
            .map(|values| JsonValue::Array(values))
    }
    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        let updates = target
            .items
            .iter()
            .map(|item| item.as_deref())
            .zip(self.items.iter().map(|item| item.as_deref()))
            .map(|(current, previous)| previous.patch(current))
            .chain(
                target
                    .items
                    .iter()
                    .map(|item| item.as_deref())
                    .skip(self.items.len())
                    .map(|item| item.to_json().map(Some)),
            )
            .collect::<Result<Vec<_>, _>>()?;

        let updates = reflex_utils::json::json_object(
            updates
                .into_iter()
                .enumerate()
                .filter_map(|(index, item)| item.map(|value| (index.to_string(), value)))
                .chain(if target.items.len() != self.items.len() {
                    Some((String::from("length"), JsonValue::from(target.items.len())))
                } else {
                    None
                }),
        );

        if json::is_empty_json_object(&updates) {
            Ok(None)
        } else {
            Ok(Some(updates))
        }
    }
}
