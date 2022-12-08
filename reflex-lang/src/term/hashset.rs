// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, hash::Hash, iter::once};

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use reflex::{
    core::{
        build_hashset_lookup_table, transform_expression_list, CompoundNode, DependencyList,
        DynamicState, Eagerness, EvaluationCache, Expression, ExpressionFactory,
        ExpressionListIter, ExpressionListType, GraphNode, HashsetTermType, HeapAllocator,
        Internable, RefType, Rewritable, SerializeJson, StackOffset, Substitutions,
    },
    hash::HashId,
};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashSetTerm<T: Expression> {
    values: T::ExpressionList<T>,
    lookup: HashSet<HashId>,
}
impl<T: Expression> HashSetTerm<T> {
    pub fn new(values: T::ExpressionList<T>) -> Self {
        let lookup = build_hashset_lookup_table(values.iter().map(|item| item.as_deref()));
        Self { values, lookup }
    }
}
impl<T: Expression> Hash for HashSetTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.values.id().hash(state)
    }
}
impl<T: Expression> HashsetTermType<T> for HashSetTerm<T> {
    type ValuesIterator<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    fn contains(&self, value: &T) -> bool {
        self.lookup.contains(&value.id())
    }
    fn values<'a>(&'a self) -> Self::ValuesIterator<'a> {
        self.values.iter()
    }
}
impl<T: Expression> GraphNode for HashSetTerm<T> {
    fn size(&self) -> usize {
        1 + self.values.size()
    }
    fn capture_depth(&self) -> StackOffset {
        self.values.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.values.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.values.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        if deep {
            self.values.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        if deep {
            self.values.has_dynamic_dependencies(deep)
        } else {
            false
        }
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.values.is_atomic()
    }
    fn is_complex(&self) -> bool {
        true
    }
}
impl<T: Expression> CompoundNode<T> for HashSetTerm<T> {
    type Children<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a> {
        self.values.iter()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for HashSetTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |value| {
            value.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|values| factory.create_hashset_term(values))
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
            transform_expression_list(&self.values, allocator, |value| {
                value.substitute_dynamic(deep, state, factory, allocator, cache)
            })
            .map(|values| factory.create_list_term(values))
        } else {
            None
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |value| {
            value.hoist_free_variables(factory, allocator)
        })
        .map(|values| factory.create_list_term(values))
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |value| {
            value.normalize(factory, allocator, cache)
        })
        .map(|values| factory.create_list_term(values))
    }
}

impl<T: Expression> Internable for HashSetTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        self.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for HashSetTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_displayed_values = 10;
        let values = &self.values;
        let num_values = values.len();
        write!(
            f,
            "HashSet({})",
            if num_values <= max_displayed_values {
                values
                    .iter()
                    .map(|item| item.as_deref())
                    .map(|value| format!("{}", value))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                values
                    .iter()
                    .map(|item| item.as_deref())
                    .take(max_displayed_values - 1)
                    .map(|value| format!("{}", value))
                    .chain(once(format!(
                        "...{} more values",
                        num_values - (max_displayed_values - 1)
                    )))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        )
    }
}
impl<T: Expression> SerializeJson for HashSetTerm<T> {
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
impl<T: Expression> serde::Serialize for HashSetTerm<T>
where
    T: serde::Serialize,
    T::ExpressionList<T>: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Into::<SerializedHashSetTerm<T>>::into(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for HashSetTerm<T>
where
    T: serde::Deserialize<'de>,
    T::ExpressionList<T>: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(SerializedHashSetTerm::<T>::deserialize(deserializer)?.into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedHashSetTerm<T: Expression> {
    values: T::ExpressionList<T>,
}
impl<'a, T: Expression> Into<SerializedHashSetTerm<T>> for &'a HashSetTerm<T> {
    fn into(self) -> SerializedHashSetTerm<T> {
        let HashSetTerm { values, .. } = self.clone();
        SerializedHashSetTerm { values }
    }
}
impl<T: Expression> Into<HashSetTerm<T>> for SerializedHashSetTerm<T> {
    fn into(self) -> HashSetTerm<T> {
        let SerializedHashSetTerm { values } = self;
        HashSetTerm::new(values)
    }
}
