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
        build_hashmap_lookup_table, transform_expression_list, CompoundNode, DependencyList,
        DynamicState, Eagerness, EvaluationCache, Expression, ExpressionFactory,
        ExpressionListIter, ExpressionListType, GraphNode, HashmapTermType, HeapAllocator,
        Internable, RefType, Rewritable, SerializeJson, StackOffset, Substitutions,
    },
    hash::{HashId, IntMap},
};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashMapTerm<T: Expression> {
    keys: T::ExpressionList,
    values: T::ExpressionList,
    lookup: IntMap<HashId, usize>,
}
impl<T: Expression> HashMapTerm<T> {
    pub fn set(
        &self,
        key: T,
        value: T,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let existing = self
            .lookup
            .get(&key.id())
            .copied()
            .and_then(|index| self.values.get(index).map(|value| (index, value)));
        let updated_entries = match existing {
            Some((existing_index, existing_value)) => {
                if value.id() == existing_value.as_deref().id() {
                    None
                } else {
                    let values = allocator.create_sized_list(
                        self.values.len(),
                        self.values
                            .iter()
                            .map(|item| item.as_deref().clone())
                            .take(existing_index)
                            .chain(once(value))
                            .chain(
                                self.values
                                    .iter()
                                    .map(|item| item.as_deref().clone())
                                    .skip(existing_index + 1),
                            ),
                    );
                    Some((None, values))
                }
            }
            None => {
                let keys = allocator.create_sized_list(
                    self.keys.len() + 1,
                    self.keys
                        .iter()
                        .map(|item| item.as_deref().clone())
                        .chain(once(key)),
                );
                let values = allocator.create_sized_list(
                    self.values.len() + 1,
                    self.values
                        .iter()
                        .map(|item| item.as_deref().clone())
                        .chain(once(value)),
                );
                Some((Some(keys), values))
            }
        };
        match updated_entries {
            None => None,
            Some((keys, values)) => Some(
                factory.create_hashmap_term(
                    keys.unwrap_or_else(|| allocator.clone_list((&self.keys).into()))
                        .iter()
                        .map(|item| item.as_deref().clone())
                        .zip(values.iter().map(|item| item.as_deref().clone())),
                ),
            ),
        }
    }
}
impl<T: Expression> Hash for HashMapTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.keys.id().hash(state);
        self.values.id().hash(state);
    }
}
impl<T: Expression> HashMapTerm<T> {
    pub fn new(keys: T::ExpressionList, values: T::ExpressionList) -> Self {
        let lookup = build_hashmap_lookup_table(keys.iter().map(|item| item.as_deref().clone()));
        Self {
            lookup,
            keys,
            values,
        }
    }
    pub fn entries<'a>(
        &'a self,
        factory: &'a impl ExpressionFactory<T>,
        allocator: &'a impl HeapAllocator<T>,
    ) -> impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T> + 'a> + 'a {
        self.keys
            .iter()
            .map(|item| item.as_deref().clone())
            .zip(self.values.iter().map(|item| item.as_deref().clone()))
            .map(move |(key, value)| {
                factory.create_list_term(allocator.create_pair(key.clone(), value.clone()))
            })
    }
}
impl<T: Expression> HashmapTermType<T> for HashMapTerm<T> {
    type KeysIterator<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    type ValuesIterator<'a> = ExpressionListIter<'a, T>
        where
            T: 'a,
            Self: 'a;
    fn keys<'a>(&'a self) -> Self::KeysIterator<'a>
    where
        T: 'a,
    {
        self.keys.iter()
    }
    fn values<'a>(&'a self) -> Self::ValuesIterator<'a>
    where
        T: 'a,
    {
        self.values.iter()
    }
    fn get<'a>(&'a self, key: &T) -> Option<T::ExpressionRef<'a>>
    where
        T: 'a,
    {
        self.lookup
            .get(&key.id())
            .and_then(|index| self.values.get(*index))
    }
}
impl<T: Expression> GraphNode for HashMapTerm<T> {
    fn size(&self) -> usize {
        1 + self.keys.size() + self.values.size()
    }
    fn capture_depth(&self) -> StackOffset {
        self.keys.capture_depth().max(self.values.capture_depth())
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        let keys_free_variables = self.keys.free_variables();
        let values_free_variables = self.values.free_variables();
        if keys_free_variables.is_empty() {
            values_free_variables
        } else if values_free_variables.is_empty() {
            keys_free_variables
        } else {
            let mut combined = keys_free_variables;
            combined.extend(values_free_variables);
            combined
        }
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.keys.count_variable_usages(offset) + self.values.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.keys.dynamic_dependencies(deep).union(if deep {
            self.values.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        })
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.keys.has_dynamic_dependencies(deep)
            || (if deep {
                self.values.has_dynamic_dependencies(deep)
            } else {
                false
            })
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.keys.is_atomic() && self.values.is_atomic()
    }
    fn is_complex(&self) -> bool {
        true
    }
}
impl<T: Expression> CompoundNode<T> for HashMapTerm<T> {
    type Children<'a> = std::iter::Chain<ExpressionListIter<'a, T>, ExpressionListIter<'a, T>>
        where
            T: 'a,
            Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        T: 'a,
    {
        self.keys.iter().chain(self.values.iter())
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for HashMapTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let keys = transform_expression_list(&self.keys, allocator, |key| {
            key.substitute_static(substitutions, factory, allocator, cache)
        });
        let values = transform_expression_list(&self.values, allocator, |value| {
            value.substitute_static(substitutions, factory, allocator, cache)
        });
        if keys.is_none() && values.is_none() {
            return None;
        }
        Some(
            factory.create_hashmap_term(
                keys.unwrap_or_else(|| allocator.clone_list((&self.keys).into()))
                    .iter()
                    .map(|item| item.as_deref().clone())
                    .zip(
                        values
                            .unwrap_or_else(|| allocator.clone_list((&self.values).into()))
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
            ),
        )
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let keys = transform_expression_list(&self.keys, allocator, |key| {
            key.substitute_dynamic(deep, state, factory, allocator, cache)
        });
        let values = if deep {
            transform_expression_list(&self.values, allocator, |value| {
                value.substitute_dynamic(deep, state, factory, allocator, cache)
            })
        } else {
            None
        };
        if keys.is_none() && values.is_none() {
            return None;
        }
        Some(
            factory.create_hashmap_term(
                keys.unwrap_or_else(|| allocator.clone_list((&self.keys).into()))
                    .iter()
                    .map(|item| item.as_deref().clone())
                    .zip(
                        values
                            .unwrap_or_else(|| allocator.clone_list((&self.values).into()))
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
            ),
        )
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let keys = transform_expression_list(&self.keys, allocator, |key| {
            key.hoist_free_variables(factory, allocator)
        });
        let values = transform_expression_list(&self.values, allocator, |value| {
            value.hoist_free_variables(factory, allocator)
        });
        if keys.is_none() && values.is_none() {
            return None;
        }
        Some(
            factory.create_hashmap_term(
                keys.unwrap_or_else(|| allocator.clone_list((&self.keys).into()))
                    .iter()
                    .map(|item| item.as_deref().clone())
                    .zip(
                        values
                            .unwrap_or_else(|| allocator.clone_list((&self.values).into()))
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
            ),
        )
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let keys = transform_expression_list(&self.keys, allocator, |key| {
            key.normalize(factory, allocator, cache)
        });
        let values = transform_expression_list(&self.values, allocator, |value| {
            value.normalize(factory, allocator, cache)
        });
        if keys.is_none() && values.is_none() {
            return None;
        }
        Some(
            factory.create_hashmap_term(
                keys.unwrap_or_else(|| allocator.clone_list((&self.keys).into()))
                    .iter()
                    .map(|item| item.as_deref().clone())
                    .zip(
                        values
                            .unwrap_or_else(|| allocator.clone_list((&self.values).into()))
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
            ),
        )
    }
}

impl<T: Expression> Internable for HashMapTerm<T> {
    fn should_intern(&self, _eager: Eagerness) -> bool {
        self.capture_depth() == 0
    }
}

impl<T: Expression> std::fmt::Display for HashMapTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_displayed_entries = 10;
        let entries = self.keys.iter().zip(self.values.iter());
        let num_entries = entries.len();
        write!(
            f,
            "HashMap({})",
            if num_entries <= max_displayed_entries {
                entries
                    .map(|(key, value)| format!("{} => {}", key.as_deref(), value.as_deref()))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                entries
                    .take(max_displayed_entries - 1)
                    .map(|(key, value)| format!("{} => {}", key.as_deref(), value.as_deref()))
                    .chain(once(format!(
                        "...{} more entries",
                        num_entries - (max_displayed_entries - 1)
                    )))
                    .collect::<Vec<_>>()
                    .join(", ")
            }
        )
    }
}
impl<T: Expression> SerializeJson for HashMapTerm<T> {
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
impl<T: Expression> serde::Serialize for HashMapTerm<T>
where
    T: serde::Serialize,
    T::ExpressionList: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Into::<SerializedHashMapTerm<T>>::into(self).serialize(serializer)
    }
}
impl<'de, T: Expression> serde::Deserialize<'de> for HashMapTerm<T>
where
    T: serde::Deserialize<'de>,
    T::ExpressionList: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(SerializedHashMapTerm::<T>::deserialize(deserializer)?.into())
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedHashMapTerm<T: Expression> {
    keys: T::ExpressionList,
    values: T::ExpressionList,
}
impl<'a, T: Expression> Into<SerializedHashMapTerm<T>> for &'a HashMapTerm<T> {
    fn into(self) -> SerializedHashMapTerm<T> {
        let HashMapTerm { keys, values, .. } = self.clone();
        SerializedHashMapTerm { keys, values }
    }
}
impl<T: Expression> Into<HashMapTerm<T>> for SerializedHashMapTerm<T> {
    fn into(self) -> HashMapTerm<T> {
        let SerializedHashMapTerm { keys, values } = self;
        HashMapTerm::new(keys, values)
    }
}
