// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter::once,
};

use crate::{
    compiler::{
        compile_expressions, Compile, Compiler, Instruction, NativeFunctionRegistry, Program,
    },
    core::{
        transform_expression_list, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, ExpressionList, GraphNode, HeapAllocator, Iterable, Rewritable,
        StackOffset, Substitutions, VarArgs,
    },
    hash::HashId,
};

#[derive(Eq, PartialEq, Debug)]
pub struct HashMapTerm<T: Expression> {
    keys: ExpressionList<T>,
    values: ExpressionList<T>,
    lookup: HashMap<HashId, usize>,
}
impl<T: Expression> Hash for HashMapTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in self.keys.iter().zip(self.values.iter()) {
            key.hash(state);
            value.hash(state)
        }
    }
}
impl<T: Expression> HashMapTerm<T> {
    pub fn new(keys: ExpressionList<T>, values: ExpressionList<T>) -> Self {
        let lookup = build_lookup_table(keys.iter());
        Self {
            lookup,
            keys,
            values,
        }
    }
    pub fn keys(&self) -> &ExpressionList<T> {
        &self.keys
    }
    pub fn values(&self) -> &ExpressionList<T> {
        &self.values
    }
    pub fn entries<'a>(
        &'a self,
        factory: &'a impl ExpressionFactory<T>,
        allocator: &'a impl HeapAllocator<T>,
    ) -> impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T> + 'a> + 'a {
        self.keys()
            .iter()
            .zip(self.values().iter())
            .map(move |(key, value)| {
                factory.create_tuple_term(allocator.create_pair(key.clone(), value.clone()))
            })
    }
    pub fn get(&self, key: &T) -> Option<&T> {
        self.lookup
            .get(&key.id())
            .and_then(|index| self.values.get(*index))
    }
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
                if value.id() == existing_value.id() {
                    None
                } else {
                    let values = allocator.create_sized_list(
                        self.values.len(),
                        self.values
                            .iter()
                            .take(existing_index)
                            .cloned()
                            .chain(once(value))
                            .chain(self.values.iter().skip(existing_index + 1).cloned()),
                    );
                    Some((None, values))
                }
            }
            None => {
                let keys = allocator.create_sized_list(
                    self.keys.len() + 1,
                    self.keys.iter().cloned().chain(once(key)),
                );
                let values = allocator.create_sized_list(
                    self.values.len() + 1,
                    self.values.iter().cloned().chain(once(value)),
                );
                Some((Some(keys), values))
            }
        };
        match updated_entries {
            None => None,
            Some((keys, values)) => Some(factory.create_hashmap_term(
                keys.unwrap_or_else(|| allocator.clone_list(&self.keys)),
                values,
            )),
        }
    }
}
impl<T: Expression> GraphNode for HashMapTerm<T> {
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
    fn dynamic_dependencies(&self) -> DependencyList {
        self.keys.dynamic_dependencies()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.keys.is_empty()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for HashMapTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        self.keys
            .iter()
            .flat_map(|key| once(key).chain(key.subexpressions()))
            .chain(
                self.values
                    .iter()
                    .flat_map(|value| once(value).chain(value.subexpressions())),
            )
            .collect()
    }
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
        Some(factory.create_hashmap_term(
            keys.unwrap_or_else(|| allocator.clone_list(&self.keys)),
            values.unwrap_or_else(|| allocator.clone_list(&self.values)),
        ))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let keys = transform_expression_list(&self.keys, allocator, |key| {
            key.substitute_dynamic(state, factory, allocator, cache)
        });
        let values = transform_expression_list(&self.values, allocator, |value| {
            value.substitute_dynamic(state, factory, allocator, cache)
        });
        if keys.is_none() && values.is_none() {
            return None;
        }
        Some(factory.create_hashmap_term(
            keys.unwrap_or_else(|| allocator.clone_list(&self.keys)),
            values.unwrap_or_else(|| allocator.clone_list(&self.values)),
        ))
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
        Some(factory.create_hashmap_term(
            keys.unwrap_or_else(|| allocator.clone_list(&self.keys)),
            values.unwrap_or_else(|| allocator.clone_list(&self.values)),
        ))
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
        Some(factory.create_hashmap_term(
            keys.unwrap_or_else(|| allocator.clone_list(&self.keys)),
            values.unwrap_or_else(|| allocator.clone_list(&self.values)),
        ))
    }
}
impl<T: Expression> Iterable for HashMapTerm<T> {
    fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }
}
impl<T: Expression + Compile<T>> Compile<T> for HashMapTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        compile_expressions(
            self.keys.iter(),
            VarArgs::Eager,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .and_then(|(keys_chunk, keys_native_functions)| {
            compile_expressions(
                self.values.iter(),
                VarArgs::Lazy,
                stack_offset,
                factory,
                allocator,
                compiler,
            )
            .map(|(values_chunk, values_native_functions)| {
                let mut program = keys_chunk;
                program.extend(values_chunk);
                program.push(Instruction::ConstructHashMap {
                    size: self.keys.len(),
                });
                (
                    program,
                    keys_native_functions.union(values_native_functions),
                )
            })
        })
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
                    .map(|(key, value)| format!("{} => {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                entries
                    .take(max_displayed_entries - 1)
                    .map(|(key, value)| format!("{} => {}", key, value))
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
impl<T: Expression> serde::Serialize for HashMapTerm<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let entries = self.keys.iter().zip(self.values.iter());
        let mut result = serializer.serialize_map(Some(entries.len()))?;
        for (key, value) in entries {
            serde::ser::SerializeMap::serialize_entry(&mut result, key, value)?;
        }
        serde::ser::SerializeMap::end(result)
    }
}

fn build_lookup_table<'a, T: Expression + 'a>(
    keys: impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
) -> HashMap<HashId, usize> {
    keys.into_iter()
        .enumerate()
        .map(|(index, key)| (key.id(), index))
        .collect()
}

pub fn deduplicate_hashmap_entries<T: Expression>(
    keys: &[T],
    values: &[T],
) -> Option<(Vec<T>, Vec<T>)> {
    let lookup = build_lookup_table(keys.iter());
    if lookup.len() == keys.len() {
        None
    } else {
        let (keys, values, _) = keys.iter().fold(
            (
                Vec::with_capacity(lookup.len()),
                Vec::with_capacity(lookup.len()),
                HashSet::new(),
            ),
            |results, key| {
                let (mut deduplicated_keys, mut deduplicated_values, mut processed_keys) = results;
                let key_hash = key.id();
                if !processed_keys.contains(&key_hash) {
                    let target_index = *lookup.get(&key_hash).unwrap();
                    let value = values.get(target_index).unwrap();
                    deduplicated_keys.push(key.clone());
                    deduplicated_values.push(value.clone());
                    processed_keys.insert(key_hash);
                }
                (deduplicated_keys, deduplicated_values, processed_keys)
            },
        );
        Some((keys, values))
    }
}
