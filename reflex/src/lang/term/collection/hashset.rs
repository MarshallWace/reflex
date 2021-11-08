// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, hash::Hash, iter::once};

use crate::{
    compiler::{compile_expressions, Compile, Compiler, Instruction, Program},
    core::{
        transform_expression_list, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, ExpressionList, GraphNode, HeapAllocator, Iterable, Rewritable,
        SerializeJson, StackOffset, Substitutions, VarArgs,
    },
    hash::HashId,
};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashSetTerm<T: Expression> {
    values: ExpressionList<T>,
    lookup: HashSet<HashId>,
}
impl<T: Expression> Hash for HashSetTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for key in self.values.iter() {
            key.hash(state);
        }
    }
}
impl<T: Expression> HashSetTerm<T> {
    pub fn new(values: ExpressionList<T>) -> Self {
        let lookup = build_lookup_table(values.iter());
        Self { values, lookup }
    }
    pub fn values(&self) -> &ExpressionList<T> {
        &self.values
    }
    pub fn contains(&self, value: &T) -> bool {
        self.lookup.contains(&value.id())
    }
}
impl<T: Expression> GraphNode for HashSetTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.values.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.values.free_variables()
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
        self.values.is_empty()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for HashSetTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        self.values
            .iter()
            .flat_map(|value| once(value).chain(value.subexpressions()))
            .collect()
    }
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
            .map(|values| factory.create_vector_term(values))
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
        .map(|values| factory.create_vector_term(values))
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
        .map(|values| factory.create_vector_term(values))
    }
}
impl<T: Expression + Compile<T>> Compile<T> for HashSetTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        compile_expressions(
            self.values.iter(),
            VarArgs::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|mut program| {
            program.push(Instruction::ConstructHashSet {
                size: self.values.len(),
            });
            program
        })
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
                    .map(|value| format!("{}", value))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                values
                    .iter()
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
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

impl<T: Expression> Iterable for HashSetTerm<T> {
    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}
fn build_lookup_table<'a, T: Expression + 'a>(
    values: impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
) -> HashSet<HashId> {
    let values = values.into_iter();
    values.into_iter().map(|value| value.id()).collect()
}

pub fn deduplicate_hashset_entries<T: Expression>(values: &[T]) -> Option<Vec<T>> {
    let lookup = build_lookup_table(values.iter());
    if lookup.len() == values.len() {
        None
    } else {
        let (values, _) = values.iter().fold(
            (Vec::with_capacity(lookup.len()), HashSet::new()),
            |results, value| {
                let (mut deduplicated_values, mut processed_values) = results;
                let value_hash = value.id();
                if !processed_values.contains(&value_hash) {
                    deduplicated_values.push(value.clone());
                    processed_values.insert(value_hash);
                }
                (deduplicated_values, processed_values)
            },
        );
        Some(values)
    }
}
