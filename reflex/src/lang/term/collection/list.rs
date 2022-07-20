// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{compile_expressions, Compile, Compiler, Instruction, Program},
    core::{
        transform_expression_list, CompoundNode, DependencyList, DynamicState, EvaluationCache,
        Expression, ExpressionFactory, ExpressionList, ExpressionListSlice, GraphNode,
        HeapAllocator, Rewritable, SerializeJson, StackOffset, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ListTerm<T: Expression> {
    items: ExpressionList<T>,
}
impl<T: Expression> ListTerm<T> {
    pub fn new(items: ExpressionList<T>) -> Self {
        Self { items }
    }
    pub fn items(&self) -> &ExpressionList<T> {
        &self.items
    }
}
impl<T: Expression> GraphNode for ListTerm<T> {
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
pub type ListTermChildren<'a, T> = ExpressionListSlice<'a, T>;
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for ListTerm<T> {
    type Children = ListTermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
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
impl<T: Expression + Compile<T>> Compile<T> for ListTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        compile_expressions(
            self.items.iter(),
            VarArgs::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|mut program| {
            program.push(Instruction::ConstructList {
                size: self.items.len(),
            });
            program
        })
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
                    .map(|item| format!("{}", item))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                items
                    .iter()
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

impl<T: Expression> SerializeJson for ListTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        self.items()
            .iter()
            .map(|key| key.to_json())
            .collect::<Result<Vec<_>, String>>()
            .map(|values| serde_json::Value::Array(values))
    }
}