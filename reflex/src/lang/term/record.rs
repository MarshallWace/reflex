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
        HeapAllocator, Reducible, Rewritable, SerializeJson, StackOffset, StructPrototype,
        Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct RecordTerm<T: Expression> {
    prototype: StructPrototype,
    values: ExpressionList<T>,
}
impl<T: Expression> RecordTerm<T> {
    pub fn new(prototype: StructPrototype, values: ExpressionList<T>) -> Self {
        Self { prototype, values }
    }
    pub fn prototype(&self) -> &StructPrototype {
        &self.prototype
    }
    pub fn values(&self) -> &ExpressionList<T> {
        &self.values
    }
    pub fn get(&self, key: &str) -> Option<&T> {
        self.prototype
            .field(key)
            .and_then(|field_offset| self.values.get(field_offset))
    }
    pub fn entries(&self) -> impl IntoIterator<Item = (&String, &T)> {
        self.prototype.keys().iter().zip(self.values.iter())
    }
}
impl<T: Expression> GraphNode for RecordTerm<T> {
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
pub type RecordTermChildren<'a, T> = ExpressionListSlice<'a, T>;
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for RecordTerm<T> {
    type Children = RecordTermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
        self.values.iter()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for RecordTerm<T> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
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
            transform_expression_list(&self.values, allocator, |expression| {
                expression.substitute_dynamic(deep, state, factory, allocator, cache)
            })
            .map(|fields| {
                factory
                    .create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
            })
        } else {
            None
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.hoist_free_variables(factory, allocator)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.values, allocator, |expression| {
            expression.normalize(factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_record_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for RecordTerm<T> {
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
            program.extend(
                once(Instruction::PushConstructor {
                    prototype: allocator.clone_struct_prototype(&self.prototype),
                })
                .chain(once(Instruction::Apply {
                    num_args: self.values.len(),
                })),
            );
            program
        })
    }
}
impl<T: Expression> std::fmt::Display for RecordTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.prototype.keys().len() {
            0 => write!(f, "{{}}"),
            _ => write!(
                f,
                "{{ {} }}",
                self.prototype
                    .keys()
                    .iter()
                    .zip(self.values.iter())
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        }
    }
}

impl<T: Expression> SerializeJson for RecordTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        let map: Result<serde_json::Map<_, _>, String> = self
            .prototype()
            .keys()
            .iter()
            .zip(self.values().iter())
            .map(|(key, value)| {
                let value = value.to_json()?;
                Ok((key.clone(), value))
            })
            .collect();

        Ok(map?.into())
    }
}

pub fn create_record<T: Expression>(
    properties: impl IntoIterator<Item = (String, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let (keys, values): (Vec<_>, Vec<_>) = properties.into_iter().unzip();
    factory.create_record_term(StructPrototype::new(keys), allocator.create_list(values))
}
