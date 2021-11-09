// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{compile_expressions, Compile, Compiler, Instruction, Program},
    core::{
        transform_expression_list, Applicable, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SerializeJson, StackOffset, StructPrototype, Substitutions, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct StructTerm<T: Expression> {
    prototype: StructPrototype,
    fields: ExpressionList<T>,
}
impl<T: Expression> StructTerm<T> {
    pub fn new(prototype: StructPrototype, fields: ExpressionList<T>) -> Self {
        Self { prototype, fields }
    }
    pub fn prototype(&self) -> &StructPrototype {
        &self.prototype
    }
    pub fn fields(&self) -> &ExpressionList<T> {
        &self.fields
    }
    pub fn get(&self, key: &str) -> Option<&T> {
        self.prototype
            .field(key)
            .and_then(|field_offset| self.fields.get(field_offset))
    }
    pub fn entries(&self) -> impl IntoIterator<Item = (&String, &T)> {
        self.prototype.keys().iter().zip(self.fields.iter())
    }
}
impl<T: Expression> GraphNode for StructTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.fields.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.fields.free_variables()
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        if deep {
            self.fields.dynamic_dependencies(deep)
        } else {
            DependencyList::empty()
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        if deep {
            self.fields.has_dynamic_dependencies(deep)
        } else {
            false
        }
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.fields.is_atomic()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for StructTerm<T> {
    fn children(&self) -> Vec<&T> {
        self.fields.iter().collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.substitute_static(substitutions, factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_struct_term(allocator.clone_struct_prototype(&self.prototype), fields)
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
            transform_expression_list(&self.fields, allocator, |expression| {
                expression.substitute_dynamic(deep, state, factory, allocator, cache)
            })
            .map(|fields| {
                factory
                    .create_struct_term(allocator.clone_struct_prototype(&self.prototype), fields)
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
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.hoist_free_variables(factory, allocator)
        })
        .map(|fields| {
            factory.create_struct_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.normalize(factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_struct_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for StructTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        compile_expressions(
            self.fields.iter(),
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
                    num_args: self.fields.len(),
                })),
            );
            program
        })
    }
}
impl<T: Expression> std::fmt::Display for StructTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.prototype.keys().len() {
            0 => write!(f, "{{}}"),
            _ => write!(
                f,
                "{{ {} }}",
                self.prototype
                    .keys()
                    .iter()
                    .zip(self.fields.iter())
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        }
    }
}

impl<T: Expression> SerializeJson for StructTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        let map: Result<serde_json::Map<_, _>, String> = self
            .prototype()
            .keys()
            .iter()
            .zip(self.fields().iter())
            .map(|(key, value)| {
                let value = value.to_json()?;
                Ok((key.clone(), value))
            })
            .collect();

        Ok(map?.into())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct ConstructorTerm {
    prototype: StructPrototype,
}
impl ConstructorTerm {
    pub fn new(prototype: StructPrototype) -> Self {
        Self { prototype }
    }
    pub fn prototype(&self) -> &StructPrototype {
        &self.prototype
    }
}
impl GraphNode for ConstructorTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
}
impl<T: Expression> Applicable<T> for ConstructorTerm {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::lazy(0, self.prototype().keys().len(), false))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_struct_term(
            allocator.clone_struct_prototype(&self.prototype),
            allocator.create_list(args),
        ))
    }
}
impl<T: Expression + Compile<T>> Compile<T> for ConstructorTerm {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        _factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _compiler: &mut Compiler,
    ) -> Result<Program, String> {
        Ok(Program::new(once(Instruction::PushConstructor {
            prototype: allocator.clone_struct_prototype(&self.prototype),
        })))
    }
}
impl std::fmt::Display for ConstructorTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<constructor:{{{}}}>",
            self.prototype
                .keys()
                .iter()
                .map(|key| format!("{}", key))
                .collect::<Vec<_>>()
                .join(","),
        )
    }
}

impl SerializeJson for ConstructorTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

pub fn create_struct<T: Expression>(
    properties: impl IntoIterator<Item = (String, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let (keys, values): (Vec<_>, Vec<_>) = properties.into_iter().unzip();
    factory.create_struct_term(StructPrototype::new(keys), allocator.create_list(values))
}
