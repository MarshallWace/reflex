// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, iter::once};

use crate::{
    compiler::{
        compile_expressions, Compile, Compiler, Instruction, NativeFunctionRegistry, Program,
    },
    core::{
        transform_expression_list, Applicable, Arity, DependencyList, DynamicState,
        EvaluationCache, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, StackOffset, StructPrototype, Substitutions, VarArgs,
    },
    hash::hash_object,
};

#[derive(Hash, Eq, PartialEq, Debug)]
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
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        self.fields.is_empty()
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for StructTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        self.fields
            .iter()
            .flat_map(|field| once(field).chain(field.subexpressions()))
            .collect()
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
        state: &DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        transform_expression_list(&self.fields, allocator, |expression| {
            expression.substitute_dynamic(state, factory, allocator, cache)
        })
        .map(|fields| {
            factory.create_struct_term(allocator.clone_struct_prototype(&self.prototype), fields)
        })
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
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        compile_expressions(
            self.fields.iter(),
            VarArgs::Lazy,
            stack_offset,
            factory,
            allocator,
            compiler,
        )
        .map(|(mut program, native_functions)| {
            program.push(Instruction::ConstructStruct {
                prototype: allocator.clone_struct_prototype(&self.prototype),
                eager: false,
            });
            (program, native_functions)
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
impl<T: Expression> serde::Serialize for StructTerm<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let entries = self.prototype.keys().iter().zip(self.fields.iter());
        let mut result = serializer.serialize_map(Some(entries.len()))?;
        for (key, value) in entries {
            serde::ser::SerializeMap::serialize_entry(&mut result, key, value)?;
        }
        serde::ser::SerializeMap::end(result)
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct ConstructorTerm {
    prototype: StructPrototype,
    eager: VarArgs,
}
impl ConstructorTerm {
    pub fn new(prototype: StructPrototype, eager: VarArgs) -> Self {
        Self { prototype, eager }
    }
    pub fn prototype(&self) -> &StructPrototype {
        &self.prototype
    }
    pub fn eager(&self) -> VarArgs {
        self.eager
    }
}
impl GraphNode for ConstructorTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
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
        let num_fields = self.prototype().keys().len();
        Some(match self.eager() {
            VarArgs::Eager => Arity::from(num_fields, 0, None),
            VarArgs::Lazy => Arity::from(0, num_fields, None),
        })
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_struct_term(
            allocator.clone_struct_prototype(&self.prototype),
            allocator.create_list(args.into_iter()),
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
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let size = self.prototype.keys().len();
        let hash = hash_object(self);
        Ok((
            Program::new(once(Instruction::PushFunction {
                target: match compiler.retrieve_compiled_chunk_address(hash) {
                    Some(address) => address,
                    None => compiler.store_compiled_chunk(
                        hash,
                        Program::new(
                            once(Instruction::Function { hash, arity: size })
                                .chain(
                                    (match self.eager {
                                        VarArgs::Lazy => None,
                                        VarArgs::Eager => {
                                            Some(self.prototype.keys().iter().flat_map(|_| {
                                                once(Instruction::PushStatic { offset: size - 1 })
                                                    .chain(once(Instruction::Evaluate))
                                            }))
                                        }
                                    })
                                    .into_iter()
                                    .flatten(),
                                )
                                .chain(once(Instruction::ConstructStruct {
                                    prototype: allocator.clone_struct_prototype(&self.prototype),
                                    eager: match self.eager {
                                        VarArgs::Eager => true,
                                        VarArgs::Lazy => false,
                                    },
                                }))
                                .chain(match self.eager {
                                    VarArgs::Eager if size > 0 => {
                                        Some(Instruction::Squash { depth: size })
                                    }
                                    _ => None,
                                })
                                .chain(once(Instruction::Return)),
                        ),
                    ),
                },
            })),
            NativeFunctionRegistry::default(),
        ))
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
impl serde::Serialize for ConstructorTerm {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
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
