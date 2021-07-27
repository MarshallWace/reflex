// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, GraphNode,
        HeapAllocator, VarArgs,
    },
    lang::BuiltinTerm,
};

pub struct ResolveDeep {}
impl<T: Expression> Applicable<T> for ResolveDeep {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(1, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
        let target = args.next().unwrap();
        if let Some(value) = factory.match_tuple_term(&target) {
            if value.fields().iter().all(GraphNode::is_atomic) {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectTuple),
                    allocator.create_list(value.fields().iter().map(|field| {
                        if field.is_atomic() {
                            field.clone()
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::ResolveDeep),
                                allocator.create_list(once(field.clone())),
                            )
                        }
                    })),
                ))
            }
        } else if let Some(value) = factory.match_struct_term(&target) {
            if value.fields().iter().all(GraphNode::is_atomic) {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_constructor_term(
                        allocator.clone_struct_prototype(value.prototype()),
                        VarArgs::Eager,
                    ),
                    allocator.create_list(value.fields().iter().map(|field| {
                        if field.is_atomic() {
                            field.clone()
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::ResolveDeep),
                                allocator.create_list(once(field.clone())),
                            )
                        }
                    })),
                ))
            }
        } else if let Some(value) = factory.match_vector_term(&target) {
            if value.items().iter().all(GraphNode::is_atomic) {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectVector),
                    allocator.create_list(value.items().iter().map(|item| {
                        if item.is_atomic() {
                            item.clone()
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::ResolveDeep),
                                allocator.create_list(once(item.clone())),
                            )
                        }
                    })),
                ))
            }
        } else {
            // TODO: Flatten HashMap collection type
            // TODO: Flatten HashSet collection type
            Ok(target)
        }
    }
}
