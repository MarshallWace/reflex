// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, GraphNode, HeapAllocator,
    },
    lang::BuiltinTerm,
};

pub struct ResolveShallow {}
impl<T: Expression> Applicable<T> for ResolveShallow {
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
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectTuple),
                    allocator.clone_list(value.fields()),
                ))
            }
        } else if let Some(value) = factory.match_struct_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectStruct),
                    allocator.clone_list(value.fields()),
                ))
            }
        } else if let Some(value) = factory.match_vector_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectVector),
                    allocator.clone_list(value.items()),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectHashSet),
                    allocator.clone_list(value.values()),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::ConstructHashMap),
                    allocator.create_pair(
                        if value.keys().is_atomic() {
                            factory.create_vector_term(allocator.clone_list(value.keys()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::CollectVector),
                                allocator.clone_list(value.keys()),
                            )
                        },
                        if value.values().is_atomic() {
                            factory.create_vector_term(allocator.clone_list(value.values()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::CollectVector),
                                allocator.clone_list(value.values()),
                            )
                        },
                    ),
                ))
            }
        } else {
            Ok(target)
        }
    }
}
