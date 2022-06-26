// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, GraphNode, HeapAllocator, Uid, Uuid,
    },
    stdlib::Stdlib,
};

pub struct ResolveDeep {}
impl ResolveDeep {
    pub(crate) const UUID: Uuid = uuid!("cf0f60ad-2182-43fb-ba9d-6763f1aaf6dd");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveDeep {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveDeep
where
    T::Builtin: From<Stdlib>,
{
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
        let target = args.next().unwrap();
        if let Some(value) = factory.match_record_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.create_sized_list(
                        value.values().len() + 1,
                        once(factory.create_constructor_term(
                            allocator.clone_struct_prototype(value.prototype()),
                        ))
                        .chain(value.values().iter().map(|field| {
                            if field.is_atomic() {
                                field.clone()
                            } else {
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::ResolveDeep),
                                    allocator.create_list(once(field.clone())),
                                )
                            }
                        })),
                    ),
                ))
            }
        } else if let Some(value) = factory.match_list_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectList),
                    allocator.create_list(value.items().iter().map(|item| {
                        if item.is_atomic() {
                            item.clone()
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::ResolveDeep),
                                allocator.create_list(once(item.clone())),
                            )
                        }
                    })),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(value.values().iter().map(|item| {
                        if item.is_atomic() {
                            item.clone()
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::ResolveDeep),
                                allocator.create_list(once(item.clone())),
                            )
                        }
                    })),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            if value.is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        if value.keys().is_atomic() {
                            factory.create_list_term(allocator.clone_list(value.keys()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(value.keys().iter().map(|item| {
                                    if item.is_atomic() {
                                        item.clone()
                                    } else {
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::ResolveDeep),
                                            allocator.create_list(once(item.clone())),
                                        )
                                    }
                                })),
                            )
                        },
                        if value.values().is_atomic() {
                            factory.create_list_term(allocator.clone_list(value.values()))
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(value.values().iter().map(|item| {
                                    if item.is_atomic() {
                                        item.clone()
                                    } else {
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::ResolveDeep),
                                            allocator.create_list(once(item.clone())),
                                        )
                                    }
                                })),
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
