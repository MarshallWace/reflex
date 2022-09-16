// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, GraphNode, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RecordTermType, RefType, Uid, Uuid,
};

use crate::Stdlib;

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
            if value.values().as_deref().is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.create_sized_list(
                        value.values().as_deref().len() + 1,
                        once(factory.create_constructor_term(
                            allocator.clone_struct_prototype(value.prototype()),
                        ))
                        .chain(
                            value
                                .values()
                                .as_deref()
                                .iter()
                                .map(|item| item.as_deref())
                                .map(|field| {
                                    if field.is_atomic() {
                                        field.clone()
                                    } else {
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::ResolveDeep),
                                            allocator.create_list(once(field.clone())),
                                        )
                                    }
                                }),
                        ),
                    ),
                ))
            }
        } else if let Some(value) = factory.match_list_term(&target) {
            if value.items().as_deref().is_atomic() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectList),
                    allocator.create_list(
                        value
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .map(|item| {
                                if item.is_atomic() {
                                    item.clone()
                                } else {
                                    factory.create_application_term(
                                        factory.create_builtin_term(Stdlib::ResolveDeep),
                                        allocator.create_list(once(item.clone())),
                                    )
                                }
                            }),
                    ),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value
                .values()
                .map(|item| item.as_deref())
                .all(|item| item.is_atomic())
            {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(value.values().map(|item| item.as_deref()).cloned().map(
                        |item| {
                            if item.is_atomic() {
                                item
                            } else {
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::ResolveDeep),
                                    allocator.create_list(once(item)),
                                )
                            }
                        },
                    )),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            let keys_are_atomic = value
                .keys()
                .map(|item| item.as_deref())
                .all(|key| key.is_atomic());
            let values_are_atomic = value
                .values()
                .map(|item| item.as_deref())
                .all(|value| value.is_atomic());
            if keys_are_atomic && values_are_atomic {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        if keys_are_atomic {
                            factory.create_list_term(
                                allocator
                                    .create_list(value.keys().map(|item| item.as_deref()).cloned()),
                            )
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(
                                    value
                                        .keys()
                                        .map(|item| item.as_deref())
                                        .cloned()
                                        .map(|item| {
                                            if item.is_atomic() {
                                                item
                                            } else {
                                                factory.create_application_term(
                                                    factory
                                                        .create_builtin_term(Stdlib::ResolveDeep),
                                                    allocator.create_list(once(item)),
                                                )
                                            }
                                        }),
                                ),
                            )
                        },
                        if values_are_atomic {
                            factory.create_list_term(
                                allocator.create_list(
                                    value.values().map(|item| item.as_deref()).cloned(),
                                ),
                            )
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::CollectList),
                                allocator.create_list(
                                    value.values().map(|item| item.as_deref()).cloned().map(
                                        |item| {
                                            if item.is_atomic() {
                                                item
                                            } else {
                                                factory.create_application_term(
                                                    factory
                                                        .create_builtin_term(Stdlib::ResolveDeep),
                                                    allocator.create_list(once(item)),
                                                )
                                            }
                                        },
                                    ),
                                ),
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
