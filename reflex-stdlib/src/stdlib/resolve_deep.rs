// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, GraphNode, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RecordTermType, RefType, Uid, Uuid,
};

use crate::{CollectHashSet, CollectList, CollectRecord, ConstructHashMap};

pub struct ResolveDeep;
impl ResolveDeep {
    pub const UUID: Uuid = uuid!("cf0f60ad-2182-43fb-ba9d-6763f1aaf6dd");
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
    T::Builtin: Builtin
        + From<CollectHashSet>
        + From<CollectList>
        + From<CollectRecord>
        + From<ConstructHashMap>
        + From<ResolveDeep>,
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
                    factory.create_builtin_term(CollectRecord),
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
                                .map(|item| item.as_deref().clone())
                                .map(|field| {
                                    if field.is_atomic() {
                                        field
                                    } else {
                                        factory.create_application_term(
                                            factory.create_builtin_term(ResolveDeep),
                                            allocator.create_unit_list(field),
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
                    factory.create_builtin_term(CollectList),
                    allocator.create_list(
                        value
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref().clone())
                            .map(|item| {
                                if item.is_atomic() {
                                    item
                                } else {
                                    factory.create_application_term(
                                        factory.create_builtin_term(ResolveDeep),
                                        allocator.create_unit_list(item),
                                    )
                                }
                            }),
                    ),
                ))
            }
        } else if let Some(value) = factory.match_hashset_term(&target) {
            if value.values().all(|item| item.as_deref().is_atomic()) {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(CollectHashSet),
                    allocator.create_list(value.values().map(|item| item.as_deref().clone()).map(
                        |item| {
                            if item.is_atomic() {
                                item
                            } else {
                                factory.create_application_term(
                                    factory.create_builtin_term(ResolveDeep),
                                    allocator.create_unit_list(item),
                                )
                            }
                        },
                    )),
                ))
            }
        } else if let Some(value) = factory.match_hashmap_term(&target) {
            let keys_are_atomic = value.keys().all(|key| key.as_deref().is_atomic());
            let values_are_atomic = value.values().all(|value| value.as_deref().is_atomic());
            if keys_are_atomic && values_are_atomic {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(ConstructHashMap),
                    allocator.create_pair(
                        if keys_are_atomic {
                            factory.create_list_term(
                                allocator
                                    .create_list(value.keys().map(|item| item.as_deref().clone())),
                            )
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(CollectList),
                                allocator.create_list(
                                    value
                                        .keys()
                                        .map(|item| item.as_deref().clone())
                                        .map(|item| {
                                            if item.is_atomic() {
                                                item
                                            } else {
                                                factory.create_application_term(
                                                    factory.create_builtin_term(ResolveDeep),
                                                    allocator.create_unit_list(item),
                                                )
                                            }
                                        }),
                                ),
                            )
                        },
                        if values_are_atomic {
                            factory.create_list_term(
                                allocator.create_list(
                                    value.values().map(|item| item.as_deref().clone()),
                                ),
                            )
                        } else {
                            factory.create_application_term(
                                factory.create_builtin_term(CollectList),
                                allocator.create_list(
                                    value.values().map(|item| item.as_deref().clone()).map(
                                        |item| {
                                            if item.is_atomic() {
                                                item
                                            } else {
                                                factory.create_application_term(
                                                    factory.create_builtin_term(ResolveDeep),
                                                    allocator.create_unit_list(item),
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
