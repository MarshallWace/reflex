// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

use crate::{Apply, CollectHashMap, CollectHashSet, CollectList, Flatten, If, Map, ResolveList};

pub struct Filter;
impl Filter {
    pub const UUID: Uuid = uuid!("110c1120-4526-4757-ae4d-fbc5cef2c4f5");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Filter {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Filter
where
    T::Builtin: Builtin
        + From<Apply>
        + From<CollectHashMap>
        + From<CollectHashSet>
        + From<CollectList>
        + From<Flatten>
        + From<If>
        + From<Map>
        + From<ResolveList>,
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
        let target = args.next().unwrap();
        let predicate = args.next().unwrap();
        let result = if let Some(_) = factory.match_list_term(&target) {
            Some(collect_filter_results(
                CollectList,
                &target,
                &predicate,
                factory,
                allocator,
            ))
        } else if let Some(_) = factory.match_hashmap_term(&target) {
            // TODO: Clarify expected behavior of filtering non-list terms
            Some(collect_filter_results(
                CollectHashMap,
                &target,
                &predicate,
                factory,
                allocator,
            ))
        } else if let Some(_) = factory.match_hashset_term(&target) {
            // TODO: Clarify expected behavior of filtering non-list terms
            Some(collect_filter_results(
                CollectHashSet,
                &target,
                &predicate,
                factory,
                allocator,
            ))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Expected (<iterable>, <function:1>), received ({}, {})",
                target, predicate,
            )),
        }
    }
}

fn collect_filter_results<T: Expression>(
    collect: impl Into<T::Builtin>,
    target: &T,
    predicate: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Apply> + From<Flatten> + From<If> + From<Map> + From<ResolveList>,
{
    factory.create_application_term(
        factory.create_builtin_term(Apply),
        allocator.create_pair(
            factory.create_builtin_term(collect),
            factory.create_application_term(
                factory.create_builtin_term(Flatten),
                allocator.create_unit_list(factory.create_application_term(
                    factory.create_builtin_term(ResolveList),
                    allocator.create_unit_list(
                        factory.create_application_term(
                            factory.create_builtin_term(Map),
                            allocator.create_pair(
                                target.clone(),
                                factory.create_lambda_term(
                                    1,
                                    factory.create_application_term(
                                        factory.create_builtin_term(If),
                                        allocator.create_triple(
                                            factory.create_application_term(
                                                predicate.clone(),
                                                allocator.create_unit_list(
                                                    factory.create_variable_term(0),
                                                ),
                                            ),
                                            factory.create_list_term(
                                                allocator.create_unit_list(
                                                    factory.create_variable_term(0),
                                                ),
                                            ),
                                            factory.create_list_term(allocator.create_empty_list()),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                )),
            ),
        ),
    )
}
