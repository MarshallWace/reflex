// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    stdlib::is_truthy,
};

use super::Stdlib;

pub struct Filter {}
impl Filter {
    pub(crate) const UUID: Uuid = uuid!("110c1120-4526-4757-ae4d-fbc5cef2c4f5");
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
        let target = args.next().unwrap();
        let predicate = args.next().unwrap();
        let result = if let Some(target) = factory.match_vector_term(&target) {
            Some(collect_filter_results(
                target.items().iter().cloned(),
                &predicate,
                Stdlib::CollectVector,
                factory,
                allocator,
            ))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(collect_filter_results(
                target.entries(factory, allocator).into_iter(),
                &predicate,
                Stdlib::CollectHashMap,
                factory,
                allocator,
            ))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(collect_filter_results(
                target.values().iter().cloned(),
                &predicate,
                Stdlib::CollectHashSet,
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
    items: impl IntoIterator<Item = T>,
    predicate: &T,
    collect: Stdlib,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    let (items, results): (Vec<_>, Vec<_>) = items
        .into_iter()
        .map(|item| {
            (
                item.clone(),
                factory
                    .create_application_term(predicate.clone(), allocator.create_list(once(item))),
            )
        })
        .unzip();
    factory.create_application_term(
        factory.create_builtin_term(Stdlib::CollectFilterResults),
        allocator.create_triple(
            factory.create_vector_term(allocator.create_list(items)),
            factory.create_application_term(
                factory.create_builtin_term(Stdlib::CollectVector),
                allocator.create_list(results),
            ),
            factory.create_builtin_term(collect),
        ),
    )
}

pub struct CollectFilterResults {}
impl CollectFilterResults {
    pub(crate) const UUID: Uuid = uuid!("36517ace-60b4-4c2f-8768-9717cf262b90");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for CollectFilterResults {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for CollectFilterResults {
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
        let items = args.next().unwrap();
        let results = args.next().unwrap();
        let combine = args.next().unwrap();
        match (
            factory.match_vector_term(&items),
            factory.match_vector_term(&results),
        ) {
            (Some(items), Some(results)) => {
                Ok(factory.create_application_term(
                    combine,
                    allocator.create_unsized_list(
                    items.items().iter().zip(results.items().iter()).filter_map(|(item, result)| if is_truthy(result, factory) {
                        Some(item.clone())
                    } else {
                        None
                    }))
                ))
            },
            _ => Err(format!("Invalid filter combiner arguments: Expected (<struct>, <struct>, <function>), received ({} {} {})",
            items, results, combine))
        }
    }
}
