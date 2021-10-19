// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::{is_truthy, BuiltinTerm},
};

pub struct Filter {}
impl Filter {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Filter {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
                BuiltinTerm::CollectVector,
                factory,
                allocator,
            ))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(collect_filter_results(
                target.entries(factory, allocator).into_iter(),
                &predicate,
                BuiltinTerm::CollectHashMap,
                factory,
                allocator,
            ))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(collect_filter_results(
                target.values().iter().cloned(),
                &predicate,
                BuiltinTerm::CollectHashSet,
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
    collect: BuiltinTerm,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
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
        factory.create_builtin_term(BuiltinTerm::CollectFilterResults),
        allocator.create_triple(
            factory.create_vector_term(allocator.create_list(items)),
            factory.create_application_term(
                factory.create_builtin_term(BuiltinTerm::CollectVector),
                allocator.create_list(results),
            ),
            factory.create_builtin_term(collect),
        ),
    )
}

pub struct CollectFilterResults {}
impl CollectFilterResults {
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for CollectFilterResults {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
