// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
};

pub struct Reduce {}
impl<T: Expression> Applicable<T> for Reduce {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 1, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 3 {
            return Err(format!("Expected 3 arguments, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let iteratee = args.next().unwrap();
        let seed = args.next().unwrap();
        if let Some(target) = factory.match_vector_term(&target) {
            Ok(target.items().iter().fold(seed, |result, item| {
                factory.create_application_term(
                    iteratee.clone(),
                    allocator.create_pair(result, item.clone()),
                )
            }))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Ok(target
                .entries(factory, allocator)
                .into_iter()
                .fold(seed, |result, entry| {
                    factory.create_application_term(
                        iteratee.clone(),
                        allocator.create_pair(result, entry),
                    )
                }))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Ok(target.values().iter().fold(seed, |result, value| {
                factory.create_application_term(
                    iteratee.clone(),
                    allocator.create_pair(result, value.clone()),
                )
            }))
        } else {
            Err(format!(
                "Expected (<collection>, <function:2>, <any>), received ({}, {}, {})",
                target, iteratee, seed,
            ))
        }
    }
}