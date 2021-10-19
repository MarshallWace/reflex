// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator,
};

pub struct Map {}
impl Map {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Map {
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
        let iteratee = args.next().unwrap();
        let result = if let Some(target) = factory.match_vector_term(&target) {
            Some(
                factory.create_vector_term(allocator.create_list(target.items().iter().map(
                    |item| {
                        factory.create_application_term(
                            iteratee.clone(),
                            allocator.create_list(once(item.clone())),
                        )
                    },
                ))),
            )
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_hashmap_term(
                allocator.clone_list(target.keys()),
                allocator.create_list(target.values().iter().map(|value| {
                    factory.create_application_term(
                        iteratee.clone(),
                        allocator.create_list(once(value.clone())),
                    )
                })),
            ))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Expected (Vector, <function:1>) or (HashMap, <function:1>), received ({}, {})",
                target, iteratee,
            )),
        }
    }
}
