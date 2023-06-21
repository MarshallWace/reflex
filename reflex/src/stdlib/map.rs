// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Map {}
impl Map {
    pub(crate) const UUID: Uuid = uuid!("5f9f3e01-8d8d-439a-aa99-979ea3da918d");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Map {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Map {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
