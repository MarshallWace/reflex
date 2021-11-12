// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Reduce {}
impl Reduce {
    pub(crate) const UUID: Uuid = uuid!("f8312370-a299-457e-b9fb-f902f84f71b2");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl Uid for Reduce {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Reduce {
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
