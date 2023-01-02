// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RefType, Uid, Uuid,
};

pub struct Reduce;
impl Reduce {
    pub const UUID: Uuid = uuid!("f8312370-a299-457e-b9fb-f902f84f71b2");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Reduce {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Reduce {
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
        let iteratee = args.next().unwrap();
        let seed = args.next().unwrap();
        if let Some(target) = factory.match_list_term(&target) {
            Ok(target
                .items()
                .as_deref()
                .iter()
                .map(|item| item.as_deref().clone())
                .fold(seed, |result, item| {
                    factory.create_application_term(
                        iteratee.clone(),
                        allocator.create_pair(result, item),
                    )
                }))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Ok(target
                .keys()
                .map(|item| item.as_deref().clone())
                .zip(target.values().map(|item| item.as_deref().clone()))
                .fold(seed, |result, (key, value)| {
                    factory.create_application_term(
                        iteratee.clone(),
                        allocator.create_pair(
                            result,
                            factory.create_list_term(allocator.create_pair(key, value)),
                        ),
                    )
                }))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Ok(target
                .values()
                .map(|item| item.as_deref().clone())
                .fold(seed, |result, value| {
                    factory.create_application_term(
                        iteratee.clone(),
                        allocator.create_pair(result, value),
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
