// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HeapAllocator, ListTermType, RefType, Uid,
    Uuid,
};

pub struct Map;
impl Map {
    pub const UUID: Uuid = uuid!("5f9f3e01-8d8d-439a-aa99-979ea3da918d");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Map {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Map {
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
        let result = if let Some(target) = factory.match_list_term(&target) {
            Some(
                factory.create_list_term(
                    allocator.create_list(
                        target
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| {
                                factory.create_application_term(
                                    iteratee.clone(),
                                    allocator.create_unit_list(item),
                                )
                            }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(
                factory.create_hashmap_term(
                    target.keys().zip(
                        target
                            .values()
                            .map(|value| {
                                factory.create_application_term(
                                    iteratee.clone(),
                                    allocator.create_unit_list(value),
                                )
                            }),
                    ),
                ),
            )
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Expected (List, <function:1>) or (HashMap, <function:1>), received ({}, {})",
                target, iteratee,
            )),
        }
    }
}
