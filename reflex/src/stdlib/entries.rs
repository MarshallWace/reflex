// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Entries {}
impl Entries {
    pub(crate) const UUID: Uuid = uuid!("a72693b4-f470-43e8-b741-be628a3ce002");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Entries {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Entries {
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
        let result = if let Some(target) = factory.match_record_term(&target) {
            Some(
                factory.create_list_term(
                    allocator.create_list(
                        target
                            .prototype()
                            .keys()
                            .iter()
                            .zip(target.values().iter())
                            .map(|(key, value)| {
                                factory.create_list_term(
                                    allocator.create_pair(
                                        factory.create_string_term(
                                            allocator.create_string(key.as_str()),
                                        ),
                                        value.clone(),
                                    ),
                                )
                            }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_list_term(&target) {
            Some(factory.create_list_term(allocator.create_list(
                target.items().iter().enumerate().map(|(index, item)| {
                    factory.create_list_term(
                        allocator.create_pair(factory.create_int_term(index as i32), item.clone()),
                    )
                }),
            )))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(
                factory.create_list_term(allocator.create_list(target.entries(factory, allocator))),
            )
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(
                factory.create_list_term(allocator.create_list(target.values().iter().map(
                    |value| {
                        factory
                            .create_list_term(allocator.create_pair(value.clone(), value.clone()))
                    },
                ))),
            )
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate entries for {}", target)),
        }
    }
}
