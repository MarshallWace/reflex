// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    get_hashmap_entries, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression,
    ExpressionFactory, ExpressionListType, FunctionArity, HashsetTermType, HeapAllocator,
    ListTermType, RecordTermType, RefType, StructPrototypeType, Uid, Uuid,
};

pub struct Entries;
impl Entries {
    pub const UUID: Uuid = uuid!("a72693b4-f470-43e8-b741-be628a3ce002");
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
                            .as_deref()
                            .keys()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .cloned()
                            .zip(
                                target
                                    .values()
                                    .as_deref()
                                    .iter()
                                    .map(|item| item.as_deref())
                                    .cloned(),
                            )
                            .map(|(key, value)| {
                                factory.create_list_term(allocator.create_pair(key, value))
                            }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_list_term(&target) {
            Some(
                factory.create_list_term(
                    allocator.create_list(
                        target
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref())
                            .enumerate()
                            .map(|(index, item)| {
                                factory.create_list_term(allocator.create_pair(
                                    factory.create_int_term(index as i32),
                                    item.clone(),
                                ))
                            }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_list_term(
                allocator.create_list(get_hashmap_entries(target, factory, allocator)),
            ))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(factory.create_list_term(
                allocator.create_list(target.values().map(|item| item.as_deref()).cloned().map(
                    |value| factory.create_list_term(allocator.create_pair(value.clone(), value)),
                )),
            ))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate entries for {}", target)),
        }
    }
}
