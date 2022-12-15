// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HashmapTermType, HeapAllocator, RefType, Uid, Uuid,
};

pub struct Insert;
impl Insert {
    pub const UUID: Uuid = uuid!("177d4614-e261-47e8-93be-645427627dae");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Insert {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Insert {
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
        let key = args.next().unwrap();
        let value = args.next().unwrap();
        if let Some(existing) = factory.match_hashmap_term(&target) {
            let (has_value, is_unchanged) = existing
                .get(&key)
                .map(|item| item.as_deref())
                .map(|existing_value| (true, existing_value.id() == value.id()))
                .unwrap_or((false, false));
            Ok(if is_unchanged {
                target
            } else {
                let existing_index = if has_value {
                    existing
                        .keys()
                        .map(|item| item.as_deref())
                        .position(|existing_key| existing_key.id() == key.id())
                } else {
                    None
                };
                let (keys, values) = if let Some(existing_index) = existing_index {
                    (
                        allocator.create_list(existing.keys().map(|item| item.as_deref()).cloned()),
                        allocator.create_list(
                            existing
                                .values()
                                .map(|item| item.as_deref())
                                .enumerate()
                                .map(|(index, existing_value)| {
                                    if index == existing_index {
                                        &value
                                    } else {
                                        existing_value
                                    }
                                })
                                .cloned(),
                        ),
                    )
                } else {
                    let existing_keys = existing.keys().map(|item| item.as_deref());
                    let existing_values = existing.values().map(|item| item.as_deref());
                    (
                        allocator.create_sized_list(
                            existing_keys.len() + 1,
                            existing_keys.cloned().chain(once(key)),
                        ),
                        allocator.create_sized_list(
                            existing_values.len() + 1,
                            existing_values.cloned().chain(once(value)),
                        ),
                    )
                };
                factory.create_hashmap_term(keys, values)
            })
        } else {
            Err(format!(
                "Invalid field update: Expected (HashMap, <any>, <any>), received ({}, {}, {})",
                target, key, value,
            ))
        }
    }
}
