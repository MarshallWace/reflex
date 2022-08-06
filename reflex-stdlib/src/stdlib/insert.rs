// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HashmapTermType, HeapAllocator, Uid, Uuid,
};

pub struct Insert {}
impl Insert {
    pub(crate) const UUID: Uuid = uuid!("177d4614-e261-47e8-93be-645427627dae");
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
                .map(|existing_value| (true, existing_value == &value))
                .unwrap_or((false, false));
            Ok(if is_unchanged {
                target
            } else {
                let existing_index = if has_value {
                    existing
                        .keys()
                        .position(|existing_key| existing_key == &key)
                } else {
                    None
                };
                let (keys, values) = if let Some(existing_index) = existing_index {
                    (
                        allocator.create_list(existing.keys().cloned()),
                        allocator.create_list(
                            existing
                                .values()
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
                    let existing_keys = existing.keys();
                    let existing_values = existing.values();
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
