// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap};

use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator,
};

pub struct Merge {}
impl Merge {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for Merge {
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
        let base = factory
            .match_struct_term(&target)
            .map(|target| (target.prototype(), target.fields()));
        match base {
            None => Err(format!(
                "Expected (<struct>, ...), received ({}{})",
                target,
                args.map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Some((base_prototype, base_values)) => {
                let (prototype, values) = {
                    if args.len() == 0 {
                        (
                            allocator.clone_struct_prototype(base_prototype),
                            allocator.clone_list(base_values),
                        )
                    } else {
                        let base_entries = base_prototype
                            .keys()
                            .iter()
                            .cloned()
                            .zip(base_values.iter().cloned())
                            .collect::<Vec<_>>();
                        let (keys, values): (Vec<_>, Vec<_>) = args
                            .fold(Ok(base_entries), |result, arg| {
                                let mut combined_properties = result?;
                                let properties = match factory.match_struct_term(&arg) {
                                    Some(value) => {
                                        combined_properties.extend(
                                            value
                                                .prototype()
                                                .keys()
                                                .iter()
                                                .cloned()
                                                .zip(value.fields().iter().cloned()),
                                        );
                                        Some(combined_properties)
                                    }
                                    _ => None,
                                };
                                match properties {
                                    Some(properties) => Ok(properties),
                                    None => Err(format!("Expected <struct>..., received {}", arg)),
                                }
                            })?
                            .into_iter()
                            .unzip();
                        let mut deduplicated_keys = keys.iter().enumerate().fold(
                            HashMap::with_capacity(keys.len()),
                            |mut result, (index, key)| {
                                match result.entry(key) {
                                    Entry::Vacant(entry) => {
                                        entry.insert(index);
                                    }
                                    Entry::Occupied(mut entry) => {
                                        entry.insert(index);
                                    }
                                }
                                result
                            },
                        );
                        let (prototype, values) =
                            if deduplicated_keys.len() == base_prototype.keys().len() {
                                let values = if keys.len() == deduplicated_keys.len() {
                                    allocator.create_list(values)
                                } else {
                                    allocator.create_list(base_prototype.keys().iter().map(|key| {
                                        deduplicated_keys
                                            .remove(key)
                                            .map(|index| values.get(index).cloned().unwrap())
                                            .unwrap()
                                    }))
                                };
                                (allocator.clone_struct_prototype(base_prototype), values)
                            } else if deduplicated_keys.len() == keys.len() {
                                (
                                    allocator.create_struct_prototype(keys),
                                    allocator.create_list(values),
                                )
                            } else {
                                let (keys, values): (Vec<_>, Vec<_>) = keys
                                    .iter()
                                    .fold(
                                        Vec::with_capacity(deduplicated_keys.len()),
                                        |mut results, key| match deduplicated_keys.remove(key) {
                                            Some(index) => {
                                                let value = values.get(index).unwrap();
                                                results.push((key.clone(), value.clone()));
                                                results
                                            }
                                            None => results,
                                        },
                                    )
                                    .into_iter()
                                    .unzip();
                                (
                                    allocator.create_struct_prototype(keys),
                                    allocator.create_list(values),
                                )
                            };
                        (prototype, values)
                    }
                };
                Ok(factory.create_struct_term(prototype, values))
            }
        }
    }
}
