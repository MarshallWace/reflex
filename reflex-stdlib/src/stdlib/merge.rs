// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{collections::hash_map::Entry, iter::once};

use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RecordTermType, RefType,
        StructPrototypeType, Uid, Uuid,
    },
    hash::FnvHashMap,
};

pub struct Merge;
impl Merge {
    pub const UUID: Uuid = uuid!("7093b95b-630d-4ad2-8d55-7d1bbaf0968a");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Merge {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Merge {
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
        let parsed_args = args.next().unwrap();
        match factory.match_list_term(&parsed_args) {
            Some(parsed_args) => {
                let mut records = parsed_args
                    .items()
                    .as_deref()
                    .iter()
                    .filter_map(|arg| match factory.match_record_term(&arg) {
                        Some(term) => Some(Ok((
                            term.prototype().as_deref().clone(),
                            term.values().as_deref().clone(),
                        ))),
                        None => match factory.match_nil_term(&arg) {
                            Some(_) => None,
                            _ => Some(Err(format!("Expected <struct>, received {}", arg))),
                        },
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter();
                match records.next().map(|(base_prototype, base_values)| {
                    (
                        base_prototype.clone(),
                        once((base_prototype, base_values)).chain(records),
                    )
                }) {
                    None => Ok(factory.create_record_term(
                        allocator.create_struct_prototype(allocator.create_empty_list()),
                        allocator.create_empty_list(),
                    )),
                    Some((base_prototype, records)) => {
                        let (keys, values): (Vec<_>, Vec<_>) = records
                            .fold(
                                Vec::new(),
                                |mut combined_properties, (prototype, values)| {
                                    combined_properties.extend(
                                        prototype.keys().as_deref().iter().zip(values.iter()),
                                    );
                                    combined_properties
                                },
                            )
                            .into_iter()
                            .unzip();
                        let mut deduplicated_keys = keys.iter().enumerate().fold(
                            {
                                let mut map = FnvHashMap::default();
                                map.reserve(keys.len());
                                map
                            },
                            |mut result, (index, key)| {
                                match result.entry(key.id()) {
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
                        let (prototype, values) = if deduplicated_keys.len()
                            == base_prototype.keys().as_deref().len()
                        {
                            let values = if keys.len() == deduplicated_keys.len() {
                                allocator.create_list(values)
                            } else {
                                allocator.create_list(base_prototype.keys().as_deref().iter().map(
                                    |key| {
                                        deduplicated_keys
                                            .remove(&key.id())
                                            .map(|index| values.get(index).cloned().unwrap())
                                            .unwrap()
                                    },
                                ))
                            };
                            (base_prototype, values)
                        } else if deduplicated_keys.len() == keys.len() {
                            (
                                allocator.create_struct_prototype(allocator.create_list(keys)),
                                allocator.create_list(values),
                            )
                        } else {
                            let (keys, values): (Vec<_>, Vec<_>) = keys
                                .iter()
                                .fold(
                                    Vec::with_capacity(deduplicated_keys.len()),
                                    |mut results, key| match deduplicated_keys.remove(&key.id()) {
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
                                allocator.create_struct_prototype(allocator.create_list(keys)),
                                allocator.create_list(values),
                            )
                        };
                        Ok(factory.create_record_term(prototype, values))
                    }
                }
            }
            _ => Err(format!(
                "Expected list of <struct>, received {}",
                parsed_args
            )),
        }
    }
}
