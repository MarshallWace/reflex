// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::hash_map::Entry, iter::once};

use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        ExpressionListType, FunctionArity, HeapAllocator, RecordTermType, RefType,
        StructPrototypeType, Uid, Uuid,
    },
    hash::FnvHashMap,
};

pub struct Merge {}
impl Merge {
    pub(crate) const UUID: Uuid = uuid!("7093b95b-630d-4ad2-8d55-7d1bbaf0968a");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
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
        let target = args.next().unwrap();
        let args = args.collect::<Vec<_>>();
        let result = match factory.match_record_term(&target) {
            None => Err((target, args)),
            Some(base) => {
                if args.is_empty() {
                    Ok(target.clone())
                } else {
                    let records = args
                        .iter()
                        .map(|arg| match factory.match_record_term(arg) {
                            Some(term) => Some(Some((
                                term.prototype().as_deref(),
                                term.values().as_deref(),
                            ))),
                            None => match factory.match_nil_term(arg) {
                                Some(_) => Some(None),
                                _ => None,
                            },
                        })
                        .collect::<Option<Vec<_>>>();
                    match records {
                        None => Err((target, args)),
                        Some(records) => {
                            let (keys, values): (Vec<_>, Vec<_>) =
                                once((base.prototype().as_deref(), base.values().as_deref()))
                                    .chain(records.into_iter().filter_map(|record| record))
                                    .fold(
                                        Vec::new(),
                                        |mut combined_properties, (prototype, values)| {
                                            combined_properties.extend(
                                                prototype
                                                    .keys()
                                                    .as_deref()
                                                    .iter()
                                                    .map(|item| item.as_deref())
                                                    .cloned()
                                                    .zip(
                                                        values
                                                            .iter()
                                                            .map(|item| item.as_deref())
                                                            .cloned(),
                                                    ),
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
                            let base_prototype = base.prototype();
                            let (prototype, values) = if deduplicated_keys.len()
                                == base_prototype.as_deref().keys().as_deref().len()
                            {
                                let values = if keys.len() == deduplicated_keys.len() {
                                    allocator.create_list(values)
                                } else {
                                    allocator.create_list(
                                        base_prototype
                                            .as_deref()
                                            .keys()
                                            .as_deref()
                                            .iter()
                                            .map(|item| item.as_deref())
                                            .map(|key| {
                                                deduplicated_keys
                                                    .remove(key)
                                                    .map(|index| {
                                                        values.get(index).cloned().unwrap()
                                                    })
                                                    .unwrap()
                                            }),
                                    )
                                };
                                (allocator.clone_struct_prototype(base_prototype), values)
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
                                        |mut results, key| match deduplicated_keys.remove(&key) {
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
            }
        };
        match result {
            Ok(result) => Ok(result),
            Err((target, args)) => Err(format!(
                "Expected (<struct>...), received ({})",
                once(target)
                    .chain(args)
                    .into_iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }
}
