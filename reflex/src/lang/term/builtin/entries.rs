// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::ValueTerm,
};

pub struct Entries {}
impl<T: Expression> Applicable<T> for Entries {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(1, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let result = if let Some(target) = factory.match_struct_term(&target) {
            Some(
                factory.create_vector_term(
                    allocator.create_list(
                        target
                            .prototype()
                            .keys()
                            .iter()
                            .zip(target.fields().iter())
                            .map(|(key, value)| {
                                factory.create_tuple_term(allocator.create_pair(
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from(key)),
                                    )),
                                    value.clone(),
                                ))
                            }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_vector_term(&target) {
            Some(factory.create_vector_term(allocator.create_list(
                target.items().iter().enumerate().map(|(index, item)| {
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::Int(index as i32)),
                        item.clone(),
                    ))
                }),
            )))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(
                factory
                    .create_vector_term(allocator.create_list(target.entries(factory, allocator))),
            )
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(
                factory.create_vector_term(allocator.create_list(target.values().iter().map(
                    |value| {
                        factory
                            .create_tuple_term(allocator.create_pair(value.clone(), value.clone()))
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
