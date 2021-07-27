// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::deduplicate_hashmap_entries,
};

pub struct HashMap {}
impl<T: Expression> Applicable<T> for HashMap {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let keys = match factory.match_vector_term(&keys) {
            Some(keys) => {
                if let Some(dynamic_key) = keys.items().iter().find(|key| !key.is_static()) {
                    Err(format!("Invalid HashMap key: {}", dynamic_key))
                } else {
                    Ok(keys)
                }
            }
            None => Err(format!("Invalid HashMap keys: {}", keys)),
        }?;
        match factory.match_vector_term(&values) {
            Some(values) => {
                if values.items().len() != keys.items().len() {
                    Err(format!(
                        "Invalid HashMap entries: received {} keys and {} values",
                        keys.items().len(),
                        values.items().len(),
                    ))
                } else {
                    let (keys, values) = match deduplicate_hashmap_entries(
                        keys.items().as_slice(),
                        values.items().as_slice(),
                    ) {
                        Some((keys, values)) => {
                            (allocator.create_list(keys), allocator.create_list(values))
                        }
                        None => (
                            allocator.clone_list(keys.items()),
                            allocator.clone_list(values.items()),
                        ),
                    };
                    Ok(factory.create_hashmap_term(keys, values))
                }
            }
            None => Err(format!("Invalid property values: {}", values)),
        }
    }
}
