// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        match_typed_expression_list, Applicable, Arity, EvaluationCache, Expression,
        ExpressionFactory, HeapAllocator, StringValue, VarArgs,
    },
    lang::{deduplicate_hashmap_entries, deduplicate_hashset_entries},
};

pub struct ConstructTuple {}
impl<T: Expression> Applicable<T> for ConstructTuple {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, 0, Some(VarArgs::Lazy)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_tuple_term(allocator.create_list(args)))
    }
}

pub struct ConstructStruct {}
impl<T: Expression> Applicable<T> for ConstructStruct {
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
        let (num_keys, keys) = match factory.match_vector_term(&keys) {
            Some(keys) => {
                let num_keys = keys.items().len();
                match_typed_expression_list(
                    keys.items().iter(),
                    |key| {
                        factory
                            .match_value_term(key)
                            .and_then(|key| match key.match_string() {
                                Some(key) => Some(String::from(key.as_str())),
                                None => None,
                            })
                    },
                    |key| format!("Invalid property key: {}", key),
                )
                .map(|keys| (num_keys, keys))
            }
            None => Err(format!("Invalid property keys: {}", keys)),
        }?;
        match factory.match_vector_term(&values) {
            Some(values) => {
                if values.items().len() != num_keys {
                    Err(format!(
                        "Invalid property entries: received {} keys and {} values",
                        num_keys,
                        values.items().len(),
                    ))
                } else {
                    Ok(factory.create_struct_term(
                        allocator.create_struct_prototype(keys),
                        allocator.clone_list(values.items()),
                    ))
                }
            }
            None => Err(format!("Invalid property values: {}", values)),
        }
    }
}

pub struct ConstructHashMap {}
impl<T: Expression> Applicable<T> for ConstructHashMap {
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

pub struct ConstructHashSet {}
impl<T: Expression> Applicable<T> for ConstructHashSet {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, 0, Some(VarArgs::Lazy)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let values = args.into_iter().collect::<Vec<_>>();
        let deduplicated_values = match deduplicate_hashset_entries(&values) {
            Some(values) => values,
            None => values,
        };
        Ok(factory.create_hashset_term(allocator.create_list(deduplicated_values)))
    }
}

pub struct ConstructVector {}
impl<T: Expression> Applicable<T> for ConstructVector {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, 0, Some(VarArgs::Lazy)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_vector_term(allocator.create_list(args)))
    }
}
