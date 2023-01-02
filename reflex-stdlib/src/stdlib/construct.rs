// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    deduplicate_hashmap_entries, deduplicate_hashset_entries, uuid, Applicable, ArgType, Arity,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListType, FunctionArity,
    HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct ConstructRecord;
impl ConstructRecord {
    pub const UUID: Uuid = uuid!("f3a1b7ad-fe7d-444b-adf3-6945332e03b7");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ConstructRecord {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ConstructRecord {
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
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let (num_keys, keys) = match factory.match_list_term(&keys) {
            Some(keys) => {
                let num_keys = keys.items().as_deref().len();
                Ok((
                    num_keys,
                    allocator.create_list(
                        keys.items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
                ))
            }
            None => Err(format!("Invalid property keys: {}", keys)),
        }?;
        match factory.match_list_term(&values) {
            Some(values) => {
                if values.items().as_deref().len() != num_keys {
                    Err(format!(
                        "Invalid property entries: received {} keys and {} values",
                        num_keys,
                        values.items().as_deref().len(),
                    ))
                } else {
                    Ok(factory.create_record_term(
                        allocator.create_struct_prototype(keys),
                        allocator.clone_list(values.items()),
                    ))
                }
            }
            None => Err(format!("Invalid property values: {}", values)),
        }
    }
}

pub struct ConstructHashMap;
impl ConstructHashMap {
    pub const UUID: Uuid = uuid!("e9d74f69-3722-47f0-810b-76730129d6d3");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ConstructHashMap {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ConstructHashMap {
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
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let keys = match factory.match_list_term(&keys) {
            Some(keys) => {
                if let Some(dynamic_key) = keys
                    .items()
                    .as_deref()
                    .iter()
                    .find(|key| !key.as_deref().is_static())
                {
                    Err(format!("Invalid HashMap key: {}", dynamic_key.as_deref()))
                } else {
                    Ok(keys)
                }
            }
            None => Err(format!("Invalid HashMap keys: {}", keys)),
        }?;
        match factory.match_list_term(&values) {
            Some(values) => {
                if values.items().as_deref().len() != keys.items().as_deref().len() {
                    Err(format!(
                        "Invalid HashMap entries: received {} keys and {} values",
                        keys.items().as_deref().len(),
                        values.items().as_deref().len(),
                    ))
                } else {
                    let keys = keys.items();
                    let values = values.items();
                    // FIXME: prevent unnecessary intermediate allocations
                    let entries = {
                        let keys = keys
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref().clone())
                            .collect::<Vec<_>>();
                        let values = values
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref().clone())
                            .collect::<Vec<_>>();
                        let entries = match deduplicate_hashmap_entries(&keys, &values) {
                            Some(entries) => entries,
                            None => keys.into_iter().zip(values).collect::<Vec<_>>(),
                        };
                        entries
                    };
                    Ok(factory.create_hashmap_term(entries))
                }
            }
            None => Err(format!("Invalid property values: {}", values)),
        }
    }
}

pub struct ConstructHashSet;
impl ConstructHashSet {
    pub const UUID: Uuid = uuid!("d2772dad-f42c-49a1-9707-e1df4b07b1ae");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ConstructHashSet {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ConstructHashSet {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let values = args.collect::<Vec<_>>();
        let deduplicated_values = match deduplicate_hashset_entries(&values) {
            Some(values) => values,
            None => values,
        };
        Ok(factory.create_hashset_term(deduplicated_values))
    }
}

pub struct ConstructList;
impl ConstructList {
    pub const UUID: Uuid = uuid!("ecdf265f-d628-415b-80d5-5977e10a1141");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ConstructList {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ConstructList {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_list_term(allocator.create_list(args)))
    }
}
