// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    deduplicate_hashmap_entries, deduplicate_hashset_entries, get_hashmap_entries, uuid,
    Applicable, ArgType, Arity, ConstructorTermType, EvaluationCache, Expression,
    ExpressionFactory, ExpressionListType, FunctionArity, HashmapTermType, HashsetTermType,
    HeapAllocator, ListTermType, RefType, SignalType, StructPrototypeType, Uid, Uuid,
};

use crate::Get;

pub struct Collect;
impl Collect {
    pub const UUID: Uuid = uuid!("c3365e24-8bbc-461c-ad86-71ca519ec20e");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Collect {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Collect
where
    T::Builtin: From<CollectList> + From<CollectHashMap> + From<CollectHashSet>,
{
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
        if let Some(collection) = factory.match_list_term(&target) {
            let has_dynamic_values = collection
                .items()
                .as_deref()
                .iter()
                .any(|item| !item.is_static());
            Ok(if !has_dynamic_values {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(CollectList),
                    allocator.clone_list(collection.items()),
                )
            })
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            let has_dynamic_values = collection.values().any(|item| !item.is_static());
            Ok(if !has_dynamic_values {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(CollectHashSet),
                    allocator.create_list(collection.values()),
                )
            })
        } else if let Some(collection) = factory.match_hashmap_term(&target) {
            let has_dynamic_keys = collection.keys().any(|item| !item.is_static());
            Ok(if !has_dynamic_keys {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(CollectHashMap),
                    allocator.create_list(get_hashmap_entries(collection, factory, allocator)),
                )
            })
        } else {
            Err(format!(
                "Expected List or HashSet or HashMap, received {}",
                target,
            ))
        }
    }
}

pub struct CollectRecord;
impl CollectRecord {
    pub const UUID: Uuid = uuid!("03e8d5bb-917d-414b-8041-d07ec403c1a9");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for CollectRecord {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for CollectRecord {
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
        let prototype = args.next().unwrap();
        match factory.match_constructor_term(&prototype) {
            Some(constructor)
                if constructor.prototype().as_deref().keys().as_deref().len() == args.len() =>
            {
                Ok(factory.create_record_term(
                    allocator.clone_struct_prototype(constructor.prototype()),
                    allocator.create_list(args),
                ))
            }
            _ => Err(format!(
                "Expected <constructor:{}>, received {}",
                args.len(),
                prototype
            )),
        }
    }
}

pub struct CollectList;
impl CollectList {
    pub const UUID: Uuid = uuid!("c99b0901-f996-4887-9403-c2f123b779b0");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for CollectList {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for CollectList {
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

pub struct CollectHashSet;
impl CollectHashSet {
    pub const UUID: Uuid = uuid!("941b9298-62d3-47e1-a909-d4252df2c935");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for CollectHashSet {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for CollectHashSet {
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

pub struct CollectHashMap;
impl CollectHashMap {
    pub const UUID: Uuid = uuid!("4a9b9c32-8597-4a23-875e-d320f187cf7a");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for CollectHashMap {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for CollectHashMap
where
    T::Builtin: From<Get>,
{
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
        let (keys, values): (Vec<_>, Vec<_>) = {
            args.map(|arg| {
                let static_entry = if let Some(entry) = factory.match_list_term(&arg) {
                    match (
                        entry
                            .items()
                            .as_deref()
                            .get(0)
                            .map(|item| item.as_deref().clone()),
                        entry
                            .items()
                            .as_deref()
                            .get(1)
                            .map(|item| item.as_deref().clone()),
                    ) {
                        (Some(key), Some(value)) => Ok(Some((key, value))),
                        _ => Err(format!("Invalid HashMap entry: {}", arg)),
                    }
                } else {
                    Ok(None)
                };
                match static_entry {
                    Ok(Some((key, value))) => (key, value),
                    Ok(None) => (
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(arg.clone(), factory.create_int_term(0)),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Get),
                            allocator.create_pair(arg, factory.create_int_term(1)),
                        ),
                    ),
                    Err(err) => {
                        let signal = create_error_signal_term(
                            factory.create_string_term(allocator.create_string(err)),
                            factory,
                            allocator,
                        );
                        (signal.clone(), signal)
                    }
                }
            })
        }
        .unzip();
        // FIXME: prevent unnecessary vector allocations
        let entries = {
            let entries = match deduplicate_hashmap_entries(&keys, &values) {
                Some(entries) => entries,
                None => keys.into_iter().zip(values).collect::<Vec<_>>(),
            };
            entries
        };
        Ok(factory.create_hashmap_term(entries))
    }
}

fn create_error_signal_term<T: Expression>(
    data: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        data,
        factory.create_nil_term(),
    ))))
}
