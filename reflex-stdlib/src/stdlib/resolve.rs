// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RecordTermType, RefType, Uid, Uuid,
};

use crate::{Apply, CollectHashSet, CollectList, ConstructHashMap};

pub struct ResolveRecord;
impl ResolveRecord {
    pub const UUID: Uuid = uuid!("0e580200-7a85-415b-ba8e-ac854dc51ec7");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveRecord {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveRecord
where
    T::Builtin: Builtin + From<Apply> + From<CollectList>,
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
        if let Some(value) = factory.match_record_term(&target) {
            let has_dynamic_values = value
                .values()
                .as_deref()
                .iter()
                .map(|item| item.as_deref().clone())
                .any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Apply),
                    allocator.create_pair(
                        factory.create_constructor_term(
                            allocator.clone_struct_prototype(value.prototype()),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(CollectList),
                            allocator.clone_list(value.values()),
                        ),
                    ),
                ))
            }
        } else {
            Err(format!("Expected <struct>, received {}", target))
        }
    }
}

pub struct ResolveList;
impl ResolveList {
    pub const UUID: Uuid = uuid!("6d324a63-2138-41ad-8775-ad4931043700");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveList {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveList
where
    T::Builtin: From<CollectList>,
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
        if let Some(value) = factory.match_list_term(&target) {
            let has_dynamic_values = value
                .items()
                .as_deref()
                .iter()
                .any(|item| !item.as_deref().is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(CollectList),
                    allocator.create_list(
                        value
                            .items()
                            .as_deref()
                            .iter()
                            .map(|item| item.as_deref().clone()),
                    ),
                ))
            }
        } else {
            Err(format!("Expected List, received {}", target))
        }
    }
}

pub struct ResolveHashSet;
impl ResolveHashSet {
    pub const UUID: Uuid = uuid!("8c3c802c-8092-4dbf-9d5f-393b8144d39e");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveHashSet {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveHashSet
where
    T::Builtin: From<CollectHashSet>,
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
        if let Some(value) = factory.match_hashset_term(&target) {
            let has_dynamic_values = value.values().any(|item| !item.as_deref().is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(CollectHashSet),
                    allocator.create_list(value.values().map(|item| item.as_deref().clone())),
                ))
            }
        } else {
            Err(format!("Expected HashSet, received {}", target))
        }
    }
}

pub struct ResolveHashMap;
impl ResolveHashMap {
    pub const UUID: Uuid = uuid!("15ccc11f-31e9-4f8d-abb9-cf9dff0b26bd");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveHashMap {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveHashMap
where
    T::Builtin: From<ConstructHashMap> + From<CollectList>,
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
        if let Some(value) = factory.match_hashmap_term(&target) {
            let has_dynamic_keys = value.keys().any(|item| !item.as_deref().is_static());
            if !has_dynamic_keys {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(ConstructHashMap),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(CollectList),
                            allocator.create_list(value.keys().map(|item| item.as_deref().clone())),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(CollectList),
                            allocator
                                .create_list(value.values().map(|item| item.as_deref().clone())),
                        ),
                    ),
                ))
            }
        } else {
            Err(format!("Expected HashMap, received {}", target))
        }
    }
}
