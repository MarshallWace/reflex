// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RecordTermType, Uid, Uuid,
};

use crate::Stdlib;

pub struct ResolveRecord {}
impl ResolveRecord {
    pub(crate) const UUID: Uuid = uuid!("0e580200-7a85-415b-ba8e-ac854dc51ec7");
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
    T::Builtin: From<Stdlib>,
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
            let has_dynamic_values = value.values().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                let values = value.values();
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectRecord),
                    allocator.create_sized_list(
                        values.len() + 1,
                        once(factory.create_constructor_term(
                            allocator.clone_struct_prototype(value.prototype()),
                        ))
                        .chain(values.iter().cloned()),
                    ),
                ))
            }
        } else {
            Err(format!("Expected <struct>, received {}", target))
        }
    }
}

pub struct ResolveList {}
impl ResolveList {
    pub(crate) const UUID: Uuid = uuid!("6d324a63-2138-41ad-8775-ad4931043700");
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
    T::Builtin: From<Stdlib>,
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
            let has_dynamic_values = value.items().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectList),
                    allocator.create_list(value.items().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected List, received {}", target))
        }
    }
}

pub struct ResolveHashSet {}
impl ResolveHashSet {
    pub(crate) const UUID: Uuid = uuid!("8c3c802c-8092-4dbf-9d5f-393b8144d39e");
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
    T::Builtin: From<Stdlib>,
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
            let has_dynamic_values = value.values().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(value.values().into_iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected HashSet, received {}", target))
        }
    }
}

pub struct ResolveHashMap {}
impl ResolveHashMap {
    pub(crate) const UUID: Uuid = uuid!("15ccc11f-31e9-4f8d-abb9-cf9dff0b26bd");
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
    T::Builtin: From<Stdlib>,
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
            let has_dynamic_keys = value.keys().any(|item| !item.is_static());
            if !has_dynamic_keys {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::CollectList),
                            allocator.create_list(value.keys().cloned()),
                        ),
                        factory.create_list_term(allocator.create_list(value.values().cloned())),
                    ),
                ))
            }
        } else {
            Err(format!("Expected HashMap, received {}", target))
        }
    }
}
