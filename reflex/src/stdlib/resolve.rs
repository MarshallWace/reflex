// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    stdlib::Stdlib,
};

pub struct ResolveTuple {}
impl ResolveTuple {
    pub(crate) const UUID: Uuid = uuid!("4422c938-62aa-4610-a388-ecaf7af487e5");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveTuple {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveTuple
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
        if let Some(value) = factory.match_tuple_term(&target) {
            let has_dynamic_values = value.fields().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectTuple),
                    allocator.create_list(value.fields().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected <tuple>, received {}", target))
        }
    }
}

pub struct ResolveStruct {}
impl ResolveStruct {
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
impl Uid for ResolveStruct {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveStruct
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
        if let Some(value) = factory.match_struct_term(&target) {
            let has_dynamic_values = value.fields().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectStruct),
                    allocator.create_sized_list(
                        value.fields().len() + 1,
                        once(factory.create_constructor_term(
                            allocator.clone_struct_prototype(value.prototype()),
                        ))
                        .chain(value.fields().iter().cloned()),
                    ),
                ))
            }
        } else {
            Err(format!("Expected <struct>, received {}", target))
        }
    }
}

pub struct ResolveVector {}
impl ResolveVector {
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
impl Uid for ResolveVector {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveVector
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
        if let Some(value) = factory.match_vector_term(&target) {
            let has_dynamic_values = value.items().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectVector),
                    allocator.create_list(value.items().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected Vector, received {}", target))
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
            let has_dynamic_values = value.values().iter().any(|item| !item.is_static());
            if !has_dynamic_values {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(value.values().iter().cloned()),
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
            let has_dynamic_keys = value.keys().iter().any(|item| !item.is_static());
            if !has_dynamic_keys {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ConstructHashMap),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::CollectVector),
                            allocator.clone_list(value.keys()),
                        ),
                        factory.create_vector_term(allocator.clone_list(value.values())),
                    ),
                ))
            }
        } else {
            Err(format!("Expected HashMap, received {}", target))
        }
    }
}
