// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, GraphNode, HeapAllocator,
    },
    lang::BuiltinTerm,
};

pub struct ResolveTuple {}
impl<T: Expression> Applicable<T> for ResolveTuple {
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
        if let Some(value) = factory.match_tuple_term(&target) {
            if value.is_static() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectTuple),
                    allocator.create_list(value.fields().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected <tuple>, received {}", target))
        }
    }
}

pub struct ResolveStruct {}
impl<T: Expression> Applicable<T> for ResolveStruct {
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
        if let Some(value) = factory.match_struct_term(&target) {
            if value.is_static() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectStruct),
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
impl<T: Expression> Applicable<T> for ResolveVector {
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
        if let Some(value) = factory.match_vector_term(&target) {
            if value.is_static() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectVector),
                    allocator.create_list(value.items().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected Vector, received {}", target))
        }
    }
}

pub struct ResolveHashSet {}
impl<T: Expression> Applicable<T> for ResolveHashSet {
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
        if let Some(value) = factory.match_hashset_term(&target) {
            if value.is_static() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectHashSet),
                    allocator.create_list(value.values().iter().cloned()),
                ))
            }
        } else {
            Err(format!("Expected HashSet, received {}", target))
        }
    }
}

pub struct ResolveHashMap {}
impl<T: Expression> Applicable<T> for ResolveHashMap {
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
        if let Some(value) = factory.match_hashmap_term(&target) {
            if value.keys().is_static() {
                Ok(target)
            } else {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::ConstructHashMap),
                    allocator.create_pair(
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::CollectVector),
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
