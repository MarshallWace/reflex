// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        GraphNode, HeapAllocator, SignalType,
    },
    lang::{deduplicate_hashmap_entries, deduplicate_hashset_entries, BuiltinTerm, ValueTerm},
};

pub struct Collect {}
impl Collect {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Collect {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        if let Some(collection) = factory.match_vector_term(&target) {
            Ok(if collection.is_static() {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectVector),
                    allocator.clone_list(collection.items()),
                )
            })
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            Ok(if collection.is_static() {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectHashSet),
                    allocator.clone_list(collection.values()),
                )
            })
        } else if let Some(collection) = factory.match_hashmap_term(&target) {
            Ok(if collection.is_static() {
                target.clone()
            } else {
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectHashMap),
                    allocator.create_list(collection.entries(factory, allocator)),
                )
            })
        } else {
            Err(format!(
                "Expected Vector or HashSet or HashMap, received {}",
                target,
            ))
        }
    }
}

const COLLECT_TUPLE_ARITY: FunctionArity<0, 0> = FunctionArity {
    required: [],
    optional: [],
    variadic: Some(ArgType::Strict),
};
pub struct CollectTuple {}
impl<T: Expression> Applicable<T> for CollectTuple {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&COLLECT_TUPLE_ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_tuple_term(allocator.create_list(args)))
    }
}

pub struct CollectStruct {}
impl CollectStruct {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for CollectStruct {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
            Some(constructor) if constructor.prototype().keys().len() == args.len() => Ok(factory
                .create_struct_term(
                    allocator.clone_struct_prototype(constructor.prototype()),
                    allocator.create_list(args),
                )),
            _ => Err(format!(
                "Expected <constructor:{}>, received {}",
                args.len(),
                prototype
            )),
        }
    }
}

pub struct CollectVector {}
impl CollectVector {
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for CollectVector {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        Ok(factory.create_vector_term(allocator.create_list(args)))
    }
}

pub struct CollectHashSet {}
impl CollectHashSet {
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for CollectHashSet {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let values = args.collect::<Vec<_>>();
        let deduplicated_values = match deduplicate_hashset_entries(&values) {
            Some(values) => values,
            None => values,
        };
        Ok(factory.create_hashset_term(allocator.create_list(deduplicated_values)))
    }
}

pub struct CollectHashMap {}
impl CollectHashMap {
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for CollectHashMap {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
                let static_entry = if let Some(entry) = factory.match_tuple_term(&arg) {
                    match (entry.get(0), entry.get(1)) {
                        (Some(key), Some(value)) if entry.size() == 0 => {
                            Ok(Some((key.clone(), value.clone())))
                        }
                        _ => Err(format!("Invalid HashMap entry: {}", entry)),
                    }
                } else if let Some(entry) = factory.match_vector_term(&arg) {
                    match (entry.items().get(0), entry.items().get(1)) {
                        (Some(key), Some(value)) => Ok(Some((key.clone(), value.clone()))),
                        _ => Err(format!("Invalid HashMap entry: {}", entry)),
                    }
                } else {
                    Ok(None)
                };
                match static_entry {
                    Ok(Some((key, value))) => (key, value),
                    Ok(None) => (
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                arg.clone(),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator
                                .create_pair(arg, factory.create_value_term(ValueTerm::Int(1))),
                        ),
                    ),
                    Err(err) => {
                        let signal = create_error_signal_term(
                            factory
                                .create_value_term(ValueTerm::String(allocator.create_string(err))),
                            factory,
                            allocator,
                        );
                        (signal.clone(), signal)
                    }
                }
            })
        }
        .unzip();
        let (keys, values) = match deduplicate_hashmap_entries(&keys, &values) {
            Some((keys, values)) => (keys, values),
            None => (keys, values),
        };
        Ok(factory.create_hashmap_term(allocator.create_list(keys), allocator.create_list(values)))
    }
}

fn create_error_signal_term<T: Expression>(
    data: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Error, allocator.create_unit_list(data)),
    )))
}
