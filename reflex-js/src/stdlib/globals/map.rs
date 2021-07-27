// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, NativeAllocator,
        VarArgs,
    },
    hash::{hash_object, HashId},
    lang::{deduplicate_hashmap_entries, BuiltinTerm, NativeFunction, ValueTerm},
};

pub fn global_map<T: Expression>(factory: &impl ExpressionFactory<T>) -> T {
    factory.create_native_function_term(map_constructor())
}

pub fn map_constructor<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new(
        MapConstructor::uid(),
        MapConstructor::name(),
        MapConstructor::arity(),
        MapConstructor::apply,
    )
}
struct MapConstructor {}
impl MapConstructor {
    fn uid() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn name() -> Option<&'static str> {
        Some("MapConstructor")
    }
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() == 0 {
            Ok(factory
                .create_hashmap_term(allocator.create_empty_list(), allocator.create_empty_list()))
        } else {
            let entries = args.next().unwrap();
            if let Some(entries) = factory.match_vector_term(&entries) {
                let entries = entries
                    .items()
                    .iter()
                    .map(|entry| {
                        match get_indexed_field(entry, 0, &factory, &allocator).and_then(|key| {
                            get_indexed_field(entry, 1, &factory, &allocator)
                                .map(|value| (key, value))
                        }) {
                            Some((key, value)) => Ok((key, value)),
                            None => Err(format!(
                                "Invalid Map constructor: Expected [key, value] pair, received {}",
                                entry
                            )),
                        }
                    })
                    .collect::<Result<Vec<_>, _>>();
                match entries {
                    Err(error) => Err(error),
                    Ok(entries) => {
                        let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
                        let has_dynamic_keys = keys.iter().any(|key| !key.is_static());
                        if has_dynamic_keys {
                            Ok(factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::HashMap),
                                allocator.create_pair(
                                    factory.create_application_term(
                                        factory.create_builtin_term(BuiltinTerm::CollectVector),
                                        allocator.create_list(keys),
                                    ),
                                    factory.create_vector_term(allocator.create_list(values)),
                                ),
                            ))
                        } else {
                            let (keys, values) = match deduplicate_hashmap_entries(&keys, &values) {
                                Some((keys, values)) => (keys, values),
                                None => (keys, values),
                            };
                            Ok(factory.create_hashmap_term(
                                allocator.create_list(keys),
                                allocator.create_list(values),
                            ))
                        }
                    }
                }
            } else {
                Err(format!(
                    "Invalid Map constructor: Expected [key, value] pairs, received {}",
                    entries
                ))
            }
        }
    }
}

fn get_indexed_field<T: Expression>(
    target: &T,
    index: usize,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    if let Some(target) = factory.match_tuple_term(target) {
        target.get(index).cloned()
    } else if let Some(target) = factory.match_vector_term(target) {
        target.items().get(index).cloned()
    } else if target.is_static() {
        None
    } else {
        Some(factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Get),
            allocator.create_pair(
                target.clone(),
                factory.create_value_term(ValueTerm::Int(index as i32)),
            ),
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::iter::empty;

    use crate::{parse, stdlib::builtin_globals, Env};
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, DynamicState, EvaluationResult, Expression,
            ExpressionFactory, HeapAllocator,
        },
        lang::{TermFactory, ValueTerm},
    };

    fn create_hashmap_term<T: Expression>(
        entries: impl IntoIterator<Item = (T, T)>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T {
        let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
        factory.create_hashmap_term(allocator.create_list(keys), allocator.create_list(values))
    }

    #[test]
    fn map_constructor() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Map([])", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_hashmap_term(empty(), &factory, &allocator),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]])",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_hashmap_term(
                    vec![
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("one"))
                            )),
                            factory.create_value_term(ValueTerm::Float(1.0)),
                        ),
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("two"))
                            )),
                            factory.create_value_term(ValueTerm::Float(2.0)),
                        ),
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("three"))
                            )),
                            factory.create_value_term(ValueTerm::Float(3.0)),
                        ),
                    ],
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]])",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_hashmap_term(
                    vec![
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("one"))
                            )),
                            factory.create_value_term(ValueTerm::Float(1.0)),
                        ),
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("two"))
                            )),
                            factory.create_value_term(ValueTerm::Float(4.0)),
                        ),
                        (
                            factory.create_value_term(ValueTerm::String(
                                allocator.create_string(String::from("three"))
                            )),
                            factory.create_value_term(ValueTerm::Float(3.0)),
                        ),
                    ],
                    &factory,
                    &allocator
                ),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_entries() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).entries()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).entries()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("one"))
                        )),
                        factory.create_value_term(ValueTerm::Float(1.0)),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                        factory.create_value_term(ValueTerm::Float(2.0)),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                        factory.create_value_term(ValueTerm::Float(3.0)),
                    )),
                ])),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).entries()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("one"))
                        )),
                        factory.create_value_term(ValueTerm::Float(1.0)),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                        factory.create_value_term(ValueTerm::Float(4.0)),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                        factory.create_value_term(ValueTerm::Float(3.0)),
                    )),
                ])),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_keys() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).keys()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).keys()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("one"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("two"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("three"))
                    )),
                ])),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).keys()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("one"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("two"))
                    )),
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from("three"))
                    )),
                ])),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_values() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).values()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).values()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(1.0)),
                    factory.create_value_term(ValueTerm::Float(2.0)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                ])),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).values()",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Float(1.0)),
                    factory.create_value_term(ValueTerm::Float(4.0)),
                    factory.create_value_term(ValueTerm::Float(3.0)),
                ])),
                DependencyList::empty(),
            )
        );
    }
}
