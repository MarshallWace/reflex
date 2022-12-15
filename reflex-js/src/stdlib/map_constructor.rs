// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    deduplicate_hashmap_entries, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression,
    ExpressionFactory, ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType,
    Uid, Uuid,
};
use reflex_stdlib::{CollectList, ConstructHashMap, Get};

pub struct MapConstructor;
impl MapConstructor {
    pub const UUID: Uuid = uuid!("81fae6f8-9557-4784-998a-13ebfbf289ef");
    const ARITY: FunctionArity<0, 1> = FunctionArity {
        required: [],
        optional: [ArgType::Strict],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for MapConstructor {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for MapConstructor
where
    T::Builtin: From<ConstructHashMap> + From<CollectList> + From<Get>,
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
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        if is_nil_term(&entries, factory) {
            Ok(factory
                .create_hashmap_term(allocator.create_empty_list(), allocator.create_empty_list()))
        } else if let Some(entries) = factory.match_list_term(&entries) {
            let entries = entries
                .items()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .map(|entry| {
                    match get_indexed_field(entry, 0, factory, allocator).and_then(|key| {
                        get_indexed_field(entry, 1, factory, allocator).map(|value| (key, value))
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
                    let has_dynamic_keys = keys.iter().any(|item| !item.is_static());
                    let has_dynamic_values = values.iter().any(|item| !item.is_static());
                    if has_dynamic_keys || has_dynamic_values {
                        Ok(factory.create_application_term(
                            factory.create_builtin_term(ConstructHashMap),
                            allocator.create_pair(
                                if has_dynamic_keys {
                                    factory.create_application_term(
                                        factory.create_builtin_term(CollectList),
                                        allocator.create_list(keys),
                                    )
                                } else {
                                    factory.create_list_term(allocator.create_list(keys))
                                },
                                if has_dynamic_values {
                                    factory.create_application_term(
                                        factory.create_builtin_term(CollectList),
                                        allocator.create_list(values),
                                    )
                                } else {
                                    factory.create_list_term(allocator.create_list(values))
                                },
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

fn get_indexed_field<T: Expression>(
    target: &T,
    index: usize,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T>
where
    T::Builtin: From<Get>,
{
    if let Some(target) = factory.match_list_term(target) {
        target
            .items()
            .as_deref()
            .get(index)
            .map(|value| value.as_deref())
            .cloned()
    } else if target.is_static() {
        None
    } else {
        Some(factory.create_application_term(
            factory.create_builtin_term(Get),
            allocator.create_pair(target.clone(), factory.create_int_term(index as i32)),
        ))
    }
}

fn is_nil_term<T: Expression>(expression: &T, factory: &impl ExpressionFactory<T>) -> bool {
    factory.match_nil_term(expression).is_some()
}

#[cfg(test)]
mod tests {
    use std::iter::empty;

    use crate::{builtins::JsBuiltins, globals::builtin_globals, parse, Env};
    use reflex::{
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, Expression, ExpressionFactory,
            HeapAllocator, StateCache,
        },
    };
    use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};

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
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Map()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                create_hashmap_term(empty(), &factory, &allocator),
                DependencyList::empty(),
            )
        );
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
                            factory.create_string_term(allocator.create_static_string("one")),
                            factory.create_float_term(1.0),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("two")),
                            factory.create_float_term(2.0),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("three")),
                            factory.create_float_term(3.0),
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
                            factory.create_string_term(allocator.create_static_string("one")),
                            factory.create_float_term(1.0),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("two")),
                            factory.create_float_term(4.0),
                        ),
                        (
                            factory.create_string_term(allocator.create_static_string("three")),
                            factory.create_float_term(3.0),
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
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Map([]).entries()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_empty_list()),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("one")),
                        factory.create_float_term(1.0),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("two")),
                        factory.create_float_term(2.0),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("three")),
                        factory.create_float_term(3.0),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("one")),
                        factory.create_float_term(1.0),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("two")),
                        factory.create_float_term(4.0),
                    )),
                    factory.create_list_term(allocator.create_pair(
                        factory.create_string_term(allocator.create_static_string("three")),
                        factory.create_float_term(3.0),
                    )),
                ])),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_keys() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Map([]).keys()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_empty_list()),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_string_term(allocator.create_static_string("one")),
                    factory.create_string_term(allocator.create_static_string("two")),
                    factory.create_string_term(allocator.create_static_string("three")),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_string_term(allocator.create_static_string("one")),
                    factory.create_string_term(allocator.create_static_string("two")),
                    factory.create_string_term(allocator.create_static_string("three")),
                ])),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_values() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Map([]).values()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_list_term(allocator.create_empty_list()),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_float_term(1.0),
                    factory.create_float_term(2.0),
                    factory.create_float_term(3.0),
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
                factory.create_list_term(allocator.create_list(vec![
                    factory.create_float_term(1.0),
                    factory.create_float_term(4.0),
                    factory.create_float_term(3.0),
                ])),
                DependencyList::empty(),
            )
        );
    }
}
