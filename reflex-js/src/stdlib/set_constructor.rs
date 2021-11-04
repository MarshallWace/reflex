// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::deduplicate_hashset_entries,
    stdlib::Stdlib,
};

pub struct SetConstructor {}
impl SetConstructor {
    const UUID: Uuid = uuid!("c87cb7a9-a926-4f78-b38b-5be67ac83baf");
    const ARITY: FunctionArity<0, 1> = FunctionArity {
        required: [],
        optional: [ArgType::Strict],
        variadic: None,
    };
}
impl Uid for SetConstructor {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for SetConstructor
where
    T::Builtin: From<Stdlib>,
{
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
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        if is_null_value_term(&values, factory) {
            Ok(factory.create_hashset_term(allocator.create_empty_list()))
        } else if let Some(values) = factory.match_vector_term(&values) {
            let values = values.items().iter().cloned().collect::<Vec<_>>();
            let has_dynamic_values = values.iter().any(|value| !value.is_static());
            if has_dynamic_values {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(Stdlib::CollectHashSet),
                    allocator.create_list(values),
                ))
            } else {
                let values = match deduplicate_hashset_entries(&values) {
                    Some(values) => values,
                    _ => values,
                };
                Ok(factory.create_hashset_term(allocator.create_list(values)))
            }
        } else {
            Err(format!(
                "Invalid Set constructor: Expected array of values, received {}",
                values,
            ))
        }
    }
}

fn is_null_value_term<T: Expression>(expression: &T, factory: &impl ExpressionFactory<T>) -> bool {
    factory
        .match_value_term(expression)
        .and_then(|value| value.match_null())
        .is_some()
}

#[cfg(test)]
mod tests {
    use crate::{builtins::JsBuiltins, globals::builtin_globals, parse, Env};
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, ExpressionFactory, HeapAllocator,
            StateCache,
        },
        lang::{SharedTermFactory, ValueTerm},
    };

    #[test]
    fn set_constructor() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Set()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_hashset_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Set([])", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_hashset_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Set(['one', 'two', 'three'])",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_hashset_term(allocator.create_list(vec![
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
            "new Set(['one', 'two', 'three', 'two'])",
            &env,
            &factory,
            &allocator,
        )
        .unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_hashset_term(allocator.create_list(vec![
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
    fn set_constructor_values() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Set([]).values()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Set(['one', 'two', 'three']).values()",
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
            "new Set(['one', 'two', 'three', 'two']).values()",
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
    fn set_constructor_entries() {
        let factory = SharedTermFactory::<JsBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let env = Env::new().with_globals(builtin_globals(&factory, &allocator));
        let mut cache = SubstitutionCache::new();
        let state = StateCache::default();
        let expression = parse("new Set([]).entries()", &env, &factory, &allocator).unwrap();
        let result = evaluate(&expression, &state, &factory, &allocator, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                factory.create_vector_term(allocator.create_empty_list()),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Set(['one', 'two', 'three']).entries()",
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
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("one"))
                        )),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                    )),
                ])),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Set(['one', 'two', 'three', 'two']).entries()",
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
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("one"))
                        )),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("two"))
                        )),
                    )),
                    factory.create_tuple_term(allocator.create_pair(
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("three"))
                        )),
                    )),
                ])),
                DependencyList::empty(),
            )
        );
    }
}
