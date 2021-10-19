// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::str::FromStr;

use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        NativeAllocator,
    },
    lang::{deduplicate_hashset_entries, BuiltinTerm, NativeFunction},
};
use uuid::Uuid;

pub fn global_set<T: Expression>(factory: &impl ExpressionFactory<T>) -> T {
    factory.create_native_function_term(set_constructor())
}

pub fn set_constructor<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        SetConstructor::uid(),
        SetConstructor::name(),
        SetConstructor::arity(),
        SetConstructor::apply,
    )
}
struct SetConstructor {}
impl SetConstructor {
    const NAME: &'static str = "SetConstructor";
    const UUID: &'static str = "c87cb7a9-a926-4f78-b38b-5be67ac83baf";
    const ARITY: FunctionArity<0, 1> = FunctionArity {
        required: [],
        optional: [ArgType::Strict],
        variadic: None,
    };
    fn uid() -> Uuid {
        Uuid::from_str(Self::UUID).unwrap()
    }
    fn name() -> Option<&'static str> {
        Some(Self::NAME)
    }
    fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        if is_null_value_term(&values, &factory) {
            Ok(factory.create_hashset_term(allocator.create_empty_list()))
        } else if let Some(values) = factory.match_vector_term(&values) {
            let values = values.items().iter().cloned().collect::<Vec<_>>();
            let has_dynamic_values = values.iter().any(|value| !value.is_static());
            if has_dynamic_values {
                Ok(factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::CollectHashSet),
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
    use crate::{parse, stdlib::builtin_globals, Env};
    use reflex::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            evaluate, DependencyList, EvaluationResult, ExpressionFactory, HeapAllocator,
            StateCache,
        },
        lang::{TermFactory, ValueTerm},
    };

    #[test]
    fn set_constructor() {
        let factory = TermFactory::default();
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
        let factory = TermFactory::default();
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
        let factory = TermFactory::default();
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
