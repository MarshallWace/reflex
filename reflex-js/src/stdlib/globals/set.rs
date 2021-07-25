// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{ApplicationTerm, Arity, Expression, NativeFunction, Signal, SignalTerm, Term},
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{hashset::HashSetTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub fn global_set() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        SetConstructor::hash(),
        SetConstructor::arity(),
        SetConstructor::apply,
    )))
}
struct SetConstructor {}
impl SetConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        let values = match values.value() {
            Term::Collection(CollectionTerm::Vector(values)) => Ok(values),
            _ => Err(format!("Invalid Set constructor: {}", values)),
        };
        match values {
            Ok(values) => {
                let values = values.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_values = values.iter().any(|value| !value.is_static());
                match has_dynamic_values {
                    false => HashSetTerm::collect(values),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        dynamic_set_constructor(),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectTuple)),
                            values,
                        )))],
                    ))),
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

fn dynamic_set_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicSetConstructor::hash(),
        DynamicSetConstructor::arity(),
        DynamicSetConstructor::apply,
    )))
}
struct DynamicSetConstructor {}
impl DynamicSetConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        let values = match values.value() {
            Term::Struct(values) if values.prototype().is_none() => Some(values.fields()),
            _ => None,
        };
        match values {
            Some(values) => Expression::new(Term::Collection(CollectionTerm::HashSet(
                HashSetTerm::new(values.iter().map(Expression::clone)),
            ))),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("Invalid Set constructor values"),
                )))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse, stdlib::builtin_globals, Env};
    use reflex::{
        cache::SubstitutionCache,
        core::{DependencyList, DynamicState, EvaluationResult, Expression, Term},
        stdlib::{
            collection::{hashset::HashSetTerm, vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn set_constructor() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Set([])", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                    vec![],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Set(['one', 'two', 'three'])", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("one")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("two")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("three")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Set(['one', 'two', 'three', 'two'])", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("one")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("two")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("three")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn set_constructor_values() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("new Set([]).values()", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Set(['one', 'two', 'three']).values()", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("one")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("two")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("three")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Set(['one', 'two', 'three', 'two']).values()", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("one")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("two")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("three")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }
}
