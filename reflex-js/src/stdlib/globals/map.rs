// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, NativeFunction, Signal, SignalTerm, StructTerm, Term,
    },
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{hashmap::HashMapTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub(crate) fn global_map() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        MapConstructor::hash(),
        MapConstructor::arity(),
        MapConstructor::apply,
    )))
}
struct MapConstructor {}
impl MapConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Ok(entries),
            _ => Err(format!("Invalid Map constructor: {}", entries)),
        };
        match entries {
            Ok(entries) => {
                let entries = entries.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_keys = entries.iter().any(|entry| match entry.value() {
                    Term::Struct(entry) if entry.prototype().is_none() => {
                        match entry.fields().get(0) {
                            Some(key) => !key.is_static(),
                            _ => true,
                        }
                    }
                    Term::Collection(entry) => match entry {
                        CollectionTerm::Vector(entry) => match entry.get(0) {
                            Some(key) => !key.is_static(),
                            _ => true,
                        },
                        _ => true,
                    },
                    _ => true,
                });
                match has_dynamic_keys {
                    false => HashMapTerm::collect(entries),
                    true => {
                        let get = Expression::new(Term::Builtin(BuiltinTerm::Get));
                        let first = Expression::new(Term::Value(ValueTerm::Int(0)));
                        let second = Expression::new(Term::Value(ValueTerm::Int(1)));
                        let (keys, values): (Vec<_>, Vec<_>) = entries
                            .into_iter()
                            .map(|entry| {
                                (
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::clone(&get),
                                        vec![Expression::clone(&entry), Expression::clone(&first)],
                                    ))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::clone(&get),
                                        vec![entry, Expression::clone(&second)],
                                    ))),
                                )
                            })
                            .unzip();
                        Expression::new(Term::Application(ApplicationTerm::new(
                            dynamic_map_constructor(),
                            vec![
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::CollectTuple)),
                                    keys,
                                ))),
                                Expression::new(Term::Struct(StructTerm::new(None, values))),
                            ],
                        )))
                    }
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

fn dynamic_map_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicMapConstructor::hash(),
        DynamicMapConstructor::arity(),
        DynamicMapConstructor::apply,
    )))
}
struct DynamicMapConstructor {}
impl DynamicMapConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let keys = match keys.value() {
            Term::Struct(keys) if keys.prototype().is_none() => Some(keys.fields()),
            _ => None,
        };
        let values = match values.value() {
            Term::Struct(values) if values.prototype().is_none() => Some(values.fields()),
            _ => None,
        };
        match (keys, values) {
            (Some(keys), Some(values)) if keys.len() == values.len() => {
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    keys.iter()
                        .map(Expression::clone)
                        .zip(values.iter().map(Expression::clone)),
                ))))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("Invalid Map constructor values"),
                )))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse, stdlib::builtin_globals, Env};
    use reflex::{
        cache::GenerationalGc,
        core::{DependencyList, DynamicState, EvaluationResult, Expression, StructTerm, Term},
        stdlib::{
            collection::{hashmap::HashMapTerm, vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn map_constructor() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("new Map([])", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse("new Map([['one', 1], ['two', 2], ['three', 3]])", &env).unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "one"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(1.0))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "two"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(2.0))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "three"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        ),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]])",
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "one"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(1.0))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "two"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "three"
                            )))),
                            Expression::new(Term::Value(ValueTerm::Float(3.0))),
                        ),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_entries() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).entries()", &env).unwrap();
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
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).entries()",
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "one"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(1.0))),
                            ]
                        ))),
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "two"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(2.0))),
                            ]
                        ))),
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "three"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                            ]
                        ))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).entries()",
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "one"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(1.0))),
                            ]
                        ))),
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "two"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(4.0))),
                            ]
                        ))),
                        Expression::new(Term::Struct(StructTerm::new(
                            None,
                            vec![
                                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                    "three"
                                )))),
                                Expression::new(Term::Value(ValueTerm::Float(3.0))),
                            ]
                        ))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn map_constructor_keys() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).keys()", &env).unwrap();
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
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).keys()",
            &env,
        )
        .unwrap();
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
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).keys()",
            &env,
        )
        .unwrap();
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

    #[test]
    fn map_constructor_values() {
        let env = Env::new().with_globals(builtin_globals());
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("new Map([]).values()", &env).unwrap();
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
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3]]).values()",
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(1.0))),
                        Expression::new(Term::Value(ValueTerm::Float(2.0))),
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = parse(
            "new Map([['one', 1], ['two', 2], ['three', 3], ['two', 4]]).values()",
            &env,
        )
        .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Float(1.0))),
                        Expression::new(Term::Value(ValueTerm::Float(4.0))),
                        Expression::new(Term::Value(ValueTerm::Float(3.0))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }
}
