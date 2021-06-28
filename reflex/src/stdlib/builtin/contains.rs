// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction,
        collection::CollectionTerm,
        signal::SignalType,
        value::ValueTerm,
    },
};

pub struct Contains {}
impl BuiltinFunction for Contains {
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        let result = match target.value() {
            Term::Struct(target) if target.prototype().is_some() => match target.prototype() {
                Some(constructor) => {
                    let field_name = match key.value() {
                        Term::Value(key) => Some(key),
                        _ => None,
                    };
                    let field_offset = field_name.and_then(|key| constructor.field(key));
                    Ok(field_offset.is_some())
                }
                None => Ok(false),
            },
            Term::Collection(CollectionTerm::HashMap(target)) => Ok(target.get(&key).is_some()),
            Term::Collection(CollectionTerm::HashSet(target)) => Ok(target.contains(&key)),
            _ => Err(format!("Unable to determine set inclusion: Expected (<struct>, <any>) or (HashMap, any) or (HashSet, any), received ({}, {})", target, key)),
        };
        match result {
            Ok(result) => Expression::new(Term::Value(ValueTerm::Boolean(result))),
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::GenerationalGc,
        core::{
            ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression,
            StructPrototype, StructTerm, Term,
        },
        stdlib::{
            builtin::BuiltinTerm,
            collection::{
                hashmap::HashMapTerm, hashset::HashSetTerm, CollectionTerm,
            },
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn struct_contains() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("first")),
                        ValueTerm::String(StringValue::from("second")),
                        ValueTerm::String(StringValue::from("third")),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                        Expression::new(Term::Value(ValueTerm::Int(2))),
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                    ],
                ))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("second")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        ValueTerm::String(StringValue::from("first")),
                        ValueTerm::String(StringValue::from("second")),
                        ValueTerm::String(StringValue::from("third")),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                        Expression::new(Term::Value(ValueTerm::Int(2))),
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                    ],
                ))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("fourth")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn hashmap_contains() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "first",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "second",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(2))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "third",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                        ),
                    ],
                )))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("first")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "first",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(1))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "second",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(2))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "third",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                        ),
                    ],
                )))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("fourth")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn hashset_contains() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("first")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                            "second",
                        )))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("third")))),
                    ],
                )))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("first")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Contains)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("first")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                            "second",
                        )))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("third")))),
                    ],
                )))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("fourth")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
                DependencyList::empty(),
            )
        );
    }
}
