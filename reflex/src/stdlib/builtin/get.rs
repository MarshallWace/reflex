// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, StructFieldOffset, Term},
    stdlib::{
        builtin::BuiltinFunction,
        collection::CollectionTerm,
        signal::SignalType,
        value::{is_integer, ValueTerm},
    },
};

pub struct Get {}
impl BuiltinFunction for Get {
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        match target.value() {
            Term::Struct(target) => match target.prototype() {
                Some(constructor) => match constructor
                    .field(&key)
                    .and_then(|field_offset| target.get(field_offset))
                {
                    Some(expression) => Expression::clone(expression),
                    None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(format!(
                            "Invalid field access: {} on struct {}",
                            key, target
                        ))],
                    )))),
                },
                None => match key.value() {
                    Term::Value(ValueTerm::Int(field_offset)) => {
                        match target.get(*field_offset as StructFieldOffset) {
                            Some(expression) => Expression::clone(expression),
                            None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                                SignalType::Error,
                                vec![ValueTerm::String(format!(
                                    "Invalid field offset: {} on struct {}",
                                    field_offset, target
                                ))],
                            )))),
                        }
                    }
                    _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(format!(
                            "Invalid field access: Expected (<struct>, Int), received ({}, {})",
                            target, key,
                        ))],
                    )))),
                },
            },
            Term::Collection(CollectionTerm::Vector(target)) => {
                let index = match key.value() {
                    Term::Value(ValueTerm::Int(value)) => {
                        let value = *value;
                        if value >= 0 {
                            Some(value as usize)
                        } else {
                            None
                        }
                    }
                    Term::Value(ValueTerm::Float(value)) => {
                        let value = *value;
                        if is_integer(value) && value >= 0.0 {
                            Some(value as usize)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                match index {
                    None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(format!(
                            "Invalid array field access: Expected integer, received {}",
                            key,
                        ))],
                    )))),
                    Some(key) => match target.get(key) {
                        Some(expression) => Expression::clone(expression),
                        None => Expression::new(Term::Value(ValueTerm::Null)),
                    },
                }
            }
            Term::Collection(CollectionTerm::HashMap(target)) => match target.get(&key) {
                Some(expression) => Expression::clone(expression),
                None => Expression::new(Term::Value(ValueTerm::Null)),
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Unable to access field {} on {}",
                    key, target,
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::EvaluationCache,
        core::{
            ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression,
            StructPrototype, StructTerm, Term,
        },
        stdlib::{
            builtin::BuiltinTerm,
            collection::{hashmap::HashMapTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn get_anonymous_struct_fields() {
        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ],
                ))),
                Expression::new(Term::Value(ValueTerm::Int(1))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(4)))),
                DependencyList::empty(),
            )
        )
    }

    #[test]
    fn get_named_struct_fields() {
        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                Expression::new(Term::Struct(StructTerm::new(
                    Some(StructPrototype::new(vec![
                        Expression::new(Term::Value(ValueTerm::Symbol(3))),
                        Expression::new(Term::Value(ValueTerm::Symbol(4))),
                        Expression::new(Term::Value(ValueTerm::Symbol(5))),
                    ])),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(6))),
                        Expression::new(Term::Value(ValueTerm::Int(7))),
                        Expression::new(Term::Value(ValueTerm::Int(8))),
                    ],
                ))),
                Expression::new(Term::Value(ValueTerm::Symbol(4))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(7)))),
                DependencyList::empty(),
            )
        )
    }

    #[test]
    fn get_hashmap_fields() {
        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
                    vec![
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "foo",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(3))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "bar",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(4))),
                        ),
                        (
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                "baz",
                            )))),
                            Expression::new(Term::Value(ValueTerm::Int(5))),
                        ),
                    ],
                )))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(4)))),
                DependencyList::empty()
            ),
        );
    }
}
