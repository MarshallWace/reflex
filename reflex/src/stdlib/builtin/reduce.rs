// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{builtin::BuiltinFunction, collection::CollectionTerm, signal::SignalType},
};

pub struct Reduce {}
impl BuiltinFunction for Reduce {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 3 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 3 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let iteratee = args.next().unwrap();
        let seed = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(target) => match target {
                CollectionTerm::HashMap(target) => {
                    Some(target.iterate().into_iter().fold(seed, |result, entry| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&iteratee),
                            vec![result, entry],
                        )))
                    }))
                }
                CollectionTerm::HashSet(target) => {
                    Some(target.iterate().into_iter().fold(seed, |result, value| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&iteratee),
                            vec![result, value],
                        )))
                    }))
                }
                CollectionTerm::Vector(target) => {
                    Some(target.iterate().into_iter().fold(seed, |result, item| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&iteratee),
                            vec![result, item],
                        )))
                    }))
                }
            },
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Unable to reduce {}",
                    target
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::GenerationalGc,
        core::{
            ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
            LambdaTerm, Term, VariableTerm,
        },
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            value::ValueTerm,
        },
    };

    #[test]
    fn reduce_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Reduce)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    Vec::new(),
                )))),
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                Expression::new(Term::Value(ValueTerm::Int(3))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Reduce)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                        Expression::new(Term::Value(ValueTerm::Int(6))),
                    ],
                )))),
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                Expression::new(Term::Value(ValueTerm::Int(3))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4 + 5 + 6))),
                DependencyList::empty(),
            ),
        );
        let collection = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Reduce)),
            vec![
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                        Expression::new(Term::Value(ValueTerm::Int(6))),
                    ],
                )))),
                Expression::new(Term::Lambda(LambdaTerm::new(
                    Arity::from(0, 2, None),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Push)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::scoped(1))),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Multiply)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(0))),
                                    Expression::new(Term::Value(ValueTerm::Int(2))),
                                ],
                            ))),
                        ],
                    ))),
                ))),
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
                )))),
            ],
        )));
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Collect)),
            vec![collection],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4 * 2))),
                        Expression::new(Term::Value(ValueTerm::Int(5 * 2))),
                        Expression::new(Term::Value(ValueTerm::Int(6 * 2))),
                    ]
                )))),
                DependencyList::empty(),
            ),
        );
    }
}
