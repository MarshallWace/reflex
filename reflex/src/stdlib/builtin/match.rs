// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Match {}
impl BuiltinFunction for Match {
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
        let matcher = args.next().unwrap();
        let result = match (match_enum(&target), matcher.value()) {
            (Some((discriminant, args)), Term::Struct(matcher)) => Some({
                match matcher.fields().get(discriminant) {
                    Some(handler) => Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::clone(handler),
                        args.into_iter().map(Expression::clone).collect(),
                    ))),
                    None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Unhandled enum index: {} for matcher {}",
                            discriminant, matcher
                        ))))],
                    )))),
                }
            }),
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Invalid pattern match: Expected (<enum>, <struct>), received ({}, {})",
                    target, matcher,
                ))))],
            )))),
        }
    }
}

fn match_enum(target: &Expression) -> Option<(usize, impl IntoIterator<Item = &Expression>)> {
    match target.value() {
        Term::Struct(target) if target.prototype().is_none() => {
            target
                .get(0)
                .and_then(|discriminant| match discriminant.value() {
                    Term::Value(ValueTerm::Int(value)) if *value >= 0 => {
                        Some((*value as usize, target.fields().iter().skip(1)))
                    }
                    _ => None,
                })
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::SubstitutionCache,
        core::{
            ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
            LambdaTerm, Signal, SignalTerm, StructTerm, Term, VariableTerm,
        },
        stdlib::builtin::BuiltinTerm,
        stdlib::signal::SignalType,
        stdlib::value::ValueTerm,
    };

    #[test]
    fn match_expressions() {
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Match)),
            vec![
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ],
                ))),
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 0, None),
                            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                                SignalType::Error,
                                vec![Expression::new(Term::Value(ValueTerm::String(
                                    String::from("foo"),
                                )))],
                            )))),
                        ))),
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 3, None),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::scoped(2))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                1,
                                            ))),
                                            Expression::new(Term::Variable(VariableTerm::scoped(
                                                0,
                                            ))),
                                        ],
                                    ))),
                                ],
                            ))),
                        ))),
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 0, None),
                            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                                SignalType::Error,
                                vec![Expression::new(Term::Value(ValueTerm::String(
                                    String::from("bar"),
                                )))],
                            )))),
                        ))),
                    ],
                ))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4 + 5))),
                DependencyList::empty(),
            )
        )
    }
}
