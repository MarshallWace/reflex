// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{builtin::BuiltinFunction, signal::SignalType},
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
                vec![SerializedTerm::string(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let matcher = args.next().unwrap();
        match (target.value(), matcher.value()) {
            (Term::Enum(target), Term::Struct(matcher)) => {
                match matcher.fields().get(target.index() as usize) {
                    Some(handler) => Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::clone(handler),
                        target.args().iter().map(Expression::clone).collect(),
                    ))),
                    None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![SerializedTerm::string(format!(
                            "Unhandled enum index: {} for matcher {}",
                            target.index(),
                            matcher
                        ))],
                    )))),
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Invalid pattern match: Expected (<enum>, <struct>), received ({}, {})",
                    target, matcher,
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
            ApplicationTerm, Arity, DependencyList, DynamicState, EnumTerm, EvaluationResult,
            Expression, LambdaTerm, Signal, SignalTerm, StaticVariableTerm, StructTerm, Term,
            VariableTerm,
        },
        serialize::SerializedTerm,
        stdlib::builtin::BuiltinTerm,
        stdlib::signal::SignalType,
        stdlib::value::ValueTerm,
    };

    #[test]
    fn match_expressions() {
        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Match)),
            vec![
                Expression::new(Term::Enum(EnumTerm::new(
                    1,
                    vec![
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
                                vec![SerializedTerm::string(String::from("foo"))],
                            )))),
                        ))),
                        Expression::new(Term::Lambda(LambdaTerm::new(
                            Arity::from(0, 3, None),
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                vec![
                                    Expression::new(Term::Variable(VariableTerm::Static(
                                        StaticVariableTerm::new(2),
                                    ))),
                                    Expression::new(Term::Application(ApplicationTerm::new(
                                        Expression::new(Term::Builtin(BuiltinTerm::Add)),
                                        vec![
                                            Expression::new(Term::Variable(VariableTerm::Static(
                                                StaticVariableTerm::new(1),
                                            ))),
                                            Expression::new(Term::Variable(VariableTerm::Static(
                                                StaticVariableTerm::new(0),
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
                                vec![SerializedTerm::string(String::from("bar"))],
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
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4 + 5)))),
                DependencyList::empty(),
            )
        )
    }
}
