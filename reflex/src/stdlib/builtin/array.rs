// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, NativeFunction, Reducible, Signal, SignalTerm, Term, VarArgs},
    stdlib::{
        signal::SignalType,
        value::{ArrayValue, ValueTerm},
    },
};

pub struct Array {}
impl NativeFunction for Array {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        let num_args = args.len();
        let values = args
            .into_iter()
            .map(|item| item.reduce().unwrap_or_else(|| Expression::clone(&item)))
            .fold(Ok(Vec::with_capacity(num_args)), |result, arg| {
                match arg.value() {
                    Term::Value(value) => match result {
                        Ok(mut args) => {
                            args.push(value.clone());
                            Ok(args)
                        }
                        _ => result,
                    },
                    _ => match result {
                        Ok(_) => Err(vec![arg]),
                        Err(mut invalid_args) => {
                            invalid_args.push(arg);
                            Err(invalid_args)
                        }
                    },
                }
            });
        match values {
            Ok(values) => Expression::new(Term::Value(ValueTerm::Array(ArrayValue::new(values)))),
            Err(args) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Invalid array values: {}",
                    args.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(",")
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
            LambdaTerm, StaticVariableTerm, Term, VariableTerm,
        },
        stdlib::{
            builtin::BuiltinTerm,
            value::{ArrayValue, ValueTerm},
        },
    };

    #[test]
    fn array_expressions() {
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Array)),
            vec![],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![])
                )))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Array)),
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3),
                        ValueTerm::Int(4),
                        ValueTerm::Int(5),
                    ]),
                )))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Array)),
            vec![
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                    ],
                ))),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                    ],
                ))),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Add)),
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                    ],
                ))),
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3 + 1),
                        ValueTerm::Int(4 + 1),
                        ValueTerm::Int(5 + 1),
                    ]),
                )))),
                DependencyList::empty(),
            )
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::Builtin(BuiltinTerm::Array)),
                    vec![Expression::new(Term::Variable(VariableTerm::Static(
                        StaticVariableTerm::new(0),
                    )))],
                ))),
            ))),
            vec![Expression::new(Term::Value(ValueTerm::Int(3)))],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![ValueTerm::Int(3)])
                )))),
                DependencyList::empty(),
            )
        );
    }
}
