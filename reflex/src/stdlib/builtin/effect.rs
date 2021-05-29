// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, SerializedTerm, Signal, SignalTerm, Term, VarArgs, VariableTerm},
    serialize,
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Effect {}
impl BuiltinFunction for Effect {
    fn arity() -> Arity {
        Arity::from(1, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() < 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 1 or more arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let signal_type = args.next().unwrap();
        match signal_type.value() {
            Term::Value(ValueTerm::String(signal_type)) => {
                let args = args
                    .into_iter()
                    .map(|arg| match serialize(arg.value()) {
                        Ok(value) => Ok(value),
                        Err(error) => Err(format!("Invalid signal argument: {}", error)),
                    })
                    .collect::<Result<Vec<_>, String>>();
                match args {
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![SerializedTerm::string(error)],
                    )))),
                    Ok(args) => {
                        let signal =
                            Signal::new(SignalType::Custom(String::from(signal_type)), args);
                        Expression::new(Term::Variable(VariableTerm::dynamic(
                            signal.id(),
                            Expression::new(Term::Signal(SignalTerm::new(signal))),
                        )))
                    }
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Invalid signal type: Expected String, received {}",
                    signal_type,
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
            ApplicationTerm, DependencyList, DynamicDependencies, DynamicState, EvaluationResult,
            Expression, Signal, SignalTerm, Term,
        },
        serialize::SerializedTerm,
        stdlib::{
            builtin::BuiltinTerm,
            signal::SignalType,
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn effect_expressions() {
        let mut cache = GenerationalGc::new();
        let mut state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Effect)),
            vec![
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("fetch")))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                    "http://example.com/",
                )))),
            ],
        )));
        let signal = Signal::new(
            SignalType::Custom(String::from("fetch")),
            vec![SerializedTerm::string(String::from("http://example.com/"))],
        );
        let signal_hash = signal.id();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(signal))),
                DependencyList::of(DynamicDependencies::of(signal_hash))
            )
        );
        state.set(
            signal_hash,
            Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
        );
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                DependencyList::of(DynamicDependencies::of(signal_hash))
            )
        );
    }
}
