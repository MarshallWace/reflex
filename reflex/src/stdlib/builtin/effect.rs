// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term, VarArgs},
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
                vec![ValueTerm::String(format!(
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
                    .map(|arg| match arg.value() {
                        Term::Value(value) => Ok(value.clone()),
                        _ => Err(format!(
                            "Invalid signal argument: Expected <value>, received {}",
                            arg,
                        )),
                    })
                    .collect::<Result<Vec<_>, String>>();
                match args {
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(error)],
                    )))),
                    Ok(args) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Custom(String::from(signal_type)),
                        args,
                    )))),
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
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
        cache::EvaluationCache,
        core::{
            ApplicationTerm, DependencyList, DynamicDependencies, DynamicState, EvaluationResult,
            Expression, Signal, Term,
        },
        hash::Hashable,
        stdlib::{
            builtin::BuiltinTerm,
            signal::SignalType,
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn effect_expressions() {
        let mut cache = EvaluationCache::new();
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
            vec![ValueTerm::String(StringValue::from("http://example.com/"))],
        );
        let signal_hash = signal.hash();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Err(vec![signal]),
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
                Ok(Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("foo")
                )))),
                DependencyList::of(DynamicDependencies::of(signal_hash))
            )
        );
    }
}
