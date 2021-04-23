// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Ceil {}
impl BuiltinFunction for Ceil {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let operand = args.next().unwrap();
        match operand.value() {
            Term::Value(ValueTerm::Float(operand)) => {
                Expression::new(Term::Value(ValueTerm::Float(operand.ceil())))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected Float, received {}",
                    operand,
                ))],
            )))),
        }
    }
}
