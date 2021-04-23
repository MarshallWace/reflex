// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Not {}
impl BuiltinFunction for Not {
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
            Term::Value(ValueTerm::Boolean(operand)) => {
                Expression::new(Term::Value(ValueTerm::Boolean(!operand)))
            }
            Term::Value(ValueTerm::Null) => Expression::new(Term::Value(ValueTerm::Boolean(true))),
            _ => Expression::new(Term::Value(ValueTerm::Boolean(false))),
        }
    }
}
