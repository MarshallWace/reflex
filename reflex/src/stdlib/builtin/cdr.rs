// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{builtin::BuiltinFunction, signal::SignalType},
};

pub struct Cdr {}
impl BuiltinFunction for Cdr {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 1 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        match target.value() {
            Term::Enum(target) if target.index() == 1 && target.args().len() == 2 => {
                Expression::clone(target.args().get(1).unwrap())
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected list, received {}",
                    target,
                ))],
            )))),
        }
    }
}
