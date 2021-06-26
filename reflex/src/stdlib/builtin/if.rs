// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct If {}
impl BuiltinFunction for If {
    fn arity() -> Arity {
        Arity::from(1, 2, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 3 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 3 arguments, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let condition = args.next().unwrap();
        let consequent = args.next().unwrap();
        let alternate = args.next().unwrap();
        if is_truthy(&condition) {
            consequent
        } else {
            alternate
        }
    }
}

pub(crate) fn is_truthy(value: &Expression) -> bool {
    match value.value() {
        Term::Value(ValueTerm::Boolean(value)) => *value,
        Term::Value(ValueTerm::Null) => false,
        _ => true,
    }
}
