// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, StructTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Cons {}
impl BuiltinFunction for Cons {
    fn arity() -> Arity {
        Arity::from(0, 2, None)
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
        let enum_variant = Expression::new(Term::Value(ValueTerm::Int(1)));
        let head = args.next().unwrap();
        let tail = args.next().unwrap();
        Expression::new(Term::Struct(StructTerm::new(
            None,
            vec![enum_variant, head, tail],
        )))
    }
}

pub(crate) fn match_cons_cell(target: &Expression) -> Option<(&Expression, &Expression)> {
    match_triple(target).and_then(|(enum_discriminant, head, tail)| {
        match enum_discriminant.value() {
            Term::Value(ValueTerm::Int(value)) if *value == 1 => Some((head, tail)),
            _ => None,
        }
    })
}

fn match_triple(target: &Expression) -> Option<(&Expression, &Expression, &Expression)> {
    match target.value() {
        Term::Struct(target) if target.prototype().is_none() && target.fields().len() == 3 => {
            target.get(0).and_then(|first| {
                target
                    .get(1)
                    .and_then(|second| target.get(2).map(|third| (first, second, third)))
            })
        }
        _ => None,
    }
}
