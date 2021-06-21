// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction, collection::CollectionTerm, signal::SignalType, value::ValueTerm,
    },
};

pub struct PushFront {}
impl BuiltinFunction for PushFront {
    fn arity() -> Arity {
        Arity::from(1, 1, None)
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
        let value = args.next().unwrap();
        match target.value() {
            Term::Collection(CollectionTerm::Vector(existing)) => existing.push_front(value),
            Term::Collection(CollectionTerm::HashSet(existing)) => match existing.add(value) {
                Some(updated) => updated,
                None => target,
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Invalid push operation: Expected (Vector, <any>) or (HashSet, <any>), received ({}, {})",
                    target, value
                ))))],
            )))),
        }
    }
}
