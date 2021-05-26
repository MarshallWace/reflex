// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{builtin::BuiltinFunction, collection::CollectionTerm, signal::SignalType},
};

pub struct Insert {}
impl BuiltinFunction for Insert {
    fn arity() -> Arity {
        Arity::from(2, 1, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 3 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 3 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        let value = args.next().unwrap();
        match target.value() {
            Term::Collection(CollectionTerm::HashMap(existing)) => match existing.set(key, value) {
                Some(updated) => updated,
                None => target,
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Invalid field update: Expected (HashMap, <any>, <any>), received ({}, {})",
                    target, key,
                ))],
            )))),
        }
    }
}
