// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Arity, DataStructureTerm, EnumTerm, Expression, NativeFunction, Signal, SignalTerm, Term,
    },
    stdlib::{signal::SignalType, value::ValueTerm},
};

pub struct Cons {}
impl NativeFunction for Cons {
    fn arity() -> Arity {
        Arity::from(0, 2, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let head = args.next().unwrap();
        let tail = args.next().unwrap();
        Expression::new(Term::DataStructure(DataStructureTerm::Enum(EnumTerm::new(
            1,
            vec![head, tail],
        ))))
    }
}
