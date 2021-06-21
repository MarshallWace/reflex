// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{Arity, Expression, NativeFunction, Signal, SignalTerm, Term},
    hash::{hash_object, HashId},
    stdlib::{signal::SignalType, value::ValueTerm},
};

use crate::builtins::to_string;

pub(crate) fn global_string() -> Expression {
    to_string()
}

pub(crate) fn global_encode_uri_component() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        EncodeUriComponent::hash(),
        EncodeUriComponent::arity(),
        EncodeUriComponent::apply,
    )))
}
struct EncodeUriComponent {}
impl EncodeUriComponent {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let input = args.next().unwrap();
        let result = match input.value() {
            Term::Value(ValueTerm::String(value)) => {
                let value = urlencoding::encode(value);
                Ok(Expression::new(Term::Value(ValueTerm::String(value))))
            }
            _ => Err(format!("Expected String, received {}", input)),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}
