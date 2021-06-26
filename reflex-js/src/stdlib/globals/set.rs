// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{ApplicationTerm, Arity, Expression, NativeFunction, Signal, SignalTerm, Term},
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{hashset::HashSetTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub(crate) fn global_set() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        SetConstructor::hash(),
        SetConstructor::arity(),
        SetConstructor::apply,
    )))
}
struct SetConstructor {}
impl SetConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        let values = match values.value() {
            Term::Collection(CollectionTerm::Vector(values)) => Ok(values),
            _ => Err(format!("Invalid Set constructor: {}", values)),
        };
        match values {
            Ok(values) => {
                let values = values.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_values = values.iter().any(|value| !value.is_static());
                match has_dynamic_values {
                    false => HashSetTerm::collect(values),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        dynamic_set_constructor(),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectTuple)),
                            values,
                        )))],
                    ))),
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

fn dynamic_set_constructor() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        DynamicSetConstructor::hash(),
        DynamicSetConstructor::arity(),
        DynamicSetConstructor::apply,
    )))
}
struct DynamicSetConstructor {}
impl DynamicSetConstructor {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let values = args.next().unwrap();
        let values = match values.value() {
            Term::Struct(values) if values.prototype().is_none() => Some(values.fields()),
            _ => None,
        };
        match values {
            Some(values) => Expression::new(Term::Collection(CollectionTerm::HashSet(
                HashSetTerm::new(values.iter().map(Expression::clone)),
            ))),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from("Invalid Set constructor values"),
                )))],
            )))),
        }
    }
}
