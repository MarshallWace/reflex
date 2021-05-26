// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, NativeFunction, SerializedTerm, Signal, SignalTerm,
        Term,
    },
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::{hashset::HashSetTerm, CollectionTerm},
        signal::SignalType,
        value::StringValue,
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
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Ok(entries),
            _ => Err(format!("Invalid Set constructor: {}", entries)),
        };
        match entries {
            Ok(entries) => {
                let entries = entries.iterate().into_iter().collect::<Vec<_>>();
                let has_dynamic_keys = entries.iter().any(|entry| match entry.value() {
                    Term::Value(_) => false,
                    _ => true,
                });
                match has_dynamic_keys {
                    false => HashSetTerm::collect(entries),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        dynamic_set_constructor(),
                        vec![Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectArgs)),
                            entries,
                        )))],
                    ))),
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(error)],
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
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let entries = args.next().unwrap();
        let entries = match entries.value() {
            Term::Collection(CollectionTerm::Vector(entries)) => Some(entries),
            _ => None,
        };
        match entries {
            Some(entries) => Expression::new(Term::Collection(CollectionTerm::HashSet(
                HashSetTerm::new(entries.iterate().into_iter()),
            ))),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(StringValue::from(
                    "Invalid Set constructor values",
                ))],
            )))),
        }
    }
}
