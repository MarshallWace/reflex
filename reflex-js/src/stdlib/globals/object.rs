// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, NativeFunction, Signal, SignalTerm, StructPrototype,
        StructTerm, Term,
    },
    hash::{hash_object, HashId},
    serialize::SerializedTerm,
    stdlib::{
        builtin::BuiltinTerm,
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::ValueTerm,
    },
};

use super::create_struct;

pub(crate) fn global_object() -> Expression {
    create_struct(vec![(String::from("fromEntries"), from_entries())])
}

fn from_entries() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        FromEntries::hash(),
        FromEntries::arity(),
        FromEntries::apply,
    )))
}
struct FromEntries {}
impl FromEntries {
    fn hash() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let result = match target.value() {
            Term::Struct(entries) if entries.prototype().is_some() => Ok(target),
            Term::Collection(entries) => match entries {
                CollectionTerm::Vector(entries) => {
                    let keys = entries
                        .items()
                        .iter()
                        .map(|entry| get_indexed_field(entry, 0))
                        .collect::<Vec<_>>();
                    let values = entries
                        .items()
                        .iter()
                        .map(|entry| get_indexed_field(entry, 1))
                        .collect::<Vec<_>>();
                    Ok(match get_static_list_items(keys.iter()) {
                        Some(keys) => Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(keys.into_iter().cloned().collect())),
                            values,
                        ))),
                        None => Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::Struct)),
                            vec![
                                Expression::new(Term::Application(ApplicationTerm::new(
                                    Expression::new(Term::Builtin(BuiltinTerm::CollectArgs)),
                                    keys,
                                ))),
                                Expression::new(Term::Collection(CollectionTerm::Vector(
                                    VectorTerm::new(values),
                                ))),
                            ],
                        ))),
                    })
                }
                _ => Err(format!("{}", entries)),
            },
            _ => Err(format!("{}", target)),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected list, received {}",
                    error
                ))],
            )))),
        }
    }
}

fn get_static_list_items<'a>(
    items: impl IntoIterator<Item = &'a Expression>,
) -> Option<Vec<&'a ValueTerm>> {
    items
        .into_iter()
        .map(|item| match item.value() {
            Term::Value(value) => Some(value),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()
}

fn get_indexed_field(target: &Expression, index: usize) -> Expression {
    match target.value() {
        Term::Struct(target) if target.prototype().is_none() && target.fields().len() > index => {
            Expression::clone(target.get(index).unwrap())
        }
        Term::Collection(CollectionTerm::Vector(target)) if target.len() > index => {
            Expression::clone(target.get(index).unwrap())
        }
        _ => Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                Expression::clone(target),
                Expression::new(Term::Value(ValueTerm::Int(index as i32))),
            ],
        ))),
    }
}
