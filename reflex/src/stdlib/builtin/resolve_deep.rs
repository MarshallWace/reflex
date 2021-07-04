// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term, VarArgs},
    stdlib::{
        builtin::{BuiltinFunction, BuiltinTerm},
        collection::CollectionTerm,
        signal::SignalType,
        value::ValueTerm,
    },
};

pub struct ResolveDeep {}
impl BuiltinFunction for ResolveDeep {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let input = args.next().unwrap();
        match input.value() {
            Term::Struct(value) => {
                let has_nested_fields = value.fields().iter().any(is_nested_structure);
                if !has_nested_fields {
                    input
                } else {
                    match value.prototype() {
                        Some(prototype) => {
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::new(Term::StructConstructor(
                                    VarArgs::Eager,
                                    prototype.clone(),
                                )),
                                prototype
                                    .keys()
                                    .iter()
                                    .map(|key| Expression::new(Term::Value(key.clone())))
                                    .chain(value.fields().iter().map(|field| {
                                        if is_nested_structure(field) {
                                            Expression::new(Term::Application(
                                                ApplicationTerm::new(
                                                    Expression::new(Term::Builtin(
                                                        BuiltinTerm::ResolveDeep,
                                                    )),
                                                    vec![Expression::clone(field)],
                                                ),
                                            ))
                                        } else {
                                            Expression::clone(field)
                                        }
                                    }))
                                    .collect(),
                            )))
                        }
                        None => Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectTuple)),
                            value
                                .fields()
                                .iter()
                                .map(|field| {
                                    if is_nested_structure(field) {
                                        Expression::new(Term::Application(ApplicationTerm::new(
                                            Expression::new(Term::Builtin(
                                                BuiltinTerm::ResolveDeep,
                                            )),
                                            vec![Expression::clone(field)],
                                        )))
                                    } else {
                                        Expression::clone(field)
                                    }
                                })
                                .collect(),
                        ))),
                    }
                }
            }
            Term::Collection(value) => match value {
                // TODO: Flatten HashMap collection type
                // TODO: Flatten HashSet collection type
                CollectionTerm::Vector(value) => {
                    let has_nested_fields = value.items().iter().any(is_nested_structure);
                    if !has_nested_fields {
                        input
                    } else {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::new(Term::Builtin(BuiltinTerm::CollectVector)),
                            value
                                .items()
                                .iter()
                                .map(|item| {
                                    if is_nested_structure(item) {
                                        Expression::new(Term::Application(ApplicationTerm::new(
                                            Expression::new(Term::Builtin(
                                                BuiltinTerm::ResolveDeep,
                                            )),
                                            vec![Expression::clone(item)],
                                        )))
                                    } else {
                                        Expression::clone(item)
                                    }
                                })
                                .collect(),
                        )))
                    }
                }
                _ => input,
            },
            _ => input,
        }
    }
}

fn is_nested_structure(value: &Expression) -> bool {
    match value.value() {
        Term::Struct(term) if !term.fields().is_empty() => true,
        Term::Enum(term) if !term.args().is_empty() => true,
        Term::Collection(collection) if !collection.is_empty() => true,
        _ => !value.is_static(),
    }
}
