// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{any::TypeId, iter::once};

use reflex::{
    core::{ApplicationTerm, Arity, Expression, NativeFunction, Signal, SignalTerm, Term, VarArgs},
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm,
        collection::CollectionTerm,
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub(crate) fn throw() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        Throw::hash(),
        Throw::arity(),
        Throw::apply,
    )))
}
struct Throw {}
impl Throw {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let message = args.next().unwrap();
        let message = match message.value() {
            Term::Value(message) => ValueTerm::String(format_value(message)),
            _ => ValueTerm::String(StringValue::from("Unknown error")),
        };
        Expression::new(Term::Signal(SignalTerm::new(Signal::new(
            SignalType::Error,
            vec![message],
        ))))
    }
}

pub(crate) fn construct() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        Construct::hash(),
        Construct::arity(),
        Construct::apply,
    )))
}

struct Construct {}
impl Construct {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, Some(VarArgs::Lazy))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let constructor = args.next().unwrap();
        match constructor.value() {
            Term::StructConstructor(eager, prototype) => {
                let properties = args.next();
                let result = match properties {
                    Some(properties) => match properties.value() {
                        Term::Struct(properties) => match properties.prototype() {
                            Some(properties_layout) => {
                                let keys = properties_layout.keys().iter();
                                let values = properties.fields().iter().map(Expression::clone);
                                let has_unresolved_fields = match eager {
                                    VarArgs::Lazy => false,
                                    VarArgs::Eager => properties
                                        .fields()
                                        .iter()
                                        .any(|property| property.is_reducible()),
                                };
                                if has_unresolved_fields {
                                    Ok(Expression::new(Term::Application(ApplicationTerm::new(
                                        constructor,
                                        keys.map(|key| Expression::new(Term::Value(key.clone())))
                                            .chain(values)
                                            .collect(),
                                    ))))
                                } else {
                                    let entries = prototype.apply(keys.zip(values));
                                    match entries {
                                        Some(entries) => Ok(Expression::new(Term::Struct(entries))),
                                        _ => Err(format!("{}", constructor)),
                                    }
                                }
                            }
                            _ => Err(format!("{}", constructor)),
                        },
                        _ => Err(format!("{}", constructor)),
                    },
                    _ => Err(format!("{}", constructor)),
                };
                match result {
                    Ok(result) => result,
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![ValueTerm::String(format!(
                            "Invalid constructor call: {}",
                            error,
                        ))],
                    )))),
                }
            }
            _ => Expression::new(Term::Application(ApplicationTerm::new(
                constructor,
                args.collect(),
            ))),
        }
    }
}

pub(crate) fn dispatch() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        Dispatch::hash(),
        Dispatch::arity(),
        Dispatch::apply,
    )))
}
struct Dispatch {}
impl Dispatch {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(2, 1, Some(VarArgs::Lazy))
    }
    fn apply(args: Vec<Expression>) -> Expression {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let method_name = args.next().unwrap();
        let fallback = args.next().unwrap();
        let builtin_method = match method_name.value() {
            Term::Value(ValueTerm::String(method_name)) => {
                get_builtin_field(Some(target.value()), method_name)
            }
            _ => None,
        };
        match builtin_method {
            Some(method) => Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(method),
                once(target).chain(args).collect(),
            ))),
            None => fallback,
        }
    }
}

pub(crate) fn to_string() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        ToString::hash(),
        ToString::arity(),
        ToString::apply,
    )))
}
struct ToString {}
impl ToString {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let operand = args.next().unwrap();
        match operand.value() {
            Term::Value(ValueTerm::String(_)) => operand,
            Term::Value(value) => Expression::new(Term::Value(ValueTerm::String(String::from(
                format_value(value),
            )))),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected printable value, received {}",
                    operand,
                ))],
            )))),
        }
    }
}

pub(crate) fn flatten_struct() -> Expression {
    Expression::new(Term::Native(NativeFunction::new(
        FlattenStruct::hash(),
        FlattenStruct::arity(),
        FlattenStruct::apply,
    )))
}
struct FlattenStruct {}
impl FlattenStruct {
    fn hash() -> HashId {
        hash_object(TypeId::of::<Self>())
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: Vec<Expression>) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let input = args.next().unwrap();
        let result = match input.value() {
            Term::Struct(input) => match input.prototype() {
                Some(prototype) => Ok(Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::new(Term::StructConstructor(VarArgs::Eager, prototype.clone())),
                    prototype
                        .keys()
                        .iter()
                        .map(|key| Expression::new(Term::Value(key.clone())))
                        .chain(input.fields().iter().map(Expression::clone))
                        .collect(),
                )))),
                None => Err(format!(
                    "Expected named struct fields, received anonymous struct {}",
                    input,
                )),
            },
            _ => Err(format!("Expected <struct>, received {}", input)),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(error)],
            )))),
        }
    }
}

pub(crate) fn get_builtin_field<'src>(target: Option<&Term>, method: &str) -> Option<Term> {
    None.or_else(|| match target {
        None | Some(Term::Collection(CollectionTerm::Vector(_))) => match method {
            "entries" => Some(Term::Builtin(BuiltinTerm::Entries)),
            "keys" => Some(Term::Builtin(BuiltinTerm::Keys)),
            "map" => Some(Term::Builtin(BuiltinTerm::Map)),
            "push" => Some(Term::Builtin(BuiltinTerm::Append)),
            "values" => Some(Term::Builtin(BuiltinTerm::Values)),
            _ => None,
        },
        _ => None,
    })
    .or_else(|| match target {
        None | Some(Term::Collection(CollectionTerm::HashMap(_))) => match method {
            "entries" => Some(Term::Builtin(BuiltinTerm::Entries)),
            "get" => Some(Term::Builtin(BuiltinTerm::Get)),
            "keys" => Some(Term::Builtin(BuiltinTerm::Keys)),
            "set" => Some(Term::Builtin(BuiltinTerm::Insert)),
            "values" => Some(Term::Builtin(BuiltinTerm::Values)),
            _ => None,
        },
        _ => None,
    })
    .or_else(|| match target {
        None | Some(Term::Collection(CollectionTerm::HashSet(_))) => match method {
            "entries" => Some(Term::Builtin(BuiltinTerm::Entries)),
            "get" => Some(Term::Builtin(BuiltinTerm::Get)),
            "add" => Some(Term::Builtin(BuiltinTerm::Append)),
            "values" => Some(Term::Builtin(BuiltinTerm::Values)),
            _ => None,
        },
        _ => None,
    })
}

pub(crate) fn format_value(value: &ValueTerm) -> String {
    match value {
        ValueTerm::Symbol(_) => format!("{}", value),
        ValueTerm::Null => String::from("null"),
        ValueTerm::Boolean(value) => format!("{}", value),
        ValueTerm::Int(value) => format!("{}", value),
        ValueTerm::Float(value) => format!("{}", value),
        ValueTerm::String(value) => StringValue::from(value),
        ValueTerm::Array(value) => format!(
            "[{}]",
            value
                .iter()
                .map(|value| format!("{}", value))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}
