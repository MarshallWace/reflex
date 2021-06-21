// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, StructPrototype, StructTerm, Term},
    stdlib::{
        builtin::BuiltinFunction, collection::CollectionTerm, signal::SignalType, value::ValueTerm,
    },
};

pub struct Struct {}
impl BuiltinFunction for Struct {
    fn arity() -> Arity {
        Arity::from(2, 0, None)
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
        let keys = args.next().unwrap();
        let values = args.next().unwrap();
        let keys = match get_list_values(&keys) {
            Err(None) => Err(format!("Invalid property keys: {}", keys)),
            Err(Some(target)) => Err(format!("Invalid property key: {}", target)),
            Ok(keys) => Ok(keys),
        };
        let result = match keys {
            Err(error) => Err(error),
            Ok(keys) => match get_list_items(&values) {
                None => Err(format!("Invalid property values: {}", values)),
                Some(values) => {
                    if keys.len() != values.len() {
                        Err(format!(
                            "Invalid property entries: received {} keys and {} values",
                            keys.len(),
                            values.len(),
                        ))
                    } else {
                        Ok(Expression::new(Term::Struct(StructTerm::new(
                            Some(StructPrototype::new(keys)),
                            values,
                        ))))
                    }
                }
            },
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

fn get_list_items(target: &Expression) -> Option<Vec<Expression>> {
    match target.value() {
        Term::Struct(target) if target.prototype().is_none() => Some(
            target
                .fields()
                .iter()
                .map(Expression::clone)
                .collect::<Vec<_>>(),
        ),
        Term::Collection(CollectionTerm::Vector(target)) => Some(
            target
                .items()
                .iter()
                .map(Expression::clone)
                .collect::<Vec<_>>(),
        ),
        _ => None,
    }
}

fn get_list_values(target: &Expression) -> Result<Vec<ValueTerm>, Option<&Expression>> {
    match target.value() {
        Term::Struct(target) if target.prototype().is_none() => target
            .fields()
            .iter()
            .map(|item| get_value_expression(item).ok_or_else(|| Some(item)))
            .collect::<Result<Vec<_>, _>>(),
        Term::Collection(CollectionTerm::Vector(target)) => target
            .items()
            .iter()
            .map(|item| get_value_expression(item).ok_or_else(|| Some(item)))
            .collect::<Result<Vec<_>, _>>(),
        _ => Err(None),
    }
}

fn get_value_expression(target: &Expression) -> Option<ValueTerm> {
    match target.value() {
        Term::Value(value) => Some(value.clone()),
        _ => None,
    }
}
