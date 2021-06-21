// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap};

use crate::{
    core::{Arity, Expression, Signal, SignalTerm, StructPrototype, StructTerm, Term, VarArgs},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Merge {}
impl BuiltinFunction for Merge {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        let args = args.into_iter();
        let fields = args.fold(Ok(Vec::new()), |result, arg| {
            let mut combined_properties = result?;
            let properties = match arg.value() {
                Term::Struct(value) => match value.prototype() {
                    Some(prototype) => Some(
                        prototype
                            .keys()
                            .iter()
                            .map(|key| key.clone())
                            .zip(value.fields().iter().map(Expression::clone)),
                    ),
                    _ => None,
                },
                _ => None,
            };
            match properties {
                Some(properties) => {
                    combined_properties.extend(properties);
                    Ok(combined_properties)
                }
                None => Err(format!("Expected <struct>..., received {}", arg,)),
            }
        });
        match fields {
            Ok(fields) => {
                let (keys, values): (Vec<_>, Vec<_>) = fields.into_iter().unzip();
                let mut deduplicated_keys = keys.iter().enumerate().fold(
                    HashMap::with_capacity(keys.len()),
                    |mut result, (index, key)| {
                        match result.entry(key) {
                            Entry::Vacant(entry) => {
                                entry.insert(index);
                            }
                            Entry::Occupied(mut entry) => {
                                entry.insert(index);
                            }
                        }
                        result
                    },
                );
                if keys.len() > deduplicated_keys.len() {
                    let (keys, values) = keys
                        .iter()
                        .fold(
                            Vec::with_capacity(deduplicated_keys.len()),
                            |mut results, key| match deduplicated_keys.remove(key) {
                                Some(index) => {
                                    let value = Expression::clone(values.get(index).unwrap());
                                    results.push((key.clone(), value));
                                    results
                                }
                                None => results,
                            },
                        )
                        .into_iter()
                        .unzip();
                    Expression::new(Term::Struct(StructTerm::new(
                        Some(StructPrototype::new(keys)),
                        values,
                    )))
                } else {
                    Expression::new(Term::Struct(StructTerm::new(
                        Some(StructPrototype::new(keys)),
                        values,
                    )))
                }
            }
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}
