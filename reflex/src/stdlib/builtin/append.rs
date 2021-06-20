// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term, VarArgs},
    serialize::SerializedTerm,
    stdlib::{
        builtin::BuiltinFunction,
        collection::{hashset::HashSetTerm, vector::VectorTerm, CollectionTerm},
        signal::SignalType,
    },
};

pub struct Append {}
impl BuiltinFunction for Append {
    fn arity() -> Arity {
        Arity::from(2, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() < 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 2 or more arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(CollectionTerm::Vector(target)) => {
                let items = args.fold(Ok(target.iterate().into_iter().collect::<Vec<Expression>>()), |results, arg| {
                    let mut results = results?;
                    match arg.value() {
                        Term::Collection(CollectionTerm::Vector(value)) => {
                            results.extend(value.iterate());
                            Ok(results)
                        },
                        _ => Err(format!("Invalid append argument: Expected Vector, received {}", arg))
                    }
                });
                match items {
                    Err(error) => Err(error),
                    Ok(items) => Ok(Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                        items
                    )))))
                }
            },
            Term::Collection(CollectionTerm::HashSet(target)) => {
                let items = args.fold(Ok(target.iterate().into_iter().collect::<Vec<Expression>>()), |results, arg| {
                    let mut results = results?;
                    match arg.value() {
                        Term::Collection(CollectionTerm::HashSet(value)) => {
                            results.extend(value.iterate());
                            Ok(results)
                        },
                        _ => Err(format!("Invalid append argument: Expected HashSet, received {}", arg))
                    }
                });
                match items {
                    Err(error) => Err(error),
                    Ok(items) => Ok(Expression::new(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
                        items
                    )))))
                }
            },
            _ => Err(format!(
                    "Invalid append operation: Expected (Vector, ...) or (HashSet, ...), received ({}, {})",
                    target, args.map(|arg| format!("{}", arg)).collect::<Vec<_>>().join(", ")
                )),
        };
        match result {
            Ok(result) => result,
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(error)],
            )))),
        }
    }
}
