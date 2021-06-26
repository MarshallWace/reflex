// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction,
        collection::CollectionTerm,
        collection::{hashmap::HashMapTerm, hashset::HashSetTerm, vector::VectorTerm},
        signal::SignalType,
        value::ValueTerm,
    },
};

pub struct Map {}
impl BuiltinFunction for Map {
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
        let target = args.next().unwrap();
        let transform = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(collection) => match collection {
                CollectionTerm::Vector(target) => Ok(VectorTerm::collect(
                    target.iterate().into_iter().map(|item| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&transform),
                            vec![item],
                        )))
                    }),
                )),
                CollectionTerm::HashMap(target) => Ok(HashMapTerm::collect(
                    target
                        .iterate()
                        .into_iter()
                        .map(|item| {
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::clone(&transform),
                                vec![item],
                            )))
                        })
                        .collect::<Vec<_>>(),
                )),
                CollectionTerm::HashSet(target) => Ok(HashSetTerm::collect(
                    target
                        .iterate()
                        .into_iter()
                        .map(|item| {
                            Expression::new(Term::Application(ApplicationTerm::new(
                                Expression::clone(&transform),
                                vec![item],
                            )))
                        })
                        .collect::<Vec<_>>(),
                )),
            },
            _ => Err(format!(
                "Expected (<iterable>, <function:1>), received ({}, {})",
                target, transform,
            )),
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

#[cfg(test)]
mod tests {
    use crate::{
        cache::GenerationalGc,
        core::{ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression, Term},
        parser::sexpr::parse,
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            value::ValueTerm,
        },
    };

    #[test]
    fn map_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let collection = Expression::new(Term::Collection(CollectionTerm::Vector(
            VectorTerm::new(vec![
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
            ]),
        )));
        let transform = parse("(lambda (value) (+ value 2))").unwrap();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Collect)),
            vec![Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Map)),
                vec![collection, transform],
            )))],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3 + 1 + 2))),
                        Expression::new(Term::Value(ValueTerm::Int(4 + 1 + 2))),
                        Expression::new(Term::Value(ValueTerm::Int(5 + 1 + 2))),
                    ]
                ),))),
                DependencyList::empty(),
            )
        );
    }
}
