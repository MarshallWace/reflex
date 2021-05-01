// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction, collection::vector::VectorTerm, collection::CollectionTerm,
        signal::SignalType, value::ValueTerm,
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
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let transform = args.next().unwrap();
        match target.value() {
            Term::Collection(collection) => match collection {
                CollectionTerm::Vector(target) => {
                    let transformed_values = target.iterate().into_iter().map(|item| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&transform),
                            vec![item],
                        )))
                    });
                    Expression::new(Term::Collection(CollectionTerm::Vector(
                        VectorTerm::collect(transformed_values),
                    )))
                }
                CollectionTerm::HashMap(target) => {
                    let transformed_values = target.iterate().into_iter().map(|item| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&transform),
                            vec![item],
                        )))
                    });
                    Expression::new(Term::Collection(CollectionTerm::Vector(
                        VectorTerm::collect(transformed_values),
                    )))
                }
                CollectionTerm::HashSet(target) => {
                    let transformed_values = target.iterate().into_iter().map(|item| {
                        Expression::new(Term::Application(ApplicationTerm::new(
                            Expression::clone(&transform),
                            vec![item],
                        )))
                    });
                    Expression::new(Term::Collection(CollectionTerm::Vector(
                        VectorTerm::collect(transformed_values),
                    )))
                }
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected (<iterable>, <function:1>), received ({}, {})",
                    target, transform,
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::EvaluationCache,
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
        let mut cache = EvaluationCache::new();
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
                Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                    VectorTerm::new(vec![
                        Expression::new(Term::Value(ValueTerm::Int(3 + 1 + 2))),
                        Expression::new(Term::Value(ValueTerm::Int(4 + 1 + 2))),
                        Expression::new(Term::Value(ValueTerm::Int(5 + 1 + 2))),
                    ]),
                )))),
                DependencyList::empty(),
            )
        );
    }
}
