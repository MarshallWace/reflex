// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{
        builtin::BuiltinFunction,
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
    },
};

pub struct Values {}
impl BuiltinFunction for Values {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(collection) => match collection {
                CollectionTerm::HashMap(target) => {
                    Some(Expression::new(Term::Collection(CollectionTerm::Vector(
                        VectorTerm::new(target.values().iter().map(Expression::clone)),
                    ))))
                }
                CollectionTerm::HashSet(target) => Some(Expression::new(Term::Collection(
                    CollectionTerm::Vector(VectorTerm::new(target.iterate())),
                ))),
                CollectionTerm::Vector(_) => Some(Expression::clone(&target)),
            },
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Unable to enumerate values for {}",
                    target
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
        stdlib::{
            builtin::BuiltinTerm,
            collection::{hashmap::HashMapTerm, vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn get_hashmap_values() {
        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Values)),
            vec![Expression::new(Term::Collection(CollectionTerm::HashMap(
                HashMapTerm::new(vec![
                    (
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                    ),
                    (
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                    ),
                    (
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ),
                ]),
            )))],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Collection(CollectionTerm::Vector(
                    VectorTerm::new(vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ]),
                )))),
                DependencyList::empty(),
            ),
        );
    }
}
