// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction,
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::ValueTerm,
    },
};

pub struct Keys {}
impl BuiltinFunction for Keys {
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
        let target = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(collection) => match collection {
                CollectionTerm::HashMap(target) => {
                    Some(Expression::new(Term::Collection(CollectionTerm::Vector(
                        VectorTerm::new(target.keys().into_iter().map(Expression::clone)),
                    ))))
                }
                CollectionTerm::HashSet(_) => None,
                CollectionTerm::Vector(target) => Some(Expression::new(Term::Collection(
                    CollectionTerm::Vector(VectorTerm::new(target.iterate().enumerate().map(
                        |(index, _)| Expression::new(Term::Value(ValueTerm::Int(index as i32))),
                    ))),
                ))),
            },
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Unable to enumerate keys for {}",
                    target
                ))))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::SubstitutionCache,
        core::{ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression, Term},
        stdlib::{
            builtin::BuiltinTerm,
            collection::{hashmap::HashMapTerm, vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    #[test]
    fn get_hashmap_keys() {
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Keys)),
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
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                    ]
                ),))),
                DependencyList::empty(),
            ),
        );
    }
}
