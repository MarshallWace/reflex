// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, StructTerm, Term, VarArgs},
    serialize::SerializedTerm,
    stdlib::{
        builtin::{BuiltinFunction, BuiltinTerm},
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
    },
};

pub struct Collect {}
impl BuiltinFunction for Collect {
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
        match target.value() {
            Term::Collection(collection) => {
                let items: Vec<Expression> = match collection {
                    CollectionTerm::Vector(target) => target.iterate().into_iter().collect(),
                    CollectionTerm::HashMap(target) => target.iterate().into_iter().collect(),
                    CollectionTerm::HashSet(target) => target.iterate().into_iter().collect(),
                };
                let has_dynamic_items = items.iter().any(|item| match item.value() {
                    Term::Value(_) => false,
                    _ => true,
                });
                match has_dynamic_items {
                    false => Expression::clone(&target),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::CollectArgs)),
                        items,
                    ))),
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected <collection>, received {}",
                    target,
                ))],
            )))),
        }
    }
}

pub struct CollectArgs {}
impl BuiltinFunction for CollectArgs {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            args.into_iter(),
        ))))
    }
}

pub struct CollectTuple {}
impl BuiltinFunction for CollectTuple {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        Expression::new(Term::Struct(StructTerm::new(
            None,
            args.into_iter().collect(),
        )))
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
    fn collect_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let collection = Expression::new(Term::Collection(CollectionTerm::Vector(
            VectorTerm::new(vec![
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
            ]),
        )));
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Collect)),
            vec![collection],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(
                    VectorTerm::new(vec![
                        Expression::new(Term::Value(ValueTerm::Int(3 + 1))),
                        Expression::new(Term::Value(ValueTerm::Int(4 + 1))),
                        Expression::new(Term::Value(ValueTerm::Int(5 + 1))),
                    ]),
                ))),
                DependencyList::empty(),
            )
        );
    }
}
