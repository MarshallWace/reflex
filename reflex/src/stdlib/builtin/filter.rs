// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{ApplicationTerm, Arity, Expression, Signal, SignalTerm, StructTerm, Term},
    stdlib::{
        builtin::{r#if::is_truthy, BuiltinFunction, BuiltinTerm},
        collection::CollectionTerm,
        signal::SignalType,
        value::ValueTerm,
    },
};

pub struct Filter {}
impl BuiltinFunction for Filter {
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
        let predicate = args.next().unwrap();
        let result = match target.value() {
            Term::Collection(collection) => match collection {
                CollectionTerm::Vector(target) => Ok(collect_filter_results(
                    target.iterate(),
                    &predicate,
                    BuiltinTerm::CollectVector,
                )),
                CollectionTerm::HashMap(target) => Ok(collect_filter_results(
                    target.iterate(),
                    &predicate,
                    BuiltinTerm::CollectHashMap,
                )),
                CollectionTerm::HashSet(target) => Ok(collect_filter_results(
                    target.iterate(),
                    &predicate,
                    BuiltinTerm::CollectHashSet,
                )),
            },
            _ => Err(format!(
                "Expected (<iterable>, <function:1>), received ({}, {})",
                target, predicate,
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

fn collect_filter_results(
    items: impl IntoIterator<Item = Expression>,
    predicate: &Expression,
    collect: BuiltinTerm,
) -> Expression {
    let (items, results): (Vec<_>, Vec<_>) = items
        .into_iter()
        .map(|item| {
            (
                Expression::clone(&item),
                Expression::new(Term::Application(ApplicationTerm::new(
                    Expression::clone(&predicate),
                    vec![item],
                ))),
            )
        })
        .unzip();
    Expression::new(Term::Application(ApplicationTerm::new(
        Expression::new(Term::Builtin(BuiltinTerm::CollectFilterResults)),
        vec![
            Expression::new(Term::Struct(StructTerm::new(None, items))),
            Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::CollectTuple)),
                results,
            ))),
            Expression::new(Term::Builtin(collect)),
        ],
    )))
}

pub(crate) struct CollectFilterResults {}
impl BuiltinFunction for CollectFilterResults {
    fn arity() -> Arity {
        Arity::from(3, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        let mut args = args.into_iter();
        let items = args.next().unwrap();
        let results = args.next().unwrap();
        let combine = args.next().unwrap();
        let result = match (items.value(), results.value()) {
            (Term::Struct(items), Term::Struct(results)) if items.prototype().is_none() && results.prototype().is_none() => {
                Ok(Expression::new(Term::Application(ApplicationTerm::new(
                    combine,
                    items.fields().iter().zip(results.fields().iter()).filter_map(|(item, result)| if is_truthy(result) {
                        Some(Expression::clone(item))
                    } else {
                        None
                    }).collect()
                ))))
            },
            _ => Err(format!("Invalid filter combiner arguments: Expected (<struct>, <struct>, <function>), received ({} {} {})",
            items, results, combine))
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
    fn filter_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let collection = Expression::new(Term::Collection(CollectionTerm::Vector(
            VectorTerm::new(vec![
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
            ]),
        )));
        let predicate = parse("(lambda (value) (= (remainder value 2) 0))").unwrap();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Collect)),
            vec![Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Filter)),
                vec![collection, predicate],
            )))],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3 + 1))),
                        Expression::new(Term::Value(ValueTerm::Int(5 + 1))),
                    ]
                ),))),
                DependencyList::empty(),
            )
        );
    }
}
