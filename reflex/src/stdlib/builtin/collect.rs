// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        ApplicationTerm, Arity, Expression, Signal, SignalTerm, StructPrototype, StructTerm, Term,
        VarArgs,
    },
    stdlib::{
        builtin::{BuiltinFunction, BuiltinTerm},
        collection::{
            hashmap::HashMapTerm, hashset::HashSetTerm, vector::VectorTerm, CollectionTerm,
        },
        signal::SignalType,
        value::{StringValue, ValueTerm},
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
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        match target.value() {
            Term::Collection(collection) => {
                let (items, collect) = match collection {
                    CollectionTerm::HashMap(target) => (
                        target.iterate().into_iter().collect::<Vec<_>>(),
                        BuiltinTerm::CollectHashMap,
                    ),
                    CollectionTerm::HashSet(target) => (
                        target.iterate().into_iter().collect::<Vec<_>>(),
                        BuiltinTerm::CollectHashSet,
                    ),
                    CollectionTerm::Vector(target) => (
                        target.iterate().into_iter().collect::<Vec<_>>(),
                        BuiltinTerm::CollectVector,
                    ),
                };
                let has_dynamic_items = items.iter().any(|item| !item.is_static());
                match has_dynamic_items {
                    false => Expression::clone(&target),
                    true => Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(collect)),
                        items,
                    ))),
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected <collection>, received {}",
                    target,
                ))))],
            )))),
        }
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

pub struct CollectStruct {}
impl BuiltinFunction for CollectStruct {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        let fields = {
            if args.len() % 2 != 0 {
                Err(format!(
                    "Expected even number of arguments, received {}",
                    args.len()
                ))
            } else {
                let num_fields = args.len() / 2;
                args.into_iter().enumerate().fold(
                    Ok((
                        Vec::with_capacity(num_fields),
                        Vec::with_capacity(num_fields),
                    )),
                    |results, (index, arg)| {
                        let (mut keys, mut values) = results?;
                        if index % 2 == 0 {
                            match arg.value() {
                                Term::Value(key) => {
                                    keys.push(key.clone());
                                    Ok((keys, values))
                                }
                                _ => Err(format!("Invalid key: {}", arg)),
                            }
                        } else {
                            values.push(arg);
                            Ok((keys, values))
                        }
                    },
                )
            }
        };
        match fields {
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(
                    StringValue::from(error),
                )))],
            )))),
            Ok((keys, values)) => Expression::new(Term::Struct(StructTerm::new(
                Some(StructPrototype::new(keys)),
                values,
            ))),
        }
    }
}

pub struct CollectVector {}
impl BuiltinFunction for CollectVector {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        VectorTerm::collect(args)
    }
}

pub struct CollectHashMap {}
impl BuiltinFunction for CollectHashMap {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        HashMapTerm::collect(args)
    }
}

pub struct CollectHashSet {}
impl BuiltinFunction for CollectHashSet {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        HashSetTerm::collect(args)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::SubstitutionCache,
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
        let mut cache = SubstitutionCache::new();
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
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3 + 1))),
                        Expression::new(Term::Value(ValueTerm::Int(4 + 1))),
                        Expression::new(Term::Value(ValueTerm::Int(5 + 1))),
                    ]
                ),))),
                DependencyList::empty(),
            )
        );
    }
}
