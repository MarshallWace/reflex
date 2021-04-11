// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, NativeFunction, Signal, SignalTerm, Term},
    stdlib::{builtin::Array, collection::CollectionTerm, signal::SignalType, value::ValueTerm},
};

pub struct Collect {}
impl NativeFunction for Collect {
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 1 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 1 argument, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        match target.value() {
            Term::Collection(collection) => Array::apply(match collection {
                CollectionTerm::Vector(target) => target.iterate(),
            }),
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected <collection>, received {}",
                    target,
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression, Term},
        parser::sexpr::parse,
        stdlib::{
            builtin::BuiltinTerm,
            collection::{vector::VectorTerm, CollectionTerm},
            value::{ArrayValue, ValueTerm},
        },
    };

    #[test]
    fn collect_expressions() {
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
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Array(
                    ArrayValue::new(vec![
                        ValueTerm::Int(3 + 1),
                        ValueTerm::Int(4 + 1),
                        ValueTerm::Int(5 + 1),
                    ]),
                )))),
                DependencyList::empty(),
            )
        );
    }
}
