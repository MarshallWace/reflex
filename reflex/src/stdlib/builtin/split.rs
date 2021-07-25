// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{
        builtin::BuiltinFunction,
        collection::{vector::VectorTerm, CollectionTerm},
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub struct Split {}
impl BuiltinFunction for Split {
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
        let separator = args.next().unwrap();
        match (target.value(), separator.value()) {
            (Term::Value(ValueTerm::String(left)), Term::Value(ValueTerm::String(right))) => {
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    left.split(right).map(|value| {
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(value))))
                    }),
                ))))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected (String, String), received ({}, {})",
                    target, separator,
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
            collection::{vector::VectorTerm, CollectionTerm},
            value::{StringValue, ValueTerm},
        },
    };

    fn create_split_expression(target: &str, separator: &str) -> Expression {
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Split)),
            vec![
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(target)))),
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(separator)))),
            ],
        )))
    }

    fn resolve_deep(target: Expression) -> Expression {
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::ResolveDeep)),
            vec![target],
        )))
    }

    #[test]
    fn split_expressions() {
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = create_split_expression("\"\"", " ");
        let result = resolve_deep(expression).evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("\"\"")
                    )))],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = create_split_expression("foo", " ");
        let result = resolve_deep(expression).evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("foo")
                    )))],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = create_split_expression("foo bar", " ");
        let result = resolve_deep(expression).evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                    ]
                )))),
                DependencyList::empty(),
            )
        );
        let expression = create_split_expression("foo bar baz", " ");
        let result = resolve_deep(expression).evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("baz")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
        let expression = create_split_expression("foo bar baz", "bar");
        let result = resolve_deep(expression).evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo ")))),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(" baz")))),
                    ],
                )))),
                DependencyList::empty(),
            )
        );
    }
}
