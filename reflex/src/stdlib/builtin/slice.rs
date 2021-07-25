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

pub struct Slice {}
impl BuiltinFunction for Slice {
    fn arity() -> Arity {
        Arity::from(3, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 3 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected 3 arguments, received {}",
                    args.len(),
                ))))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let offset = args.next().unwrap();
        let length = args.next().unwrap();
        let bounds = {
            let offset = parse_integer_argument(&offset);
            let length = parse_integer_argument(&length);
            match (offset, length) {
                (Some(offset), Some(length)) => Some(if offset < 0 {
                    (0, (offset + length).max(0) as usize)
                } else {
                    (offset as usize, length.max(0) as usize)
                }),
                _ => None,
            }
        };
        match (target.value(), bounds) {
            (Term::Collection(CollectionTerm::Vector(target)), Some((offset, length))) => {
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    target.iterate().into_iter().skip(offset).take(length),
                ))))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected (<vector>, Int, Int), received ({}, {}, {})",
                    target, offset, length,
                ))))],
            )))),
        }
    }
}

fn parse_integer_argument(value: &Expression) -> Option<i32> {
    match value.value() {
        Term::Value(value) => match value {
            ValueTerm::Int(value) => Some(*value),
            ValueTerm::Float(value) => Some(*value as i32),
            _ => None,
        },
        _ => None,
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
            value::ValueTerm,
        },
    };

    #[test]
    fn get_vector_slice() {
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let collection = Expression::new(Term::Collection(CollectionTerm::Vector(
            VectorTerm::new(vec![
                Expression::new(Term::Value(ValueTerm::Int(0))),
                Expression::new(Term::Value(ValueTerm::Int(1))),
                Expression::new(Term::Value(ValueTerm::Int(2))),
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ]),
        )));
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Slice)),
            vec![
                Expression::clone(&collection),
                Expression::new(Term::Value(ValueTerm::Int(0))),
                Expression::new(Term::Value(ValueTerm::Int(0))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![]
                ),))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Slice)),
            vec![
                Expression::clone(&collection),
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(1))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![Expression::new(Term::Value(ValueTerm::Int(3))),]
                ),))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Slice)),
            vec![
                Expression::clone(&collection),
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ]
                ),))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Slice)),
            vec![
                Expression::clone(&collection),
                Expression::new(Term::Value(ValueTerm::Int(-1))),
                Expression::new(Term::Value(ValueTerm::Int(3))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(0))),
                        Expression::new(Term::Value(ValueTerm::Int(1))),
                    ]
                ),))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Slice)),
            vec![
                Expression::clone(&collection),
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(-1))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
                    vec![]
                ),))),
                DependencyList::empty(),
            ),
        );
    }
}
