// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Add {}
impl BuiltinFunction for Add {
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
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        match (left.value(), right.value()) {
            (Term::Value(ValueTerm::Int(left)), Term::Value(ValueTerm::Int(right))) => {
                Expression::new(Term::Value(ValueTerm::Int(left + right)))
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Float(right))) => {
                Expression::new(Term::Value(ValueTerm::Float(left + right)))
            }
            (Term::Value(ValueTerm::Int(left)), Term::Value(ValueTerm::Float(right))) => {
                Expression::new(Term::Value(ValueTerm::Float((*left as f64) + right)))
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Int(right))) => {
                Expression::new(Term::Value(ValueTerm::Float(left + (*right as f64))))
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(format!(
                    "Expected (Int, Int) or (Float, Float), received ({}, {})",
                    left, right,
                ))))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::GenerationalGc,
        core::{
            DependencyList, DynamicState, EvaluationResult, Expression, Signal, SignalTerm, Term,
        },
        parser::sexpr::parse,
        stdlib::{signal::SignalType, value::ValueTerm},
    };

    #[test]
    fn add_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("(+ 0 0)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(0 + 0))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ 3 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ -3 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(-3 + 4))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ 3 -4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + -4))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ -3 -4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(-3 + -4))),
                DependencyList::empty(),
            )
        );

        let expression = parse("(+ 0.0 0.0)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(0.0 + 0.0))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ 2.718 3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(2.718 + 3.142))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ -2.718 3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(-2.718 + 3.142))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(2.718 + -3.142))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(+ -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Float(-2.718 + -3.142))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn invalid_add_expression_operands() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = parse("(+ 3 #f)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from("Expected (Int, Int) or (Float, Float), received (3, false)")
                    )))]
                )))),
                DependencyList::empty()
            )
        );
        let expression = parse("(+ 3 \"3\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from("Expected (Int, Int) or (Float, Float), received (3, \"3\")")
                    )))]
                )))),
                DependencyList::empty()
            )
        );

        let expression = parse("(+ #f 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from("Expected (Int, Int) or (Float, Float), received (false, 3)")
                    )))]
                )))),
                DependencyList::empty()
            )
        );
        let expression = parse("(+ \"3\" 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from("Expected (Int, Int) or (Float, Float), received (\"3\", 3)")
                    )))]
                )))),
                DependencyList::empty()
            )
        );

        let expression = parse("(+ 3.142 #f)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from(
                            "Expected (Int, Int) or (Float, Float), received (3.142, false)"
                        )
                    )))]
                )))),
                DependencyList::empty()
            )
        );
        let expression = parse("(+ 3.142 \"3\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from(
                            "Expected (Int, Int) or (Float, Float), received (3.142, \"3\")"
                        )
                    )))]
                )))),
                DependencyList::empty()
            )
        );

        let expression = parse("(+ #f 3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from(
                            "Expected (Int, Int) or (Float, Float), received (false, 3.142)"
                        )
                    )))]
                )))),
                DependencyList::empty()
            )
        );
        let expression = parse("(+ \"3\" 3.142)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        String::from(
                            "Expected (Int, Int) or (Float, Float), received (\"3\", 3.142)"
                        )
                    )))]
                )))),
                DependencyList::empty()
            )
        );
    }
}
