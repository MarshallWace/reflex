// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    stdlib::{builtin::BuiltinFunction, signal::SignalType, value::ValueTerm},
};

pub struct Divide {}
impl BuiltinFunction for Divide {
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
                if *right == 0 {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Division by zero: {} / {}",
                            left, right,
                        ))))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Int(left / right)))
                }
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Float(right))) => {
                if *right == 0.0 {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Division by zero: {} / {}",
                            left, right,
                        ))))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Float(left / right)))
                }
            }
            (Term::Value(ValueTerm::Int(left)), Term::Value(ValueTerm::Float(right))) => {
                if *right == 0.0 {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Division by zero: {} / {}",
                            left, right,
                        ))))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Float((*left as f64) / right)))
                }
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Int(right))) => {
                if *right == 0 {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Division by zero: {} / {}",
                            left, right,
                        ))))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Float(left / (*right as f64))))
                }
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
