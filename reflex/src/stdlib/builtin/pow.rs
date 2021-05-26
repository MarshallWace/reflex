// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term},
    serialize::SerializedTerm,
    stdlib::{
        builtin::BuiltinFunction,
        signal::SignalType,
        value::{is_integer, ValueTerm},
    },
};

pub struct Pow {}
impl BuiltinFunction for Pow {
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        match (left.value(), right.value()) {
            (Term::Value(ValueTerm::Int(left)), Term::Value(ValueTerm::Int(right))) => {
                let left = *left;
                let right = *right;
                Expression::new(Term::Value(if right < 0 {
                    ValueTerm::Float((left as f64).powi(right))
                } else {
                    ValueTerm::Int(left.pow(right as u32))
                }))
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Int(right))) => {
                Expression::new(Term::Value(ValueTerm::Float(left.powi(*right))))
            }
            (Term::Value(ValueTerm::Int(left)), Term::Value(ValueTerm::Float(right))) => {
                let left = *left;
                let right = *right;
                if left < 0 && !is_integer(right) {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![SerializedTerm::string(format!("Invalid exponentiation operands: ({}, {})", left, right))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Float((left as f64).powf(right))))
                }
            }
            (Term::Value(ValueTerm::Float(left)), Term::Value(ValueTerm::Float(right))) => {
                let left = *left;
                let right = *right;
                if left < 0.0 && !is_integer(right) {
                    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![SerializedTerm::string(format!("Invalid exponentiation operands: ({}, {})", left, right))],
                    ))))
                } else {
                    Expression::new(Term::Value(ValueTerm::Float(left.powf(right))))
                }
            }
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![SerializedTerm::string(format!(
                    "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received ({}, {})",
                    left, right,
                ))],
            )))),
        }
    }
}
