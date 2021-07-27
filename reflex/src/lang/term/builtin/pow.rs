// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::{is_integer, ValueTerm},
};

pub struct Pow {}
impl<T: Expression> Applicable<T> for Pow {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        let result = match (
            factory.match_value_term(&left),
            factory.match_value_term(&right),
        ) {
            (Some(left), Some(right)) => match (left, right) {
                (ValueTerm::Int(left), ValueTerm::Int(right)) => {
                    let left = *left;
                    let right = *right;
                    Some(Ok(factory.create_value_term(if right < 0 {
                        ValueTerm::Float((left as f64).powi(right))
                    } else {
                        ValueTerm::Int(left.pow(right as u32))
                    })))
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    let left = *left;
                    let right = *right;
                    if left < 0.0 && !is_integer(right) {
                        Some(Err(format!(
                            "Invalid exponentiation operands: ({}, {})",
                            left, right
                        )))
                    } else {
                        Some(Ok(
                            factory.create_value_term(ValueTerm::Float(left.powf(right)))
                        ))
                    }
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    let left = *left;
                    let right = *right;
                    if left < 0 && !is_integer(right) {
                        Some(Err(format!(
                            "Invalid exponentiation operands: ({}, {})",
                            left, right
                        )))
                    } else {
                        Some(Ok(factory.create_value_term(ValueTerm::Float(
                            (left as f64).powf(right),
                        ))))
                    }
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => Some(Ok(
                    factory.create_value_term(ValueTerm::Float(left.powi(*right)))
                )),
                _ => None,
            },
            _ => None,
        };
        match result {
            Some(result) => result,
            None => Err(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                left, right
            )),
        }
    }
}
