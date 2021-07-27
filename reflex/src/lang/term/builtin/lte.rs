// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::ValueTerm,
};

pub struct Lte {}
impl<T: Expression> Applicable<T> for Lte {
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
                    Some(factory.create_value_term(ValueTerm::Boolean(left <= right)))
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(left <= right)))
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean((*left as f64) <= *right)))
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(*left <= (*right as f64))))
                }
                _ => None,
            },
            _ => None,
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                left, right
            )),
        }
    }
}
