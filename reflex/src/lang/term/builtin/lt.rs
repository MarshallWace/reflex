// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::ValueTerm,
};

pub struct Lt {}
impl Lt {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Lt {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        let result = match (
            factory.match_value_term(&left),
            factory.match_value_term(&right),
        ) {
            (Some(left), Some(right)) => match (left, right) {
                (ValueTerm::Int(left), ValueTerm::Int(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(left < right)))
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(left < right)))
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean((*left as f64) < *right)))
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(*left < (*right as f64))))
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
