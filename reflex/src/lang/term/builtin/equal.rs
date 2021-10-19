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

pub struct Equal {}
impl Equal {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Equal {
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
        match (
            factory.match_value_term(&left),
            factory.match_value_term(&right),
        ) {
            (Some(ValueTerm::Int(left)), Some(ValueTerm::Int(right))) => {
                Ok(factory.create_value_term(ValueTerm::Boolean(left == right)))
            }
            (Some(ValueTerm::Float(left)), Some(ValueTerm::Float(right))) => {
                Ok(factory.create_value_term(ValueTerm::Boolean(left == right)))
            }
            (Some(ValueTerm::Int(left)), Some(ValueTerm::Float(right))) => {
                Ok(factory.create_value_term(ValueTerm::Boolean((*left as f64) == *right)))
            }
            (Some(ValueTerm::Float(left)), Some(ValueTerm::Int(right))) => {
                Ok(factory.create_value_term(ValueTerm::Boolean(*left == (*right as f64))))
            }
            _ => Err(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                left, right,
            )),
        }
    }
}
