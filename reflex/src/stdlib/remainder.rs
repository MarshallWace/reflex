// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Remainder {}
impl Remainder {
    const UUID: Uuid = uuid!("ad5c5195-ffcd-4eda-98b7-d94683c38f71");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Remainder {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Remainder {
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
                    if *right == 0 {
                        Some(Err(format!("Division by zero: {} % {}", left, right)))
                    } else {
                        Some(Ok(ValueTerm::Int(left % right)))
                    }
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    if *right == 0.0 {
                        Some(Err(format!("Division by zero: {} % {}", left, right)))
                    } else {
                        Some(Ok(ValueTerm::Float(left % right)))
                    }
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    if *right == 0.0 {
                        Some(Err(format!("Division by zero: {} % {}", left, right)))
                    } else {
                        Some(Ok(ValueTerm::Float((*left as f64) % right)))
                    }
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => {
                    if *right == 0 {
                        Some(Err(format!("Division by zero: {} % {}", left, right)))
                    } else {
                        Some(Ok(ValueTerm::Float(left % (*right as f64))))
                    }
                }
                _ => None,
            },
            _ => None,
        };
        match result {
            Some(result) => result.map(|value| factory.create_value_term(value)),
            None => Err(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                left, right
            )),
        }
    }
}