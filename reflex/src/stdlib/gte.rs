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

pub struct Gte {}
impl Gte {
    const UUID: Uuid = uuid!("568c973c-9792-4001-9074-1453fc2ac2bb");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Gte {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Gte {
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
                    Some(factory.create_value_term(ValueTerm::Boolean(left >= right)))
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(left >= right)))
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean((*left as f64) >= *right)))
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => {
                    Some(factory.create_value_term(ValueTerm::Boolean(*left >= (*right as f64))))
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