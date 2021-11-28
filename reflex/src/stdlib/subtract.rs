// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Subtract {}
impl Subtract {
    pub(crate) const UUID: Uuid = uuid!("c904b333-c919-4df3-9868-59c9c0dd6343");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Subtract {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Subtract {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
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
                    Some(factory.create_value_term(ValueTerm::Int(left - right)))
                }
                (ValueTerm::Float(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Float(left - right)))
                }
                (ValueTerm::Int(left), ValueTerm::Float(right)) => {
                    Some(factory.create_value_term(ValueTerm::Float((*left as f64) - right)))
                }
                (ValueTerm::Float(left), ValueTerm::Int(right)) => {
                    Some(factory.create_value_term(ValueTerm::Float(left - (*right as f64))))
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
