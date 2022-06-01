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

pub struct Equal {}
impl Equal {
    pub(crate) const UUID: Uuid = uuid!("00353bbf-544d-4515-9767-5a9a0a7c4ee1");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Equal {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Equal {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
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
