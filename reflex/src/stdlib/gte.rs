// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Gte {}
impl Gte {
    pub(crate) const UUID: Uuid = uuid!("568c973c-9792-4001-9074-1453fc2ac2bb");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Gte {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Gte {
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
        let result = if let (Some(left), Some(right)) = (
            factory.match_int_term(&left),
            factory.match_int_term(&right),
        ) {
            Some(factory.create_boolean_term(left.value >= right.value))
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_float_term(&right),
        ) {
            Some(factory.create_boolean_term(left.value >= right.value))
        } else if let (Some(left), Some(right)) = (
            factory.match_int_term(&left),
            factory.match_float_term(&right),
        ) {
            Some(factory.create_boolean_term((left.value as f64) >= right.value))
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_int_term(&right),
        ) {
            Some(factory.create_boolean_term(left.value >= (right.value as f64)))
        } else {
            None
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
