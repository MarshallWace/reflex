// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    is_integer, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FloatTermType, FloatValue, FunctionArity, HeapAllocator, IntTermType, Uid, Uuid,
};

pub struct Pow;
impl Pow {
    pub const UUID: Uuid = uuid!("2e8f2ac5-ed34-4e7d-beba-891bd1ea8505");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Pow {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Pow {
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
            let left = left.value();
            let right = right.value();
            Some(Ok(if right < 0 {
                factory.create_float_term((left as FloatValue).powi(right as i32))
            } else {
                factory.create_int_term(left.pow(right as u32))
            }))
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_float_term(&right),
        ) {
            let left = left.value();
            let right = right.value();
            if left < 0.0 && !is_integer(right) {
                Some(Err(format!(
                    "Invalid exponentiation operands: ({}, {})",
                    left, right
                )))
            } else {
                Some(Ok(factory.create_float_term(left.powf(right))))
            }
        } else if let (Some(left), Some(right)) = (
            factory.match_int_term(&left),
            factory.match_float_term(&right),
        ) {
            let left = left.value();
            let right = right.value();
            if left < 0 && !is_integer(right) {
                Some(Err(format!(
                    "Invalid exponentiation operands: ({}, {})",
                    left, right
                )))
            } else {
                Some(Ok(factory.create_float_term((left as f64).powf(right))))
            }
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_int_term(&right),
        ) {
            Some(Ok(
                factory.create_float_term(left.value().powi(right.value() as i32))
            ))
        } else {
            None
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
