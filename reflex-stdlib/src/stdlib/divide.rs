// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FloatTermType, FunctionArity, HeapAllocator, IntTermType, Uid, Uuid,
};

pub struct Divide;
impl Divide {
    pub const UUID: Uuid = uuid!("082fbcc9-31b0-4475-8e51-992d0ccc44dd");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Divide {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Divide {
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
            if right.value() == 0 {
                Some(Err(format!(
                    "Division by zero: {} / {}",
                    left.value(),
                    right.value()
                )))
            } else {
                Some(Ok(factory.create_int_term(left.value() / right.value())))
            }
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_float_term(&right),
        ) {
            if right.value() == 0.0 {
                Some(Err(format!(
                    "Division by zero: {} / {}",
                    left.value(),
                    right.value()
                )))
            } else {
                Some(Ok(factory.create_float_term(left.value() / right.value())))
            }
        } else if let (Some(left), Some(right)) = (
            factory.match_int_term(&left),
            factory.match_float_term(&right),
        ) {
            if right.value() == 0.0 {
                Some(Err(format!(
                    "Division by zero: {} / {}",
                    left.value(),
                    right.value()
                )))
            } else {
                Some(Ok(
                    factory.create_float_term((left.value() as f64) / right.value())
                ))
            }
        } else if let (Some(left), Some(right)) = (
            factory.match_float_term(&left),
            factory.match_int_term(&right),
        ) {
            if right.value() == 0 {
                Some(Err(format!(
                    "Division by zero: {} / {}",
                    left.value(),
                    right.value()
                )))
            } else {
                Some(Ok(
                    factory.create_float_term(left.value() / (right.value() as f64))
                ))
            }
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
