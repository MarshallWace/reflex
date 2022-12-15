// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FloatTermType, FunctionArity, HeapAllocator, IntTermType, Uid, Uuid,
};

pub struct Abs;
impl Abs {
    pub const UUID: Uuid = uuid!("493be078-9705-40d8-9967-b3d0867b0869");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Abs {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Abs {
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
        let operand = args.next().unwrap();
        let result = if let Some(operand) = factory.match_int_term(&operand) {
            Some(factory.create_int_term(operand.value().abs()))
        } else if let Some(operand) = factory.match_float_term(&operand) {
            Some(factory.create_float_term(operand.value().abs()))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Expected Int or Float, received {}", operand)),
        }
    }
}
