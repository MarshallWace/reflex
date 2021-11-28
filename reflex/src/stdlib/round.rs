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

pub struct Round {}
impl Round {
    pub(crate) const UUID: Uuid = uuid!("8abe6c68-60e8-45e7-8497-d91b845d6e7e");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Round {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Round {
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
        let operand = args.next().unwrap();
        match factory.match_value_term(&operand) {
            Some(ValueTerm::Int(_)) => Ok(operand),
            Some(ValueTerm::Float(operand)) => {
                Ok(factory.create_value_term(ValueTerm::Float(operand.round())))
            }
            _ => Err(format!("Expected Int or Float, received {}", operand)),
        }
    }
}
