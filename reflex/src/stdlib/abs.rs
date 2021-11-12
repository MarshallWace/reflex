// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use uuid::Uuid;

use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid,
    },
    lang::ValueTerm,
};

pub struct Abs {}
impl Abs {
    pub(crate) const UUID: Uuid = uuid!("493be078-9705-40d8-9967-b3d0867b0869");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Abs {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Abs {
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
        let operand = args.next().unwrap();
        let result = match factory.match_value_term(&operand) {
            Some(ValueTerm::Int(operand)) => {
                Some(factory.create_value_term(ValueTerm::Int(operand.abs())))
            }
            Some(ValueTerm::Float(operand)) => {
                Some(factory.create_value_term(ValueTerm::Float(operand.abs())))
            }
            _ => None,
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Expected Int or Float, received {}", operand)),
        }
    }
}
