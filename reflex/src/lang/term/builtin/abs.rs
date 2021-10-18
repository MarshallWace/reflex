// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::ValueTerm,
};

pub struct Abs {}
impl Abs {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Abs {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
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
