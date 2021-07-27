// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator},
    lang::ValueTerm,
};

pub struct Abs {}
impl<T: Expression> Applicable<T> for Abs {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(1, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
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
