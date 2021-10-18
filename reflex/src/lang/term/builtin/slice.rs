// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::{as_integer, ValueTerm},
};

pub struct Slice {}
impl Slice {
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Slice {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let start_index = args.next().unwrap();
        let end_index = args.next().unwrap();
        let bounds = {
            let start_index = parse_integer_argument(&start_index, factory);
            let end_index = parse_integer_argument(&end_index, factory);
            match (start_index, end_index) {
                (Some(start_index), Some(end_index)) => {
                    let start_index = start_index.max(0) as usize;
                    let end_index = end_index.max(start_index as i32) as usize;
                    Some((start_index, end_index))
                }
                _ => None,
            }
        };
        match (factory.match_vector_term(&target), bounds) {
            (Some(target), Some((start_index, end_index))) => Ok(factory.create_vector_term(
                allocator.create_list(
                    target
                        .items()
                        .iter()
                        .skip(start_index)
                        .take(end_index - start_index)
                        .cloned(),
                ),
            )),
            _ => Err(format!(
                "Expected (<vector>, Int, Int), received ({}, {}, {})",
                target, start_index, end_index,
            )),
        }
    }
}

fn parse_integer_argument<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<i32> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Int(value)) => Some(*value),
        Some(ValueTerm::Float(value)) => as_integer(*value),
        _ => None,
    }
}
