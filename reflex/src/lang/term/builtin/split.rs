// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        StringValue,
    },
    lang::ValueTerm,
};

pub struct Split {}
impl<T: Expression> Applicable<T> for Split {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let separator = args.next().unwrap();
        match (
            factory.match_value_term(&target),
            factory.match_value_term(&separator),
        ) {
            (Some(ValueTerm::String(target)), Some(ValueTerm::String(separator))) => Ok(factory
                .create_vector_term(allocator.create_unsized_list(
                    target.as_str().split(separator.as_str()).map(|value| {
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(value.into()),
                        ))
                    }),
                ))),
            _ => Err(format!(
                "Expected (String, String), received ({}, {})",
                target, separator,
            )),
        }
    }
}
