// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator, StringValue,
    },
    lang::ValueTerm,
};

pub struct Split {}
impl Split {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Split {
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
