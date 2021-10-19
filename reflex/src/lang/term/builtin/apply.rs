// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator,
};

pub struct Apply {}
impl Apply {
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression + Applicable<T>> Applicable<T> for Apply {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let args = args.next().unwrap();
        let result = if target.arity().is_some() {
            let arg_values = if let Some(args) = factory.match_tuple_term(&args) {
                Some(args.fields())
            } else if let Some(args) = factory.match_vector_term(&args) {
                Some(args.items())
            } else {
                None
            };
            match arg_values {
                Some(arg_values) => {
                    Ok(factory.create_application_term(target, allocator.clone_list(arg_values)))
                }
                None => Err((target, args)),
            }
        } else {
            Err((target, args))
        };
        match result {
            Ok(result) => Ok(result),
            Err((target, args)) => Err(format!(
                "Expected (<function>, <tuple>) or (<function>, Vector), received ({}, {})",
                target, args
            )),
        }
    }
}
