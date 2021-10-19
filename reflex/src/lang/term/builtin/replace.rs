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

pub struct Replace {}
impl Replace {
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Replace {
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
        let pattern = args.next().unwrap();
        let replacement = args.next().unwrap();
        match (
            factory.match_value_term(&target),
            factory.match_value_term(&pattern),
            factory.match_value_term(&replacement),
        ) {
            (
                Some(ValueTerm::String(target)),
                Some(ValueTerm::String(pattern)),
                Some(ValueTerm::String(replacement)),
            ) => Ok(factory.create_value_term(ValueTerm::String(
                allocator.create_string(target.as_str().replacen(
                    pattern.as_str(),
                    replacement.as_str(),
                    1,
                )),
            ))),
            _ => Err(format!(
                "Expected (String, String, String), received ({}, {}, {})",
                target, pattern, replacement,
            )),
        }
    }
}
