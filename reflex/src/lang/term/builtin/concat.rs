// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        match_typed_expression_list, Applicable, ArgType, Arity, EvaluationCache, Expression,
        ExpressionFactory, FunctionArity, HeapAllocator, StringValue,
    },
    lang::ValueTerm,
};

pub struct Concat {}
impl Concat {
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl<T: Expression> Applicable<T> for Concat {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let args = args.collect::<Vec<_>>();
        let value = match_typed_expression_list(
            args.iter(),
            |arg| {
                if let Some(ValueTerm::String(value)) = factory.match_value_term(arg) {
                    Some(value.as_str())
                } else {
                    None
                }
            },
            |arg| format!("Expected String, received {}", arg),
        )?
        .into_iter()
        .collect::<String>();
        Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(value))))
    }
}
