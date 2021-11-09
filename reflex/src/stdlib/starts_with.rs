// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct StartsWith {}
impl StartsWith {
    const UUID: Uuid = uuid!("7f6b68cb-5e6b-4170-aa88-336f5e948ae9");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for StartsWith {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for StartsWith {
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
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        match (
            factory.match_value_term(&left),
            factory.match_value_term(&right),
        ) {
            (Some(ValueTerm::String(left)), Some(ValueTerm::String(right))) => Ok(factory
                .create_value_term(ValueTerm::Boolean(left.as_str().ends_with(right.as_str())))),
            _ => Err(format!(
                "Expected (String, String), received ({}, {})",
                left, right,
            )),
        }
    }
}