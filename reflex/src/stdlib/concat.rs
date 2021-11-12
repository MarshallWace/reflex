// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    core::{
        match_typed_expression_list, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression,
        ExpressionFactory, FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Concat {}
impl Concat {
    pub(crate) const UUID: Uuid = uuid!("46e03082-2c5f-41c7-af49-fc09516e2dfe");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
}
impl Uid for Concat {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
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
