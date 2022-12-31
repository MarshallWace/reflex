// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use reflex::core::{
    match_typed_expression_list, uuid, Applicable, ArgType, Arity, EvaluationCache, Expression,
    ExpressionFactory, FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid,
    Uuid,
};

pub struct Concat;
impl Concat {
    pub const UUID: Uuid = uuid!("46e03082-2c5f-41c7-af49-fc09516e2dfe");
    const ARITY: FunctionArity<0, 0> = FunctionArity {
        required: [],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Concat {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Concat {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
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
                if let Some(term) = factory.match_string_term(arg) {
                    Some(String::from(term.value().as_deref().as_str().deref()))
                } else {
                    None
                }
            },
            |arg| format!("Expected String, received {}", arg),
        )?
        .into_iter()
        .collect::<String>();
        Ok(factory.create_string_term(allocator.create_string(value)))
    }
}
