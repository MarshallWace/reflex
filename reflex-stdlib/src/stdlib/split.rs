// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};

pub struct Split;
impl Split {
    pub const UUID: Uuid = uuid!("806bc218-7b0e-4594-b948-2382f2f8738a");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Split {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Split {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let separator = args.next().unwrap();
        match (
            factory.match_string_term(&target),
            factory.match_string_term(&separator),
        ) {
            (Some(target), Some(separator)) => Ok(factory.create_list_term(
                allocator.create_unsized_list(
                    target
                        .value()
                        .as_deref()
                        .as_str()
                        .split(separator.value().as_deref().as_str().deref())
                        .map(|value| factory.create_string_term(allocator.create_string(value))),
                ),
            )),
            _ => Err(format!(
                "Expected (String, String), received ({}, {})",
                target, separator,
            )),
        }
    }
}
