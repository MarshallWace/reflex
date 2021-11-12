// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
};

pub struct Replace {}
impl Replace {
    pub(crate) const UUID: Uuid = uuid!("19684ad2-cfe3-4850-9a4b-7787a5e47a16");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Replace {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
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
