// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};

pub struct JsonDeserialize;
impl JsonDeserialize {
    pub const UUID: Uuid = uuid!("6e8468d3-34cc-423c-a787-3d3e9154885d");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for JsonDeserialize {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for JsonDeserialize {
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
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        match factory.match_string_term(&source) {
            Some(source) => crate::parse(
                source.value().as_deref().as_str().deref(),
                factory,
                allocator,
            )
            .map_err(|error| format!("JSON deserialization failed: {}", error)),
            _ => Err(format!(
                "JSON deserialization failed: expected string argument, received {}",
                source
            )),
        }
    }
}
