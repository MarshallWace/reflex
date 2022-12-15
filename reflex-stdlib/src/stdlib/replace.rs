// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, RefType, StringTermType, StringValue, Uid, Uuid,
};

pub struct Replace;
impl Replace {
    pub const UUID: Uuid = uuid!("19684ad2-cfe3-4850-9a4b-7787a5e47a16");
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Replace {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Replace {
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
        let pattern = args.next().unwrap();
        let replacement = args.next().unwrap();
        match (
            factory.match_string_term(&target),
            factory.match_string_term(&pattern),
            factory.match_string_term(&replacement),
        ) {
            (Some(target), Some(pattern), Some(replacement)) => Ok(factory.create_string_term(
                allocator.create_string(target.value().as_deref().as_str().replacen(
                    pattern.value().as_deref().as_str(),
                    replacement.value().as_deref().as_str(),
                    1,
                )),
            )),
            _ => Err(format!(
                "Expected (String, String, String), received ({}, {}, {})",
                target, pattern, replacement,
            )),
        }
    }
}
