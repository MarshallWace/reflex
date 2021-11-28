// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Apply {}
impl Apply {
    pub(crate) const UUID: Uuid = uuid!("faf2e936-d76c-4f54-bdae-bd67a2ab36a1");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Apply {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Apply {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
        let args = args.next().unwrap();
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
            None => Err(format!(
                "Expected (<function>, <tuple>) or (<function>, Vector), received ({}, {})",
                target, args
            )),
        }
    }
}
