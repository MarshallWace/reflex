// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use crate::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HeapAllocator, Uid, Uuid,
};

pub struct Push {}
impl Push {
    pub(crate) const UUID: Uuid = uuid!("3e426e31-dddb-4732-93a7-216dc6c009dd");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl Uid for Push {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Push {
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
        let value = args.next().unwrap();
        if let Some(collection) = factory.match_vector_term(&target) {
            Ok(factory.create_vector_term(allocator.create_sized_list(
                collection.items().len() + 1,
                collection.items().iter().cloned().chain(once(value)),
            )))
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            Ok(if collection.contains(&value) {
                target
            } else {
                factory.create_hashset_term(allocator.create_sized_list(
                    collection.values().len() + 1,
                    collection.values().iter().cloned().chain(once(value)),
                ))
            })
        } else {
            Err(format!(
                "Invalid push operation: Expected (Vector, <any>) or (HashSet, <any>), received ({}, {})",
                target, value
            ))
        }
    }
}
