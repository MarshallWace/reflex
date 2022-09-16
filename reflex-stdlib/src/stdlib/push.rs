// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashsetTermType, HeapAllocator, ListTermType, RefType, Uid,
    Uuid,
};

pub struct Push {}
impl Push {
    pub(crate) const UUID: Uuid = uuid!("3e426e31-dddb-4732-93a7-216dc6c009dd");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Push {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Push {
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
        let value = args.next().unwrap();
        if let Some(collection) = factory.match_list_term(&target) {
            Ok(factory.create_list_term(
                allocator.create_sized_list(
                    collection.items().as_deref().len() + 1,
                    collection
                        .items()
                        .as_deref()
                        .iter()
                        .map(|item| item.as_deref())
                        .cloned()
                        .chain(once(value)),
                ),
            ))
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            Ok(if collection.contains(&value) {
                target
            } else {
                let values = collection.values();
                factory.create_hashset_term(
                    allocator.create_sized_list(
                        values.len() + 1,
                        values
                            .map(|item| item.as_deref())
                            .cloned()
                            .chain(once(value)),
                    ),
                )
            })
        } else {
            Err(format!(
                "Invalid push operation: Expected (List, <any>) or (HashSet, <any>), received ({}, {})",
                target, value
            ))
        }
    }
}
