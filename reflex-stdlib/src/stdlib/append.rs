// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    deduplicate_hashset_entries, match_typed_expression_list, uuid, Applicable, ArgType, Arity,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListType, FunctionArity,
    HashsetTermType, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Append;
impl Append {
    pub const UUID: Uuid = uuid!("d5a4e3e7-0bef-45e8-bb6b-69b09fbd7666");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Strict),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Append {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Append {
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
        if let Some(collection) = factory.match_list_term(&target) {
            let args = args.collect::<Vec<_>>();
            let additional_collections = match_typed_expression_list(
                args.iter(),
                |term| factory.match_list_term(term),
                |arg| format!("Expected List, received {}", arg),
            )?;
            let combined_collections = once(collection).chain(additional_collections);
            let combined_items = combined_collections.flat_map(|arg| {
                arg.items()
                    .as_deref()
                    .iter()
                    .map(|item| item.as_deref())
                    .cloned()
            });
            Ok(factory.create_list_term(allocator.create_unsized_list(combined_items)))
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            let args = args.collect::<Vec<_>>();
            let additional_collections = match_typed_expression_list(
                args.iter().map(|item| item.as_deref()),
                |term| factory.match_hashset_term(term),
                |arg| format!("Expected HashSet, received {}", arg),
            )?;
            let combined_collections = once(collection).chain(additional_collections);
            let combined_values = combined_collections
                .flat_map(|arg| arg.values().map(|item| item.as_deref()).cloned());
            let values = combined_values.collect::<Vec<_>>();
            let deduplicated_values = match deduplicate_hashset_entries(&values) {
                Some(values) => values,
                None => values,
            };
            Ok(factory.create_hashset_term(deduplicated_values))
        } else {
            Err(format!(
                "Expected (List, ...) or (HashSet, ...), received ({})",
                once(target)
                    .chain(args)
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        }
    }
}
