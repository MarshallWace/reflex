// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::{
    core::{
        match_typed_expression_list, Applicable, Arity, EvaluationCache, Expression,
        ExpressionFactory, HeapAllocator, VarArgs,
    },
    lang::deduplicate_hashset_entries,
};

pub struct Append {}
impl<T: Expression> Applicable<T> for Append {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, Some(VarArgs::Eager)))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() < 2 {
            return Err(format!(
                "Expected 2 or more arguments, received {}",
                args.len()
            ));
        }
        let target = args.next().unwrap();
        if let Some(collection) = factory.match_vector_term(&target) {
            let args = args.collect::<Vec<_>>();
            let additional_collections = match_typed_expression_list(
                args.iter(),
                |term| factory.match_vector_term(term),
                |arg| format!("Expected Vector, received {}", arg),
            )?;
            let combined_collections = once(collection).chain(additional_collections);
            let combined_items = combined_collections.flat_map(|arg| arg.items().iter().cloned());
            Ok(factory.create_vector_term(allocator.create_unsized_list(combined_items)))
        } else if let Some(collection) = factory.match_hashset_term(&target) {
            let args = args.collect::<Vec<_>>();
            let additional_collections = match_typed_expression_list(
                args.iter(),
                |term| factory.match_hashset_term(term),
                |arg| format!("Expected HashSet, received {}", arg),
            )?;
            let combined_collections = once(collection).chain(additional_collections);
            let combined_values = combined_collections.flat_map(|arg| arg.values().iter().cloned());
            let values = combined_values.collect::<Vec<_>>();
            let deduplicated_values = match deduplicate_hashset_entries(&values) {
                Some(values) => values,
                None => values,
            };
            Ok(factory.create_hashset_term(allocator.create_list(deduplicated_values)))
        } else {
            Err(format!(
                "Expected (Vector, ...) or (HashSet, ...), received ({})",
                once(target)
                    .chain(args)
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        }
    }
}
