// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator,
        StringValue,
    },
    lang::ValueTerm,
};

pub struct Contains {}
impl<T: Expression> Applicable<T> for Contains {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(2, 0, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 2 {
            return Err(format!("Expected 2 arguments, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        let result = if let Some(target) = factory.match_struct_term(&target) {
            let field_name = match factory.match_value_term(&key) {
                Some(ValueTerm::String(value)) => Some(value),
                _ => None,
            };
            Ok(field_name.and_then(|key| target.get(key.as_str())).is_some())
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Ok(target.get(&key).is_some())
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Ok(target.contains(&key))
        } else {
            Err(format!(
                "Expected (<struct>, <any>) or (HashMap, any) or (HashSet, any), received ({}, {})",
                target, key
            ))
        };
        result.map(|result| factory.create_value_term(ValueTerm::Boolean(result)))
    }
}
