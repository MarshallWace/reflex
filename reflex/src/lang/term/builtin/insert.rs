// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator,
};

pub struct Insert {}
impl Insert {
    const ARITY: FunctionArity<3, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict, ArgType::Lazy],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Insert {
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
        let key = args.next().unwrap();
        let value = args.next().unwrap();
        if let Some(existing) = factory.match_hashmap_term(&target) {
            match existing.set(key, value, factory, allocator) {
                Some(updated) => Ok(updated),
                None => Ok(target),
            }
        } else {
            Err(format!(
                "Invalid field update: Expected (HashMap, <any>, <any>), received ({}, {}, {})",
                target, key, value,
            ))
        }
    }
}
