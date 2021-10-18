// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::core::{
    Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
    HeapAllocator,
};

pub struct Values {}
impl Values {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Values {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let result = if let Some(_) = factory.match_vector_term(&target) {
            Some(target.clone())
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_vector_term(allocator.create_list(target.values().iter().cloned())))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(factory.create_vector_term(allocator.create_list(target.values().iter().cloned())))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate keys for {}", target)),
        }
    }
}
