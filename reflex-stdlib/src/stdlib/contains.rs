// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator, RecordTermType, Uid, Uuid,
};

pub struct Contains;
impl Contains {
    pub const UUID: Uuid = uuid!("7ed23b1e-14d6-4e39-b314-01184bf66b5a");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Contains {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Contains {
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
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let key = args.next().unwrap();
        let result = if let Some(target) = factory.match_record_term(&target) {
            Ok(target.get(&key).is_some())
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
        result.map(|result| factory.create_boolean_term(result))
    }
}
