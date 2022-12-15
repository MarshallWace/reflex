// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator,
    ListTermType, RefType, Uid, Uuid,
};

pub struct Length;
impl Length {
    pub const UUID: Uuid = uuid!("e1f49f93-3125-48d4-8686-8c09f33c1a07");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Length {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Length {
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
        let result = if let Some(target) = factory.match_list_term(&target) {
            Some(factory.create_int_term(target.items().as_deref().len() as i32))
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_int_term(target.keys().len() as i32))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(factory.create_int_term(target.values().len() as i32))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to get length for for {}", target)),
        }
    }
}
