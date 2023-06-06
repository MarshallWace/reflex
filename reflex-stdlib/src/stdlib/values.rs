// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    FunctionArity, HashmapTermType, HashsetTermType, HeapAllocator, Uid, Uuid,
};

pub struct Values;
impl Values {
    pub const UUID: Uuid = uuid!("d41e9dee-b0c7-48cd-8002-06f4315268b3");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Values {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Values {
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
        if args.len() != 1 {
            return Err(format!("Expected 1 argument, received {}", args.len()));
        }
        let target = args.next().unwrap();
        let result = if let Some(_) = factory.match_list_term(&target) {
            Some(target.clone())
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_list_term(allocator.create_list(target.values())))
        } else if let Some(target) = factory.match_hashset_term(&target) {
            Some(factory.create_list_term(allocator.create_list(target.values())))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate keys for {}", target)),
        }
    }
}
