// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{collections::hash_map::DefaultHasher, hash::Hasher};

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Hash;
impl Hash {
    pub const UUID: Uuid = uuid!("8312114e-8e8d-4ab0-a4d6-7a58f4e51f1d");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Hash {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Hash {
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let parsed_args = args.next().unwrap();
        match factory.match_list_term(&parsed_args) {
            Some(parsed_args) => {
                let mut hasher = DefaultHasher::new();
                for arg in parsed_args.items().as_deref().iter() {
                    std::hash::Hash::hash(&arg.as_deref().id(), &mut hasher);
                }
                let hash = hasher.finish();
                Ok(factory.create_symbol_term(hash))
            }
            _ => Err(format!("Expected list, received {}", parsed_args)),
        }
    }
}
