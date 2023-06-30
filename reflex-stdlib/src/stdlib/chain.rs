// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Chain;
impl Chain {
    pub const UUID: Uuid = uuid!("c3c4d9a4-d115-4b6f-9d15-d96eb500462c");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Chain {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Chain {
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
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        if let (Some(left), Some(right)) = (
            factory.match_list_term(&left),
            factory.match_list_term(&right),
        ) {
            let left_items = left.items();
            let right_items = right.items();
            let left_items = left_items.as_deref();
            let right_items = right_items.as_deref();
            Ok(factory.create_list_term(
                allocator.create_sized_list(
                    left_items.len() + right_items.len(),
                    left_items
                        .iter()
                        .chain(right_items.iter())
                        .map(|item| item.as_deref().clone()),
                ),
            ))
        } else {
            Err(format!(
                "Expected (List, List), received ({}, {})",
                left, right
            ))
        }
    }
}
