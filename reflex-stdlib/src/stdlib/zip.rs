// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Zip;
impl Zip {
    pub const UUID: Uuid = uuid!("fd604ce6-1dd0-460e-ba68-be3522f84168");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Zip {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Zip {
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
        let result = if let (Some(left), Some(right)) = (
            factory.match_list_term(&left),
            factory.match_list_term(&right),
        ) {
            let left_items = left.items();
            let right_items = right.items();
            let left_items = left_items.as_deref();
            let right_items = right_items.as_deref();
            let num_items = left_items.len().min(right_items.len());
            Some(
                factory.create_list_term(
                    allocator.create_sized_list(
                        num_items,
                        left_items
                            .iter()
                            .map(|item| item.as_deref().clone())
                            .zip(right_items.iter().map(|item| item.as_deref().clone()))
                            .map(|(left, right)| {
                                factory.create_list_term(allocator.create_pair(left, right))
                            }),
                    ),
                ),
            )
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Expected (List, List), received ({}, {})",
                left, right
            )),
        }
    }
}
