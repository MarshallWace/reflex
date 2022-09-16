// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Flatten {}
impl Flatten {
    pub(crate) const UUID: Uuid = uuid!("da2105be-0212-4de0-89b2-b63478292c7a");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Flatten {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Flatten {
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
        let target = args.next().unwrap();
        let result = if let Some(target) = factory.match_list_term(&target) {
            let items = target
                .items()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .flat_map(|item| {
                    let (list_items, standalone_item) =
                        if let Some(inner) = factory.match_list_term(item) {
                            (
                                Some(
                                    inner
                                        .items()
                                        .as_deref()
                                        .iter()
                                        .map(|item| item.as_deref())
                                        .cloned(),
                                ),
                                None,
                            )
                        } else {
                            (None, Some(item.clone()))
                        };
                    list_items.into_iter().flatten().chain(standalone_item)
                });
            let num_items = target
                .items()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .fold(0, |num_items, item| {
                    if let Some(inner) = factory.match_list_term(item) {
                        num_items + inner.items().as_deref().len()
                    } else {
                        num_items + 1
                    }
                });
            Some(factory.create_list_term(allocator.create_sized_list(num_items, items)))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Expected List, received {}", target)),
        }
    }
}
