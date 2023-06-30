// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HashmapTermType, HeapAllocator, ListTermType, RefType, Uid,
    Uuid,
};

pub struct Unzip;
impl Unzip {
    pub const UUID: Uuid = uuid!("01fcc643-5960-47e0-a906-dc2ad5212ac3");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Unzip {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Unzip {
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
        let entries = args.next().unwrap();
        let result = if let Some(entries) = factory.match_list_term(&entries) {
            let items = entries.items();
            let items = items.as_deref();
            let (keys, values): (Vec<_>, Vec<_>) = items
                .iter()
                .map(|item| {
                    let (key, value) = if let Some(entry) = factory.match_list_term(item.as_deref())
                    {
                        let entry_items = entry.items();
                        let entry_items = entry_items.as_deref();
                        let key = entry_items.get(0).map(|item| item.as_deref().clone());
                        let value = entry_items.get(1).map(|item| item.as_deref().clone());
                        (key, value)
                    } else {
                        (None, None)
                    };
                    (
                        key.unwrap_or_else(|| factory.create_nil_term()),
                        value.unwrap_or_else(|| factory.create_nil_term()),
                    )
                })
                .unzip();

            Some(factory.create_list_term(allocator.create_pair(
                factory.create_list_term(allocator.create_list(keys)),
                factory.create_list_term(allocator.create_list(values)),
            )))
        } else if let Some(entries) = factory.match_hashmap_term(&entries) {
            let keys = entries.keys().map(|item| item.as_deref().clone());
            let values = entries.values().map(|item| item.as_deref().clone());
            Some(factory.create_list_term(allocator.create_pair(
                factory.create_list_term(allocator.create_list(keys)),
                factory.create_list_term(allocator.create_list(values)),
            )))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Expected List, received {}", entries)),
        }
    }
}
