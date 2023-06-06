// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

pub struct Flatten;
impl Flatten {
    pub const UUID: Uuid = uuid!("da2105be-0212-4de0-89b2-b63478292c7a");
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
            let items = target.items();
            let items = items
                .as_deref()
                .iter()
                // TODO: avoid unnecessary intermediate allocations
                .fold(Vec::<T>::new(), |mut results, item| {
                    if let Some(inner) = factory.match_list_term(&item) {
                        results.extend(inner.items().as_deref().iter());
                    } else {
                        results.push(item)
                    };
                    results
                });
            Some(factory.create_list_term(allocator.create_list(items)))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Expected List, received {}", target)),
        }
    }
}
