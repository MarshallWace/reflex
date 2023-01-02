// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RefType, Uid, Uuid,
};

use super::CollectQueryListItems;

pub struct DynamicQueryBranch;
impl DynamicQueryBranch {
    pub const UUID: Uuid = uuid!("58dd19b7-c9f0-473b-84c5-34af607c176b");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for DynamicQueryBranch {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for DynamicQueryBranch
where
    T::Builtin: From<CollectQueryListItems> + From<DynamicQueryBranch>,
{
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
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let shape = args.next().unwrap();
        if let Some(_) = factory.match_nil_term(&target) {
            Ok(target)
        } else if let Some(list) = factory.match_list_term(&target) {
            Ok(factory.create_application_term(
                factory.create_builtin_term(CollectQueryListItems),
                allocator.create_list(
                    list.items()
                        .as_deref()
                        .iter()
                        .map(|item| item.as_deref().clone())
                        .map(|item| {
                            factory.create_application_term(
                                factory.create_builtin_term(DynamicQueryBranch),
                                allocator.create_pair(item, shape.clone()),
                            )
                        }),
                ),
            ))
        } else {
            Ok(factory.create_application_term(shape, allocator.create_unit_list(target)))
        }
    }
}
