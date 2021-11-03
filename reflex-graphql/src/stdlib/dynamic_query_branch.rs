// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as GraphQlStdlib;
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, Uid, Uuid,
    },
    lang::ValueTerm,
    stdlib::Stdlib,
};

pub struct DynamicQueryBranch {}
impl DynamicQueryBranch {
    const UUID: Uuid = uuid!("58dd19b7-c9f0-473b-84c5-34af607c176b");
    const ARITY: FunctionArity<2, 0> = FunctionArity {
        required: [ArgType::Strict, ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for DynamicQueryBranch {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for DynamicQueryBranch
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
        if let Some(ValueTerm::Null) = factory.match_value_term(&target) {
            Ok(target)
        } else if let Some(list) = factory.match_vector_term(&target) {
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::CollectVector),
                allocator.create_list(list.items().iter().map(|item| {
                    factory.create_application_term(
                        factory.create_builtin_term(GraphQlStdlib::DynamicQueryBranch),
                        allocator.create_pair(item.clone(), shape.clone()),
                    )
                })),
            ))
        } else {
            Ok(factory.create_application_term(shape, allocator.create_unit_list(target)))
        }
    }
}
