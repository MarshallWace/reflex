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

pub struct FlattenDeep {}
impl FlattenDeep {
    const UUID: Uuid = uuid!("52299fbb-a84b-488e-81ef-98c439869f74");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for FlattenDeep {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for FlattenDeep
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
        if let Some(ValueTerm::Null) = factory.match_value_term(&target) {
            Ok(target)
        } else if let Some(list) = factory.match_vector_term(&target) {
            Ok(factory.create_application_term(
                factory.create_builtin_term(Stdlib::CollectVector),
                allocator.create_list(list.items().iter().map(|item| {
                    factory.create_application_term(
                        factory.create_builtin_term(GraphQlStdlib::FlattenDeep),
                        allocator.create_unit_list(item.clone()),
                    )
                })),
            ))
        } else {
            Ok(target)
        }
    }
}