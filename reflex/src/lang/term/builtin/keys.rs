// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory, FunctionArity,
        HeapAllocator,
    },
    lang::ValueTerm,
};

pub struct Keys {}
impl Keys {
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl<T: Expression> Applicable<T> for Keys {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        let result = if let Some(target) = factory.match_struct_term(&target) {
            Some(factory.create_vector_term(allocator.create_list(
                target.prototype().keys().iter().map(|key| {
                    factory.create_value_term(ValueTerm::String(
                        allocator.create_string(String::from(key)),
                    ))
                }),
            )))
        } else if let Some(target) = factory.match_vector_term(&target) {
            Some(
                factory.create_vector_term(
                    allocator.create_list(
                        target.items().iter().enumerate().map(|(index, _)| {
                            factory.create_value_term(ValueTerm::Int(index as i32))
                        }),
                    ),
                ),
            )
        } else if let Some(target) = factory.match_hashmap_term(&target) {
            Some(factory.create_vector_term(allocator.create_list(target.keys().iter().cloned())))
        } else {
            None
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!("Unable to enumerate keys for {}", target)),
        }
    }
}
