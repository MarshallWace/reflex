// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionList, FunctionArity, HeapAllocator, Signal, SignalType, StringValue, Uid, Uuid,
};

pub struct Throw {}
impl Throw {
    pub(crate) const UUID: Uuid = uuid!("fb9bef4b-da7a-46ef-af03-50ed2984274c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
}
impl Uid for Throw {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Throw {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(&Self::ARITY))
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
        let error = args.next().unwrap();
        if !error.is_static() {
            return Err(String::from(
                "Thrown exceptions cannot contain dynamic values",
            ));
        }
        let signals = if let Some(errors) = parse_aggregate_error(&error, factory) {
            allocator.create_signal_list(
                errors
                    .iter()
                    .cloned()
                    .map(|error| create_error_signal(error, allocator)),
            )
        } else {
            allocator.create_signal_list(once(create_error_signal(error, allocator)))
        };
        Ok(factory.create_signal_term(signals))
    }
}

fn parse_aggregate_error<'a, T: Expression + 'a>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a ExpressionList<T>> {
    factory.match_struct_term(target).and_then(|target| {
        target.get("name").and_then(|error_type| {
            factory.match_value_term(error_type).and_then(|error_type| {
                error_type.match_string().and_then(|name| {
                    if name.as_str() == "AggregateError" {
                        target.get("errors").and_then(|errors| {
                            factory
                                .match_vector_term(errors)
                                .map(|errors| errors.items())
                        })
                    } else {
                        None
                    }
                })
            })
        })
    })
}

fn create_error_signal<T: Expression>(error: T, allocator: &impl HeapAllocator<T>) -> Signal<T> {
    allocator.create_signal(SignalType::Error, allocator.create_unit_list(error))
}
