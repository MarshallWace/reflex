// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RecordTermType, RefType,
    SignalType, StringTermType, StringValue, Uid, Uuid,
};

pub struct Throw;
impl Throw {
    pub const UUID: Uuid = uuid!("fb9bef4b-da7a-46ef-af03-50ed2984274c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for Throw {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for Throw {
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
        let error = args.next().unwrap();
        if !error.is_atomic() {
            return Err(String::from(
                "Thrown exceptions cannot contain dynamic values",
            ));
        }
        let signals = if let Some(errors) = parse_aggregate_error(&error, factory, allocator) {
            allocator.create_signal_list(
                errors
                    .iter()
                    .map(|item| item.as_deref())
                    .cloned()
                    .map(|error| create_error_signal(error, factory, allocator)),
            )
        } else {
            allocator.create_signal_list(once(create_error_signal(error, factory, allocator)))
        };
        Ok(factory.create_signal_term(signals))
    }
}

fn parse_aggregate_error<'a, T: Expression + 'a>(
    target: &'a T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<&'a T::ExpressionList> {
    factory.match_record_term(target).and_then(|target| {
        target
            .as_deref()
            .get(&factory.create_string_term(allocator.create_static_string("name")))
            .map(|item| item.as_deref())
            .and_then(|error_type| {
                factory.match_string_term(error_type).and_then(|name| {
                    if name.value().as_deref().as_str() == "AggregateError" {
                        target
                            .get(
                                &factory
                                    .create_string_term(allocator.create_static_string("errors")),
                            )
                            .map(|item| item.as_deref())
                            .and_then(|errors| {
                                factory
                                    .match_list_term(errors)
                                    .map(|errors| errors.items().as_deref())
                            })
                    } else {
                        None
                    }
                })
            })
    })
}

fn create_error_signal<T: Expression>(
    error: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T::Signal {
    allocator.create_signal(SignalType::Error, error, factory.create_nil_term())
}
