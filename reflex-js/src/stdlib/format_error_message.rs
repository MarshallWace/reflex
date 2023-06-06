// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::ops::Deref;

use reflex::core::{
    uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
    ExpressionListType, FunctionArity, HeapAllocator, ListTermType, RecordTermType, RefType,
    StringTermType, StringValue, Uid, Uuid,
};

use super::format_value;

const UNKNOWN_ERROR_MESSAGE: &'static str = "Unknown error";

pub struct FormatErrorMessage;
impl FormatErrorMessage {
    pub const UUID: Uuid = uuid!("3f88d05d-47f1-4b49-b16e-b8dc2f4ee61c");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: Some(ArgType::Lazy),
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for FormatErrorMessage {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for FormatErrorMessage {
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
        let operand = args.next().unwrap();
        let message = parse_error_message(&operand, factory, allocator)
            .or_else(|| {
                if let Some(value) = factory.match_list_term(&operand) {
                    let max_displayed_errors = 10;
                    let num_errors = value.items().as_deref().len();
                    let items = value.items();
                    let messages = items
                        .as_deref()
                        .iter()
                        .take(if num_errors > max_displayed_errors {
                            max_displayed_errors - 1
                        } else {
                            num_errors
                        })
                        .map(|item| {
                            parse_error_message(&item, factory, allocator)
                                .unwrap_or_else(|| String::from(UNKNOWN_ERROR_MESSAGE))
                        });
                    Some(
                        messages
                            .chain(if num_errors > max_displayed_errors {
                                Some(format!(
                                    "...{} more errors",
                                    num_errors - max_displayed_errors - 1
                                ))
                            } else {
                                None
                            })
                            .collect::<Vec<_>>()
                            .join("\n"),
                    )
                } else {
                    None
                }
            })
            .unwrap_or_else(|| String::from(UNKNOWN_ERROR_MESSAGE));
        Ok(factory.create_string_term(allocator.create_string(message)))
    }
}

fn parse_error_message<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<String> {
    if let Some(value) = format_value(target, factory) {
        Some(value)
    } else if let Some(value) = factory.match_record_term(&target) {
        value
            .as_deref()
            .get(&factory.create_string_term(allocator.create_static_string("message")))
            .and_then(|value| {
                let value = value.as_deref();
                factory
                    .match_string_term(value)
                    .map(|message| String::from(message.value().as_deref().as_str().deref()))
            })
    } else {
        None
    }
}
