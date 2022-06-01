// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::{
    core::{
        uuid, Applicable, ArgType, Arity, EvaluationCache, Expression, ExpressionFactory,
        FunctionArity, HeapAllocator, StringValue, Uid, Uuid,
    },
    lang::ValueTerm,
};

use super::format_value;

const UNKNOWN_ERROR_MESSAGE: &'static str = "Unknown error";

pub struct FormatErrorMessage {}
impl FormatErrorMessage {
    pub(crate) const UUID: Uuid = uuid!("3f88d05d-47f1-4b49-b16e-b8dc2f4ee61c");
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
        let message = parse_error_message(&operand, factory)
            .or_else(|| {
                if let Some(value) = factory.match_vector_term(&operand) {
                    let max_displayed_errors = 10;
                    let num_errors = value.items().len();
                    let messages = value
                        .items()
                        .iter()
                        .take(if num_errors > max_displayed_errors {
                            max_displayed_errors - 1
                        } else {
                            num_errors
                        })
                        .map(|item| {
                            parse_error_message(item, factory)
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
        Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(message))))
    }
}

fn parse_error_message<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    if let Some(value) = factory.match_value_term(&target) {
        Some(format_value(value))
    } else if let Some(value) = factory.match_struct_term(&target) {
        value.get("message").and_then(|value| {
            factory.match_value_term(value).and_then(|value| {
                value
                    .match_string()
                    .map(|value| String::from(value.as_str()))
            })
        })
    } else {
        None
    }
}
