// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_stdlib::ResolveList;

use crate::stdlib::FormatErrorMessage;

pub fn global_error<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    // TODO: Add optional JS Error data argument
    factory.create_lambda_term(
        1,
        create_record(
            once((
                factory.create_string_term(allocator.create_static_string("name")),
                factory.create_string_term(allocator.create_static_string("Error")),
            ))
            .chain(once((
                factory.create_string_term(allocator.create_static_string("message")),
                factory.create_variable_term(0),
            ))),
            factory,
            allocator,
        ),
    )
}

pub fn global_aggregate_error<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<FormatErrorMessage> + From<ResolveList>,
{
    // TODO: Add optional JS AggregateError override message
    factory.create_lambda_term(
        1,
        create_record(
            once((
                factory.create_string_term(allocator.create_static_string("name")),
                factory.create_string_term(allocator.create_static_string("AggregateError")),
            ))
            .chain(once((
                factory.create_string_term(allocator.create_static_string("message")),
                factory.create_application_term(
                    factory.create_builtin_term(FormatErrorMessage),
                    allocator.create_unit_list(factory.create_variable_term(0)),
                ),
            )))
            .chain(once((
                factory.create_string_term(allocator.create_static_string("errors")),
                factory.create_application_term(
                    factory.create_builtin_term(ResolveList),
                    allocator.create_unit_list(factory.create_variable_term(0)),
                ),
            ))),
            factory,
            allocator,
        ),
    )
}
