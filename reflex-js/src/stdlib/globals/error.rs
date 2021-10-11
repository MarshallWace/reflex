// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, BuiltinTerm, ValueTerm},
};

use crate::builtins::format_error_message;

pub fn global_error<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    // TODO: Add optional JS Error data argument
    factory.create_lambda_term(
        1,
        create_struct(
            once((
                String::from("name"),
                factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("Error")),
                )),
            ))
            .chain(once((
                String::from("message"),
                factory.create_static_variable_term(0),
            ))),
            factory,
            allocator,
        ),
    )
}

pub fn global_aggregate_error<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    // TODO: Add optional JS AggregateError override message
    factory.create_lambda_term(
        1,
        create_struct(
            once((
                String::from("name"),
                factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("AggregateError")),
                )),
            ))
            .chain(once((
                String::from("message"),
                factory.create_application_term(
                    factory.create_native_function_term(format_error_message()),
                    allocator.create_unit_list(factory.create_static_variable_term(0)),
                ),
            )))
            .chain(once((
                String::from("errors"),
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::ResolveVector),
                    allocator.create_unit_list(factory.create_static_variable_term(0)),
                ),
            ))),
            factory,
            allocator,
        ),
    )
}
