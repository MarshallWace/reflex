// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};

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
                factory.create_string_term(allocator.create_static_string("Error")),
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
) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    // TODO: Add optional JS AggregateError override message
    factory.create_lambda_term(
        1,
        create_struct(
            once((
                String::from("name"),
                factory.create_string_term(allocator.create_static_string("AggregateError")),
            ))
            .chain(once((
                String::from("message"),
                factory.create_application_term(
                    factory.create_builtin_term(JsStdlib::FormatErrorMessage),
                    allocator.create_unit_list(factory.create_static_variable_term(0)),
                ),
            )))
            .chain(once((
                String::from("errors"),
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::ResolveVector),
                    allocator.create_unit_list(factory.create_static_variable_term(0)),
                ),
            ))),
            factory,
            allocator,
        ),
    )
}
