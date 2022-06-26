// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, SignalType},
    lang::create_record,
};
use types::import_types;

use crate::stdlib::Stdlib as JsStdlib;

mod types;

pub fn import_utils<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    create_record(
        vec![
            (
                String::from("graph"),
                factory.create_lambda_term(
                    1,
                    factory.create_recursive_term(factory.create_static_variable_term(0)),
                ),
            ),
            (
                String::from("hash"),
                factory.create_builtin_term(JsStdlib::Hash),
            ),
            (
                String::from("log"),
                factory.create_builtin_term(JsStdlib::Log),
            ),
            (
                String::from("pending"),
                factory.create_lambda_term(
                    0,
                    factory.create_signal_term(allocator.create_signal_list(once(
                        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
                    ))),
                ),
            ),
            (String::from("Types"), import_types(factory, allocator)),
        ],
        factory,
        allocator,
    )
}
