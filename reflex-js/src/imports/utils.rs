// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
mod types;
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
};
use types::import_types;

pub fn import_utils<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<JsStdlib>,
{
    create_struct(
        vec![
            (
                String::from("graph"),
                factory.create_lambda_term(
                    1,
                    factory.create_recursive_term(factory.create_static_variable_term(0)),
                ),
            ),
            (String::from("Types"), import_types(factory, allocator)),
        ],
        factory,
        allocator,
    )
}
