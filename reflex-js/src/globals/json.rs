// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::stdlib::Stdlib as JsStdlib;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_struct,
    stdlib::Stdlib,
};

pub fn global_json<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    create_struct(
        vec![
            (
                String::from("parse"),
                factory.create_builtin_term(JsStdlib::JsonParse),
            ),
            (
                String::from("stringify"),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(JsStdlib::JsonStringify),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_builtin_term(Stdlib::ResolveDeep),
                            allocator.create_unit_list(factory.create_static_variable_term(0)),
                        )),
                    ),
                ),
            ),
        ],
        factory,
        allocator,
    )
}
