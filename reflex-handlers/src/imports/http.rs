// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::{create_struct, ValueTerm},
    stdlib::Stdlib,
};
use reflex_json::stdlib::Stdlib as JsonStdlib;

use crate::{
    actor::fetch::EFFECT_TYPE_FETCH,
    stdlib::{to_request::request_prototype, Stdlib as HandlersStdlib},
};

pub fn import_http<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<HandlersStdlib>,
{
    create_struct(
        vec![
            (String::from("fetch"), import_http_fetch(factory, allocator)),
            (
                String::from("Request"),
                import_http_request(factory, allocator),
            ),
        ],
        factory,
        allocator,
    )
}

fn import_http_request<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_constructor_term(request_prototype(allocator))
}

fn import_http_fetch<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<HandlersStdlib>,
{
    factory.create_lambda_term(
        1,
        factory.create_let_term(
            factory.create_application_term(
                factory.create_builtin_term(HandlersStdlib::ToRequest),
                allocator.create_unit_list(factory.create_static_variable_term(0)),
            ),
            factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Effect),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_static_string(EFFECT_TYPE_FETCH),
                        )),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_static_string("url"),
                                )),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_static_string("method"),
                                )),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::ResolveDeep),
                            allocator.create_unit_list(factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_static_string("headers"),
                                    )),
                                ),
                            )),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_static_string("body"),
                                )),
                            ),
                        ),
                    ]),
                ),
                factory.create_struct_term(
                    allocator.create_struct_prototype(vec![
                        String::from("status"),
                        String::from("ok"),
                        String::from("text"),
                        String::from("json"),
                    ]),
                    allocator.create_list(vec![
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Lt),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_value_term(ValueTerm::Int(0)),
                                    ),
                                ),
                                factory.create_value_term(ValueTerm::Int(400)),
                            ),
                        ),
                        factory.create_lambda_term(
                            0,
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                            ),
                        ),
                        factory.create_lambda_term(
                            0,
                            factory.create_application_term(
                                factory.create_builtin_term(JsonStdlib::JsonDeserialize),
                                allocator.create_unit_list(factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_value_term(ValueTerm::Int(1)),
                                    ),
                                )),
                            ),
                        ),
                    ]),
                ),
            ),
        ),
    )
}
