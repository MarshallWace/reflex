// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Expression, ExpressionFactory, HeapAllocator};
use reflex_json::stdlib::Stdlib as JsonStdlib;
use reflex_stdlib::Stdlib;

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
    create_record(
        vec![
            (
                factory.create_string_term(allocator.create_static_string("fetch")),
                import_http_fetch(factory, allocator),
            ),
            (
                factory.create_string_term(allocator.create_static_string("Request")),
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
    factory.create_constructor_term(request_prototype(factory, allocator))
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
                allocator.create_unit_list(factory.create_variable_term(0)),
            ),
            factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(Stdlib::Effect),
                    allocator.create_list(vec![
                        factory
                            .create_string_term(allocator.create_static_string(EFFECT_TYPE_FETCH)),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_string_term(allocator.create_static_string("url")),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory
                                    .create_string_term(allocator.create_static_string("method")),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::ResolveDeep),
                            allocator.create_unit_list(factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_string_term(
                                        allocator.create_static_string("headers"),
                                    ),
                                ),
                            )),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_string_term(allocator.create_static_string("body")),
                            ),
                        ),
                    ]),
                ),
                factory.create_record_term(
                    allocator.create_struct_prototype(allocator.create_list([
                        factory.create_string_term(allocator.create_static_string("status")),
                        factory.create_string_term(allocator.create_static_string("ok")),
                        factory.create_string_term(allocator.create_static_string("text")),
                        factory.create_string_term(allocator.create_static_string("json")),
                    ])),
                    allocator.create_list(vec![
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Lt),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_int_term(0),
                                    ),
                                ),
                                factory.create_int_term(400),
                            ),
                        ),
                        factory.create_lambda_term(
                            0,
                            factory.create_application_term(
                                factory.create_builtin_term(Stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_int_term(1),
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
                                        factory.create_variable_term(0),
                                        factory.create_int_term(1),
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
