// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{create_record, Builtin, Expression, ExpressionFactory, HeapAllocator};
use reflex_macros::blanket_trait;
use reflex_stdlib::stdlib;

use crate::{actor::fetch::EFFECT_TYPE_FETCH, stdlib::to_request::request_prototype};

blanket_trait!(
    pub trait HttpImportBuiltin:
        Builtin
        + From<crate::stdlib::ToRequest>
        + From<reflex_json::stdlib::JsonDeserialize>
        + From<stdlib::CollectList>
        + From<stdlib::Contains>
        + From<stdlib::Effect>
        + From<stdlib::Get>
        + From<stdlib::If>
        + From<stdlib::Lt>
        + From<stdlib::ResolveDeep>
    {
    }
);

pub fn import_http<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: HttpImportBuiltin,
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
    T::Builtin: Builtin
        + From<crate::stdlib::ToRequest>
        + From<reflex_json::stdlib::JsonDeserialize>
        + From<stdlib::CollectList>
        + From<stdlib::Contains>
        + From<stdlib::Effect>
        + From<stdlib::Get>
        + From<stdlib::If>
        + From<stdlib::Lt>
        + From<stdlib::ResolveDeep>,
{
    factory.create_lambda_term(
        1,
        factory.create_let_term(
            factory.create_application_term(
                factory.create_builtin_term(crate::stdlib::ToRequest),
                allocator.create_unit_list(factory.create_variable_term(0)),
            ),
            factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(stdlib::Effect),
                    allocator.create_triple(
                        factory
                            .create_string_term(allocator.create_static_string(EFFECT_TYPE_FETCH)),
                        factory.create_application_term(
                            factory.create_builtin_term(stdlib::CollectList),
                            allocator.create_list([
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("url"),
                                        ),
                                    ),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("method"),
                                        ),
                                    ),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::ResolveDeep),
                                    allocator.create_unit_list(factory.create_application_term(
                                        factory.create_builtin_term(stdlib::Get),
                                        allocator.create_pair(
                                            factory.create_variable_term(0),
                                            factory.create_string_term(
                                                allocator.create_static_string("headers"),
                                            ),
                                        ),
                                    )),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("body"),
                                        ),
                                    ),
                                ),
                            ]),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(stdlib::If),
                            allocator.create_triple(
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Contains),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("token"),
                                        ),
                                    ),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_static_string("token"),
                                        ),
                                    ),
                                ),
                                factory.create_nil_term(),
                            ),
                        ),
                    ),
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
                            factory.create_builtin_term(stdlib::Get),
                            allocator.create_pair(
                                factory.create_variable_term(0),
                                factory.create_int_term(0),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(stdlib::Lt),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
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
                                factory.create_builtin_term(stdlib::Get),
                                allocator.create_pair(
                                    factory.create_variable_term(0),
                                    factory.create_int_term(1),
                                ),
                            ),
                        ),
                        factory.create_lambda_term(
                            0,
                            factory.create_application_term(
                                factory.create_builtin_term(reflex_json::stdlib::JsonDeserialize),
                                allocator.create_unit_list(factory.create_application_term(
                                    factory.create_builtin_term(stdlib::Get),
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
