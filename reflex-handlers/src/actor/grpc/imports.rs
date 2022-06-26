// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
    stdlib::Stdlib,
};

use super::{schema::GrpcServiceId, EFFECT_TYPE_GRPC};

pub fn create_grpc_import<'a, T: Expression>(
    protocol_id: GrpcServiceId,
    methods: impl IntoIterator<Item = &'a str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T
where
    T::Builtin: From<Stdlib>,
{
    factory.create_lambda_term(
        1,
        create_record(
            methods.into_iter().map(|method_name| {
                (
                    String::from(method_name),
                    factory.create_lambda_term(
                        2,
                        factory.create_application_term(
                            factory.create_builtin_term(Stdlib::Effect),
                            allocator.create_list(vec![
                                factory.create_string_term(
                                    allocator.create_string(String::from(EFFECT_TYPE_GRPC)),
                                ),
                                factory.create_int_term(protocol_id.as_i32()),
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(2),
                                        factory.create_string_term(
                                            allocator.create_string(String::from("url")),
                                        ),
                                    ),
                                ),
                                factory.create_string_term(
                                    allocator.create_string(String::from(method_name)),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::ResolveDeep),
                                    allocator
                                        .create_unit_list(factory.create_static_variable_term(1)),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::If),
                                    allocator.create_triple(
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Contains),
                                            allocator.create_pair(
                                                factory.create_static_variable_term(0),
                                                factory.create_string_term(
                                                    allocator
                                                        .create_string(String::from("accumulate")),
                                                ),
                                            ),
                                        ),
                                        factory.create_application_term(
                                            factory.create_builtin_term(Stdlib::Get),
                                            allocator.create_pair(
                                                factory.create_static_variable_term(0),
                                                factory.create_string_term(
                                                    allocator
                                                        .create_string(String::from("accumulate")),
                                                ),
                                            ),
                                        ),
                                        factory.create_nil_term(),
                                    ),
                                ),
                                factory.create_application_term(
                                    factory.create_builtin_term(Stdlib::Get),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_string_term(
                                            allocator.create_string(String::from("token")),
                                        ),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                )
            }),
            factory,
            allocator,
        ),
    )
}
