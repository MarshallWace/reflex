// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::str::FromStr;

use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        HeapAllocator, NativeAllocator, StructPrototype,
    },
    lang::{create_struct, BuiltinTerm, NativeFunction, ValueTerm},
};
use uuid::Uuid;

use crate::stdlib::globals::json::json_parse;

pub fn import_http<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
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

fn import_http_fetch<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_lambda_term(
        1,
        factory.create_let_term(
            factory.create_application_term(
                factory.create_native_function_term(to_request()),
                allocator.create_unit_list(factory.create_static_variable_term(0)),
            ),
            factory.create_let_term(
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Effect),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::String(
                            allocator.create_string(String::from("reflex::http::fetch")),
                        )),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("url")),
                                )),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("method")),
                                )),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::ResolveDeep),
                            allocator.create_unit_list(factory.create_application_term(
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::String(
                                        allocator.create_string(String::from("headers")),
                                    )),
                                ),
                            )),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::String(
                                    allocator.create_string(String::from("body")),
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
                            factory.create_builtin_term(BuiltinTerm::Get),
                            allocator.create_pair(
                                factory.create_static_variable_term(0),
                                factory.create_value_term(ValueTerm::Int(0)),
                            ),
                        ),
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Lt),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Get),
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
                                factory.create_builtin_term(BuiltinTerm::Get),
                                allocator.create_pair(
                                    factory.create_static_variable_term(0),
                                    factory.create_value_term(ValueTerm::Int(1)),
                                ),
                            ),
                        ),
                        factory.create_lambda_term(
                            0,
                            factory.create_application_term(
                                factory.create_native_function_term(json_parse()),
                                allocator.create_unit_list(factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Get),
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

fn import_http_request<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_constructor_term(http_request_prototype(allocator))
}

fn http_request_prototype<T: Expression>(allocator: &impl HeapAllocator<T>) -> StructPrototype {
    allocator.create_struct_prototype(vec![
        String::from("url"),
        String::from("method"),
        String::from("headers"),
        String::from("body"),
    ])
}

pub fn to_request<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        ToRequest::uid(),
        ToRequest::name(),
        ToRequest::arity(),
        ToRequest::apply,
    )
}
struct ToRequest {}
impl ToRequest {
    const NAME: &'static str = "ToRequest";
    const UUID: &'static str = "29d89369-0b7b-41df-aa14-47ea708a8fa6";
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    fn uid() -> Uuid {
        Uuid::from_str(Self::UUID).unwrap()
    }
    fn name() -> Option<&'static str> {
        Some(Self::NAME)
    }
    fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let result = match factory.match_value_term(&target) {
            Some(ValueTerm::String(_)) => {
                let url = target.clone();
                let method = factory.create_value_term(ValueTerm::String(
                    allocator.create_string(String::from("GET")),
                ));
                let headers = factory.create_struct_term(
                    allocator.create_struct_prototype(Vec::new()),
                    allocator.create_empty_list(),
                );
                let body = factory.create_value_term(ValueTerm::Null);
                Some(factory.create_struct_term(
                    http_request_prototype(&allocator),
                    allocator.create_list(vec![url, method, headers, body]),
                ))
            }
            _ => http_request_prototype(&allocator).parse_struct(&target, &factory, &allocator),
        };
        match result {
            Some(result) => Ok(result),
            None => Err(format!(
                "Invalid Request constructor init object: {}",
                target
            )),
        }
    }
}
