// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::str::FromStr;

use reflex::{
    core::{
        Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, NativeAllocator,
        StringValue,
    },
    lang::{create_struct, BuiltinTerm, NativeFunction, ValueTerm},
};
use uuid::Uuid;

pub fn global_json<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(
        vec![
            (
                String::from("parse"),
                factory.create_native_function_term(json_parse()),
            ),
            (
                String::from("stringify"),
                factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_native_function_term(json_stringify()),
                        allocator.create_unit_list(factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::ResolveDeep),
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

pub fn json_parse<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        JsonParse::uid(),
        JsonParse::name(),
        JsonParse::arity(),
        JsonParse::apply,
    )
}
struct JsonParse {}
impl JsonParse {
    fn uid() -> Uuid {
        Uuid::from_str("6e8468d3-34cc-423c-a787-3d3e9154885d").expect("Hardocded uuid value")
    }
    fn name() -> Option<&'static str> {
        Some("JsonParse")
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        match factory.match_value_term(&source) {
            Some(ValueTerm::String(source)) => {
                reflex_json::parse(source.as_str(), &factory, &allocator)
                    .map_err(|error| format!("JSON deserialization failed: {}", error))
            }
            _ => Err(format!(
                "Invalid JSON.parse() call: expected string argument, received {}",
                source
            )),
        }
    }
}

pub fn json_stringify<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        JsonStringify::uid(),
        JsonStringify::name(),
        JsonStringify::arity(),
        JsonStringify::apply,
    )
}
struct JsonStringify {}
impl JsonStringify {
    fn uid() -> Uuid {
        Uuid::from_str("b0a2caca-1101-402c-a21e-bccdf276e44c").expect("Hardcoded uuid value")
    }
    fn name() -> Option<&'static str> {
        Some("JsonStringify")
    }
    fn arity() -> Arity {
        Arity::from(1, 0, None)
    }
    fn apply<T: Expression>(
        args: ExpressionList<T>,
        factory: &dyn ExpressionFactory<T>,
        allocator: &dyn NativeAllocator<T>,
    ) -> Result<T, String> {
        let mut args = args.into_iter();
        let source = args.next().unwrap();
        match reflex_json::stringify(&source) {
            Ok(result) => {
                Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(result))))
            }
            Err(error) => Err(format!("JSON serialization failed: {}", error)),
        }
    }
}
