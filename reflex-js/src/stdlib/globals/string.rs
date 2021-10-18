// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::str::FromStr;

use reflex::{
    core::{
        ArgType, Arity, Expression, ExpressionFactory, ExpressionList, FunctionArity,
        HeapAllocator, NativeAllocator, StringValue,
    },
    lang::{NativeFunction, ValueTerm},
};
use uuid::Uuid;

use crate::builtins::to_string;

pub fn global_string<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_native_function_term(to_string())
}

pub fn global_encode_uri_component<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_native_function_term(encode_uri_component())
}

pub fn encode_uri_component<T: Expression>() -> NativeFunction<T> {
    NativeFunction::new_with_uuid(
        EncodeUriComponent::uid(),
        EncodeUriComponent::name(),
        EncodeUriComponent::arity(),
        EncodeUriComponent::apply,
    )
}
struct EncodeUriComponent {}
impl EncodeUriComponent {
    const NAME: &'static str = "EncodeUriComponent";
    const UUID: &'static str = "ad730068-31a7-49a4-ae09-ecbda0c9914a";
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
        let input = args.next().unwrap();
        match factory.match_value_term(&input) {
            Some(ValueTerm::String(value)) => {
                let value = urlencoding::encode(value.as_str());
                Ok(factory.create_value_term(ValueTerm::String(allocator.create_string(value))))
            }
            _ => Err(format!("Expected String, received {}", input)),
        }
    }
}
