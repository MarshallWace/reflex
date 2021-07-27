// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::any::TypeId;

use reflex::{
    core::{
        Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, NativeAllocator,
        StringValue,
    },
    hash::{hash_object, HashId},
    lang::{NativeFunction, ValueTerm},
};

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
    NativeFunction::new(
        EncodeUriComponent::uid(),
        EncodeUriComponent::name(),
        EncodeUriComponent::arity(),
        EncodeUriComponent::apply,
    )
}
struct EncodeUriComponent {}
impl EncodeUriComponent {
    fn uid() -> HashId {
        hash_object(&TypeId::of::<Self>())
    }
    fn name() -> Option<&'static str> {
        Some("EncodeUriComponent")
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
