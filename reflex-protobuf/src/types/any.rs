// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use prost_reflect::{DynamicMessage, MessageDescriptor};
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

use crate::{utils::create_invalid_field_type_error_message, CustomType};

pub struct AnyMessage;
impl AnyMessage {
    pub const NAME: &'static str = "google.protobuf.Any";
}
impl CustomType for AnyMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        _value: &T,
        _message_type: &MessageDescriptor,
        _factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        Err(create_invalid_field_type_error_message(self.name()))
    }
    fn deserialize<T: Expression>(
        &self,
        _message: &DynamicMessage,
        _factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        Err(create_invalid_field_type_error_message(self.name()))
    }
}

#[cfg(test)]
mod tests {
    use bytes::Bytes;
    use prost_reflect::{prost_types::Any, ReflectMessage, Value};
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    #[test]
    fn name() {
        assert_eq!(AnyMessage.name(), "google.protobuf.Any");
    }

    #[test]
    fn serialize() {
        let serializer = AnyMessage;
        let message_type = Any::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let value = factory.create_string_term(allocator.create_string("0s"));
        let result = serializer.serialize(&value, &message_type, &factory);
        assert_eq!(
            result,
            Err(String::from("Unsupported type: google.protobuf.Any")),
        );
    }

    #[test]
    fn deserialize() {
        let serializer = AnyMessage;
        let message_type = Any::default().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let message = {
            let mut message = DynamicMessage::new(message_type);
            message.set_field_by_name(
                "type_url",
                Value::String(String::from("http://example.com/")),
            );
            message.set_field_by_name("value", Value::Bytes(Bytes::from_static(b"foo")));
            message
        };
        let result = serializer.deserialize(&message, &factory, &allocator);
        assert_eq!(
            result,
            Err(String::from("Unsupported type: google.protobuf.Any")),
        );
    }
}
