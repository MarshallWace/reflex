// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::empty;

use prost_reflect::{DynamicMessage, MessageDescriptor};
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    lang::create_record,
};

use crate::CustomType;

pub struct EmptyMessage;
impl EmptyMessage {
    pub const NAME: &'static str = "google.protobuf.Empty";
}
impl CustomType for EmptyMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(_) = factory.match_record_term(value) {
            Ok(DynamicMessage::new(message_type.clone()))
        } else {
            Err(format!("Expected <struct>, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        _message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        Ok(create_record(empty(), factory, allocator))
    }
}

#[cfg(test)]
mod tests {
    use reflex::{
        allocator::DefaultAllocator,
        lang::{CachedSharedTerm, SharedTermFactory},
        stdlib::Stdlib,
    };

    use prost_reflect::ReflectMessage;

    use super::*;

    #[test]
    fn name() {
        assert_eq!(EmptyMessage.name(), "google.protobuf.Empty");
    }

    #[test]
    fn serialize() {
        let serializer = EmptyMessage;
        let message_type = ().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let value = create_record(empty(), &factory, &allocator);
        let result = serializer.serialize(&value, &message_type, &factory);
        assert_eq!(result, Ok(DynamicMessage::new(message_type.clone())),);
    }

    #[test]
    fn deserialize() {
        let serializer = EmptyMessage;
        let message_type = ().descriptor();
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
        let message = DynamicMessage::new(message_type.clone());
        let result = serializer.deserialize(&message, &factory, &allocator);
        assert_eq!(result, Ok(create_record(empty(), &factory, &allocator)));
    }
}
