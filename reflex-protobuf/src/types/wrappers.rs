// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::ops::Deref;

use prost_reflect::{DynamicMessage, MessageDescriptor, Value};
use reflex::core::{
    as_integer, BooleanTermType, Expression, ExpressionFactory, FloatTermType, FloatValue,
    HeapAllocator, IntTermType, IntValue, RefType, StringTermType, StringValue,
};

use crate::{
    get_optional_message_field, utils::create_invalid_field_type_error_message, CustomType,
};

pub struct DoubleValueMessage;
impl DoubleValueMessage {
    pub const NAME: &'static str = "google.protobuf.DoubleValue";
}
impl CustomType for DoubleValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(term) = factory.match_float_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::F64(term.value()),
            ))
        } else if let Some(term) = factory.match_int_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::F64(term.value() as f64),
            ))
        } else {
            Err(format!("Expected Float or Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_float_term(Default::default())),
            Some(Value::F64(value)) => Ok(factory.create_float_term(*value as FloatValue)),
            _ => Err(format!("Expected f64, received {:?}", value)),
        }
    }
}

pub struct FloatValueMessage;
impl FloatValueMessage {
    pub const NAME: &'static str = "google.protobuf.FloatValue";
}
impl CustomType for FloatValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(term) = factory.match_float_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::F32(term.value() as f32),
            ))
        } else if let Some(term) = factory.match_int_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::F32(term.value() as f32),
            ))
        } else {
            Err(format!("Expected Float or Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_float_term(Default::default())),
            Some(Value::F32(value)) => Ok(factory.create_float_term(*value as FloatValue)),
            _ => Err(format!("Expected f32, received {:?}", value)),
        }
    }
}

pub struct Int64ValueMessage;
impl Int64ValueMessage {
    pub const NAME: &'static str = "google.protobuf.Int64Value";
}
impl CustomType for Int64ValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory
            .match_float_term(value)
            .and_then(|value| as_integer(value.value()))
        {
            Ok(create_value_message_wrapper(
                message_type,
                Value::I64(value as i64),
            ))
        } else if let Some(term) = factory.match_int_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::I64(term.value() as i64),
            ))
        } else {
            Err(format!("Expected Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_int_term(Default::default())),
            Some(Value::I64(value)) => {
                let value = *value;
                if value < IntValue::MIN as i64 || value > IntValue::MAX as i64 {
                    Err(format!("Invalid Int value: {}", value))
                } else {
                    Ok(factory.create_int_term(value as IntValue))
                }
            }
            _ => Err(format!("Expected i64, received {:?}", value)),
        }
    }
}

pub struct UInt64ValueMessage;
impl UInt64ValueMessage {
    pub const NAME: &'static str = "google.protobuf.UInt64Value";
}
impl CustomType for UInt64ValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory
            .match_float_term(value)
            .and_then(|value| as_integer(value.value()))
        {
            if value < u64::MIN as IntValue {
                Err(format!("Invalid u64 value: {}", value))
            } else {
                Ok(create_value_message_wrapper(
                    message_type,
                    Value::U64(value as u64),
                ))
            }
        } else if let Some(term) = factory.match_int_term(value) {
            let value = term.value();
            if value < (u64::MIN as IntValue) {
                Err(format!("Invalid u64 value: {}", value))
            } else {
                Ok(create_value_message_wrapper(
                    message_type,
                    Value::U64(value as u64),
                ))
            }
        } else {
            Err(format!("Expected Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_int_term(Default::default())),
            Some(Value::U64(value)) => {
                let value = *value;
                if value > IntValue::MAX as u64 {
                    Err(format!("Invalid Int value: {}", value))
                } else {
                    Ok(factory.create_int_term(value as IntValue))
                }
            }
            _ => Err(format!("Expected u64, received {:?}", value)),
        }
    }
}

pub struct Int32ValueMessage;
impl Int32ValueMessage {
    pub const NAME: &'static str = "google.protobuf.Int32Value";
}
impl CustomType for Int32ValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory
            .match_float_term(value)
            .and_then(|value| as_integer(value.value()))
        {
            Ok(create_value_message_wrapper(
                message_type,
                Value::I32(value),
            ))
        } else if let Some(term) = factory.match_int_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::I32(term.value()),
            ))
        } else {
            Err(format!("Expected Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_int_term(Default::default())),
            Some(Value::I32(value)) => Ok(factory.create_int_term(*value)),
            _ => Err(format!("Expected i32, received {:?}", value)),
        }
    }
}

pub struct UInt32ValueMessage;
impl UInt32ValueMessage {
    pub const NAME: &'static str = "google.protobuf.UInt32Value";
}
impl CustomType for UInt32ValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory
            .match_float_term(value)
            .and_then(|value| as_integer(value.value()))
        {
            if value < (u32::MIN as IntValue) {
                Err(format!("Invalid u32 value: {}", value))
            } else {
                Ok(create_value_message_wrapper(
                    message_type,
                    Value::U32(value as u32),
                ))
            }
        } else if let Some(term) = factory.match_int_term(value) {
            let value = term.value();
            if value < (u32::MIN as IntValue) {
                Err(format!("Invalid u32 value: {}", value))
            } else {
                Ok(create_value_message_wrapper(
                    message_type,
                    Value::U32(value as u32),
                ))
            }
        } else {
            Err(format!("Expected Int, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_int_term(Default::default())),
            Some(Value::U32(value)) => {
                let value = *value;
                if value > IntValue::MAX as u32 {
                    Err(format!("Invalid Int value: {}", value))
                } else {
                    Ok(factory.create_int_term(value as IntValue))
                }
            }
            _ => Err(format!("Expected u32, received {:?}", value)),
        }
    }
}

pub struct BoolValueMessage;
impl BoolValueMessage {
    pub const NAME: &'static str = "google.protobuf.BoolValue";
}
impl CustomType for BoolValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory.match_boolean_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::Bool(value.value()),
            ))
        } else {
            Err(format!("Expected Boolean, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        _allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_boolean_term(Default::default())),
            Some(Value::Bool(value)) => Ok(factory.create_boolean_term(*value)),
            _ => Err(format!("Expected bool, received {:?}", value)),
        }
    }
}

pub struct StringValueMessage;
impl StringValueMessage {
    pub const NAME: &'static str = "google.protobuf.StringValue";
}
impl CustomType for StringValueMessage {
    fn name(&self) -> &str {
        Self::NAME
    }
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String> {
        if let Some(value) = factory.match_string_term(value) {
            Ok(create_value_message_wrapper(
                message_type,
                Value::String(String::from(value.value().as_deref().as_str().deref())),
            ))
        } else {
            Err(format!("Expected String, received {}", value))
        }
    }
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String> {
        let value = get_optional_message_field(message, "value")?;
        match value {
            None => Ok(factory.create_string_term(allocator.create_static_string(""))),
            Some(Value::String(value)) => {
                Ok(factory.create_string_term(allocator.create_string(value.as_str())))
            }
            _ => Err(format!("Expected bool, received {:?}", value)),
        }
    }
}

pub struct BytesValueMessage;
impl BytesValueMessage {
    pub const NAME: &'static str = "google.protobuf.BytesValue";
}
impl CustomType for BytesValueMessage {
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

fn create_value_message_wrapper(message_type: &MessageDescriptor, value: Value) -> DynamicMessage {
    let mut message = DynamicMessage::new(message_type.clone());
    message.set_field_by_name("value", value);
    message
}

#[cfg(test)]
mod tests {
    use prost_reflect::{ReflectMessage, Value};
    use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
    use reflex_stdlib::Stdlib;

    use super::*;

    mod double_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = DoubleValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.DoubleValue");
        }

        #[test]
        fn serialize() {
            let serializer = DoubleValueMessage;
            let message_type = f64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_float_term(3.142);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::F64(3.142));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = DoubleValueMessage;
            let message_type = f64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::F64(3.142));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_float_term(3.142)),);
            }
        }
    }

    mod float_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = FloatValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.FloatValue");
        }

        #[test]
        fn serialize() {
            let serializer = FloatValueMessage;
            let message_type = f32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_float_term(3.142_f32 as f64);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::F32(3.142));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = FloatValueMessage;
            let message_type = f32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::F32(3.142));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_float_term(3.142_f32 as f64)),);
            }
        }
    }

    mod int64_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = Int64ValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.Int64Value");
        }

        #[test]
        fn serialize() {
            let serializer = Int64ValueMessage;
            let message_type = i64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_int_term(0);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I64(0));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I64(3));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(-3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I64(-3));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = Int64ValueMessage;
            let message_type = i64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I64(0));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(0)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I64(3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I64(-3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(-3)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I64(IntValue::MAX as i64 + 1));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Err(String::from("Invalid Int value: 2147483648")));
            }
        }
    }

    mod uint64_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = UInt64ValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.UInt64Value");
        }

        #[test]
        fn serialize() {
            let serializer = UInt64ValueMessage;
            let message_type = u64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_int_term(0);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U64(0));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U64(3));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(-3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(result, Err(String::from("Invalid u64 value: -3")));
            }
        }

        #[test]
        fn deserialize() {
            let serializer = UInt64ValueMessage;
            let message_type = u64::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U64(0));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(0)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U64(3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U64(IntValue::MAX as u64 + 1));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Err(String::from("Invalid Int value: 2147483648")));
            }
        }
    }

    mod int32_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = Int32ValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.Int32Value");
        }

        #[test]
        fn serialize() {
            let serializer = Int32ValueMessage;
            let message_type = i32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_int_term(0);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I32(0));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I32(3));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(-3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::I32(-3));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = Int32ValueMessage;
            let message_type = i32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I32(0));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(0)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I32(3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::I32(-3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(-3)));
            }
        }
    }

    mod uint32_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = UInt32ValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.UInt32Value");
        }

        #[test]
        fn serialize() {
            let serializer = UInt32ValueMessage;
            let message_type = u32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_int_term(0);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U32(0));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::U32(3));
                        message
                    }),
                );
            }
            {
                let value = factory.create_int_term(-3);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(result, Err(String::from("Invalid u32 value: -3")));
            }
        }

        #[test]
        fn deserialize() {
            let serializer = UInt32ValueMessage;
            let message_type = u32::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U32(0));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(0)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U32(3));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_int_term(3)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::U32(IntValue::MAX as u32 + 1));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Err(String::from("Invalid Int value: 2147483648")));
            }
        }
    }

    mod bool_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = BoolValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.BoolValue");
        }

        #[test]
        fn serialize() {
            let serializer = BoolValueMessage;
            let message_type = bool::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            {
                let value = factory.create_boolean_term(false);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::Bool(false));
                        message
                    }),
                );
            }
            {
                let value = factory.create_boolean_term(true);
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::Bool(true));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = BoolValueMessage;
            let message_type = bool::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::Bool(false));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_boolean_term(false)));
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::Bool(true));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(result, Ok(factory.create_boolean_term(true)));
            }
        }
    }

    mod string_value {
        use super::*;

        #[test]
        fn name() {
            let serializer = StringValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.StringValue");
        }

        #[test]
        fn serialize() {
            let serializer = StringValueMessage;
            let message_type = String::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let value = factory.create_string_term(allocator.create_string(""));
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::String(String::from("")));
                        message
                    }),
                );
            }
            {
                let value = factory.create_string_term(allocator.create_string("foo"));
                let result = serializer.serialize(&value, &message_type, &factory);
                assert_eq!(
                    result,
                    Ok({
                        let mut message = DynamicMessage::new(message_type.clone());
                        message.set_field_by_name("value", Value::String(String::from("foo")));
                        message
                    }),
                );
            }
        }

        #[test]
        fn deserialize() {
            let serializer = StringValueMessage;
            let message_type = String::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::String(String::from("")));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(
                    result,
                    Ok(factory.create_string_term(allocator.create_string("")))
                );
            }
            {
                let message = {
                    let mut message = DynamicMessage::new(message_type.clone());
                    message.set_field_by_name("value", Value::String(String::from("foo")));
                    message
                };
                let result = serializer.deserialize(&message, &factory, &allocator);
                assert_eq!(
                    result,
                    Ok(factory.create_string_term(allocator.create_string("foo")))
                );
            }
        }
    }

    mod bytes_value {
        use bytes::Bytes;

        use super::*;

        #[test]
        fn name() {
            let serializer = BytesValueMessage;
            assert_eq!(serializer.name(), "google.protobuf.BytesValue");
        }

        #[test]
        fn serialize() {
            let serializer = BytesValueMessage;
            let message_type = Bytes::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let value = factory.create_string_term(allocator.create_string("foo"));
            let result = serializer.serialize(&value, &message_type, &factory);
            assert_eq!(
                result,
                Err(String::from("Unsupported type: google.protobuf.BytesValue")),
            );
        }

        #[test]
        fn deserialize() {
            let serializer = BytesValueMessage;
            let message_type = Bytes::default().descriptor();
            let factory = SharedTermFactory::<Stdlib>::default();
            let allocator = DefaultAllocator::<CachedSharedTerm<Stdlib>>::default();
            let message = {
                let mut message = DynamicMessage::new(message_type.clone());
                message.set_field_by_name("value", Value::Bytes(Bytes::from("foo")));
                message
            };
            let result = serializer.deserialize(&message, &factory, &allocator);
            assert_eq!(
                result,
                Err(String::from("Unsupported type: google.protobuf.BytesValue"))
            );
        }
    }
}
