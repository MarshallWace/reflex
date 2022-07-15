// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::hash::Hash;

include!(concat!(env!("OUT_DIR"), "/protos/protobuf.rs"));

impl std::hash::Hash for self::google::protobuf::DoubleValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_f64(&self.value, state);
    }
}

impl std::hash::Hash for self::google::protobuf::FloatValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_f32(&self.value, state);
    }
}

impl std::hash::Hash for self::google::protobuf::value::Kind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            self::google::protobuf::value::Kind::NullValue(value) => value.hash(state),
            self::google::protobuf::value::Kind::NumberValue(value) => hash_f64(value, state),
            self::google::protobuf::value::Kind::StringValue(value) => value.hash(state),
            self::google::protobuf::value::Kind::BoolValue(value) => value.hash(state),
            self::google::protobuf::value::Kind::StructValue(value) => value.hash(state),
            self::google::protobuf::value::Kind::ListValue(value) => value.hash(state),
        }
    }
}

impl std::hash::Hash for self::google::protobuf::Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in self.fields.iter() {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl std::hash::Hash for self::google::protobuf::UninterpretedOption {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.identifier_value.hash(state);
        self.positive_int_value.hash(state);
        self.negative_int_value.hash(state);
        hash_optional_f64(&self.double_value, state);
        self.string_value.hash(state);
        self.aggregate_value.hash(state);
    }
}

fn hash_f32(value: &f32, state: &mut impl std::hash::Hasher) {
    state.write(&value.to_le_bytes())
}

fn hash_f64(value: &f64, state: &mut impl std::hash::Hasher) {
    state.write(&value.to_le_bytes())
}

fn hash_optional_f64(value: &Option<f64>, state: &mut impl std::hash::Hasher) {
    core::mem::discriminant(value).hash(state);
    if let Some(value) = value.as_ref() {
        hash_f64(value, state);
    }
}
