// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use deserialize::deserialize_generic_message;
use prost_reflect::MapKey;
use prost_reflect::{DescriptorPool, DynamicMessage, MessageDescriptor};
use reflection::{EnumDescriptor, ServiceDescriptor};
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};

use self::serialize::serialize_generic_message;

pub use bytes::{Buf, Bytes};
pub use prost::{DecodeError, EncodeError, Message};
pub use prost_reflect as reflection;
pub use prost_reflect::prost_types as well_known_types;

mod deserialize;
mod serialize;
mod utils;

pub mod types;

pub use self::utils::*;

#[derive(Clone, Default, Debug)]
pub struct ProtoLibrary(DescriptorPool);
impl ProtoLibrary {
    pub fn services(&self) -> impl ExactSizeIterator<Item = ServiceDescriptor> + '_ {
        self.as_inner().services()
    }
    pub fn messages(&self) -> impl ExactSizeIterator<Item = MessageDescriptor> + '_ {
        self.as_inner().all_messages()
    }
    pub fn enums(&self) -> impl ExactSizeIterator<Item = EnumDescriptor> + '_ {
        self.as_inner().all_enums()
    }
    pub fn as_inner(&self) -> &DescriptorPool {
        let Self(inner) = self;
        inner
    }
}

pub trait ProtoTranscoder {
    fn serialize_message<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<DynamicMessage, TranscodeError>;
    fn deserialize_message<T: Expression>(
        &self,
        message: &DynamicMessage,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, TranscodeError>;
}

#[derive(Clone, Copy, Debug)]
pub struct GenericTranscoder;
impl ProtoTranscoder for GenericTranscoder {
    fn serialize_message<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<DynamicMessage, TranscodeError> {
        serialize_generic_message(value, message_type, transcoder, factory, allocator)
    }
    fn deserialize_message<T: Expression>(
        &self,
        message: &DynamicMessage,
        transcoder: &impl ProtoTranscoder,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, TranscodeError> {
        deserialize_generic_message(message, transcoder, factory, allocator)
    }
}

pub trait CustomType {
    fn name(&self) -> &str;
    fn serialize<T: Expression>(
        &self,
        value: &T,
        message_type: &MessageDescriptor,
        factory: &impl ExpressionFactory<T>,
    ) -> Result<DynamicMessage, String>;
    fn deserialize<T: Expression>(
        &self,
        message: &DynamicMessage,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Result<T, String>;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TranscodeError {
    message_type: Option<MessageDescriptor>,
    error: String,
    path: ErrorPath,
}
impl TranscodeError {
    fn with_path_prefix(self, key: PathSegment) -> Self {
        let Self {
            message_type,
            error,
            path,
        } = self;
        Self {
            message_type,
            error,
            path: path.with_prefix(key),
        }
    }
}
impl From<String> for TranscodeError {
    fn from(error: String) -> Self {
        Self {
            message_type: Default::default(),
            error,
            path: Default::default(),
        }
    }
}
impl std::fmt::Display for TranscodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}{}",
            self.path,
            if let Some(message_type) = self.message_type.as_ref() {
                format!(" [{}] ", message_type.name())
            } else {
                String::new()
            },
            self.error
        )
    }
}

#[derive(Default, PartialEq, Eq, Clone, Debug, Hash)]
pub struct ErrorPath(Vec<PathSegment>);
impl ErrorPath {
    pub fn new(key: PathSegment) -> Self {
        Self(vec![key])
    }
    pub fn with_prefix(self, key: PathSegment) -> Self {
        let Self(mut path) = self;
        path.push(key);
        Self(path)
    }
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = &'a PathSegment> + 'a {
        let Self(path) = self;
        path.iter().rev()
    }
}
impl IntoIterator for ErrorPath {
    type Item = PathSegment;
    type IntoIter = std::iter::Rev<std::vec::IntoIter<PathSegment>>;
    fn into_iter(self) -> Self::IntoIter {
        let Self(path) = self;
        path.into_iter().rev()
    }
}
impl std::fmt::Display for ErrorPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(path) = self;
        write!(
            f,
            "{}",
            path.iter()
                .rev()
                .map(|key| format!("{}", key))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum PathSegment {
    Key(String),
    List(usize),
    Map(MapKey),
}
impl From<String> for PathSegment {
    fn from(value: String) -> Self {
        Self::Key(value)
    }
}
impl<'a> From<&'a str> for PathSegment {
    fn from(value: &'a str) -> Self {
        Self::Key(value.into())
    }
}
impl From<usize> for PathSegment {
    fn from(value: usize) -> Self {
        Self::List(value)
    }
}
impl From<MapKey> for PathSegment {
    fn from(value: MapKey) -> Self {
        Self::Map(value)
    }
}
impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Key(value) => std::fmt::Display::fmt(value, f),
            Self::List(value) => std::fmt::Display::fmt(value, f),
            Self::Map(value) => format_map_key(value, f),
        }
    }
}
fn format_map_key(value: &MapKey, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match value {
        MapKey::Bool(value) => write!(f, "{}", value),
        MapKey::I32(value) => write!(f, "{}", value),
        MapKey::I64(value) => write!(f, "{}", value),
        MapKey::U32(value) => write!(f, "{}", value),
        MapKey::U64(value) => write!(f, "{}", value),
        MapKey::String(value) => write!(f, "{}", value),
    }
}

pub enum SerializationError<'a> {
    InvalidMessageName(&'a str),
    TranscodeError(TranscodeError),
}

pub fn serialize_message<'a, T: Expression>(
    value: &T,
    message_type: &'a str,
    protos: &ProtoLibrary,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<Vec<u8>, SerializationError<'a>> {
    let message_type = protos
        .as_inner()
        .get_message_by_name(message_type)
        .ok_or_else(|| SerializationError::InvalidMessageName(message_type))?;
    let dynamic_message = transcoder
        .serialize_message(value, &message_type, transcoder, factory, allocator)
        .map_err(SerializationError::TranscodeError)?;
    Ok(dynamic_message.encode_to_vec())
}

pub enum DeserializationError<'a> {
    InvalidMessageName(&'a str),
    DecodeProtosError(crate::DecodeError),
    TranscodeError(TranscodeError),
}
pub fn deserialize_message<'a, T: Expression>(
    message: &[u8],
    message_type: &'a str,
    protos: &ProtoLibrary,
    transcoder: &impl ProtoTranscoder,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, DeserializationError<'a>> {
    let message_proto = protos
        .as_inner()
        .get_message_by_name(message_type)
        .ok_or_else(|| DeserializationError::InvalidMessageName(message_type))?;
    let dynamic_message = DynamicMessage::decode(message_proto, message)
        .map_err(DeserializationError::DecodeProtosError)?;
    transcoder
        .deserialize_message(&dynamic_message, transcoder, factory, allocator)
        .map_err(DeserializationError::TranscodeError)
}
