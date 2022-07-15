// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::borrow::Cow;

use bytes::Bytes;
use prost::{DecodeError, Message};
use prost_reflect::{
    prost_types::FileDescriptorSet, DescriptorError, DescriptorPool, DynamicMessage,
    ReflectMessage, Value,
};

use crate::ProtoLibrary;

#[derive(Debug)]
pub enum ProtoLibraryError {
    Decode(DecodeError),
    Parse(DescriptorError),
}
impl std::error::Error for ProtoLibraryError {}
impl std::fmt::Display for ProtoLibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decode(err) => std::fmt::Display::fmt(err, f),
            Self::Parse(err) => std::fmt::Display::fmt(err, f),
        }
    }
}
pub fn load_proto_library(bytes: &[u8]) -> Result<ProtoLibrary, ProtoLibraryError> {
    let buffer = Bytes::from(bytes.iter().copied().collect::<Vec<_>>());
    let mut file_set = FileDescriptorSet::decode(buffer).map_err(ProtoLibraryError::Decode)?;
    strip_proto_source_locations(&mut file_set);
    let mut protos = DescriptorPool::new();
    protos
        .add_file_descriptor_set(file_set)
        .map_err(ProtoLibraryError::Parse)?;
    Ok(ProtoLibrary(protos))
}

fn strip_proto_source_locations(proto: &mut FileDescriptorSet) {
    for proto in proto.file.iter_mut() {
        proto.source_code_info = None;
    }
}

pub(crate) fn create_invalid_field_type_error_message(type_name: &str) -> String {
    format!("Unsupported type: {}", type_name)
}

pub(crate) fn get_optional_message_field<'a>(
    message: &'a DynamicMessage,
    field_name: &str,
) -> Result<Option<&'a Value>, String> {
    let message_type = message.descriptor();
    match message_type.get_field_by_name(field_name) {
        None => Err(format!(
            "Invalid field access: {} on {}",
            field_name,
            message_type.name()
        )),
        Some(field_type) => match message.get_field(&field_type) {
            Cow::Borrowed(value) => Ok(Some(value)),
            Cow::Owned(_) => Ok(None),
        },
    }
}

pub(crate) fn get_message_field<'a>(
    message: &'a DynamicMessage,
    field_name: &str,
) -> Result<&'a Value, String> {
    get_optional_message_field(message, field_name).and_then(|value| {
        value.ok_or_else(|| {
            let message_type = message.descriptor();
            format!(
                "Missing value for required field: {} on {}",
                field_name,
                message_type.name()
            )
        })
    })
}

pub(crate) enum CountedIteratorItems<T, TMany: Iterator<Item = T>> {
    None,
    One(T),
    Many(TMany),
}

pub(crate) fn count_iterator_items<T>(
    source: impl IntoIterator<Item = T>,
) -> CountedIteratorItems<T, impl Iterator<Item = T>> {
    let mut remaining = source.into_iter();
    match remaining.next() {
        None => CountedIteratorItems::None,
        Some(first) => match remaining.next() {
            None => CountedIteratorItems::One(first),
            Some(second) => {
                CountedIteratorItems::Many([first, second].into_iter().chain(remaining))
            }
        },
    }
}

pub(crate) mod nom {
    // See https://github.com/Geal/nom/issues/1422
    pub(crate) fn map_err<I, O, E1, E2>(
        mut parser: impl nom::Parser<I, O, E1>,
        mut f: impl FnMut(E1) -> E2,
    ) -> impl nom::Parser<I, O, E2> {
        move |input: I| match parser.parse(input) {
            Ok((remainder, value)) => Ok((remainder, value)),
            Err(err) => Err(err.map(|e| f(e))),
        }
    }
}
