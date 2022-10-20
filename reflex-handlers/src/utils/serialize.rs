// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializedBytes(Vec<u8>);
impl<'a> From<&'a Bytes> for SerializedBytes {
    fn from(value: &'a Bytes) -> Self {
        Self(value.to_vec())
    }
}
impl From<SerializedBytes> for Bytes {
    fn from(value: SerializedBytes) -> Self {
        let SerializedBytes(inner) = value;
        Self::from(inner)
    }
}
impl<'a> From<&'a ()> for SerializedBytes {
    fn from(_: &'a ()) -> Self {
        Self(Vec::new())
    }
}
impl From<SerializedBytes> for () {
    fn from(_: SerializedBytes) -> Self {
        ()
    }
}
