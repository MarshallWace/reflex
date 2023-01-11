// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use serde::{ser::SerializeSeq, Serialize};

pub fn serialize<'a, S>(value: &'a Vec<Bytes>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut seq = serializer.serialize_seq(Some(value.len()))?;
    for item in value.iter() {
        seq.serialize_element(&BytesSerializer(item))?;
    }
    seq.end()
}
pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<Bytes>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let items: Vec<Box<[u8]>> = serde::Deserialize::deserialize(deserializer)?;
    Ok(items.into_iter().map(Bytes::from).collect())
}

struct BytesSerializer<'a>(&'a Bytes);
impl<'a> Serialize for BytesSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let Self(bytes) = self;
        serializer.serialize_bytes(bytes)
    }
}
