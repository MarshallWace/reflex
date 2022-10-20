// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use prost::bytes::BufMut;
use reflex_protobuf::{Buf, Bytes};
use tonic::{
    codec::{Codec, DecodeBuf, Decoder, EncodeBuf, Encoder},
    Status,
};

#[derive(Debug, Clone, Copy)]
pub struct BytesCodec;
impl Codec for BytesCodec {
    type Encode = Bytes;
    type Decode = Bytes;
    type Encoder = BytesEncoder;
    type Decoder = BytesDecoder;
    fn encoder(&mut self) -> Self::Encoder {
        BytesEncoder
    }
    fn decoder(&mut self) -> Self::Decoder {
        BytesDecoder
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BytesEncoder;
impl Encoder for BytesEncoder {
    type Item = Bytes;
    type Error = Status;
    fn encode(&mut self, item: Self::Item, buf: &mut EncodeBuf<'_>) -> Result<(), Self::Error> {
        buf.put_slice(&item);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BytesDecoder;
impl Decoder for BytesDecoder {
    type Item = Bytes;
    type Error = Status;
    fn decode(&mut self, buf: &mut DecodeBuf<'_>) -> Result<Option<Self::Item>, Self::Error> {
        Ok(Some(buf.copy_to_bytes(buf.remaining())))
    }
}
