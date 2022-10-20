// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use prost::Message;
use reflex_protobuf::reflection::{DynamicMessage, MessageDescriptor, MethodDescriptor};
use tonic::{
    codec::{Codec, DecodeBuf, Decoder, EncodeBuf, Encoder},
    Code, Status,
};

#[derive(Debug, Clone)]
pub struct DynamicMessageCodec {
    descriptor: MethodDescriptor,
}
impl DynamicMessageCodec {
    pub fn new(descriptor: MethodDescriptor) -> Self {
        Self { descriptor }
    }
}
impl Codec for DynamicMessageCodec {
    type Encode = DynamicMessage;
    type Decode = DynamicMessage;
    type Encoder = DynamicMessageEncoder;
    type Decoder = DynamicMessageDecoder;
    fn encoder(&mut self) -> Self::Encoder {
        DynamicMessageEncoder
    }
    fn decoder(&mut self) -> Self::Decoder {
        DynamicMessageDecoder(self.descriptor.output())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DynamicMessageEncoder;
impl Encoder for DynamicMessageEncoder {
    type Item = DynamicMessage;
    type Error = Status;
    fn encode(&mut self, item: Self::Item, buf: &mut EncodeBuf<'_>) -> Result<(), Self::Error> {
        item.encode(buf)
            .map_err(|err| Status::new(Code::DataLoss, format!("{}", err)))
    }
}

#[derive(Debug, Clone)]
pub struct DynamicMessageDecoder(MessageDescriptor);
impl Decoder for DynamicMessageDecoder {
    type Item = DynamicMessage;
    type Error = Status;
    fn decode(&mut self, buf: &mut DecodeBuf<'_>) -> Result<Option<Self::Item>, Self::Error> {
        let Self(descriptor) = &self;
        let descriptor = descriptor.clone();
        let item = DynamicMessage::decode(descriptor, buf)
            .map(Some)
            .map_err(|err| Status::new(Code::DataLoss, err.to_string()))?;
        Ok(item)
    }
}
