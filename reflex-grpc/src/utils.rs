// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    error::Error,
    hash::Hasher,
    time::Duration,
};

use futures::{future, FutureExt, Stream, StreamExt};
use prost::{DecodeError, Message};
use reflex_protobuf::{load_proto_library, reflection::MethodDescriptor, ProtoLibraryError};
use reflex_utils::reconnect::ReconnectTimeout;
use tonic::{Code, Status};

use crate::proto::google::protobuf::FileDescriptorSet;

pub(crate) type TransportError = hyper::Error;
pub fn get_transport_error(status: &Status) -> Option<&TransportError> {
    status.source()?.downcast_ref()
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) struct ProtoId(u64);
impl std::fmt::Display for ProtoId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(value) = self;
        write!(f, "0x{:x}", value)
    }
}
impl From<u64> for ProtoId {
    fn from(value: u64) -> Self {
        Self(value)
    }
}
impl Into<u64> for ProtoId {
    fn into(self) -> u64 {
        let Self(value) = self;
        value
    }
}

pub(crate) fn load_proto_descriptor(proto: &[u8]) -> Result<FileDescriptorSet, DecodeError> {
    FileDescriptorSet::decode(proto)
}

pub(crate) fn get_proto_checksum(proto: &FileDescriptorSet) -> ProtoId {
    // TODO: Traverse imports when computing protobuf schema hash
    ProtoId(hash_object(&proto))
}

fn hash_object(value: &impl std::hash::Hash) -> u64 {
    let mut state = DefaultHasher::new();
    value.hash(&mut state);
    state.finish()
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub(crate) struct GrpcServiceName(pub(crate) String);
impl GrpcServiceName {
    pub(crate) fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    pub(crate) fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl std::fmt::Display for GrpcServiceName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub(crate) struct GrpcMethodName(pub(crate) String);
impl GrpcMethodName {
    pub fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    pub fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl std::fmt::Display for GrpcMethodName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct GrpcMethod {
    pub(crate) descriptor: MethodDescriptor,
}

#[derive(Clone, Debug)]
pub struct GrpcServiceLibrary {
    protos: HashMap<ProtoId, HashMap<GrpcServiceName, HashMap<GrpcMethodName, GrpcMethod>>>,
}
impl GrpcServiceLibrary {
    pub(crate) fn get<'a>(
        &'a self,
        proto_id: &ProtoId,
        service: &GrpcServiceName,
        method: &GrpcMethodName,
    ) -> Option<&GrpcMethod> {
        let proto = self.protos.get(proto_id)?;
        let service = proto.get(service)?;
        service.get(method)
    }
    pub fn load<'a>(
        compiled_protos: impl IntoIterator<Item = &'a [u8]>,
    ) -> Result<Self, ProtoLibraryError> {
        Ok(GrpcServiceLibrary {
            protos: compiled_protos
                .into_iter()
                .map(|bytes| {
                    let proto_id = {
                        let proto =
                            load_proto_descriptor(bytes).map_err(ProtoLibraryError::Decode)?;
                        get_proto_checksum(&proto)
                    };
                    let parsed_proto = load_proto_library(bytes)?;
                    Ok((
                        proto_id,
                        parsed_proto
                            .services()
                            .map(|service| {
                                (
                                    GrpcServiceName(String::from(service.name())),
                                    service
                                        .methods()
                                        .map(|method| {
                                            (
                                                GrpcMethodName(String::from(method.name())),
                                                GrpcMethod { descriptor: method },
                                            )
                                        })
                                        .collect::<HashMap<_, _>>(),
                                )
                            })
                            .collect::<HashMap<_, _>>(),
                    ))
                })
                .collect::<Result<HashMap<_, _>, _>>()?,
        })
    }
}

pub fn ignore_repeated_grpc_errors<V: Send>(
    stream: impl Stream<Item = Result<V, Status>>,
    retry: impl ReconnectTimeout,
) -> impl Stream<Item = Result<V, Status>> {
    stream
        .scan(
            None,
            move |existing_err: &mut Option<(Code, String, usize)>, result: Result<V, Status>| {
                let repeated_error =
                    existing_err
                        .take()
                        .and_then(|(code, message, repeat_count)| match &result {
                            Err(err) if err.code() == code && err.message() == message.as_str() => {
                                Some((code, message, repeat_count))
                            }
                            _ => None,
                        });
                if let Some((code, message, repeat_count)) = repeated_error {
                    *existing_err = Some((code, message, repeat_count + 1));
                    tokio::time::sleep(
                        retry
                            .duration(repeat_count)
                            .unwrap_or(Duration::from_millis(1)),
                    )
                    .map(|_| Some(None))
                    .left_future()
                } else {
                    let (err, result) = match result {
                        Ok(payload) => (None, Ok(payload)),
                        Err(err) => (Some((err.code(), String::from(err.message()), 0)), Err(err)),
                    };
                    *existing_err = err;
                    future::ready(Some(Some(result))).right_future()
                }
            },
        )
        .filter_map(|result| future::ready(result))
}
