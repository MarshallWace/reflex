// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{path::Path, time::Duration};

use prost::DecodeError;
use reflex_protobuf::{reflection::DescriptorError, ProtoLibraryError};
pub use tonic;
use tonic::transport::ClientTlsConfig;
use utils::GrpcServiceLibrary;

pub mod action;
pub mod actor;
pub mod codec;
pub mod loader;
pub mod task;
pub mod utils;

pub(crate) mod proto;

pub trait GrpcConfig {
    type ConfigError: std::error::Error;
    fn configure(
        &self,
        endpoint: tonic::transport::Endpoint,
    ) -> Result<tonic::transport::Endpoint, Self::ConfigError>;
}

#[derive(Clone, Debug, Default)]
pub struct DefaultGrpcConfig {
    pub user_agent: Option<String>,
    pub timeout: Option<Duration>,
    pub concurrency_limit: Option<usize>,
    pub rate_limit: Option<(u64, Duration)>,
    pub tls_cert: Option<Vec<u8>>,
    pub initial_stream_window_size: Option<u32>,
    pub initial_connection_window_size: Option<u32>,
    pub tcp_keepalive: Option<Duration>,
    pub tcp_nodelay: bool,
    pub http2_keep_alive_interval: Option<Duration>,
    pub http2_keep_alive_timeout: Option<Duration>,
    pub http2_keep_alive_while_idle: Option<bool>,
    pub connect_timeout: Option<Duration>,
    pub http2_adaptive_window: Option<bool>,
}
impl DefaultGrpcConfig {
    pub fn user_agent(mut self, value: Option<String>) -> Self {
        self.user_agent = value;
        self
    }
    pub fn timeout(mut self, value: Option<Duration>) -> Self {
        self.timeout = value;
        self
    }
    pub fn concurrency_limit(mut self, value: Option<usize>) -> Self {
        self.concurrency_limit = value;
        self
    }
    pub fn rate_limit(mut self, value: Option<(u64, Duration)>) -> Self {
        self.rate_limit = value;
        self
    }
    pub fn tls_cert(mut self, value: Option<Vec<u8>>) -> Self {
        self.tls_cert = value;
        self
    }
    pub fn initial_stream_window_size(mut self, value: Option<u32>) -> Self {
        self.initial_stream_window_size = value;
        self
    }
    pub fn initial_connection_window_size(mut self, value: Option<u32>) -> Self {
        self.initial_connection_window_size = value;
        self
    }
    pub fn tcp_keepalive(mut self, value: Option<Duration>) -> Self {
        self.tcp_keepalive = value;
        self
    }
    pub fn tcp_nodelay(mut self, value: bool) -> Self {
        self.tcp_nodelay = value;
        self
    }
    pub fn http2_keep_alive_interval(mut self, value: Option<Duration>) -> Self {
        self.http2_keep_alive_interval = value;
        self
    }
    pub fn http2_keep_alive_timeout(mut self, value: Option<Duration>) -> Self {
        self.http2_keep_alive_timeout = value;
        self
    }
    pub fn http2_keep_alive_while_idle(mut self, value: Option<bool>) -> Self {
        self.http2_keep_alive_while_idle = value;
        self
    }
    pub fn connect_timeout(mut self, value: Option<Duration>) -> Self {
        self.connect_timeout = value;
        self
    }
    pub fn http2_adaptive_window(mut self, value: Option<bool>) -> Self {
        self.http2_adaptive_window = value;
        self
    }
}
impl GrpcConfig for DefaultGrpcConfig {
    type ConfigError = tonic::transport::Error;
    fn configure(
        &self,
        endpoint: tonic::transport::Endpoint,
    ) -> Result<tonic::transport::Endpoint, Self::ConfigError> {
        let endpoint = if let Some(user_agent) = self.user_agent.as_ref() {
            endpoint.user_agent(user_agent)
        } else {
            Ok(endpoint)
        }?;
        let endpoint = if let Some(timeout) = self.timeout {
            endpoint.timeout(timeout)
        } else {
            endpoint
        };
        let endpoint = if let Some(concurrency_limit) = self.concurrency_limit {
            endpoint.concurrency_limit(concurrency_limit)
        } else {
            endpoint
        };
        let endpoint = if let Some((limit, duration)) = self.rate_limit {
            endpoint.rate_limit(limit, duration)
        } else {
            endpoint
        };
        let endpoint = if let Some(tls_cert) = self.tls_cert.as_ref() {
            endpoint.tls_config(
                ClientTlsConfig::new()
                    .ca_certificate(tonic::transport::Certificate::from_pem(tls_cert)),
            )
        } else {
            Ok(endpoint)
        }?;
        let endpoint = if let Some(initial_stream_window_size) = self.initial_stream_window_size {
            endpoint.initial_stream_window_size(Some(initial_stream_window_size))
        } else {
            endpoint
        };
        let endpoint =
            if let Some(initial_connection_window_size) = self.initial_connection_window_size {
                endpoint.initial_connection_window_size(Some(initial_connection_window_size))
            } else {
                endpoint
            };
        let endpoint = if let Some(tcp_keepalive) = self.tcp_keepalive {
            endpoint.tcp_keepalive(Some(tcp_keepalive))
        } else {
            endpoint
        };
        let endpoint = if self.tcp_nodelay {
            endpoint.tcp_nodelay(true)
        } else {
            endpoint
        };
        let endpoint = if let Some(http2_keep_alive_interval) = self.http2_keep_alive_interval {
            endpoint.http2_keep_alive_interval(http2_keep_alive_interval)
        } else {
            endpoint
        };
        let endpoint = if let Some(http2_keep_alive_timeout) = self.http2_keep_alive_timeout {
            endpoint.keep_alive_timeout(http2_keep_alive_timeout)
        } else {
            endpoint
        };
        let endpoint = if let Some(http2_keep_alive_while_idle) = self.http2_keep_alive_while_idle {
            endpoint.keep_alive_while_idle(http2_keep_alive_while_idle)
        } else {
            endpoint
        };
        let endpoint = if let Some(connect_timeout) = self.connect_timeout {
            endpoint.connect_timeout(connect_timeout)
        } else {
            endpoint
        };
        let endpoint = if let Some(http2_adaptive_window) = self.http2_adaptive_window {
            endpoint.http2_adaptive_window(http2_adaptive_window)
        } else {
            endpoint
        };
        Ok(endpoint)
    }
}

#[derive(Debug)]
pub enum GrpcServiceLoaderError {
    Load(std::io::Error),
    Decode(DecodeError),
    Parse(DescriptorError),
}
impl std::error::Error for GrpcServiceLoaderError {}
impl std::fmt::Display for GrpcServiceLoaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load(err) => std::fmt::Display::fmt(err, f),
            Self::Decode(err) => std::fmt::Display::fmt(err, f),
            Self::Parse(err) => std::fmt::Display::fmt(err, f),
        }
    }
}

pub fn load_grpc_services(
    paths: impl IntoIterator<Item = impl AsRef<Path>>,
) -> Result<GrpcServiceLibrary, GrpcServiceLoaderError> {
    let protos = paths
        .into_iter()
        .map(|path| std::fs::read(path.as_ref()))
        .collect::<Result<Vec<_>, _>>()
        .map_err(GrpcServiceLoaderError::Load)?;
    GrpcServiceLibrary::load(protos.iter().map(|proto| proto.as_slice())).map_err(|err| match err {
        ProtoLibraryError::Decode(err) => GrpcServiceLoaderError::Decode(err),
        ProtoLibraryError::Parse(err) => GrpcServiceLoaderError::Parse(err),
    })
}
