// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Duration;

pub use tonic;

pub mod action;
pub mod actor;
pub mod codec;
pub mod loader;
pub mod task;
pub mod utils;

pub(crate) mod proto;

pub trait GrpcConfig {
    fn configure(&self, endpoint: tonic::transport::Endpoint) -> tonic::transport::Endpoint;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DefaultGrpcConfig;
impl GrpcConfig for DefaultGrpcConfig {
    fn configure(&self, endpoint: tonic::transport::Endpoint) -> tonic::transport::Endpoint {
        endpoint
            .keep_alive_while_idle(true)
            .tcp_keepalive(Some(Duration::from_secs(30)))
            .http2_keep_alive_interval(Duration::from_secs(30))
            .keep_alive_timeout(Duration::from_secs(20))
            .concurrency_limit(100)
    }
}
