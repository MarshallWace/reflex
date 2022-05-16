// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{net::SocketAddr, time::Duration};

use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::{JsonMap, JsonValue};

use crate::cli::reflex_server::OpenTelemetryConfig;

#[derive(Clone, Debug)]
pub enum InitAction {
    PrometheusMetrics(InitPrometheusMetricsAction),
    OpenTelemetry(InitOpenTelemetryAction),
    GraphRoot(InitGraphRootAction),
    HttpServer(InitHttpServerAction),
}
impl Action for InitAction {}
impl NamedAction for InitAction {
    fn name(&self) -> &'static str {
        match self {
            Self::PrometheusMetrics(action) => action.name(),
            Self::OpenTelemetry(action) => action.name(),
            Self::GraphRoot(action) => action.name(),
            Self::HttpServer(action) => action.name(),
        }
    }
}
impl SerializableAction for InitAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::PrometheusMetrics(action) => action.serialize(),
            Self::OpenTelemetry(action) => action.serialize(),
            Self::GraphRoot(action) => action.serialize(),
            Self::HttpServer(action) => action.serialize(),
        }
    }
}

impl From<InitPrometheusMetricsAction> for InitAction {
    fn from(value: InitPrometheusMetricsAction) -> Self {
        Self::PrometheusMetrics(value)
    }
}
impl From<InitAction> for Option<InitPrometheusMetricsAction> {
    fn from(value: InitAction) -> Self {
        match value {
            InitAction::PrometheusMetrics(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a InitAction> for Option<&'a InitPrometheusMetricsAction> {
    fn from(value: &'a InitAction) -> Self {
        match value {
            InitAction::PrometheusMetrics(value) => Some(value),
            _ => None,
        }
    }
}

impl From<InitOpenTelemetryAction> for InitAction {
    fn from(value: InitOpenTelemetryAction) -> Self {
        Self::OpenTelemetry(value)
    }
}
impl From<InitAction> for Option<InitOpenTelemetryAction> {
    fn from(value: InitAction) -> Self {
        match value {
            InitAction::OpenTelemetry(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a InitAction> for Option<&'a InitOpenTelemetryAction> {
    fn from(value: &'a InitAction) -> Self {
        match value {
            InitAction::OpenTelemetry(value) => Some(value),
            _ => None,
        }
    }
}

impl From<InitGraphRootAction> for InitAction {
    fn from(value: InitGraphRootAction) -> Self {
        Self::GraphRoot(value)
    }
}
impl From<InitAction> for Option<InitGraphRootAction> {
    fn from(value: InitAction) -> Self {
        match value {
            InitAction::GraphRoot(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a InitAction> for Option<&'a InitGraphRootAction> {
    fn from(value: &'a InitAction) -> Self {
        match value {
            InitAction::GraphRoot(value) => Some(value),
            _ => None,
        }
    }
}

impl From<InitHttpServerAction> for InitAction {
    fn from(value: InitHttpServerAction) -> Self {
        Self::HttpServer(value)
    }
}
impl From<InitAction> for Option<InitHttpServerAction> {
    fn from(value: InitAction) -> Self {
        match value {
            InitAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a InitAction> for Option<&'a InitHttpServerAction> {
    fn from(value: &'a InitAction) -> Self {
        match value {
            InitAction::HttpServer(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InitPrometheusMetricsAction {
    pub address: SocketAddr,
}
impl Action for InitPrometheusMetricsAction {}
impl NamedAction for InitPrometheusMetricsAction {
    fn name(&self) -> &'static str {
        "InitPrometheusMetricsAction"
    }
}
impl SerializableAction for InitPrometheusMetricsAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("host", JsonValue::from(self.address.ip().to_string())),
            ("port", JsonValue::from(self.address.port())),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct InitOpenTelemetryAction {
    pub config: OpenTelemetryConfig,
}
impl<'a> Action for InitOpenTelemetryAction {}
impl<'a> NamedAction for InitOpenTelemetryAction {
    fn name(&self) -> &'static str {
        "InitOpenTelemetryAction"
    }
}
impl<'a> SerializableAction for InitOpenTelemetryAction {
    fn serialize(&self) -> SerializedAction {
        match &self.config {
            OpenTelemetryConfig::Http(config) => SerializedAction::from_iter([
                ("protocol", JsonValue::String(String::from("http/protobuf"))),
                (
                    "endpoint",
                    JsonValue::String(config.endpoint.as_str().into()),
                ),
                (
                    "http_headers",
                    JsonValue::Object(JsonMap::from_iter(config.http_headers.iter().map(
                        |(key, value)| {
                            (
                                String::from(key.as_str()),
                                match value.to_str() {
                                    Ok(value) => JsonValue::String(value.into()),
                                    Err(_) => JsonValue::Null,
                                },
                            )
                        },
                    ))),
                ),
                ("tls_cert", JsonValue::Bool(config.tls_cert.is_some())),
                (
                    "resource_attributes",
                    JsonValue::Object(JsonMap::from_iter(config.resource_attributes.iter().map(
                        |variable| {
                            (
                                String::from(variable.key.as_str()),
                                JsonValue::String(variable.value.as_str().into()),
                            )
                        },
                    ))),
                ),
            ]),
            OpenTelemetryConfig::Grpc(config) => SerializedAction::from_iter([
                ("protocol", JsonValue::String(String::from("grpc"))),
                (
                    "endpoint",
                    JsonValue::String(config.endpoint.as_str().into()),
                ),
                ("tls_cert", JsonValue::Bool(config.tls_cert.is_some())),
                (
                    "resource_attributes",
                    JsonValue::Object(JsonMap::from_iter(config.resource_attributes.iter().map(
                        |variable| {
                            (
                                String::from(variable.key.as_str()),
                                JsonValue::String(variable.value.as_str().into()),
                            )
                        },
                    ))),
                ),
            ]),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InitGraphRootAction {
    pub compiler_duration: Duration,
    pub instruction_count: usize,
}
impl Action for InitGraphRootAction {}
impl NamedAction for InitGraphRootAction {
    fn name(&self) -> &'static str {
        "InitGraphRootAction"
    }
}
impl SerializableAction for InitGraphRootAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "compiler_duration",
                JsonValue::from(self.compiler_duration.as_secs_f64()),
            ),
            ("instruction_count", JsonValue::from(self.instruction_count)),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct InitHttpServerAction {
    pub address: SocketAddr,
}
impl Action for InitHttpServerAction {}
impl NamedAction for InitHttpServerAction {
    fn name(&self) -> &'static str {
        "InitHttpServerAction"
    }
}
impl SerializableAction for InitHttpServerAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            ("host", JsonValue::from(self.address.ip().to_string())),
            ("port", JsonValue::from(self.address.port())),
        ])
    }
}
