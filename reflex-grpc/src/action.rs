// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::HashMap;

use hyper::{header::HeaderName, http::HeaderValue, HeaderMap};
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::json;
use reflex_protobuf::Bytes;
use reflex_utils::serialize::bytes as serialize_bytes;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use tonic::{metadata::MetadataMap, Code, Status};
use uuid::Uuid;

mod serialize_code {
    use tonic::Code;
    pub fn serialize<'a, S>(value: &'a Code, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_i32(i32::from(*value))
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Code, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: i32 = serde::Deserialize::deserialize(deserializer)?;
        Ok(value.into())
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcStatus {
    #[serde(with = "serialize_code")]
    pub code: Code,
    pub message: String,
}
impl GrpcStatus {
    pub fn to_json(&self) -> JsonValue {
        json!({
            "code": i32::from(self.code),
            "message": self.message,
        })
    }
}
impl From<Status> for GrpcStatus {
    fn from(value: Status) -> Self {
        Self {
            code: value.code(),
            message: String::from(value.message()),
        }
    }
}
impl From<GrpcStatus> for Status {
    fn from(value: GrpcStatus) -> Self {
        let GrpcStatus { code, message } = value;
        Self::new(code, message)
    }
}

#[derive(Default, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcMetadata {
    headers: HashMap<String, String>,
}
impl GrpcMetadata {
    fn to_json(&self) -> JsonValue {
        json!({
            "headers": self.headers,
        })
    }
}
impl FromIterator<(String, String)> for GrpcMetadata {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        Self {
            headers: iter.into_iter().collect(),
        }
    }
}
impl From<MetadataMap> for GrpcMetadata {
    fn from(value: MetadataMap) -> Self {
        let headers = value.into_headers();
        Self {
            headers: headers
                .into_iter()
                .filter_map(|(key, value)| {
                    key.and_then(move |key| {
                        value
                            .to_str()
                            .ok()
                            .map(move |value| (String::from(key.as_str()), String::from(value)))
                    })
                })
                .collect(),
        }
    }
}
impl From<GrpcMetadata> for MetadataMap {
    fn from(value: GrpcMetadata) -> Self {
        let GrpcMetadata { headers } = value;
        Self::from_headers(HeaderMap::from_iter(headers.into_iter().filter_map(
            |(key, value)| {
                HeaderName::try_from(key)
                    .ok()
                    .and_then(|key| HeaderValue::try_from(value).ok().map(|value| (key, value)))
            },
        )))
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum GrpcHandlerActions {
    ConnectSuccess(GrpcHandlerConnectSuccessAction),
    ConnectError(GrpcHandlerConnectErrorAction),
    RequestStart(GrpcHandlerRequestStartAction),
    RequestStop(GrpcHandlerRequestStopAction),
    SuccessResponse(GrpcHandlerSuccessResponseAction),
    ErrorResponse(GrpcHandlerErrorResponseAction),
    TransportError(GrpcHandlerTransportErrorAction),
    AbortRequest(GrpcHandlerAbortRequestAction),
    ConnectionTerminate(GrpcHandlerConnectionTerminateAction),
}
impl Action for GrpcHandlerActions {}
impl NamedAction for GrpcHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::ConnectSuccess(action) => action.name(),
            Self::ConnectError(action) => action.name(),
            Self::RequestStart(action) => action.name(),
            Self::RequestStop(action) => action.name(),
            Self::SuccessResponse(action) => action.name(),
            Self::ErrorResponse(action) => action.name(),
            Self::TransportError(action) => action.name(),
            Self::AbortRequest(action) => action.name(),
            Self::ConnectionTerminate(action) => action.name(),
        }
    }
}
impl SerializableAction for GrpcHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::ConnectSuccess(action) => action.to_json(),
            Self::ConnectError(action) => action.to_json(),
            Self::RequestStart(action) => action.to_json(),
            Self::RequestStop(action) => action.to_json(),
            Self::SuccessResponse(action) => action.to_json(),
            Self::ErrorResponse(action) => action.to_json(),
            Self::TransportError(action) => action.to_json(),
            Self::AbortRequest(action) => action.to_json(),
            Self::ConnectionTerminate(action) => action.to_json(),
        }
    }
}

impl From<GrpcHandlerConnectSuccessAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerConnectSuccessAction) -> Self {
        Self::ConnectSuccess(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerConnectSuccessAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerConnectSuccessAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerConnectErrorAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerConnectErrorAction) -> Self {
        Self::ConnectError(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerConnectErrorAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerConnectErrorAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerRequestStartAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerRequestStartAction) -> Self {
        Self::RequestStart(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerRequestStartAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::RequestStart(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerRequestStartAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::RequestStart(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerRequestStopAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerRequestStopAction) -> Self {
        Self::RequestStop(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerRequestStopAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::RequestStop(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerRequestStopAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::RequestStop(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerSuccessResponseAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerSuccessResponseAction) -> Self {
        Self::SuccessResponse(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerSuccessResponseAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::SuccessResponse(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerSuccessResponseAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::SuccessResponse(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerErrorResponseAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerErrorResponseAction) -> Self {
        Self::ErrorResponse(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerErrorResponseAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ErrorResponse(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerErrorResponseAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ErrorResponse(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerTransportErrorAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerTransportErrorAction) -> Self {
        Self::TransportError(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerTransportErrorAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::TransportError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerTransportErrorAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::TransportError(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerAbortRequestAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerAbortRequestAction) -> Self {
        Self::AbortRequest(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerAbortRequestAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::AbortRequest(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerAbortRequestAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::AbortRequest(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerConnectionTerminateAction> for GrpcHandlerActions {
    fn from(value: GrpcHandlerConnectionTerminateAction) -> Self {
        Self::ConnectionTerminate(value)
    }
}
impl From<GrpcHandlerActions> for Option<GrpcHandlerConnectionTerminateAction> {
    fn from(value: GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectionTerminate(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerActions> for Option<&'a GrpcHandlerConnectionTerminateAction> {
    fn from(value: &'a GrpcHandlerActions) -> Self {
        match value {
            GrpcHandlerActions::ConnectionTerminate(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerConnectSuccessAction {
    pub connection_id: Uuid,
    pub url: String,
}
impl Action for GrpcHandlerConnectSuccessAction {}
impl NamedAction for GrpcHandlerConnectSuccessAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareConnectSuccessAction"
    }
}
impl SerializableAction for GrpcHandlerConnectSuccessAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerConnectErrorAction {
    pub connection_id: Uuid,
    pub url: String,
    pub message: String,
}
impl Action for GrpcHandlerConnectErrorAction {}
impl NamedAction for GrpcHandlerConnectErrorAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareConnectErrorAction"
    }
}
impl SerializableAction for GrpcHandlerConnectErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
            ("message", JsonValue::String(self.message.clone())),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerRequestStartAction {
    pub connection_id: Uuid,
    pub url: String,
    pub operation_id: Uuid,
    pub service_name: String,
    pub method_name: String,
    pub method_path: String,
    pub streaming: bool,
    pub input: JsonValue,
    pub metadata: GrpcMetadata,
    #[serde(with = "serialize_bytes")]
    pub message: Bytes,
}
impl Action for GrpcHandlerRequestStartAction {}
impl NamedAction for GrpcHandlerRequestStartAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareRequestStartAction"
    }
}
impl SerializableAction for GrpcHandlerRequestStartAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
            ("service", JsonValue::String(self.service_name.clone())),
            ("method", JsonValue::String(self.method_name.clone())),
            ("path", JsonValue::String(self.method_path.clone())),
            ("streaming", JsonValue::Bool(self.streaming)),
            (
                "input",
                serde_json::to_value(self.input.clone())
                    .unwrap_or(JsonValue::Object(Default::default())),
            ),
            ("metadata", self.metadata.to_json()),
            ("content_length", JsonValue::from(self.message.len())),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerRequestStopAction {
    pub connection_id: Uuid,
    pub url: String,
    pub operation_id: Uuid,
    pub service_name: String,
    pub method_name: String,
    pub method_path: String,
    pub input: JsonValue,
    pub metadata: GrpcMetadata,
}
impl Action for GrpcHandlerRequestStopAction {}
impl NamedAction for GrpcHandlerRequestStopAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareRequestStopAction"
    }
}
impl SerializableAction for GrpcHandlerRequestStopAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
            ("service", JsonValue::String(self.service_name.clone())),
            ("method", JsonValue::String(self.method_name.clone())),
            ("path", JsonValue::String(self.method_path.clone())),
            (
                "input",
                serde_json::to_value(self.input.clone())
                    .unwrap_or(JsonValue::Object(Default::default())),
            ),
            ("metadata", self.metadata.to_json()),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerSuccessResponseAction {
    pub connection_id: Uuid,
    pub operation_id: Uuid,
    #[serde(with = "serialize_bytes")]
    pub data: Bytes,
}
impl Action for GrpcHandlerSuccessResponseAction {}
impl NamedAction for GrpcHandlerSuccessResponseAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareSuccessResponseAction"
    }
}
impl SerializableAction for GrpcHandlerSuccessResponseAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
            ("content_length", JsonValue::from(self.data.len())),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerErrorResponseAction {
    pub connection_id: Uuid,
    pub url: String,
    pub operation_id: Uuid,
    pub service_name: String,
    pub method_name: String,
    pub method_path: String,
    pub input: JsonValue,
    pub metadata: GrpcMetadata,
    pub status: GrpcStatus,
}
impl Action for GrpcHandlerErrorResponseAction {}
impl NamedAction for GrpcHandlerErrorResponseAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareErrorResponseAction"
    }
}
impl SerializableAction for GrpcHandlerErrorResponseAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
            ("service", JsonValue::String(self.service_name.clone())),
            ("method", JsonValue::String(self.method_name.clone())),
            ("path", JsonValue::String(self.method_path.clone())),
            (
                "input",
                serde_json::to_value(self.input.clone())
                    .unwrap_or(JsonValue::Object(Default::default())),
            ),
            ("metadata", self.metadata.to_json()),
            ("status", self.status.to_json()),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerTransportErrorAction {
    pub connection_id: Uuid,
    pub url: String,
    pub operation_id: Uuid,
    pub service_name: String,
    pub method_name: String,
    pub method_path: String,
    pub input: JsonValue,
    pub metadata: GrpcMetadata,
    pub status: GrpcStatus,
}
impl Action for GrpcHandlerTransportErrorAction {}
impl NamedAction for GrpcHandlerTransportErrorAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareTransportErrorAction"
    }
}
impl SerializableAction for GrpcHandlerTransportErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
            ("service", JsonValue::String(self.service_name.clone())),
            ("method", JsonValue::String(self.method_name.clone())),
            ("path", JsonValue::String(self.method_path.clone())),
            (
                "input",
                serde_json::to_value(self.input.clone())
                    .unwrap_or(JsonValue::Object(Default::default())),
            ),
            ("metadata", self.metadata.to_json()),
            ("status", self.status.to_json()),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerAbortRequestAction {
    pub connection_id: Uuid,
    pub operation_id: Uuid,
}
impl Action for GrpcHandlerAbortRequestAction {}
impl NamedAction for GrpcHandlerAbortRequestAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareAbortRequestAction"
    }
}
impl SerializableAction for GrpcHandlerAbortRequestAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            (
                "operation_id",
                JsonValue::String(self.operation_id.to_string()),
            ),
        ])
    }
}

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerConnectionTerminateAction {
    pub connection_id: Uuid,
    pub url: String,
}
impl Action for GrpcHandlerConnectionTerminateAction {}
impl NamedAction for GrpcHandlerConnectionTerminateAction {
    fn name(&self) -> &'static str {
        "GrpcMiddlewareConnectionTerminateAction"
    }
}
impl SerializableAction for GrpcHandlerConnectionTerminateAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::String(self.connection_id.to_string()),
            ),
            ("url", JsonValue::String(self.url.clone())),
        ])
    }
}
