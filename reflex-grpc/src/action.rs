// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use uuid::Uuid;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GrpcHandlerActions {
    ConnectSuccess(GrpcHandlerConnectSuccessAction),
    ConnectError(GrpcHandlerConnectErrorAction),
    TransportError(GrpcHandlerTransportErrorAction),
}
impl Action for GrpcHandlerActions {}
impl NamedAction for GrpcHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::ConnectSuccess(action) => action.name(),
            Self::ConnectError(action) => action.name(),
            Self::TransportError(action) => action.name(),
        }
    }
}
impl SerializableAction for GrpcHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::ConnectSuccess(action) => action.to_json(),
            Self::ConnectError(action) => action.to_json(),
            Self::TransportError(action) => action.to_json(),
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
    pub error: String,
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
            ("error", JsonValue::String(self.error.clone())),
        ])
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GrpcHandlerTransportErrorAction {
    pub connection_id: Uuid,
    pub url: String,
    pub method: String,
    pub input: JsonValue,
    pub error: String,
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
            ("method", JsonValue::String(self.method.clone())),
            ("input", self.input.clone()),
            ("error", JsonValue::String(self.error.clone())),
        ])
    }
}
