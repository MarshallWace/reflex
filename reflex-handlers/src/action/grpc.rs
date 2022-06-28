// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum GrpcHandlerActions {
    ConnectSuccess(GrpcHandlerConnectSuccessAction),
    ConnectError(GrpcHandlerConnectErrorAction),
}
impl Action for GrpcHandlerActions {}
impl NamedAction for GrpcHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::ConnectSuccess(action) => action.name(),
            Self::ConnectError(action) => action.name(),
        }
    }
}
impl SerializableAction for GrpcHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::ConnectSuccess(action) => action.to_json(),
            Self::ConnectError(action) => action.to_json(),
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

#[derive(Clone, Debug, Serialize, Deserialize)]
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
                JsonValue::from(self.connection_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
        ])
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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
                JsonValue::from(self.connection_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("error", JsonValue::from(self.error.clone())),
        ])
    }
}
