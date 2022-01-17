// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_json::JsonValue;
use uuid::Uuid;

#[derive(Clone, Debug)]
pub enum GrpcHandlerAction {
    ConnectSuccess(GrpcHandlerConnectSuccessAction),
    ConnectError(GrpcHandlerConnectErrorAction),
}
impl Action for GrpcHandlerAction {}
impl NamedAction for GrpcHandlerAction {
    fn name(&self) -> &'static str {
        match self {
            Self::ConnectSuccess(action) => action.name(),
            Self::ConnectError(action) => action.name(),
        }
    }
}
impl SerializableAction for GrpcHandlerAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::ConnectSuccess(action) => action.serialize(),
            Self::ConnectError(action) => action.serialize(),
        }
    }
}

impl From<GrpcHandlerConnectSuccessAction> for GrpcHandlerAction {
    fn from(value: GrpcHandlerConnectSuccessAction) -> Self {
        Self::ConnectSuccess(value)
    }
}
impl From<GrpcHandlerAction> for Option<GrpcHandlerConnectSuccessAction> {
    fn from(value: GrpcHandlerAction) -> Self {
        match value {
            GrpcHandlerAction::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerAction> for Option<&'a GrpcHandlerConnectSuccessAction> {
    fn from(value: &'a GrpcHandlerAction) -> Self {
        match value {
            GrpcHandlerAction::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GrpcHandlerConnectErrorAction> for GrpcHandlerAction {
    fn from(value: GrpcHandlerConnectErrorAction) -> Self {
        Self::ConnectError(value)
    }
}
impl From<GrpcHandlerAction> for Option<GrpcHandlerConnectErrorAction> {
    fn from(value: GrpcHandlerAction) -> Self {
        match value {
            GrpcHandlerAction::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GrpcHandlerAction> for Option<&'a GrpcHandlerConnectErrorAction> {
    fn from(value: &'a GrpcHandlerAction) -> Self {
        match value {
            GrpcHandlerAction::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
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
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(self.connection_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
        ])
    }
}

#[derive(Clone, Debug)]
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
    fn serialize(&self) -> SerializedAction {
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
