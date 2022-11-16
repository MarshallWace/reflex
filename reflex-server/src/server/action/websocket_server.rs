// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::Request;
use reflex::core::Uuid;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_graphql::subscriptions::{
    GraphQlSubscriptionClientMessage, GraphQlSubscriptionServerMessage,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::Named;
use serde::{Deserialize, Serialize};

use crate::{server::utils::clone_http_request_wrapper, utils::serialize::SerializedRequest};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum WebSocketServerActions {
    Connect(WebSocketServerConnectAction),
    Receive(WebSocketServerReceiveAction),
    Send(WebSocketServerSendAction),
    Disconnect(WebSocketServerDisconnectAction),
    ThrottleTimeout(WebSocketServerThrottleTimeoutAction),
}
impl Named for WebSocketServerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Connect(action) => action.name(),
            Self::Receive(action) => action.name(),
            Self::Send(action) => action.name(),
            Self::Disconnect(action) => action.name(),
            Self::ThrottleTimeout(action) => action.name(),
        }
    }
}
impl Action for WebSocketServerActions {}
impl SerializableAction for WebSocketServerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Connect(action) => action.to_json(),
            Self::Receive(action) => action.to_json(),
            Self::Send(action) => action.to_json(),
            Self::Disconnect(action) => action.to_json(),
            Self::ThrottleTimeout(action) => action.to_json(),
        }
    }
}

impl From<WebSocketServerConnectAction> for WebSocketServerActions {
    fn from(value: WebSocketServerConnectAction) -> Self {
        Self::Connect(value)
    }
}
impl From<WebSocketServerActions> for Option<WebSocketServerConnectAction> {
    fn from(value: WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Connect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerActions> for Option<&'a WebSocketServerConnectAction> {
    fn from(value: &'a WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Connect(value) => Some(value),
            _ => None,
        }
    }
}
impl From<WebSocketServerReceiveAction> for WebSocketServerActions {
    fn from(value: WebSocketServerReceiveAction) -> Self {
        Self::Receive(value)
    }
}
impl From<WebSocketServerActions> for Option<WebSocketServerReceiveAction> {
    fn from(value: WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Receive(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerActions> for Option<&'a WebSocketServerReceiveAction> {
    fn from(value: &'a WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Receive(value) => Some(value),
            _ => None,
        }
    }
}

impl From<WebSocketServerSendAction> for WebSocketServerActions {
    fn from(value: WebSocketServerSendAction) -> Self {
        Self::Send(value)
    }
}
impl From<WebSocketServerActions> for Option<WebSocketServerSendAction> {
    fn from(value: WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Send(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerActions> for Option<&'a WebSocketServerSendAction> {
    fn from(value: &'a WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Send(value) => Some(value),
            _ => None,
        }
    }
}
impl From<WebSocketServerDisconnectAction> for WebSocketServerActions {
    fn from(value: WebSocketServerDisconnectAction) -> Self {
        Self::Disconnect(value)
    }
}
impl From<WebSocketServerActions> for Option<WebSocketServerDisconnectAction> {
    fn from(value: WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Disconnect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerActions> for Option<&'a WebSocketServerDisconnectAction> {
    fn from(value: &'a WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::Disconnect(value) => Some(value),
            _ => None,
        }
    }
}

impl From<WebSocketServerThrottleTimeoutAction> for WebSocketServerActions {
    fn from(value: WebSocketServerThrottleTimeoutAction) -> Self {
        Self::ThrottleTimeout(value)
    }
}
impl From<WebSocketServerActions> for Option<WebSocketServerThrottleTimeoutAction> {
    fn from(value: WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::ThrottleTimeout(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerActions> for Option<&'a WebSocketServerThrottleTimeoutAction> {
    fn from(value: &'a WebSocketServerActions) -> Self {
        match value {
            WebSocketServerActions::ThrottleTimeout(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, Debug)]
pub struct WebSocketServerConnectAction {
    pub connection_id: Uuid,
    pub request: Request<()>,
}
impl Clone for WebSocketServerConnectAction {
    fn clone(&self) -> Self {
        Self {
            connection_id: self.connection_id.clone(),
            request: clone_http_request_wrapper(&self.request),
        }
    }
}
impl Action for WebSocketServerConnectAction {}
impl SerializableAction for WebSocketServerConnectAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "connection_id",
            JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
        )])
    }
}
impl Serialize for WebSocketServerConnectAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedWebSocketServerConnectAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for WebSocketServerConnectAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedWebSocketServerConnectAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedWebSocketServerConnectAction {
    connection_id: Uuid,
    request: SerializedRequest,
}
impl<'a> From<&'a WebSocketServerConnectAction> for SerializedWebSocketServerConnectAction {
    fn from(value: &'a WebSocketServerConnectAction) -> Self {
        let WebSocketServerConnectAction {
            connection_id,
            request,
        } = value;
        Self {
            connection_id: *connection_id,
            request: request.into(),
        }
    }
}
impl From<SerializedWebSocketServerConnectAction> for WebSocketServerConnectAction {
    fn from(value: SerializedWebSocketServerConnectAction) -> Self {
        let SerializedWebSocketServerConnectAction {
            connection_id,
            request,
        } = value;
        Self {
            connection_id,
            request: request.into(),
        }
    }
}

#[derive(Named, Clone, Debug, Serialize, Deserialize)]
pub struct WebSocketServerReceiveAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionClientMessage,
}
impl Action for WebSocketServerReceiveAction {}
impl SerializableAction for WebSocketServerReceiveAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
            ),
            ("message", self.message.clone().into_json()),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct WebSocketServerSendAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionServerMessage,
}
impl Action for WebSocketServerSendAction {}
impl SerializableAction for WebSocketServerSendAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
            ),
            (
                "message",
                match &self.message {
                    GraphQlSubscriptionServerMessage::Data(operation_id, _) => {
                        JsonValue::Object(JsonMap::from_iter([
                            (
                                String::from("type"),
                                JsonValue::String(String::from("data")),
                            ),
                            (String::from("id"), JsonValue::String(operation_id.clone())),
                        ]))
                    }
                    GraphQlSubscriptionServerMessage::Patch(operation_id, _) => {
                        JsonValue::Object(JsonMap::from_iter([
                            (
                                String::from("type"),
                                JsonValue::String(String::from("patch")),
                            ),
                            (String::from("id"), JsonValue::String(operation_id.clone())),
                        ]))
                    }
                    _ => self.message.clone().into_json(),
                },
            ),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct WebSocketServerDisconnectAction {
    pub connection_id: Uuid,
}
impl Action for WebSocketServerDisconnectAction {}
impl SerializableAction for WebSocketServerDisconnectAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "connection_id",
            JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
        )])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct WebSocketServerThrottleTimeoutAction {
    pub subscription_id: Uuid,
}
impl Action for WebSocketServerThrottleTimeoutAction {}
impl SerializableAction for WebSocketServerThrottleTimeoutAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "subscription_id",
            JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
        )])
    }
}
