// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::Request;
use reflex::core::Uuid;
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_graphql::subscriptions::{
    GraphQlSubscriptionClientMessage, GraphQlSubscriptionServerMessage,
};
use reflex_json::{JsonMap, JsonValue};

use crate::server::utils::clone_request_wrapper;

#[derive(Clone, Debug)]
pub enum WebSocketServerAction {
    Connect(WebSocketServerConnectAction),
    Receive(WebSocketServerReceiveAction),
    Send(WebSocketServerSendAction),
    Disconnect(WebSocketServerDisconnectAction),
    ThrottleTimeout(WebSocketServerThrottleTimeoutAction),
}
impl Action for WebSocketServerAction {}
impl NamedAction for WebSocketServerAction {
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
impl SerializableAction for WebSocketServerAction {
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Connect(action) => action.serialize(),
            Self::Receive(action) => action.serialize(),
            Self::Send(action) => action.serialize(),
            Self::Disconnect(action) => action.serialize(),
            Self::ThrottleTimeout(action) => action.serialize(),
        }
    }
}

impl From<WebSocketServerConnectAction> for WebSocketServerAction {
    fn from(value: WebSocketServerConnectAction) -> Self {
        Self::Connect(value)
    }
}
impl From<WebSocketServerAction> for Option<WebSocketServerConnectAction> {
    fn from(value: WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Connect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerAction> for Option<&'a WebSocketServerConnectAction> {
    fn from(value: &'a WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Connect(value) => Some(value),
            _ => None,
        }
    }
}
impl From<WebSocketServerReceiveAction> for WebSocketServerAction {
    fn from(value: WebSocketServerReceiveAction) -> Self {
        Self::Receive(value)
    }
}
impl From<WebSocketServerAction> for Option<WebSocketServerReceiveAction> {
    fn from(value: WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Receive(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerAction> for Option<&'a WebSocketServerReceiveAction> {
    fn from(value: &'a WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Receive(value) => Some(value),
            _ => None,
        }
    }
}

impl From<WebSocketServerSendAction> for WebSocketServerAction {
    fn from(value: WebSocketServerSendAction) -> Self {
        Self::Send(value)
    }
}
impl From<WebSocketServerAction> for Option<WebSocketServerSendAction> {
    fn from(value: WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Send(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerAction> for Option<&'a WebSocketServerSendAction> {
    fn from(value: &'a WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Send(value) => Some(value),
            _ => None,
        }
    }
}
impl From<WebSocketServerDisconnectAction> for WebSocketServerAction {
    fn from(value: WebSocketServerDisconnectAction) -> Self {
        Self::Disconnect(value)
    }
}
impl From<WebSocketServerAction> for Option<WebSocketServerDisconnectAction> {
    fn from(value: WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Disconnect(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerAction> for Option<&'a WebSocketServerDisconnectAction> {
    fn from(value: &'a WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::Disconnect(value) => Some(value),
            _ => None,
        }
    }
}

impl From<WebSocketServerThrottleTimeoutAction> for WebSocketServerAction {
    fn from(value: WebSocketServerThrottleTimeoutAction) -> Self {
        Self::ThrottleTimeout(value)
    }
}
impl From<WebSocketServerAction> for Option<WebSocketServerThrottleTimeoutAction> {
    fn from(value: WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::ThrottleTimeout(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a WebSocketServerAction> for Option<&'a WebSocketServerThrottleTimeoutAction> {
    fn from(value: &'a WebSocketServerAction) -> Self {
        match value {
            WebSocketServerAction::ThrottleTimeout(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct WebSocketServerConnectAction {
    pub connection_id: Uuid,
    pub request: Request<()>,
}
impl Clone for WebSocketServerConnectAction {
    fn clone(&self) -> Self {
        Self {
            connection_id: self.connection_id.clone(),
            request: clone_request_wrapper(&self.request),
        }
    }
}
impl Action for WebSocketServerConnectAction {}
impl NamedAction for WebSocketServerConnectAction {
    fn name(&self) -> &'static str {
        "WebSocketServerConnectAction"
    }
}
impl SerializableAction for WebSocketServerConnectAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "connection_id",
            JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
        )])
    }
}

#[derive(Clone, Debug)]
pub struct WebSocketServerReceiveAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionClientMessage,
}
impl Action for WebSocketServerReceiveAction {}
impl NamedAction for WebSocketServerReceiveAction {
    fn name(&self) -> &'static str {
        "WebSocketServerReceiveAction"
    }
}
impl SerializableAction for WebSocketServerReceiveAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
            ),
            ("message", self.message.clone().into_json()),
        ])
    }
}

#[derive(Clone, Debug)]
pub struct WebSocketServerSendAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionServerMessage,
}
impl NamedAction for WebSocketServerSendAction {
    fn name(&self) -> &'static str {
        "WebSocketServerSendAction"
    }
}
impl Action for WebSocketServerSendAction {}
impl SerializableAction for WebSocketServerSendAction {
    fn serialize(&self) -> SerializedAction {
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

#[derive(Clone, Debug)]
pub struct WebSocketServerDisconnectAction {
    pub connection_id: Uuid,
}
impl Action for WebSocketServerDisconnectAction {}
impl NamedAction for WebSocketServerDisconnectAction {
    fn name(&self) -> &'static str {
        "WebSocketServerDisconnectAction"
    }
}
impl SerializableAction for WebSocketServerDisconnectAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "connection_id",
            JsonValue::from(format!("{}", self.connection_id.as_hyphenated())),
        )])
    }
}

#[derive(Clone, Debug)]
pub struct WebSocketServerThrottleTimeoutAction {
    pub subscription_id: Uuid,
}
impl Action for WebSocketServerThrottleTimeoutAction {}
impl NamedAction for WebSocketServerThrottleTimeoutAction {
    fn name(&self) -> &'static str {
        "WebSocketServerThrottleTimeoutAction"
    }
}
impl SerializableAction for WebSocketServerThrottleTimeoutAction {
    fn serialize(&self) -> SerializedAction {
        SerializedAction::from_iter([(
            "subscription_id",
            JsonValue::from(format!("{}", self.subscription_id.as_hyphenated())),
        )])
    }
}
