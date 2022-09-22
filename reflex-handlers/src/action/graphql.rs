// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, NamedAction, SerializableAction, SerializedAction};
use reflex_graphql::subscriptions::GraphQlSubscriptionServerMessage;
use reflex_json::{JsonMap, JsonValue};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GraphQlHandlerActions {
    ConnectSuccess(GraphQlHandlerWebSocketConnectSuccessAction),
    ConnectError(GraphQlHandlerWebSocketConnectErrorAction),
    ServerMessage(GraphQlHandlerWebSocketServerMessageAction),
}
impl Action for GraphQlHandlerActions {}
impl NamedAction for GraphQlHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::ConnectSuccess(action) => action.name(),
            Self::ConnectError(action) => action.name(),
            Self::ServerMessage(action) => action.name(),
        }
    }
}
impl SerializableAction for GraphQlHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::ConnectSuccess(action) => action.to_json(),
            Self::ConnectError(action) => action.to_json(),
            Self::ServerMessage(action) => action.to_json(),
        }
    }
}

impl From<GraphQlHandlerWebSocketConnectSuccessAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        Self::ConnectSuccess(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketConnectSuccessAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketConnectErrorAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketConnectErrorAction) -> Self {
        Self::ConnectError(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions> for Option<&'a GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ConnectError(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketServerMessageAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        Self::ServerMessage(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketServerMessageAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ServerMessage(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::ServerMessage(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectSuccessAction {
    pub connection_id: Uuid,
    pub url: String,
}
impl Action for GraphQlHandlerWebSocketConnectSuccessAction {}
impl NamedAction for GraphQlHandlerWebSocketConnectSuccessAction {
    fn name(&self) -> &'static str {
        "GraphQlMiddlewareWebSocketConnectSuccessAction"
    }
}
impl SerializableAction for GraphQlHandlerWebSocketConnectSuccessAction {
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectErrorAction {
    pub connection_id: Uuid,
    pub url: String,
    pub error: String,
}
impl Action for GraphQlHandlerWebSocketConnectErrorAction {}
impl NamedAction for GraphQlHandlerWebSocketConnectErrorAction {
    fn name(&self) -> &'static str {
        "GraphQlMiddlewareWebSocketConnectErrorAction"
    }
}
impl SerializableAction for GraphQlHandlerWebSocketConnectErrorAction {
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketServerMessageAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionServerMessage,
}
impl Action for GraphQlHandlerWebSocketServerMessageAction {}
impl NamedAction for GraphQlHandlerWebSocketServerMessageAction {
    fn name(&self) -> &'static str {
        "GraphQlMiddlewareWebSocketServerMessageAction"
    }
}
impl SerializableAction for GraphQlHandlerWebSocketServerMessageAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(self.connection_id.to_string()),
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
