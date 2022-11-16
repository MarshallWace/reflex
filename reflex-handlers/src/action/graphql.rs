// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::sync::Arc;

use bytes::Bytes;
use http::StatusCode;
use reflex_dispatcher::{Action, Named, SerializableAction, SerializedAction};
use reflex_graphql::subscriptions::{
    GraphQlSubscriptionClientMessage, GraphQlSubscriptionServerMessage,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::Named;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::utils::serialize::SerializedBytes;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GraphQlHandlerActions {
    HttpFetchComplete(GraphQlHandlerHttpFetchCompleteAction),
    HttpConnectionError(GraphQlHandlerHttpConnectionErrorAction),
    WebSocketConnectSuccess(GraphQlHandlerWebSocketConnectSuccessAction),
    WebSocketClientMessage(GraphQlHandlerWebSocketClientMessageAction),
    WebSocketServerMessage(GraphQlHandlerWebSocketServerMessageAction),
    WebSocketConnectionTerminate(GraphQlHandlerWebSocketConnectionTerminateAction),
    WebSocketConnectionError(GraphQlHandlerWebSocketConnectionErrorAction),
}
impl Named for GraphQlHandlerActions {
    fn name(&self) -> &'static str {
        match self {
            Self::HttpFetchComplete(action) => action.name(),
            Self::HttpConnectionError(action) => action.name(),
            Self::WebSocketConnectSuccess(action) => action.name(),
            Self::WebSocketClientMessage(action) => action.name(),
            Self::WebSocketServerMessage(action) => action.name(),
            Self::WebSocketConnectionTerminate(action) => action.name(),
            Self::WebSocketConnectionError(action) => action.name(),
        }
    }
}
impl Action for GraphQlHandlerActions {}
impl SerializableAction for GraphQlHandlerActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::HttpFetchComplete(action) => action.to_json(),
            Self::HttpConnectionError(action) => action.to_json(),
            Self::WebSocketConnectSuccess(action) => action.to_json(),
            Self::WebSocketClientMessage(action) => action.to_json(),
            Self::WebSocketServerMessage(action) => action.to_json(),
            Self::WebSocketConnectionTerminate(action) => action.to_json(),
            Self::WebSocketConnectionError(action) => action.to_json(),
        }
    }
}

impl From<GraphQlHandlerHttpFetchCompleteAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerHttpFetchCompleteAction) -> Self {
        Self::HttpFetchComplete(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerHttpFetchCompleteAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::HttpFetchComplete(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions> for Option<&'a GraphQlHandlerHttpFetchCompleteAction> {
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::HttpFetchComplete(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerHttpConnectionErrorAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerHttpConnectionErrorAction) -> Self {
        Self::HttpConnectionError(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerHttpConnectionErrorAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::HttpConnectionError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions> for Option<&'a GraphQlHandlerHttpConnectionErrorAction> {
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::HttpConnectionError(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketConnectSuccessAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        Self::WebSocketConnectSuccess(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketConnectSuccessAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectSuccess(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketClientMessageAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketClientMessageAction) -> Self {
        Self::WebSocketClientMessage(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketClientMessageAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketClientMessage(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketClientMessageAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketClientMessage(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketServerMessageAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        Self::WebSocketServerMessage(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketServerMessageAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketServerMessage(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketServerMessage(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketConnectionTerminateAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketConnectionTerminateAction) -> Self {
        Self::WebSocketConnectionTerminate(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketConnectionTerminateAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectionTerminate(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketConnectionTerminateAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectionTerminate(value) => Some(value),
            _ => None,
        }
    }
}

impl From<GraphQlHandlerWebSocketConnectionErrorAction> for GraphQlHandlerActions {
    fn from(value: GraphQlHandlerWebSocketConnectionErrorAction) -> Self {
        Self::WebSocketConnectionError(value)
    }
}
impl From<GraphQlHandlerActions> for Option<GraphQlHandlerWebSocketConnectionErrorAction> {
    fn from(value: GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectionError(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a GraphQlHandlerActions>
    for Option<&'a GraphQlHandlerWebSocketConnectionErrorAction>
{
    fn from(value: &'a GraphQlHandlerActions) -> Self {
        match value {
            GraphQlHandlerActions::WebSocketConnectionError(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug)]
pub struct GraphQlHandlerHttpFetchCompleteAction {
    pub operation_id: Uuid,
    pub url: String,
    pub status_code: StatusCode,
    pub body: Bytes,
}
impl Action for GraphQlHandlerHttpFetchCompleteAction {}
impl SerializableAction for GraphQlHandlerHttpFetchCompleteAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "operation_id",
                JsonValue::from(self.operation_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("status_code", JsonValue::from(self.status_code.as_u16())),
            ("content_length", JsonValue::from(self.body.len())),
        ])
    }
}
impl Serialize for GraphQlHandlerHttpFetchCompleteAction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedGraphQlHandlerHttpFetchCompleteAction::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for GraphQlHandlerHttpFetchCompleteAction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedGraphQlHandlerHttpFetchCompleteAction::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Clone, Serialize, Deserialize)]
struct SerializedGraphQlHandlerHttpFetchCompleteAction {
    operation_id: u128,
    url: String,
    status_code: u16,
    body: SerializedBytes,
}
impl<'a> From<&'a GraphQlHandlerHttpFetchCompleteAction>
    for SerializedGraphQlHandlerHttpFetchCompleteAction
{
    fn from(value: &'a GraphQlHandlerHttpFetchCompleteAction) -> Self {
        let GraphQlHandlerHttpFetchCompleteAction {
            operation_id,
            url,
            status_code,
            body,
        } = value;
        Self {
            operation_id: operation_id.as_u128(),
            url: url.into(),
            status_code: status_code.as_u16(),
            body: body.into(),
        }
    }
}
impl From<SerializedGraphQlHandlerHttpFetchCompleteAction>
    for GraphQlHandlerHttpFetchCompleteAction
{
    fn from(value: SerializedGraphQlHandlerHttpFetchCompleteAction) -> Self {
        let SerializedGraphQlHandlerHttpFetchCompleteAction {
            operation_id,
            url,
            status_code,
            body,
        } = value;
        Self {
            operation_id: Uuid::from_u128(operation_id),
            url: url.into(),
            status_code: StatusCode::from_u16(status_code).unwrap_or_default(),
            body: body.into(),
        }
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerHttpConnectionErrorAction {
    pub operation_id: Uuid,
    pub url: String,
    pub message: String,
}
impl Action for GraphQlHandlerHttpConnectionErrorAction {}
impl SerializableAction for GraphQlHandlerHttpConnectionErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "operation_id",
                JsonValue::from(self.operation_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("message", JsonValue::from(self.message.clone())),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectSuccessAction {
    pub connection_id: Uuid,
    pub url: String,
}
impl Action for GraphQlHandlerWebSocketConnectSuccessAction {}
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

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketClientMessageAction {
    pub connection_id: Uuid,
    pub message: GraphQlSubscriptionClientMessage,
}
impl Action for GraphQlHandlerWebSocketClientMessageAction {}
impl SerializableAction for GraphQlHandlerWebSocketClientMessageAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(self.connection_id.to_string()),
            ),
            ("message", self.message.clone().into_json()),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketServerMessageAction {
    pub connection_id: Uuid,
    pub message: Arc<GraphQlSubscriptionServerMessage>,
}
impl Action for GraphQlHandlerWebSocketServerMessageAction {}
impl SerializableAction for GraphQlHandlerWebSocketServerMessageAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(self.connection_id.to_string()),
            ),
            (
                "message",
                match self.message.as_ref() {
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
                    _ => (&*self.message).clone().into_json(),
                },
            ),
        ])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectionTerminateAction {
    pub connection_id: Uuid,
    pub url: String,
}
impl Action for GraphQlHandlerWebSocketConnectionTerminateAction {}
impl SerializableAction for GraphQlHandlerWebSocketConnectionTerminateAction {
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

#[derive(Named, PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectionErrorAction {
    pub connection_id: Uuid,
    pub url: String,
    pub message: String,
    pub retryable: bool,
}
impl Action for GraphQlHandlerWebSocketConnectionErrorAction {}
impl SerializableAction for GraphQlHandlerWebSocketConnectionErrorAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([
            (
                "connection_id",
                JsonValue::from(self.connection_id.to_string()),
            ),
            ("url", JsonValue::from(self.url.clone())),
            ("message", JsonValue::from(self.message.clone())),
            ("retryable", JsonValue::from(self.retryable)),
        ])
    }
}
