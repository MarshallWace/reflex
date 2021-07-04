// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::{once, FromIterator};

use reflex::serialize::{SerializedObjectTerm, SerializedTerm};
use reflex_json::{deserialize, sanitize_term};

use crate::operation::{deserialize_graphql_operation_payload, GraphQlOperationPayload};

#[derive(Clone, Debug)]
pub enum GraphQlSubscriptionClientMessage {
    ConnectionInit,
    Start(GraphQlSubscriptionStartMessage),
    Stop(GraphQlSubscriptionStopMessage),
    ConnectionTerminate,
}
impl GraphQlSubscriptionClientMessage {
    pub fn connection_init() -> Self {
        GraphQlSubscriptionClientMessage::ConnectionInit
    }
    pub fn start(subscription_id: String, payload: GraphQlOperationPayload) -> Self {
        GraphQlSubscriptionClientMessage::Start(GraphQlSubscriptionStartMessage {
            id: subscription_id,
            payload,
        })
    }
    pub fn stop(subscription_id: String) -> Self {
        GraphQlSubscriptionClientMessage::Stop(GraphQlSubscriptionStopMessage {
            id: subscription_id,
        })
    }
    pub fn connection_terminate() -> Self {
        GraphQlSubscriptionClientMessage::ConnectionTerminate
    }
    pub fn serialize(&self) -> Result<String, String> {
        match self {
            Self::ConnectionInit => Ok(serialize_message("connection_init", None, None)),
            Self::Start(message) => Ok(serialize_message(
                "start",
                Some(&message.id),
                Some(message.payload.serialize()?),
            )),
            Self::Stop(message) => Ok(serialize_message("stop", Some(&message.id), None)),
            Self::ConnectionTerminate => Ok(serialize_message("connection_terminate", None, None)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlSubscriptionStartMessage {
    id: SubscriptionId,
    payload: GraphQlOperationPayload,
}
impl GraphQlSubscriptionStartMessage {
    pub fn subscription_id(&self) -> &SubscriptionId {
        &self.id
    }
    pub fn query(&self) -> &str {
        self.payload.query()
    }
    pub fn operation_name(&self) -> Option<&str> {
        self.payload.operation_name()
    }
    pub fn variables(&self) -> &SerializedObjectTerm {
        self.payload.variables()
    }
    pub fn extensions(&self) -> &SerializedObjectTerm {
        self.payload.extensions()
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlSubscriptionStopMessage {
    id: SubscriptionId,
}
impl GraphQlSubscriptionStopMessage {
    pub fn subscription_id(&self) -> &SubscriptionId {
        &self.id
    }
}

pub type SubscriptionId = String;

#[derive(Clone, Debug)]
pub enum GraphQlSubscriptionServerMessage {
    ConnectionAck,
    ConnectionError(String),
    Data(SubscriptionId, SerializedTerm),
    Patch(SubscriptionId, SerializedTerm),
    Error(SubscriptionId, String),
    Complete(SubscriptionId),
    // TODO: Implement keepalive
    #[allow(dead_code)]
    ConnectionKeepAlive,
}
impl GraphQlSubscriptionServerMessage {
    pub fn serialize(&self) -> Result<String, String> {
        match self {
            Self::ConnectionAck => Ok(serialize_message("connection_ack", None, None)),
            Self::ConnectionError(error) => Ok(serialize_message(
                "connection_error",
                None,
                Some(serialize_error(error)),
            )),
            Self::Data(id, payload) => {
                let payload = sanitize_term(payload.clone())?;
                Ok(serialize_message("data", Some(id), Some(payload)))
            }
            Self::Patch(id, payload) => {
                let payload = sanitize_term(payload.clone())?;
                Ok(serialize_message("patch", Some(id), Some(payload)))
            }
            Self::Error(id, error) => Ok(serialize_message(
                "error",
                Some(id),
                Some(serialize_error(error)),
            )),
            Self::Complete(id) => Ok(serialize_message("complete", Some(id), None)),
            Self::ConnectionKeepAlive => Ok(serialize_message("ka", None, None)),
        }
    }
}

fn serialize_error(error: &str) -> serde_json::Value {
    serde_json::Value::Object(serde_json::Map::from_iter(once((
        String::from("message"),
        serde_json::Value::String(String::from(error)),
    ))))
}

fn serialize_message(
    message_type: &str,
    id: Option<&str>,
    payload: Option<serde_json::Value>,
) -> String {
    let message_type = String::from(message_type);
    let id = id.map(|id| String::from(id));
    let properties = vec![
        ("type", Some(serde_json::Value::String(message_type))),
        ("id", id.map(|id| serde_json::Value::String(id))),
        ("payload", payload),
    ];
    serde_json::Value::Object(serde_json::Map::from_iter(
        properties
            .into_iter()
            .filter_map(|(key, value)| value.map(|value| (String::from(key), value))),
    ))
    .to_string()
}

pub fn deserialize_graphql_client_message(
    data: &str,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    match serde_json::from_str::<serde_json::value::Value>(data) {
        Err(error) => Err(format!("{}", error)),
        Ok(value) => match &value {
            serde_json::value::Value::Object(message) => {
                let message_type = deserialize_message_type(message)?;
                match message_type.as_str() {
                    "connection_init" => deserialize_client_connection_init_message(message),
                    "start" => deserialize_client_start_message(message),
                    "stop" => deserialize_client_stop_message(message),
                    "connection_terminate" => {
                        deserialize_client_connection_terminate_message(message)
                    }
                    _ => Err(format!("Invalid message type: {}", message_type)),
                }
            }
            _ => Err(String::from("Invalid message format")),
        },
    }
}

pub fn deserialize_graphql_server_message(
    data: &str,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    match serde_json::from_str::<serde_json::value::Value>(data) {
        Err(error) => Err(format!("{}", error)),
        Ok(value) => match &value {
            serde_json::value::Value::Object(message) => {
                let message_type = deserialize_message_type(message)?;
                match message_type.as_str() {
                    "connection_ack" => deserialize_server_connection_ack_message(message),
                    "connection_error" => deserialize_server_connection_error_message(message),
                    "data" => deserialize_server_data_message(message),
                    "error" => deserialize_server_error_message(message),
                    "complete" => deserialize_server_complete_message(message),
                    "ka" => deserialize_server_ka_message(message),
                    _ => Err(format!("Invalid message type: {}", message_type)),
                }
            }
            _ => Err(String::from("Invalid message format")),
        },
    }
}

fn deserialize_message_type(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<String, String> {
    match message.get("type") {
        None => Err(String::from("Missing message type")),
        Some(message_type) => match message_type {
            serde_json::value::Value::String(message_type) => Ok(message_type.clone()),
            _ => Err(String::from("Invalid message type")),
        },
    }
}

fn deserialize_message_id(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<String, String> {
    match message.get("id") {
        None => Err(String::from("Missing subscription ID")),
        Some(id) => match id {
            serde_json::value::Value::String(subscription_id) => Ok(subscription_id.clone()),
            _ => Err(String::from("Invalid subscription ID")),
        },
    }
}

fn deserialize_message_payload(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<&serde_json::Value, String> {
    match message.get("payload") {
        None => Err(String::from("Missing message payload")),
        Some(payload) => Ok(payload),
    }
}

fn deserialize_error_payload(payload: &serde_json::Value) -> Option<String> {
    match payload {
        serde_json::Value::String(message) => Some(String::from(message)),
        serde_json::Value::Object(payload) => match payload.get("message") {
            Some(message) => match message {
                serde_json::Value::String(message) => Some(String::from(message)),
                _ => None,
            },
            None => None,
        },
        _ => None,
    }
}

fn deserialize_client_connection_init_message(
    _message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_init())
}

fn deserialize_client_start_message(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(message)?;
    let payload = deserialize_message_payload(message)?;
    let payload = match payload {
        serde_json::Value::Object(payload) => deserialize_graphql_operation_payload(payload),
        _ => Err(String::from("Invalid message payload")),
    }?;
    Ok(GraphQlSubscriptionClientMessage::start(id, payload))
}

fn deserialize_client_stop_message(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(message)?;
    Ok(GraphQlSubscriptionClientMessage::stop(id))
}

fn deserialize_client_connection_terminate_message(
    _message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_terminate())
}

fn deserialize_server_connection_ack_message(
    _message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    Ok(GraphQlSubscriptionServerMessage::ConnectionAck)
}

fn deserialize_server_connection_error_message(
    message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let payload = deserialize_message_payload(message).ok();
    let error = payload
        .and_then(deserialize_error_payload)
        .unwrap_or_else(|| String::from("Connection error"));
    Ok(GraphQlSubscriptionServerMessage::ConnectionError(error))
}

fn deserialize_server_data_message(
    message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(message)?;
    let payload = deserialize_message_payload(message)?;
    let payload = deserialize(payload)?;
    Ok(GraphQlSubscriptionServerMessage::Data(id, payload))
}

fn deserialize_server_error_message(
    message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(message)?;
    let payload = deserialize_message_payload(message).ok();
    let error = payload
        .and_then(deserialize_error_payload)
        .unwrap_or_else(|| String::from("Subscription error"));
    Ok(GraphQlSubscriptionServerMessage::Error(id, error))
}

fn deserialize_server_complete_message(
    message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(message)?;
    Ok(GraphQlSubscriptionServerMessage::Complete(id))
}

fn deserialize_server_ka_message(
    _message: &serde_json::Map<String, serde_json::Value>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    Ok(GraphQlSubscriptionServerMessage::ConnectionKeepAlive)
}
