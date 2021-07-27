// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::{once, FromIterator};

use reflex_json::{deserialize, JsonMap, JsonValue};

use crate::operation::{parse_graphql_operation_payload, GraphQlOperationPayload};

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
    pub fn into_serialized(self) -> Result<String, String> {
        match self {
            Self::ConnectionInit => Ok(serialize_message("connection_init", None, None)),
            Self::Start(message) => Ok(serialize_message(
                "start",
                Some(message.id),
                Some(message.payload.into_json()),
            )),
            Self::Stop(message) => Ok(serialize_message("stop", Some(message.id), None)),
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
    pub fn payload(&self) -> &GraphQlOperationPayload {
        &self.payload
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
    ConnectionError(JsonValue),
    Data(SubscriptionId, JsonValue),
    Patch(SubscriptionId, JsonValue),
    Error(SubscriptionId, JsonValue),
    Complete(SubscriptionId),
    // TODO: Implement keepalive
    #[allow(dead_code)]
    ConnectionKeepAlive,
}
impl GraphQlSubscriptionServerMessage {
    pub fn into_serialized(self) -> Result<String, String> {
        match self {
            Self::ConnectionAck => Ok(serialize_message("connection_ack", None, None)),
            Self::ConnectionError(error) => {
                Ok(serialize_message("connection_error", None, Some(error)))
            }
            Self::Data(id, payload) => Ok(serialize_message("data", Some(id), Some(payload))),
            Self::Patch(id, payload) => Ok(serialize_message("patch", Some(id), Some(payload))),
            Self::Error(id, error) => Ok(serialize_message("error", Some(id), Some(error))),
            Self::Complete(id) => Ok(serialize_message("complete", Some(id), None)),
            Self::ConnectionKeepAlive => Ok(serialize_message("ka", None, None)),
        }
    }
}

fn serialize_message(
    message_type: &'static str,
    id: Option<String>,
    payload: Option<JsonValue>,
) -> String {
    let id = id.map(|id| String::from(id));
    let properties = vec![
        ("type", Some(JsonValue::String(String::from(message_type)))),
        ("id", id.map(|id| JsonValue::String(id))),
        ("payload", payload),
    ];
    JsonValue::Object(JsonMap::from_iter(
        properties
            .into_iter()
            .filter_map(|(key, value)| value.map(|value| (String::from(key), value))),
    ))
    .to_string()
}

pub fn deserialize_graphql_client_message(
    data: &str,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    match deserialize(data) {
        Err(error) => Err(error),
        Ok(value) => match value {
            JsonValue::Object(message) => {
                let message_type = deserialize_message_type(&message)?;
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
    match deserialize(data) {
        Err(error) => Err(error),
        Ok(value) => match value {
            JsonValue::Object(message) => {
                let message_type = deserialize_message_type(&message)?;
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

fn deserialize_message_type(message: &JsonMap<String, JsonValue>) -> Result<String, String> {
    match message.get("type") {
        None => Err(String::from("Missing message type")),
        Some(message_type) => match message_type {
            JsonValue::String(message_type) => Ok(message_type.clone()),
            _ => Err(String::from("Invalid message type")),
        },
    }
}

fn deserialize_message_id(message: &JsonMap<String, JsonValue>) -> Result<String, String> {
    match message.get("id") {
        None => Err(String::from("Missing subscription ID")),
        Some(id) => match id {
            JsonValue::String(subscription_id) => Ok(subscription_id.clone()),
            _ => Err(String::from("Invalid subscription ID")),
        },
    }
}

fn deserialize_message_payload(message: JsonMap<String, JsonValue>) -> Result<JsonValue, String> {
    message
        .into_iter()
        .find_map(|(key, value)| if key == "payload" { Some(value) } else { None })
        .ok_or_else(|| String::from("Missing message payload"))
}

fn deserialize_client_connection_init_message(
    _message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_init())
}

fn deserialize_client_start_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(&message)?;
    let payload = deserialize_message_payload(message)?;
    let payload = match payload {
        JsonValue::Object(payload) => parse_graphql_operation_payload(&payload),
        _ => Err(String::from("Invalid message payload")),
    }?;
    Ok(GraphQlSubscriptionClientMessage::start(id, payload))
}

fn deserialize_client_stop_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(&message)?;
    Ok(GraphQlSubscriptionClientMessage::stop(id))
}

fn deserialize_client_connection_terminate_message(
    _message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_terminate())
}

fn deserialize_server_connection_ack_message(
    _message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    Ok(GraphQlSubscriptionServerMessage::ConnectionAck)
}

fn deserialize_server_connection_error_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let payload = deserialize_message_payload(message).ok();
    let error = payload.unwrap_or_else(|| create_error_object(String::from("Connection error")));
    Ok(GraphQlSubscriptionServerMessage::ConnectionError(error))
}

fn deserialize_server_data_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(&message)?;
    let payload = deserialize_message_payload(message)?;
    Ok(GraphQlSubscriptionServerMessage::Data(id, payload))
}

fn deserialize_server_error_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(&message)?;
    let payload = deserialize_message_payload(message).ok();
    let error = payload.unwrap_or_else(|| create_error_object(String::from("Subscription error")));
    Ok(GraphQlSubscriptionServerMessage::Error(id, error))
}

fn deserialize_server_complete_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    let id = deserialize_message_id(&message)?;
    Ok(GraphQlSubscriptionServerMessage::Complete(id))
}

fn deserialize_server_ka_message(
    _message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionServerMessage, String> {
    Ok(GraphQlSubscriptionServerMessage::ConnectionKeepAlive)
}

fn create_error_object(message: String) -> JsonValue {
    JsonValue::Object(JsonMap::from_iter(once((
        String::from("message"),
        JsonValue::String(message),
    ))))
}
