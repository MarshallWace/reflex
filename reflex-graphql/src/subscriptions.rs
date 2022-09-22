// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::HashMap,
    iter::{once, FromIterator},
};

use reflex_json::{deserialize, JsonMap, JsonValue};
use serde::{Deserialize, Serialize};

use crate::{parse_graphql_operation_payload, GraphQlOperationPayload};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GraphQlSubscriptionClientMessage {
    ConnectionInit(GraphQlSubscriptionConnectionInitMessage),
    Start(GraphQlSubscriptionStartMessage),
    Stop(GraphQlSubscriptionStopMessage),
    Update(GraphQlSubscriptionUpdateMessage),
    ConnectionTerminate,
}
impl GraphQlSubscriptionClientMessage {
    pub fn connection_init(payload: Option<JsonValue>) -> Self {
        GraphQlSubscriptionClientMessage::ConnectionInit(GraphQlSubscriptionConnectionInitMessage {
            payload,
        })
    }
    pub fn start(operation_id: String, payload: GraphQlOperationPayload) -> Self {
        GraphQlSubscriptionClientMessage::Start(GraphQlSubscriptionStartMessage {
            id: operation_id,
            payload,
        })
    }
    pub fn stop(operation_id: String) -> Self {
        GraphQlSubscriptionClientMessage::Stop(GraphQlSubscriptionStopMessage { id: operation_id })
    }
    pub fn update(
        operation_id: String,
        payload: impl IntoIterator<Item = (String, JsonValue)>,
    ) -> Self {
        GraphQlSubscriptionClientMessage::Update(GraphQlSubscriptionUpdateMessage {
            id: operation_id,
            payload: payload.into_iter().collect(),
        })
    }
    pub fn connection_terminate() -> Self {
        GraphQlSubscriptionClientMessage::ConnectionTerminate
    }
    pub fn into_json(self) -> JsonValue {
        match self {
            Self::ConnectionInit(message) => {
                serialize_message("connection_init", None, message.into_payload())
            }
            Self::Start(message) => {
                serialize_message("start", Some(message.id), Some(message.payload.into_json()))
            }
            Self::Stop(message) => serialize_message("stop", Some(message.id), None),
            Self::Update(message) => serialize_message(
                "update",
                Some(message.id),
                Some(JsonValue::Object(JsonMap::from_iter(
                    message
                        .payload
                        .iter()
                        .map(|(key, value)| (key.clone(), value.clone())),
                ))),
            ),
            Self::ConnectionTerminate => serialize_message("connection_terminate", None, None),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlSubscriptionConnectionInitMessage {
    payload: Option<JsonValue>,
}
impl GraphQlSubscriptionConnectionInitMessage {
    pub fn payload(&self) -> Option<&JsonValue> {
        self.payload.as_ref()
    }
    pub fn into_payload(self) -> Option<JsonValue> {
        self.payload
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlSubscriptionStartMessage {
    id: OperationId,
    payload: GraphQlOperationPayload,
}
impl GraphQlSubscriptionStartMessage {
    pub fn operation_id(&self) -> &OperationId {
        &self.id
    }
    pub fn payload(&self) -> &GraphQlOperationPayload {
        &self.payload
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlSubscriptionStopMessage {
    id: OperationId,
}
impl GraphQlSubscriptionStopMessage {
    pub fn operation_id(&self) -> &OperationId {
        &self.id
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlSubscriptionUpdateMessage {
    id: OperationId,
    payload: HashMap<String, JsonValue>,
}
impl GraphQlSubscriptionUpdateMessage {
    pub fn operation_id(&self) -> &OperationId {
        &self.id
    }
    pub fn payload(&self) -> &HashMap<String, JsonValue> {
        &self.payload
    }
    pub fn into_payload(self) -> HashMap<String, JsonValue> {
        self.payload
    }
}

pub type OperationId = String;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum GraphQlSubscriptionServerMessage {
    ConnectionAck,
    ConnectionError(JsonValue),
    Data(OperationId, JsonValue),
    Patch(OperationId, JsonValue),
    Error(OperationId, JsonValue),
    Complete(OperationId),
    // TODO: Implement keepalive
    #[allow(dead_code)]
    ConnectionKeepAlive,
}
impl GraphQlSubscriptionServerMessage {
    pub fn into_json(self) -> JsonValue {
        match self {
            Self::ConnectionAck => serialize_message("connection_ack", None, None),
            Self::ConnectionError(error) => {
                serialize_message("connection_error", None, Some(error))
            }
            Self::Data(id, payload) => serialize_message("data", Some(id), Some(payload)),
            Self::Patch(id, payload) => serialize_message("patch", Some(id), Some(payload)),
            Self::Error(id, error) => serialize_message("error", Some(id), Some(error)),
            Self::Complete(id) => serialize_message("complete", Some(id), None),
            Self::ConnectionKeepAlive => serialize_message("ka", None, None),
        }
    }
}

fn serialize_message(
    message_type: &'static str,
    id: Option<String>,
    payload: Option<JsonValue>,
) -> JsonValue {
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
                    "update" => deserialize_client_update_message(message),
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
            JsonValue::String(operation_id) => Ok(operation_id.clone()),
            _ => Err(String::from("Invalid subscription ID")),
        },
    }
}

fn deserialize_message_payload(message: JsonMap<String, JsonValue>) -> Result<JsonValue, String> {
    deserialize_optional_message_payload(message)
        .ok_or_else(|| String::from("Missing message payload"))
}

fn deserialize_optional_message_payload(message: JsonMap<String, JsonValue>) -> Option<JsonValue> {
    message
        .into_iter()
        .find_map(|(key, value)| if key == "payload" { Some(value) } else { None })
}

fn deserialize_client_connection_init_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let payload = deserialize_optional_message_payload(message);
    Ok(GraphQlSubscriptionClientMessage::connection_init(payload))
}

fn deserialize_client_start_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(&message)?;
    let payload = deserialize_message_payload(message)?;
    let payload = match payload {
        JsonValue::Object(payload) => {
            parse_graphql_operation_payload(&payload).map_err(|err| format!("{}", err))
        }
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

fn deserialize_client_update_message(
    message: JsonMap<String, JsonValue>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_message_id(&message)?;
    let payload = deserialize_message_payload(message).and_then(|variables| match variables {
        JsonValue::Object(entries) => Ok(entries),
        _ => Err(format!("Invalid update payload: {}", variables)),
    })?;
    Ok(GraphQlSubscriptionClientMessage::update(id, payload))
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
