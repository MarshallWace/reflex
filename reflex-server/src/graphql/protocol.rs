// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_js::stdlib::json_stringify_string;

pub fn deserialize_graphql_request(data: &str) -> Result<GraphQlOperationPayload, String> {
    match serde_json::from_str::<serde_json::value::Value>(data) {
        Err(error) => Err(format!("{}", error)),
        Ok(value) => match &value {
            serde_json::value::Value::Object(payload) => {
                deserialize_graphql_operation_payload(payload)
            }
            _ => Err(String::from("Invalid request payload")),
        },
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlOperationPayload {
    query: String,
    operation_name: Option<String>,
}
impl GraphQlOperationPayload {
    fn new(query: String, operation_name: Option<String>) -> Self {
        Self {
            query,
            operation_name,
        }
    }
    pub fn query(&self) -> &str {
        &self.query
    }
    pub fn operation_name(&self) -> Option<&str> {
        self.operation_name.as_ref().map(|operation| operation.as_str())
    }
}

fn deserialize_graphql_operation_payload(
    payload: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlOperationPayload, String> {
    let query = match payload.get("query") {
        None => Err(String::from("Missing query")),
        Some(value) => match value {
            serde_json::value::Value::String(value) => Ok(value),
            _ => Err(String::from("Invalid query")),
        },
    }?;
    let operation_name = match payload.get("operationName") {
        None => Ok(None),
        Some(value) => match value {
            serde_json::value::Value::String(value) => Ok(Some(value)),
            _ => Err(String::from("Invalid operation name")),
        },
    }?;
    Ok(GraphQlOperationPayload::new(
        String::from(query),
        operation_name.map(String::from),
    ))
}

#[derive(Clone, Debug)]
pub enum GraphQlSubscriptionClientMessage {
    ConnectionInit,
    Start(GraphQlSubscriptionStartMessage),
    Stop(GraphQlSubscriptionStopMessage),
    ConnectionTerminate,
}
impl GraphQlSubscriptionClientMessage {
    fn connection_init() -> Self {
        GraphQlSubscriptionClientMessage::ConnectionInit
    }
    fn start(subscription_id: String, payload: GraphQlOperationPayload) -> Self {
        GraphQlSubscriptionClientMessage::Start(GraphQlSubscriptionStartMessage {
            subscription_id,
            payload,
        })
    }
    fn stop(subscription_id: String) -> Self {
        GraphQlSubscriptionClientMessage::Stop(GraphQlSubscriptionStopMessage { subscription_id })
    }
    fn connection_terminate() -> Self {
        GraphQlSubscriptionClientMessage::ConnectionTerminate
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlSubscriptionStartMessage {
    subscription_id: SubscriptionId,
    payload: GraphQlOperationPayload,
}
impl GraphQlSubscriptionStartMessage {
    pub fn subscription_id(&self) -> &SubscriptionId {
        &self.subscription_id
    }
    pub fn query(&self) -> &str {
        &self.payload.query
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlSubscriptionStopMessage {
    subscription_id: SubscriptionId,
}
impl GraphQlSubscriptionStopMessage {
    pub fn subscription_id(&self) -> &SubscriptionId {
        &self.subscription_id
    }
}

pub type SubscriptionId = String;

#[derive(Clone, Debug)]
pub enum GraphQlSubscriptionServerMessage {
    ConnectionError(String),
    ConnectionAck,
    Data(SubscriptionId, String),
    Error(SubscriptionId, String),
    Complete(SubscriptionId),
    // TODO: Implement keepalive
    #[allow(dead_code)]
    ConnectionKeepAlive,
}

pub fn deserialize_graphql_client_message(
    data: &str,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    match serde_json::from_str::<serde_json::value::Value>(data) {
        Err(error) => Err(format!("{}", error)),
        Ok(value) => match &value {
            serde_json::value::Value::Object(message) => {
                let message_type = deserialize_client_message_type(message)?;
                match message_type.as_str() {
                    "connection_init" => deserialize_connection_init_message(message),
                    "start" => deserialize_start_message(message),
                    "stop" => deserialize_stop_message(message),
                    "connection_terminate" => deserialize_connection_terminate_message(message),
                    _ => Err(format!("Invalid message type: {}", message_type)),
                }
            }
            _ => Err(String::from("Invalid message format")),
        },
    }
}

fn deserialize_client_message_type(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<&String, String> {
    match message.get("type") {
        None => Err(String::from("Missing message type")),
        Some(message_type) => match message_type {
            serde_json::value::Value::String(message_type) => Ok(message_type),
            _ => Err(String::from("Invalid message type")),
        },
    }
}

fn deserialize_client_message_subscription_id(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<&String, String> {
    match message.get("id") {
        None => Err(String::from("Missing subscription ID")),
        Some(id) => match id {
            serde_json::value::Value::String(subscription_id) => Ok(subscription_id),
            _ => Err(String::from("Invalid subscription ID")),
        },
    }
}

fn deserialize_client_message_payload(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<&serde_json::value::Map<String, serde_json::value::Value>, String> {
    match message.get("payload") {
        None => Err(String::from("Missing message payload")),
        Some(payload) => match payload {
            serde_json::value::Value::Object(payload) => Ok(payload),
            _ => Err(String::from("Invalid message payload")),
        },
    }
}

fn deserialize_connection_init_message(
    _message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_init())
}

fn deserialize_start_message(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_client_message_subscription_id(message)?;
    let payload = deserialize_client_message_payload(message)?;
    let payload = deserialize_graphql_operation_payload(payload)?;
    Ok(GraphQlSubscriptionClientMessage::start(id.clone(), payload))
}

fn deserialize_stop_message(
    message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    let id = deserialize_client_message_subscription_id(message)?;
    Ok(GraphQlSubscriptionClientMessage::stop(id.clone()))
}

fn deserialize_connection_terminate_message(
    _message: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlSubscriptionClientMessage, String> {
    Ok(GraphQlSubscriptionClientMessage::connection_terminate())
}

pub fn serialize_graphql_server_message(message: GraphQlSubscriptionServerMessage) -> String {
    match message {
        GraphQlSubscriptionServerMessage::ConnectionAck => {
            wrap_server_message("connection_ack", None, None)
        }
        GraphQlSubscriptionServerMessage::ConnectionError(payload) => {
            wrap_server_message("connection_error", None, Some(payload))
        }
        GraphQlSubscriptionServerMessage::Data(id, payload) => {
            wrap_server_message("data", Some(id), Some(payload))
        }
        GraphQlSubscriptionServerMessage::Error(id, payload) => {
            wrap_server_message("error", Some(id), Some(payload))
        }
        GraphQlSubscriptionServerMessage::Complete(id) => {
            wrap_server_message("complete", Some(id), None)
        }
        GraphQlSubscriptionServerMessage::ConnectionKeepAlive => {
            wrap_server_message("ka", None, None)
        }
    }
}

fn wrap_server_message(message_type: &str, id: Option<String>, payload: Option<String>) -> String {
    let id_field = match id {
        Some(id) => format!(",\"id\":{}", json_stringify_string(&id)),
        None => String::from(""),
    };
    let payload_field = match payload {
        Some(payload) => format!(",\"payload\":{}", payload),
        None => String::from(""),
    };
    format!(
        "{{\"type\":\"{}\"{}{}}}",
        message_type, id_field, payload_field
    )
}
