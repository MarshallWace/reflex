// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::{empty, FromIterator};

use reflex::{
    serialize::{SerializedObjectTerm, SerializedTerm},
    stdlib::value::ValueTerm,
};
use reflex_json::{deserialize, sanitize_term};

pub fn deserialize_graphql_operation(data: &str) -> Result<GraphQlOperationPayload, String> {
    match serde_json::from_str::<serde_json::value::Value>(data) {
        Err(error) => Err(format!("{}", error)),
        Ok(value) => match value {
            serde_json::value::Value::Object(payload) => {
                deserialize_graphql_operation_payload(&payload)
            }
            _ => Err(String::from("Invalid request payload")),
        },
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlOperationPayload {
    query: String,
    operation_name: Option<String>,
    variables: SerializedObjectTerm,
    extensions: SerializedObjectTerm,
}
impl GraphQlOperationPayload {
    pub fn new(
        query: String,
        operation_name: Option<String>,
        variables: SerializedObjectTerm,
        extensions: SerializedObjectTerm,
    ) -> Self {
        Self {
            query,
            operation_name,
            variables,
            extensions,
        }
    }
    pub fn query(&self) -> &str {
        &self.query
    }
    pub fn operation_name(&self) -> Option<&str> {
        self.operation_name
            .as_ref()
            .map(|operation| operation.as_str())
    }
    pub fn variables(&self) -> &SerializedObjectTerm {
        &self.variables
    }
    pub fn extensions(&self) -> &SerializedObjectTerm {
        &self.extensions
    }
    pub(crate) fn serialize(&self) -> Result<serde_json::Value, String> {
        let variables = sanitize_term(SerializedTerm::Object(self.variables.clone()))?;
        Ok(serde_json::Value::Object(serde_json::Map::from_iter(vec![
            (
                String::from("query"),
                serde_json::Value::String(self.query.clone()),
            ),
            (
                String::from("operationName"),
                match &self.operation_name {
                    Some(operation_name) => serde_json::Value::String(operation_name.clone()),
                    None => serde_json::Value::Null,
                },
            ),
            (String::from("variables"), variables),
        ])))
    }
    pub fn stringify(&self) -> Result<String, String> {
        match self.serialize() {
            Err(error) => Err(error),
            Ok(result) => Ok(result.to_string()),
        }
    }
}

pub(crate) fn deserialize_graphql_operation_payload(
    payload: &serde_json::value::Map<String, serde_json::value::Value>,
) -> Result<GraphQlOperationPayload, String> {
    let query = match payload.get("query") {
        None => Err(String::from("Missing query")),
        Some(value) => match value {
            serde_json::value::Value::String(value) => Ok(String::from(value)),
            _ => Err(String::from("Invalid query")),
        },
    }?;
    let operation_name = match payload.get("operationName") {
        None => Ok(None),
        Some(value) => match value {
            serde_json::value::Value::String(value) => Ok(Some(String::from(value))),
            serde_json::value::Value::Null => Ok(None),
            _ => Err(String::from("Invalid operation name")),
        },
    }?;
    let variables = match payload.get("variables") {
        None => Ok(None),
        Some(variables) => match deserialize(variables)? {
            SerializedTerm::Value(ValueTerm::Null) => Ok(None),
            SerializedTerm::Object(variables) => Ok(Some(variables)),
            _ => Err(String::from("Invalid variables")),
        },
    }?
    .unwrap_or_else(|| SerializedObjectTerm::new(empty()));
    let extensions = match payload.get("extensions") {
        None => Ok(None),
        Some(extensions) => match deserialize(extensions)? {
            SerializedTerm::Value(ValueTerm::Null) => Ok(None),
            SerializedTerm::Object(extensions) => Ok(Some(extensions)),
            _ => Err(String::from("Invalid extensions")),
        },
    }?
    .unwrap_or_else(|| SerializedObjectTerm::new(empty()));
    Ok(GraphQlOperationPayload::new(
        String::from(query),
        operation_name,
        variables,
        extensions,
    ))
}
