// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::FromIterator};

use reflex_json::{deserialize, JsonMap, JsonValue};

pub fn deserialize_graphql_operation(data: &str) -> Result<GraphQlOperationPayload, String> {
    match deserialize(data) {
        Err(error) => Err(error),
        Ok(value) => match value {
            JsonValue::Object(payload) => parse_graphql_operation_payload(&payload),
            _ => Err(String::from("Invalid request payload")),
        },
    }
}

#[derive(Clone, Debug)]
pub struct GraphQlOperationPayload {
    query: String,
    operation_name: Option<String>,
    variables: HashMap<String, JsonValue>,
    extensions: HashMap<String, JsonValue>,
}
impl GraphQlOperationPayload {
    pub fn new(
        query: String,
        operation_name: Option<String>,
        variables: Option<impl IntoIterator<Item = (String, JsonValue)>>,
        extensions: Option<impl IntoIterator<Item = (String, JsonValue)>>,
    ) -> Self {
        Self {
            query,
            operation_name,
            variables: match variables {
                Some(variables) => variables.into_iter().collect(),
                None => HashMap::new(),
            },
            extensions: match extensions {
                Some(extensions) => extensions.into_iter().collect(),
                None => HashMap::new(),
            },
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
    pub fn variable(&self, name: &str) -> Option<&JsonValue> {
        self.variables.get(name)
    }
    pub fn extension(&self, name: &str) -> Option<&JsonValue> {
        self.extensions.get(name)
    }
    pub fn variables(&self) -> impl Iterator<Item = (&str, &JsonValue)> {
        self.variables
            .iter()
            .map(|(key, value)| (key.as_str(), value))
    }
    pub fn extensions(&self) -> impl Iterator<Item = (&str, &JsonValue)> {
        self.extensions
            .iter()
            .map(|(key, value)| (key.as_str(), value))
    }
    pub fn into_parts(
        self,
    ) -> (
        String,
        Option<String>,
        impl IntoIterator<Item = (String, JsonValue)>,
        impl IntoIterator<Item = (String, JsonValue)>,
    ) {
        (
            self.query,
            self.operation_name,
            self.variables,
            self.extensions,
        )
    }
    pub fn into_json(self) -> JsonValue {
        JsonValue::Object(JsonMap::from_iter(vec![
            (String::from("query"), JsonValue::String(self.query)),
            (
                String::from("operationName"),
                match self.operation_name {
                    Some(operation_name) => JsonValue::String(operation_name),
                    None => JsonValue::Null,
                },
            ),
            (
                String::from("variables"),
                JsonValue::Object(JsonMap::from_iter(self.variables)),
            ),
            (
                String::from("extensions"),
                JsonValue::Object(JsonMap::from_iter(self.extensions)),
            ),
        ]))
    }
}

pub(crate) fn parse_graphql_operation_payload(
    payload: &JsonMap<String, JsonValue>,
) -> Result<GraphQlOperationPayload, String> {
    let query = match payload.get("query") {
        None => Err(String::from("Missing query")),
        Some(value) => match value {
            JsonValue::String(value) => Ok(String::from(value)),
            _ => Err(String::from("Invalid query")),
        },
    }?;
    let operation_name = match payload.get("operationName") {
        None => Ok(None),
        Some(value) => match value {
            JsonValue::String(value) => Ok(Some(String::from(value))),
            JsonValue::Null => Ok(None),
            _ => Err(String::from("Invalid operation name")),
        },
    }?;
    let variables = match payload.get("variables") {
        None => Ok(None),
        Some(JsonValue::Object(variables)) => Ok(Some(
            variables
                .iter()
                .map(|(key, value)| (key.clone(), value.clone())),
        )),
        _ => Err(String::from("Invalid variables")),
    }?;
    let extensions = match payload.get("extensions") {
        None => Ok(None),
        Some(JsonValue::Object(extensions)) => Ok(Some(
            extensions
                .iter()
                .map(|(key, value)| (key.clone(), value.clone())),
        )),
        _ => Err(String::from("Invalid extensions")),
    }?;
    Ok(GraphQlOperationPayload::new(
        String::from(query),
        operation_name,
        variables,
        extensions,
    ))
}
