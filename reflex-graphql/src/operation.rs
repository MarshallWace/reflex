// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, iter::FromIterator};

use graphql_parser::{
    parse_query,
    query::{Document, ParseError},
    Style,
};
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct GraphQlOperationPayload {
    query: GraphQlQuery,
    operation_name: Option<String>,
    variables: HashMap<String, JsonValue>,
    extensions: HashMap<String, JsonValue>,
}
#[derive(Clone, Debug)]
pub enum GraphQlQuery {
    Source(String),
    Ast(Document<'static, String>),
}
impl Eq for GraphQlQuery {}
impl PartialEq for GraphQlQuery {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Source(left), Self::Source(right)) => left == right,
            (Self::Ast(left), Self::Source(right)) => &format!("{}", left) == right,
            (Self::Source(left), Self::Ast(right)) => left == &format!("{}", right),
            (Self::Ast(left), Self::Ast(right)) => format!("{}", left) == format!("{}", right),
        }
    }
}
impl GraphQlQuery {
    pub fn into_ast(self) -> Result<Document<'static, String>, ParseError> {
        match self {
            Self::Ast(ast) => Ok(ast),
            Self::Source(source) => {
                parse_query::<String>(&source).map(|document| document.into_static())
            }
        }
    }
}
impl std::fmt::Display for GraphQlQuery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Source(source) => std::fmt::Display::fmt(source, f),
            Self::Ast(ast) => std::fmt::Display::fmt(&ast.format(&Style::default()), f),
        }
    }
}
impl GraphQlOperationPayload {
    pub fn new(
        query: GraphQlQuery,
        operation_name: Option<String>,
        variables: impl IntoIterator<Item = (String, JsonValue)>,
        extensions: impl IntoIterator<Item = (String, JsonValue)>,
    ) -> Self {
        Self {
            query,
            operation_name,
            variables: variables.into_iter().collect(),
            extensions: extensions.into_iter().collect(),
        }
    }
    pub fn query(&self) -> &GraphQlQuery {
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
    pub fn set_variable(&mut self, name: impl Into<String>, value: JsonValue) -> Option<JsonValue> {
        self.variables.insert(name.into(), value)
    }
    pub fn variables(&self) -> impl Iterator<Item = (&str, &JsonValue)> {
        self.variables
            .iter()
            .map(|(key, value)| (key.as_str(), value))
    }
    pub fn extension(&self, name: &str) -> Option<&JsonValue> {
        self.extensions.get(name)
    }
    pub fn set_extension(
        &mut self,
        name: impl Into<String>,
        value: JsonValue,
    ) -> Option<JsonValue> {
        self.extensions.insert(name.into(), value)
    }
    pub fn extensions(&self) -> impl Iterator<Item = (&str, &JsonValue)> {
        self.extensions
            .iter()
            .map(|(key, value)| (key.as_str(), value))
    }
    pub fn into_parts(
        self,
    ) -> (
        GraphQlQuery,
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
            (
                String::from("query"),
                JsonValue::String(format!("{}", self.query)),
            ),
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

pub fn graphql_variables_are_equal<'a>(
    value1: impl IntoIterator<Item = (impl Into<&'a str>, &'a JsonValue)>,
    value2: impl IntoIterator<Item = (impl Into<&'a str>, &'a JsonValue)>,
) -> bool {
    let variables1 = value1
        .into_iter()
        .map(|(key, value)| (key.into(), value))
        .collect::<HashMap<&'a str, _>>();
    let variables2 = value2
        .into_iter()
        .map(|(key, value)| (key.into(), value))
        .collect::<HashMap<&'a str, _>>();
    if variables1.len() != variables2.len() {
        return false;
    }
    variables1
        .iter()
        .all(|(key, value)| variables2.get(key) == Some(value))
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
        GraphQlQuery::Source(query),
        operation_name,
        variables.into_iter().flat_map(|variables| variables),
        extensions.into_iter().flat_map(|extensions| extensions),
    ))
}
