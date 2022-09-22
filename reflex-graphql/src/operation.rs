// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_json::{JsonMap, JsonValue};
use serde::{Deserialize, Serialize};

use crate::{GraphQlExtensions, GraphQlQuery, GraphQlVariables};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct GraphQlOperation {
    query: GraphQlQuery,
    operation_name: Option<String>,
    variables: GraphQlVariables,
    extensions: GraphQlExtensions,
}
impl GraphQlOperation {
    pub fn new(
        query: GraphQlQuery,
        operation_name: Option<String>,
        variables: GraphQlVariables,
        extensions: GraphQlExtensions,
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
    pub fn variables(&self) -> &GraphQlVariables {
        &self.variables
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
    pub fn extensions(&self) -> &GraphQlExtensions {
        &self.extensions
    }
    pub fn into_parts(
        self,
    ) -> (
        GraphQlQuery,
        Option<String>,
        GraphQlVariables,
        GraphQlExtensions,
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
            (String::from("variables"), JsonValue::Object(self.variables)),
            (
                String::from("extensions"),
                JsonValue::Object(self.extensions),
            ),
        ]))
    }
}

pub fn graphql_variables_are_equal<'a>(
    value1: &GraphQlVariables,
    value2: &GraphQlVariables,
) -> bool {
    if value1.len() != value2.len() {
        return false;
    }
    value1
        .iter()
        .all(|(key, value)| value2.get(key) == Some(value))
}
