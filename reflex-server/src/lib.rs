// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
pub use ::bytes;
pub use ::http;
pub use ::metrics;
pub use ::metrics_exporter_prometheus;
use http::StatusCode;

pub mod action;
pub mod imports;
pub mod logger;
pub mod middleware;
pub mod server;
pub mod stdlib;
pub mod utils;

pub(crate) mod service;
pub(crate) mod transport;

use reflex_graphql::{
    create_json_error_object, graphql_parser, validate_query::ValidateQueryGraphQlTransform,
    GraphQlOperationPayload, GraphQlQuery, GraphQlQueryTransform,
};
use reflex_json::JsonValue;
use server::{HttpGraphQlServerQueryTransform, WebSocketGraphQlServerQueryTransform};
pub use service::*;

pub mod builtins;
pub mod cli {
    pub mod execute_query;
    pub mod reflex_server;
}

#[derive(Clone)]
pub struct GraphQlServerQueryTransform {
    validate: Option<ValidateQueryGraphQlTransform<'static, String>>,
}
impl GraphQlServerQueryTransform {
    pub fn new(
        schema: Option<graphql_parser::schema::Document<'static, String>>,
    ) -> Result<Self, String> {
        Ok(Self {
            validate: schema.map(ValidateQueryGraphQlTransform::new).transpose()?,
        })
    }
}
impl HttpGraphQlServerQueryTransform for GraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        _request: &http::Request<bytes::Bytes>,
    ) -> Result<GraphQlOperationPayload, (http::StatusCode, String)> {
        if let Some(validate) = &self.validate {
            apply_graphql_query_transform(operation, validate)
        } else {
            Ok(operation)
        }
    }
}
impl WebSocketGraphQlServerQueryTransform for GraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        _request: &http::Request<()>,
        _connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperationPayload, JsonValue> {
        if let Some(validate) = &self.validate {
            apply_graphql_query_transform(operation, validate).map_err(|(status, message)| {
                create_json_error_object(
                    message,
                    [(String::from("status"), JsonValue::from(status.as_u16()))],
                )
            })
        } else {
            Ok(operation)
        }
    }
}

pub fn apply_graphql_query_transform(
    operation: GraphQlOperationPayload,
    transform: &impl GraphQlQueryTransform,
) -> Result<GraphQlOperationPayload, (StatusCode, String)> {
    let (query, operation_name, variables, extensions) = operation.into_parts();
    let query = query
        .into_ast()
        .map_err(|err| format!("{}", err))
        .and_then(|query| transform.transform(query))
        .map_err(|err| {
            (
                StatusCode::BAD_REQUEST,
                format!("GraphQL query transformation failed: {}", err),
            )
        })?;
    Ok(GraphQlOperationPayload::new(
        GraphQlQuery::Ast(query),
        operation_name,
        variables,
        extensions,
    ))
}
