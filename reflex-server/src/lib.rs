// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    sync::{Arc, Mutex},
    time::SystemTime,
};

pub use ::bytes;
pub use ::http;
pub use ::metrics;
pub use ::metrics_exporter_prometheus;
pub use ::opentelemetry;
use chrono::{DateTime, Utc};

use http::StatusCode;
use logger::ActionLogger;
use server::action::opentelemetry::OpenTelemetryMiddlewareErrorAction;

pub mod action;
pub mod imports;
pub mod logger;
pub mod middleware;
pub mod recorder;
pub mod server;
pub mod stdlib;
pub mod tokio_runtime_metrics_export;
pub mod utils;

pub(crate) mod service;

use reflex_dispatcher::{Action, OutboundAction, StateOperation};
use reflex_graphql::{
    create_json_error_object, validate::ValidateQueryGraphQlTransform, GraphQlOperation,
    GraphQlQueryTransform, GraphQlSchemaTypes,
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
    pub fn new(schema_types: Option<GraphQlSchemaTypes<'static, String>>) -> Self {
        Self {
            validate: schema_types.map(ValidateQueryGraphQlTransform::new),
        }
    }
}
impl HttpGraphQlServerQueryTransform for GraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperation,
        _request: &http::Request<bytes::Bytes>,
    ) -> Result<GraphQlOperation, (http::StatusCode, String)> {
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
        operation: GraphQlOperation,
        _request: &http::Request<()>,
        _connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue> {
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
    operation: GraphQlOperation,
    transform: &impl GraphQlQueryTransform,
) -> Result<GraphQlOperation, (StatusCode, String)> {
    let (query, operation_name, variables, extensions) = operation.into_parts();
    let (query, variables, extensions) = transform
        .transform(query, variables, extensions)
        .map_err(|err| {
            (
                StatusCode::BAD_REQUEST,
                format!("GraphQL query transformation failed: {}", err),
            )
        })?;
    Ok(GraphQlOperation::new(
        query,
        operation_name,
        variables,
        extensions,
    ))
}

pub fn register_opentelemetry_error_logger<TAction>(
    logger: impl ActionLogger<Action = TAction> + Send + 'static,
) -> Result<(), OpenTelemetryMiddlewareErrorAction>
where
    TAction: Action + OutboundAction<OpenTelemetryMiddlewareErrorAction>,
{
    opentelemetry::global::set_error_handler({
        let logger = Arc::new(Mutex::new(logger));
        move |error| {
            if let Ok(mut logger) = logger.lock() {
                let action = OpenTelemetryMiddlewareErrorAction {
                    error: format!("{}", error),
                };
                logger.log(
                    &StateOperation::Send(Default::default(), action.into()),
                    None,
                    None,
                )
            }
        }
    })
    .map_err(|err| OpenTelemetryMiddlewareErrorAction {
        error: format!("{}", err),
    })
}

pub fn generate_session_recording_filename(suffix: Option<&str>) -> String {
    let datetime: DateTime<Utc> = SystemTime::now().into();
    format!(
        "{}{}.session",
        datetime.format("%Y-%m-%d-%s"),
        suffix.unwrap_or("")
    )
}
