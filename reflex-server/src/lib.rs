// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};

use actor::ServerActor;
use http::StatusCode;
use logger::ActionLogger;
use opentelemetry::trace::Tracer;
use reflex::core::{ExpressionFactory, HeapAllocator};
use reflex_graphql::GraphQlParserBuiltin;
use reflex_runtime::{actor::RuntimeMetricNames, runtime_actors, AsyncExpression};
use server::{
    GraphQlServerOperationMetricLabels, GraphQlServerQueryLabel,
    HttpGraphQlServerQueryMetricLabels, WebSocketGraphQlServerConnectionMetricLabels,
};
use utils::datetime::format_datetime;

use crate::server::{
    action::opentelemetry::OpenTelemetryMiddlewareErrorAction,
    ChainedHttpGraphQlServerQueryTransform, ChainedWebSocketGraphQlServerQueryTransform,
    GraphQlServer, GraphQlServerMetricNames, HttpGraphQlServer, HttpGraphQlServerMetricNames,
    HttpGraphQlServerQueryTransform, WebSocketGraphQlServer, WebSocketGraphQlServerMetricNames,
    WebSocketGraphQlServerQueryTransform,
};

pub use ::bytes;
pub use ::http;
pub use ::metrics;
pub use ::metrics_exporter_prometheus;
pub use ::opentelemetry;

pub mod action;
pub mod actor;
pub mod logger;
pub mod scheduler_metrics;
pub mod server;
pub mod task;
pub mod tokio_runtime_metrics_export;
pub mod utils;

pub(crate) mod service;

use reflex_dispatcher::{Action, ProcessId};
use reflex_graphql::{
    create_json_error_object, validate::ValidateQueryGraphQlTransform, GraphQlOperation,
    GraphQlQueryTransform, GraphQlSchemaTypes,
};
use reflex_json::JsonValue;
pub use service::*;

pub mod builtins;
pub mod cli {
    pub mod execute_query;
    pub mod reflex_server;
    pub mod task;
}

#[derive(Default, Clone, Copy, Debug)]
pub struct ServerMetricNames {
    pub runtime: RuntimeMetricNames,
    pub graphql_server: GraphQlServerMetricNames,
    pub http_graphql_server: HttpGraphQlServerMetricNames,
    pub websocket_graphql_server: WebSocketGraphQlServerMetricNames,
}

pub fn server_actors<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TTracer,
>(
    schema_types: Option<GraphQlSchemaTypes<'static, String>>,
    factory: TFactory,
    allocator: TAllocator,
    transform_http: TTransformHttp,
    transform_ws: TTransformWs,
    effect_throttle: Option<Duration>,
    metric_names: ServerMetricNames,
    get_graphql_query_label: TGraphQlQueryLabel,
    get_http_query_metric_labels: THttpMetricLabels,
    get_websocket_connection_metric_labels: TConnectionMetricLabels,
    get_operation_metric_labels: TOperationMetricLabels,
    tracer: TTracer,
    main_pid: ProcessId,
) -> impl IntoIterator<
    Item = ServerActor<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >,
>
where
    T: AsyncExpression,
    T::Builtin: GraphQlParserBuiltin,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    let validate_query_transform = schema_types.clone().map(ValidateQueryGraphQlTransform::new);
    {
        runtime_actors(
            factory.clone(),
            allocator.clone(),
            effect_throttle,
            metric_names.runtime,
            main_pid,
        )
        .into_iter()
        .map(ServerActor::Runtime)
    }
    .chain([
        ServerActor::GraphQlServer(GraphQlServer::new(
            factory.clone(),
            allocator.clone(),
            metric_names.graphql_server,
            get_graphql_query_label,
            get_operation_metric_labels,
            tracer,
            main_pid,
        )),
        ServerActor::HttpGraphQlServer(HttpGraphQlServer::new(
            schema_types.clone(),
            factory.clone(),
            ChainedHttpGraphQlServerQueryTransform {
                left: validate_query_transform.clone(),
                right: transform_http,
            },
            metric_names.http_graphql_server,
            get_http_query_metric_labels,
            main_pid,
        )),
        ServerActor::WebSocketGraphQlServer(WebSocketGraphQlServer::new(
            schema_types,
            factory.clone(),
            ChainedWebSocketGraphQlServerQueryTransform {
                left: validate_query_transform,
                right: transform_ws,
            },
            metric_names.websocket_graphql_server,
            get_websocket_connection_metric_labels,
            main_pid,
        )),
    ])
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
    TAction: Action + From<OpenTelemetryMiddlewareErrorAction>,
{
    opentelemetry::global::set_error_handler({
        let logger = Arc::new(Mutex::new(logger));
        move |error| {
            if let Ok(mut logger) = logger.lock() {
                let action = TAction::from(OpenTelemetryMiddlewareErrorAction {
                    error: format!("{}", error),
                });
                logger.log(&action)
            }
        }
    })
    .map_err(|err| OpenTelemetryMiddlewareErrorAction {
        error: format!("{}", err),
    })
}

pub fn generate_session_recording_filename(suffix: Option<&str>) -> String {
    format!(
        "{}{}.session",
        format_datetime(SystemTime::now(), "%Y-%m-%d-%s"),
        suffix.unwrap_or("")
    )
}
