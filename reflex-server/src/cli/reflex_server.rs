// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, VecDeque},
    convert::Infallible,
    iter::once,
    net::SocketAddr,
    str::FromStr,
    sync::Arc,
};

use opentelemetry::KeyValue;
use reflex_dispatcher::{compose_actors, Actor};

use anyhow::{anyhow, Context, Result};
use futures::Future;
use http::{header::HeaderName, HeaderMap, HeaderValue};
use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use nom::{
    bytes::complete::{tag, take_while1},
    multi::separated_list0,
    sequence::tuple,
    IResult,
};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, Reducible, Rewritable},
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload};
use reflex_handlers::utils::tls::native_tls::Certificate;
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::{stdlib::Stdlib as JsonStdlib, JsonMap, JsonValue};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};

use crate::{
    graphql_service,
    middleware::{
        create_http_otlp_tracer, OpenTelemetryMiddleware, OpenTelemetryMiddlewareAction,
        ServerMiddleware, TelemetryMiddleware, TelemetryMiddlewareAction,
    },
    server::actor::{
        http_graphql_server::HttpGraphQlServerQueryTransform,
        websocket_graphql_server::WebSocketGraphQlServerQueryTransform,
    },
    GraphQlWebServer, GraphQlWebServerAction,
};

pub use reflex;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, imports::builtin_imports, static_module_loader,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ReflexServerCliOptions {
    pub address: SocketAddr,
}

#[derive(Clone)]
pub struct OpenTelemetryHttpConfig {
    pub endpoint: String,
    pub http_headers: Vec<(HeaderName, HeaderValue)>,
    pub resource_attributes: Vec<KeyValue>,
    pub tls_cert: Option<Certificate>,
}
impl std::fmt::Debug for OpenTelemetryHttpConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenTelemetryHttpConfig")
            .field("endpoint", &self.endpoint)
            .field("http_headers", &self.http_headers)
            .field("resource_attributes", &self.resource_attributes)
            .field("tls_cert", &format!("{:?}", self.tls_cert.is_some()))
            .finish()
    }
}
impl OpenTelemetryHttpConfig {
    pub fn parse_env(args: std::env::Vars) -> Result<Option<OpenTelemetryHttpConfig>> {
        let named_args = args.into_iter().collect::<HashMap<_, _>>();
        match named_args.get("OTEL_EXPORTER_OTLP_ENDPOINT") {
            None => Ok(None),
            Some(endpoint) => {
                let resource_attributes =
                    if let Some(env_var) = named_args.get("OTEL_RESOURCE_ATTRIBUTES") {
                        parse_otlp_resource_attributes(env_var)?
                    } else {
                        Default::default()
                    };
                let http_headers =
                    if let Some(env_var) = named_args.get("OTEL_EXPORTER_OTLP_HEADERS") {
                        parse_otlp_http_headers(env_var)?
                    } else {
                        Default::default()
                    };
                Ok(Some(OpenTelemetryHttpConfig {
                    resource_attributes,
                    endpoint: String::from(endpoint),
                    http_headers,
                    tls_cert: None,
                }))
            }
        }
    }
    pub fn with_tls_cert(self, tls_cert: Certificate) -> Self {
        Self {
            tls_cert: Some(tls_cert),
            ..self
        }
    }
    pub fn into_middleware<
        T: AsyncExpression,
        TAction: TelemetryMiddlewareAction<T> + OpenTelemetryMiddlewareAction,
    >(
        self,
        get_operation_transaction_labels: impl Fn(
            &GraphQlOperationPayload,
        ) -> (String, Vec<(String, String)>),
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<impl Actor<TAction>>
    where
        T: Applicable<T>,
    {
        let Self {
            resource_attributes,
            endpoint,
            http_headers,
            tls_cert,
        } = self;
        let tracer = create_http_otlp_tracer(resource_attributes, endpoint, http_headers, tls_cert)
            .map_err(|err| anyhow!("{}", err))
            .with_context(|| anyhow!("Failed to initialize OpenTelemetry agent"))?;
        Ok(compose_actors(
            TelemetryMiddleware::new(
                factory.clone(),
                allocator.clone(),
                get_operation_transaction_labels,
            ),
            OpenTelemetryMiddleware::new(tracer),
        ))
    }
}

pub fn cli<T, TFactory, TAllocator, TAction, TPre, TPost>(
    args: ReflexServerCliOptions,
    middleware: ServerMiddleware<TAction, TPre, TPost>,
    graph_root: (Program, InstructionPointer),
    factory: &TFactory,
    allocator: &TAllocator,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    transform_http: impl HttpGraphQlServerQueryTransform + Send + 'static,
    transform_ws: impl WebSocketGraphQlServerQueryTransform + Send + 'static,
    get_http_query_metric_labels: impl Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>
        + Send
        + 'static,
    get_websocket_connection_metric_labels: impl Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>
        + Send
        + 'static,
    get_websocket_operation_metric_labels: impl Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>
        + Send
        + 'static,
) -> Result<impl Future<Output = Result<(), hyper::Error>>>
where
    T: AsyncExpression
        + Expression<String = String>
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>,
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<JsStdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TPre: Actor<TAction> + Send + 'static,
    TPost: Actor<TAction> + Send + 'static,
    TAction: GraphQlWebServerAction<T> + Send + 'static,
{
    let service = make_service_fn({
        let app = Arc::new(GraphQlWebServer::new(
            graph_root,
            middleware,
            compiler_options,
            interpreter_options,
            factory.clone(),
            allocator.clone(),
            transform_http,
            transform_ws,
            get_http_query_metric_labels,
            get_websocket_connection_metric_labels,
            get_websocket_operation_metric_labels,
        ));
        move |_socket: &AddrStream| {
            let app = Arc::clone(&app);
            async move { Ok::<_, Infallible>(graphql_service(Arc::clone(&app))) }
        }
    });
    let server = Server::try_bind(&args.address)
        .with_context(|| anyhow!("Failed to bind server address"))?
        .serve(service);
    Ok(server)
}

pub fn get_operation_transaction_labels(
    operation: &GraphQlOperationPayload,
) -> (String, Vec<(String, String)>) {
    let transaction_label = format!(
        "GraphQL {}",
        operation.operation_name().unwrap_or("<anonymous>")
    );
    let transaction_attributes = once((
        String::from("graphql.operationName"),
        String::from(operation.operation_name().unwrap_or("")),
    ))
    .chain(
        flatten_json_fields([
            (
                String::from("graphql.variables"),
                JsonValue::Object(JsonMap::from_iter(
                    operation
                        .variables()
                        .map(|(key, value)| (String::from(key), value.clone())),
                )),
            ),
            (
                String::from("graphql.extensions"),
                JsonValue::Object(JsonMap::from_iter(
                    operation
                        .extensions()
                        .map(|(key, value)| (String::from(key), value.clone())),
                )),
            ),
        ])
        .into_iter()
        .map(|(key, value)| {
            (
                key,
                match value {
                    JsonValue::String(value) => value,
                    _ => value.to_string(),
                },
            )
        }),
    )
    .collect::<Vec<_>>();
    (transaction_label, transaction_attributes)
}

fn parse_otlp_resource_attributes(input: &str) -> Result<Vec<KeyValue>> {
    let parse_key = take_while1(|chr| chr != '=');
    let parse_equals = tag("=");
    let parse_value = take_while1(|chr| chr != ',');
    let parse_variable = tuple((parse_key, parse_equals, parse_value));
    let parse_separator = tag(",");
    let mut parse_variables = separated_list0(parse_separator, parse_variable);
    let result: IResult<_, _> = parse_variables(input);
    match result {
        Ok((remaining, result)) => {
            if !remaining.is_empty() {
                return Err(anyhow!("Trailing characters: {}", remaining));
            }
            let values = result
                .into_iter()
                .map(|(key, _, value)| KeyValue::new(String::from(key), String::from(value)))
                .collect::<Vec<_>>();
            Ok(values)
        }
        Err(err) => Err(anyhow!("Parse error: {}", err)),
    }
    .with_context(|| anyhow!("Failed to parse OpenTelemetry resource attributes"))
}

fn parse_otlp_http_headers(input: &str) -> Result<Vec<(HeaderName, HeaderValue)>> {
    let parse_key = take_while1(|chr| chr != '=');
    let parse_equals = tag("=");
    let parse_value = take_while1(|chr| chr != ',');
    let parse_variable = tuple((parse_key, parse_equals, parse_value));
    let parse_separator = tag("\n");
    let mut parse_variables = separated_list0(parse_separator, parse_variable);
    let result: IResult<_, _> = parse_variables(input);
    match result {
        Ok((remaining, result)) => {
            if !remaining.is_empty() {
                return Err(anyhow!("Trailing characters: {}", remaining));
            }
            let values = result
                .into_iter()
                .map(|(key, _, value)| {
                    let key = HeaderName::from_str(key)
                        .map_err(|err| anyhow!("{}", err))
                        .with_context(|| anyhow!("Failed to parse header name: \"{}\"", key))?;
                    let value = HeaderValue::from_str(value)
                        .map_err(|err| anyhow!("{}", err))
                        .with_context(|| anyhow!("Failed to parse header value: \"{}\"", value))?;
                    Ok((key, value))
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(values)
        }
        Err(err) => Err(anyhow!("Parse error: {}", err)),
    }
    .with_context(|| anyhow!("Failed to parse OpenTelemetry HTTP headers"))
}

pub fn flatten_json_fields(
    fields: impl IntoIterator<Item = (String, JsonValue)>,
) -> impl IntoIterator<Item = (String, JsonValue)> {
    let mut results = Vec::new();
    let mut remaining = fields.into_iter().collect::<VecDeque<_>>();
    while let Some((key, value)) = remaining.pop_front() {
        match value {
            JsonValue::Object(fields) if !fields.is_empty() => {
                for (child_key, child_value) in fields.into_iter().rev() {
                    let prefixed_key = format!("{}.{}", key, child_key);
                    remaining.push_front((prefixed_key, child_value));
                }
            }
            _ => results.push((key, value)),
        }
    }
    results
}
