// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, VecDeque},
    convert::Infallible,
    fs,
    iter::once,
    net::SocketAddr,
    path::Path,
    str::FromStr,
    sync::Arc,
};

use opentelemetry::KeyValue;
use reflex_dispatcher::{compose_actors, Actor};

use anyhow::{anyhow, Context, Result};
use futures::{future, Future};
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
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperation, GraphQlSchema};
use reflex_handlers::{actor::grpc::tonic, utils::tls::tokio_native_tls::native_tls};
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::{stdlib::Stdlib as JsonStdlib, JsonMap, JsonValue};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};

use crate::{
    graphql_service,
    middleware::{
        create_grpc_otlp_tracer, create_http_otlp_tracer, OpenTelemetryMiddleware,
        OpenTelemetryMiddlewareAction, ServerMiddleware, TelemetryMiddleware,
        TelemetryMiddlewareAction, TelemetryMiddlewareMetricNames,
    },
    server::actor::{
        http_graphql_server::HttpGraphQlServerQueryTransform,
        websocket_graphql_server::WebSocketGraphQlServerQueryTransform,
    },
    GraphQlWebServer, GraphQlWebServerAction, GraphQlWebServerMetricNames,
};

pub use reflex;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, imports::builtin_imports, static_module_loader,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ReflexServerCliOptions {
    pub address: SocketAddr,
}

#[derive(Clone, Debug)]
pub enum OpenTelemetryConfig {
    Http(OpenTelemetryHttpConfig),
    Grpc(OpenTelemetryGrpcConfig),
}
impl OpenTelemetryConfig {
    pub fn parse_env(args: std::env::Vars) -> Result<Option<Self>> {
        // See https://github.com/open-telemetry/opentelemetry-java/blob/main/sdk-extensions/autoconfigure/README.md#otlp-exporter-span-metric-and-log-exporters
        let named_args = args.into_iter().collect::<HashMap<_, _>>();
        match named_args.get("OTEL_EXPORTER_OTLP_ENDPOINT") {
            None => Ok(None),
            Some(endpoint) => {
                let protocol = if let Some(env_var) = named_args.get("OTEL_EXPORTER_OTLP_PROTOCOL")
                {
                    parse_otlp_protocol(env_var)?
                } else {
                    Default::default()
                };
                let http_headers =
                    if let Some(env_var) = named_args.get("OTEL_EXPORTER_OTLP_HEADERS") {
                        parse_otlp_http_headers(env_var)?
                    } else {
                        Default::default()
                    };
                let tls_cert =
                    if let Some(env_var) = named_args.get("OTEL_EXPORTER_OTLP_CERTIFICATE") {
                        load_otlp_certificate(env_var.as_str()).map(Some)?
                    } else {
                        Default::default()
                    };
                let resource_attributes =
                    if let Some(env_var) = named_args.get("OTEL_RESOURCE_ATTRIBUTES") {
                        parse_otlp_resource_attributes(env_var)?
                    } else {
                        Default::default()
                    };
                match protocol {
                    OpenTelemetryProtocol::Grpc => {
                        let tls_cert = tls_cert
                            .as_ref()
                            .map(|tls_cert| parse_tonic_tls_cert(tls_cert))
                            .transpose()?;
                        Ok(Some(Self::Grpc(OpenTelemetryGrpcConfig {
                            endpoint: String::from(endpoint),
                            tls_cert,
                            resource_attributes,
                        })))
                    }
                    OpenTelemetryProtocol::Http => {
                        let tls_cert = tls_cert
                            .as_ref()
                            .map(|tls_cert| parse_hyper_tls_cert(tls_cert))
                            .transpose()?;
                        Ok(Some(Self::Http(OpenTelemetryHttpConfig {
                            endpoint: String::from(endpoint),
                            http_headers,
                            tls_cert,
                            resource_attributes,
                        })))
                    }
                }
            }
        }
    }
    pub fn into_middleware<
        T: AsyncExpression,
        TAction: TelemetryMiddlewareAction<T> + OpenTelemetryMiddlewareAction + Send,
    >(
        self,
        get_operation_transaction_labels: impl Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
        metric_names: TelemetryMiddlewareMetricNames,
    ) -> Result<impl Actor<TAction, State = impl Send>>
    where
        T: Applicable<T>,
    {
        let tracer = match self {
            Self::Http(config) => {
                let OpenTelemetryHttpConfig {
                    endpoint,
                    http_headers,
                    resource_attributes,
                    tls_cert,
                } = config;
                create_http_otlp_tracer(endpoint, http_headers, tls_cert, resource_attributes)
            }
            Self::Grpc(config) => {
                let OpenTelemetryGrpcConfig {
                    endpoint,
                    resource_attributes,
                    tls_cert,
                } = config;
                create_grpc_otlp_tracer(endpoint, tls_cert, resource_attributes)
            }
        }
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| anyhow!("Failed to initialize OpenTelemetry agent"))?;
        Ok(compose_actors(
            TelemetryMiddleware::new(
                factory.clone(),
                allocator.clone(),
                get_operation_transaction_labels,
                metric_names,
            ),
            OpenTelemetryMiddleware::new(tracer),
        ))
    }
}

#[derive(Clone)]
pub struct OpenTelemetryHttpConfig {
    pub endpoint: String,
    pub http_headers: Vec<(HeaderName, HeaderValue)>,
    pub resource_attributes: Vec<KeyValue>,
    pub tls_cert: Option<native_tls::Certificate>,
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

#[derive(Clone)]
pub struct OpenTelemetryGrpcConfig {
    pub endpoint: String,
    pub resource_attributes: Vec<KeyValue>,
    pub tls_cert: Option<tonic::transport::Certificate>,
}
impl std::fmt::Debug for OpenTelemetryGrpcConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenTelemetryGrpcConfig")
            .field("endpoint", &self.endpoint)
            .field("resource_attributes", &self.resource_attributes)
            .field("tls_cert", &format!("{:?}", self.tls_cert.is_some()))
            .finish()
    }
}

pub fn cli<T, TFactory, TAllocator, TAction, TPre, TPost>(
    args: ReflexServerCliOptions,
    graph_root: (Program, InstructionPointer),
    schema: Option<GraphQlSchema>,
    middleware: ServerMiddleware<TAction, TPre, TPost>,
    factory: &TFactory,
    allocator: &TAllocator,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    transform_http: impl HttpGraphQlServerQueryTransform + Send + 'static,
    transform_ws: impl WebSocketGraphQlServerQueryTransform + Send + 'static,
    metric_names: GraphQlWebServerMetricNames,
    get_http_query_metric_labels: impl Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>
        + Send
        + 'static,
    get_websocket_connection_metric_labels: impl Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>
        + Send
        + 'static,
    get_websocket_operation_metric_labels: impl Fn(Option<&str>, &GraphQlOperation) -> Vec<(String, String)>
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
    TPre::State: Send,
    TPost: Actor<TAction> + Send + 'static,
    TPost::State: Send,
    TAction: GraphQlWebServerAction<T> + Send + 'static,
{
    let app = GraphQlWebServer::new(
        graph_root,
        schema,
        middleware,
        compiler_options,
        interpreter_options,
        factory.clone(),
        allocator.clone(),
        transform_http,
        transform_ws,
        metric_names,
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_websocket_operation_metric_labels,
    )
    .map_err(|err| anyhow!(err))
    .context("Failed to initialize server")?;
    let service = make_service_fn({
        let app = Arc::new(app);
        move |_socket: &AddrStream| {
            let app = Arc::clone(&app);
            let service = graphql_service(app);
            future::ready(Ok::<_, Infallible>(service))
        }
    });
    let server = Server::try_bind(&args.address)
        .with_context(|| "Failed to bind server address")?
        .serve(service);
    Ok(server)
}

pub fn get_operation_transaction_labels(
    operation: &GraphQlOperation,
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

enum OpenTelemetryProtocol {
    Grpc,
    Http,
}
impl Default for OpenTelemetryProtocol {
    fn default() -> Self {
        Self::Grpc
    }
}
fn parse_otlp_protocol(input: &str) -> Result<OpenTelemetryProtocol> {
    match input {
        "grpc" => Ok(OpenTelemetryProtocol::Grpc),
        "http/protobuf" => Ok(OpenTelemetryProtocol::Http),
        _ => Err(anyhow!(
            "Failed to parse OpenTelemetry protocol (allowed values: \"grpc\", \"http/protobuf\")"
        )),
    }
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

fn load_otlp_certificate(path: impl AsRef<Path>) -> Result<Vec<u8>> {
    fs::read(path.as_ref()).with_context(|| {
        format!(
            "Failed to load Opentelemetry TLS certificate: {}",
            path.as_ref().to_string_lossy()
        )
    })
}

fn parse_hyper_tls_cert(pem: &[u8]) -> Result<native_tls::Certificate> {
    native_tls::Certificate::from_pem(pem).context("Failed to parse TLS certificate")
}

fn parse_tonic_tls_cert(pem: &[u8]) -> Result<tonic::transport::Certificate> {
    Ok(tonic::transport::Certificate::from_pem(pem))
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
