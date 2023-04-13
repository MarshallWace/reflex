// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::{HashMap, VecDeque},
    convert::Infallible,
    fs,
    iter::once,
    net::SocketAddr,
    path::Path,
    str::FromStr,
    sync::Arc,
    time::Duration,
};

use metrics::SharedString;
use opentelemetry::{
    sdk::{
        resource::{EnvResourceDetector, ResourceDetector, SdkProvidedResourceDetector},
        Resource,
    },
    trace::{Span, Tracer},
    KeyValue,
};
use reflex_dispatcher::{Action, Actor, Handler, ProcessId, SchedulerTransition, TaskFactory};

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
use reflex::core::{Applicable, Expression, InstructionPointer, Reducible, Rewritable};
use reflex_graphql::{GraphQlOperation, GraphQlParserBuiltin, GraphQlSchema};
use reflex_handlers::utils::tls::{parse_ca_certs, rustls};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::bytecode_interpreter::BytecodeInterpreterMetricLabels, task::RuntimeTask,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_scheduler::tokio::{
    TokioInbox, TokioSchedulerInstrumentation, TokioSchedulerLogger, TokioThreadPoolFactory,
};
use serde::{Deserialize, Serialize};

use crate::{
    graphql_service,
    logger::SkipRedispatchedActionsLogger,
    server::{
        actor::{
            create_grpc_otlp_tracer, create_http_otlp_tracer,
            graphql_server::{GraphQlServerOperationMetricLabels, GraphQlServerQueryLabel},
            http_graphql_server::{
                HttpGraphQlServerQueryMetricLabels, HttpGraphQlServerQueryTransform,
            },
            websocket_graphql_server::{
                WebSocketGraphQlServerConnectionMetricLabels, WebSocketGraphQlServerQueryTransform,
            },
            OpenTelemetryAction, OpenTelemetryActor, TelemetryMiddleware,
            TelemetryMiddlewareAction,
        },
        task::websocket_graphql_server::WebSocketGraphQlServerTask,
    },
    utils::operation::format_graphql_operation_label,
    GraphQlWebServer, GraphQlWebServerAction, GraphQlWebServerActor, GraphQlWebServerActorFactory,
    GraphQlWebServerInitContext, GraphQlWebServerInstrumentation, GraphQlWebServerMetricNames,
    GraphQlWebServerTask,
};

use crate::tokio_runtime_metrics_export::{
    start_runtime_monitoring, TokioRuntimeMonitorMetricNames,
};
pub use reflex;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, imports::builtin_imports, static_module_loader,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ReflexServerCliOptions {
    pub address: SocketAddr,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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
                let service_name_attribute = SdkProvidedResourceDetector.detect(Default::default());
                let additional_attributes = EnvResourceDetector::new().detect(Default::default());
                let combined_attributes = service_name_attribute.merge(&additional_attributes);
                match protocol {
                    OpenTelemetryProtocol::Grpc => {
                        let tls_cert = tls_cert
                            .as_ref()
                            .map(|tls_cert| tonic::transport::Certificate::from_pem(tls_cert));
                        Ok(Some(Self::Grpc(OpenTelemetryGrpcConfig {
                            endpoint: String::from(endpoint),
                            tls_cert,
                            resource_attributes: combined_attributes,
                        })))
                    }
                    OpenTelemetryProtocol::Http => {
                        let tls_cert = tls_cert
                            .as_ref()
                            .map(|tls_cert| {
                                parse_ca_certs(tls_cert).context("Failed to parse TLS certificate")
                            })
                            .transpose()?;
                        Ok(Some(Self::Http(OpenTelemetryHttpConfig {
                            endpoint: String::from(endpoint),
                            http_headers,
                            tls_cert,
                            resource_attributes: combined_attributes,
                        })))
                    }
                }
            }
        }
    }
    pub fn into_tracer(self) -> Result<opentelemetry::sdk::trace::Tracer> {
        match self {
            Self::Http(config) => {
                let OpenTelemetryHttpConfig {
                    endpoint,
                    http_headers,
                    resource_attributes,
                    tls_cert,
                } = config;
                create_http_otlp_tracer(endpoint, http_headers, tls_cert, Some(resource_attributes))
            }
            Self::Grpc(config) => {
                let OpenTelemetryGrpcConfig {
                    endpoint,
                    resource_attributes,
                    tls_cert,
                } = config;
                create_grpc_otlp_tracer(endpoint, tls_cert, Some(resource_attributes))
            }
        }
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| anyhow!("Failed to initialize OpenTelemetry agent"))
    }
    pub fn into_actor<T: Expression, TAction, TTask>(
        self,
        get_graphql_query_label: impl Fn(&GraphQlOperation) -> String,
        get_operation_transaction_labels: impl Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
        main_pid: ProcessId,
    ) -> Result<(impl Actor<TAction, TTask>, impl Actor<TAction, TTask>)>
    where
        T: AsyncExpression + Applicable<T>,
        T::String: Send,
        T::Builtin: Send,
        T::Signal: Send,
        T::SignalList: Send,
        T::StructPrototype: Send,
        T::ExpressionList: Send,
        TAction: Action + Send + TelemetryMiddlewareAction<T> + OpenTelemetryAction,
        TTask: TaskFactory<TAction, TTask>,
    {
        let tracer = self.into_tracer()?;
        Ok((
            TelemetryMiddleware::new(
                factory.clone(),
                allocator.clone(),
                get_graphql_query_label,
                get_operation_transaction_labels,
                main_pid,
            ),
            OpenTelemetryActor::new(tracer),
        ))
    }
}

#[derive(Clone)]
pub struct OpenTelemetryHttpConfig {
    pub endpoint: String,
    pub http_headers: Vec<(HeaderName, HeaderValue)>,
    pub resource_attributes: Resource,
    pub tls_cert: Option<Vec<rustls::Certificate>>,
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
impl Serialize for OpenTelemetryHttpConfig {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedOpenTelemetryHttpConfig::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for OpenTelemetryHttpConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedOpenTelemetryHttpConfig::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Serialize, Deserialize)]
struct SerializedOpenTelemetryHttpConfig {
    endpoint: String,
    http_headers: Vec<(String, String)>,
    resource_attributes: Vec<(String, String)>,
    tls_cert: Option<Vec<Vec<u8>>>,
}
impl<'a> From<&'a OpenTelemetryHttpConfig> for SerializedOpenTelemetryHttpConfig {
    fn from(value: &'a OpenTelemetryHttpConfig) -> Self {
        let OpenTelemetryHttpConfig {
            endpoint,
            http_headers,
            resource_attributes,
            tls_cert,
        } = value;
        Self {
            endpoint: String::from(endpoint),
            http_headers: http_headers
                .into_iter()
                .filter_map(|(key, value)| {
                    value
                        .to_str()
                        .ok()
                        .map(|value| (String::from(key.as_str()), String::from(value)))
                })
                .collect(),
            resource_attributes: resource_attributes
                .iter()
                .map(|(key, value)| (String::from(key.as_str()), String::from(value.as_str())))
                .collect(),
            tls_cert: tls_cert.as_ref().map(|tls_certs| {
                tls_certs
                    .iter()
                    .map(|tls_cert| Vec::from(tls_cert.as_ref()))
                    .collect()
            }),
        }
    }
}
impl From<SerializedOpenTelemetryHttpConfig> for OpenTelemetryHttpConfig {
    fn from(value: SerializedOpenTelemetryHttpConfig) -> Self {
        let SerializedOpenTelemetryHttpConfig {
            endpoint,
            http_headers,
            resource_attributes,
            tls_cert,
        } = value;
        Self {
            endpoint,
            http_headers: http_headers
                .into_iter()
                .filter_map(|(key, value)| {
                    match (HeaderName::from_str(&key), HeaderValue::from_str(&value)) {
                        (Ok(key), Ok(value)) => Some((key, value)),
                        _ => None,
                    }
                })
                .collect(),
            resource_attributes: Resource::new(
                resource_attributes
                    .into_iter()
                    .map(|(key, value)| KeyValue::new(key, value)),
            ),
            tls_cert: tls_cert.map(|tls_certs| {
                tls_certs
                    .into_iter()
                    .map(|tls_cert| rustls::Certificate(tls_cert))
                    .collect()
            }),
        }
    }
}

#[derive(Clone)]
pub struct OpenTelemetryGrpcConfig {
    pub endpoint: String,
    pub resource_attributes: Resource,
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
impl Serialize for OpenTelemetryGrpcConfig {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedOpenTelemetryGrpcConfig::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for OpenTelemetryGrpcConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedOpenTelemetryGrpcConfig::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Serialize, Deserialize)]
struct SerializedOpenTelemetryGrpcConfig {
    endpoint: String,
    resource_attributes: Vec<(String, String)>,
    tls_cert: Option<Vec<u8>>,
}
impl<'a> From<&'a OpenTelemetryGrpcConfig> for SerializedOpenTelemetryGrpcConfig {
    fn from(value: &'a OpenTelemetryGrpcConfig) -> Self {
        let OpenTelemetryGrpcConfig {
            endpoint,
            resource_attributes,
            tls_cert,
        } = value;
        Self {
            endpoint: String::from(endpoint),
            resource_attributes: resource_attributes
                .iter()
                .map(|(key, value)| (String::from(key.as_str()), String::from(value.as_str())))
                .collect(),
            tls_cert: tls_cert.as_ref().map(|value| value.clone().into_inner()),
        }
    }
}
impl From<SerializedOpenTelemetryGrpcConfig> for OpenTelemetryGrpcConfig {
    fn from(value: SerializedOpenTelemetryGrpcConfig) -> Self {
        let SerializedOpenTelemetryGrpcConfig {
            endpoint,
            resource_attributes,
            tls_cert,
        } = value;
        Self {
            endpoint,
            resource_attributes: Resource::new(
                resource_attributes
                    .into_iter()
                    .map(|(key, value)| KeyValue::new(key, value)),
            ),
            tls_cert: tls_cert.map(|value| tonic::transport::Certificate::from_pem(&value)),
        }
    }
}

pub fn cli<
    TAction,
    TTask,
    T,
    TFactory,
    TAllocator,
    TActorFactory,
    TActors,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TWorkerMetricLabels,
    TTracer,
    TLogger,
    TInstrumentation,
    TAsyncTasks,
    TBlockingTasks,
>(
    args: ReflexServerCliOptions,
    graph_root: (CompiledProgram, InstructionPointer),
    schema: Option<GraphQlSchema>,
    custom_actors: GraphQlWebServerActorFactory<
        TAction,
        TTask,
        SkipRedispatchedActionsLogger<TLogger, TAction, TTask>,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
        TActorFactory,
        TActors,
    >,
    factory: &TFactory,
    allocator: &TAllocator,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    transform_http: TTransformHttp,
    transform_ws: TTransformWs,
    metric_names: GraphQlWebServerMetricNames,
    tokio_runtime_metric_names: TokioRuntimeMonitorMetricNames,
    get_graphql_query_label: TGraphQlQueryLabel,
    get_http_query_metric_labels: THttpMetricLabels,
    get_websocket_connection_metric_labels: TConnectionMetricLabels,
    get_operation_metric_labels: TOperationMetricLabels,
    get_worker_metric_labels: TWorkerMetricLabels,
    tracer: TTracer,
    logger: TLogger,
    instrumentation: TInstrumentation,
    async_tasks: TAsyncTasks,
    blocking_tasks: TBlockingTasks,
    effect_throttle: Option<Duration>,
) -> Result<impl Future<Output = Result<(), hyper::Error>>>
where
    T: AsyncExpression
        + Expression<String = String>
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    T::Builtin: GraphQlParserBuiltin,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TActorFactory: for<'a> FnOnce(
        &'a mut GraphQlWebServerInitContext<
            'a,
            TAction,
            TTask,
            SkipRedispatchedActionsLogger<TLogger, TAction, TTask>,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
        >,
    ) -> TActors,
    TActors: IntoIterator<Item = (ProcessId, TTask::Actor)> + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform + Send + 'static,
    TTransformWs: WebSocketGraphQlServerQueryTransform + Send + 'static,
    TGraphQlQueryLabel: GraphQlServerQueryLabel + Send + 'static,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels + Send + 'static,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels + Send + 'static,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels + Send + 'static,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TTracer: Tracer + Send + 'static,
    TTracer::Span: Span + Send + Sync + 'static,
    TLogger: TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
    TInstrumentation: GraphQlWebServerInstrumentation
        + TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
    TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TAction: Action + GraphQlWebServerAction<T> + Clone + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask>
        + GraphQlWebServerTask<T, TFactory, TAllocator>
        + Send
        + Sync
        + 'static,
    TTask: RuntimeTask<T, TFactory, TAllocator> + WebSocketGraphQlServerTask + Send + 'static,
    TTask::Actor: GraphQlWebServerActor<
            T,
            TFactory,
            TAllocator,
            TTransformHttp,
            TTransformWs,
            TGraphQlQueryLabel,
            THttpMetricLabels,
            TConnectionMetricLabels,
            TOperationMetricLabels,
            TWorkerMetricLabels,
            TTracer,
        > + Send
        + Sync
        + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    start_runtime_monitoring(
        tokio::runtime::Handle::current(),
        tokio_runtime_metric_names,
        "main",
    );
    let app = GraphQlWebServer::new(
        graph_root,
        schema,
        custom_actors,
        compiler_options,
        interpreter_options,
        factory.clone(),
        allocator.clone(),
        transform_http,
        transform_ws,
        metric_names,
        get_graphql_query_label,
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_operation_metric_labels,
        get_worker_metric_labels,
        tracer,
        logger,
        instrumentation.clone(),
        async_tasks,
        blocking_tasks,
        effect_throttle,
    )
    .map_err(|err| anyhow!(err))
    .context("Failed to initialize server")?;
    let main_pid = app.main_pid();
    let runtime = Arc::new(app);
    let service = make_service_fn({
        move |_socket: &AddrStream| {
            let service = graphql_service(Arc::clone(&runtime), main_pid, instrumentation.clone());
            future::ready(Ok::<_, Infallible>(service))
        }
    });
    let server = Server::try_bind(&args.address)
        .with_context(|| "Failed to bind server address")?
        .serve(service);
    Ok(server)
}

#[derive(Clone, Copy, Debug)]
pub struct GraphQlWebServerMetricLabels;
impl GraphQlServerQueryLabel for GraphQlWebServerMetricLabels {
    fn label(&self, operation: &GraphQlOperation) -> String {
        format_graphql_operation_label(operation)
    }
}
impl BytecodeInterpreterMetricLabels for GraphQlWebServerMetricLabels {
    fn labels(&self, query_name: &str) -> Vec<(SharedString, SharedString)> {
        vec![("worker".into(), String::from(query_name).into())]
    }
}
impl GraphQlServerOperationMetricLabels for GraphQlWebServerMetricLabels {
    fn labels(&self, operation: &GraphQlOperation) -> Vec<(String, String)> {
        vec![(
            String::from("operation_name"),
            String::from(operation.operation_name().unwrap_or("<null>")),
        )]
    }
}
impl HttpGraphQlServerQueryMetricLabels for GraphQlWebServerMetricLabels {
    fn labels(&self, operation: &GraphQlOperation, _headers: &HeaderMap) -> Vec<(String, String)> {
        vec![(
            String::from("operation_name"),
            String::from(operation.operation_name().unwrap_or("<null>")),
        )]
    }
}
impl WebSocketGraphQlServerConnectionMetricLabels for GraphQlWebServerMetricLabels {
    fn labels(
        &self,
        _connection_params: Option<&JsonValue>,
        _headers: &HeaderMap,
    ) -> Vec<(String, String)> {
        Vec::new()
    }
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
                JsonValue::Object(operation.variables().clone()),
            ),
            (
                String::from("graphql.extensions"),
                JsonValue::Object(operation.extensions().clone()),
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
