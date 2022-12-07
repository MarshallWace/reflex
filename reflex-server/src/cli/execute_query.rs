// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::time::Duration;

use anyhow::{anyhow, Context, Result};
use futures::{future, Future, FutureExt};
use http::{header, HeaderMap, Request, Response};
use metrics::SharedString;
use opentelemetry::trace::{Span, Tracer};
use reflex::core::{Applicable, InstructionPointer, Reducible, Rewritable};
use reflex_dispatcher::{
    Action, Actor, Handler, ProcessId, SchedulerTransition, SerializableAction, TaskFactory,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperation, GraphQlSchema};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_json::{json, JsonValue};
use reflex_runtime::{
    actor::bytecode_interpreter::BytecodeInterpreterMetricLabels, AsyncExpression,
    AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_scheduler::tokio::{
    TokioInbox, TokioSchedulerInstrumentation, TokioSchedulerLogger, TokioThreadPoolFactory,
};
use reflex_stdlib::Stdlib;
use serde::{Deserialize, Serialize};

use crate::{
    server::{
        GraphQlServerOperationMetricLabels, GraphQlServerQueryLabel,
        HttpGraphQlServerQueryMetricLabels, HttpGraphQlServerQueryTransform,
        WebSocketGraphQlServerConnectionMetricLabels, WebSocketGraphQlServerQueryTransform,
    },
    tokio_runtime_metrics_export::{start_runtime_monitoring, TokioRuntimeMonitorMetricNames},
    utils::operation::format_graphql_operation_label,
    GraphQlWebServer, GraphQlWebServerAction, GraphQlWebServerActor, GraphQlWebServerActorFactory,
    GraphQlWebServerInitContext, GraphQlWebServerInstrumentation, GraphQlWebServerMetricNames,
    GraphQlWebServerTask,
};
pub use hyper::Body;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ExecuteQueryCliOptions {
    pub query: String,
    pub variables: Option<String>,
    pub headers: Option<HeaderMap>,
    pub effect_throttle: Option<Duration>,
    pub debug_compiler: bool,
    pub debug_interpreter: bool,
    pub debug_stack: bool,
}

pub async fn cli<
    TAction,
    TTask,
    T,
    TFactory,
    TAllocator,
    TActorFactory,
    TActors,
    TTransformHttp,
    TTransformWs,
    THttpMiddleware,
    THttpPre,
    THttpPost,
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
    options: ExecuteQueryCliOptions,
    graph_root: (CompiledProgram, InstructionPointer),
    schema: Option<GraphQlSchema>,
    custom_actors: GraphQlWebServerActorFactory<
        TAction,
        TTask,
        TLogger,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
        TActorFactory,
        TActors,
    >,
    factory: &TFactory,
    allocator: &TAllocator,
    transform_http: TTransformHttp,
    transform_ws: TTransformWs,
    http_middleware: THttpMiddleware,
    get_graphql_query_label: TGraphQlQueryLabel,
    get_http_query_metric_labels: THttpMetricLabels,
    get_websocket_connection_metric_labels: TConnectionMetricLabels,
    get_operation_metric_labels: TOperationMetricLabels,
    get_worker_metric_labels: TWorkerMetricLabels,
    tracer: TTracer,
    logger: TLogger,
    instrumentation: TInstrumentation,
    metric_names: GraphQlWebServerMetricNames,
    tokio_runtime_metric_names: TokioRuntimeMonitorMetricNames,
    async_tasks: TAsyncTasks,
    blocking_tasks: TBlockingTasks,
) -> Result<String>
where
    T: AsyncExpression
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + Serialize
        + for<'de> Deserialize<'de>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TActorFactory: for<'a> FnOnce(
        &'a mut GraphQlWebServerInitContext<
            'a,
            TAction,
            TTask,
            TLogger,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
        >,
    ) -> TActors,
    TActors: IntoIterator<Item = (ProcessId, TTask::Actor)> + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform + Send + 'static,
    TTransformWs: WebSocketGraphQlServerQueryTransform + Send + 'static,
    THttpMiddleware: HttpMiddleware<THttpPre, THttpPost>,
    THttpPre: Future<Output = Request<Body>>,
    THttpPost: Future<Output = Response<Body>>,
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
    TAction:
        Action + SerializableAction + GraphQlWebServerAction<T> + Clone + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask>
        + GraphQlWebServerTask<T, TFactory, TAllocator>
        + Send
        + 'static,
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
    let compiler_options = if options.debug_compiler {
        CompilerOptions::debug()
    } else {
        CompilerOptions::default()
    };
    let interpreter_options = InterpreterOptions {
        debug_instructions: options.debug_interpreter,
        debug_stack: options.debug_stack,
        ..InterpreterOptions::default()
    };
    let query = options.query;
    let variables = match options.variables {
        None => serde_json::Value::Object(serde_json::Map::new()),
        Some(variables) => serde_json::from_str(&variables)
            .with_context(|| anyhow!("Invalid query parameters: {}", variables))?,
    };
    let effect_throttle = options.effect_throttle;
    let request = {
        let request = Request::builder()
            .method("POST")
            .header(header::CONTENT_TYPE, "application/json");
        let request = options
            .headers
            .into_iter()
            .flatten()
            .filter_map(|(key, value)| key.map(|key| (key, value)))
            .fold(request, |request, (key, value)| request.header(key, value));
        request
            .body({
                json!({
                    "query": query,
                    "variables": variables,
                })
                .to_string()
                .into()
            })
            .with_context(|| anyhow!("Failed to create GraphQL request payload"))
    }?;
    let app = GraphQlWebServer::<TAction, TTask>::new(
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
        instrumentation,
        async_tasks,
        blocking_tasks,
        effect_throttle,
    )
    .map_err(|err| anyhow!(err))
    .context("Failed to initialize server")?;
    let response = {
        let mut http_middleware = http_middleware;
        let request = http_middleware.pre(request).await;
        let response = app.handle_graphql_http_request(request).await;
        let response = http_middleware.post(response).await;
        response
    };
    let status = response.status();
    let bytes = hyper::body::to_bytes(response.into_body())
        .await
        .with_context(|| anyhow!("Invalid response encoding"))?;
    let response = String::from_utf8(bytes.into_iter().collect())
        .with_context(|| anyhow!("Invalid response encoding"))?;
    if status.is_success() {
        Ok(response)
    } else {
        Err(anyhow!("HTTP error {}:\n\n{}", status, response))
    }
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
    fn labels(&self, _operation: &GraphQlOperation) -> Vec<(String, String)> {
        Vec::new()
    }
}
impl HttpGraphQlServerQueryMetricLabels for GraphQlWebServerMetricLabels {
    fn labels(&self, _operation: &GraphQlOperation, _headers: &HeaderMap) -> Vec<(String, String)> {
        Vec::new()
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

pub trait HttpMiddleware<
    TPre: Future<Output = Request<Body>>,
    TPost: Future<Output = Response<Body>>,
>
{
    fn pre(&mut self, req: Request<Body>) -> TPre;
    fn post(&mut self, res: Response<Body>) -> TPost;
}

pub struct NoopHttpMiddleware;
impl HttpMiddleware<future::Ready<Request<Body>>, future::Ready<Response<Body>>>
    for NoopHttpMiddleware
{
    fn pre(&mut self, req: Request<Body>) -> future::Ready<Request<Body>> {
        future::ready(req)
    }
    fn post(&mut self, res: Response<Body>) -> future::Ready<Response<Body>> {
        future::ready(res)
    }
}

impl<T, TPre, TPost>
    HttpMiddleware<
        future::Either<TPre, future::Ready<Request<Body>>>,
        future::Either<TPost, future::Ready<Response<Body>>>,
    > for Option<T>
where
    T: HttpMiddleware<TPre, TPost>,
    TPre: Future<Output = Request<Body>>,
    TPost: Future<Output = Response<Body>>,
{
    fn pre(&mut self, req: Request<Body>) -> future::Either<TPre, future::Ready<Request<Body>>> {
        match self {
            Some(inner) => inner.pre(req).left_future(),
            None => future::ready(req).right_future(),
        }
    }
    fn post(
        &mut self,
        res: Response<Body>,
    ) -> future::Either<TPost, future::Ready<Response<Body>>> {
        match self {
            Some(inner) => inner.post(res).left_future(),
            None => future::ready(res).right_future(),
        }
    }
}
