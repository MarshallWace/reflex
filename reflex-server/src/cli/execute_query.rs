// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use anyhow::{anyhow, Context, Result};
use futures::{future, Future, FutureExt};
use http::{header, HeaderMap, Request, Response};
use reflex::core::{Applicable, InstructionPointer, Reducible, Rewritable};
use reflex_dispatcher::{
    Action, Actor, PostMiddleware, PreMiddleware, SchedulerMiddleware, SerializableAction,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperation, GraphQlSchema};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::{json, stdlib::Stdlib as JsonStdlib, JsonValue};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_stdlib::Stdlib;

use crate::{
    server::{HttpGraphQlServerQueryTransform, NoopWebSocketGraphQlServerQueryTransform},
    utils::operation::format_graphql_operation_label,
    GraphQlWebServer, GraphQlWebServerAction, GraphQlWebServerMetricNames,
};

pub use hyper::Body;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ExecuteQueryCliOptions {
    pub query: String,
    pub variables: Option<String>,
    pub headers: Option<HeaderMap>,
    pub debug_compiler: bool,
    pub debug_interpreter: bool,
    pub debug_stack: bool,
}

pub async fn cli<
    'de,
    TAction,
    T,
    TFactory,
    TAllocator,
    TTransform,
    THttpMiddleware,
    THttpPre,
    THttpPost,
>(
    options: ExecuteQueryCliOptions,
    graph_root: (CompiledProgram, InstructionPointer),
    schema: Option<GraphQlSchema>,
    actor: impl Actor<TAction, State = impl Send + 'static> + Send + 'static,
    middleware: SchedulerMiddleware<
        impl PreMiddleware<TAction, State = impl Send + 'static> + Send + 'static,
        impl PostMiddleware<TAction, State = impl Send + 'static> + Send + 'static,
        TAction,
    >,
    factory: &TFactory,
    allocator: &TAllocator,
    query_transform: TTransform,
    http_middleware: THttpMiddleware,
    metric_names: GraphQlWebServerMetricNames,
) -> Result<String>
where
    T: AsyncExpression
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + serde::Serialize
        + serde::Deserialize<'de>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<JsStdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    THttpMiddleware: HttpMiddleware<THttpPre, THttpPost>,
    THttpPre: Future<Output = Request<Body>>,
    THttpPost: Future<Output = Response<Body>>,
    TAction: Action + SerializableAction + GraphQlWebServerAction<T> + Clone + Send + 'static,
    TTransform: HttpGraphQlServerQueryTransform + Send + 'static,
{
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
    let app = GraphQlWebServer::<TAction>::new(
        graph_root,
        schema,
        actor,
        middleware,
        compiler_options,
        interpreter_options,
        factory.clone(),
        allocator.clone(),
        query_transform,
        NoopWebSocketGraphQlServerQueryTransform,
        metric_names,
        get_graphql_query_label,
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_websocket_operation_metric_labels,
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

fn get_graphql_query_label(operation: &GraphQlOperation) -> String {
    format_graphql_operation_label(operation)
}

fn get_http_query_metric_labels(
    _operation: &GraphQlOperation,
    _headers: &HeaderMap,
) -> Vec<(String, String)> {
    Vec::new()
}

fn get_websocket_connection_metric_labels(
    _connection_params: Option<&JsonValue>,
    _headers: &HeaderMap,
) -> Vec<(String, String)> {
    Vec::new()
}

fn get_websocket_operation_metric_labels(_operation: &GraphQlOperation) -> Vec<(String, String)> {
    Vec::new()
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
