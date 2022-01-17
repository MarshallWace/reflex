// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use http::{header, HeaderMap, Request};
use reflex::{
    compiler::{Compile, CompilerOptions},
    core::{Applicable, Reducible, Rewritable},
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_cli::{compile_entry_point, Syntax};
use reflex_dispatcher::{compose_actors, Action, Actor, EitherActor, SerializableAction};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload};
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::{json, stdlib::Stdlib as JsonStdlib, JsonValue};
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};

use crate::{
    logger::json::JsonActionLogger,
    middleware::{LoggerMiddleware, ServerMiddleware},
    server::{HttpGraphQlServerQueryTransform, NoopWebSocketGraphQlServerQueryTransform},
    GraphQlWebServer, GraphQlWebServerAction,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ExecuteQueryCliOptions {
    pub graph_root: PathBuf,
    pub syntax: Syntax,
    pub query: String,
    pub variables: Option<String>,
    pub headers: Option<HeaderMap>,
    pub capture_effects: Option<PathBuf>,
    pub replay_effects: Option<PathBuf>,
    pub captured_effects: Vec<String>,
    pub debug_actions: bool,
    pub debug_compiler: bool,
    pub debug_interpreter: bool,
    pub debug_stack: bool,
}

pub async fn cli<'de, T, TFactory, TAllocator, TLoader, TMiddleware, TAction, TEnv, TTransform>(
    options: ExecuteQueryCliOptions,
    env: Option<TEnv>,
    module_loader: Option<TLoader>,
    middleware: TMiddleware,
    factory: &TFactory,
    allocator: &TAllocator,
    query_transform: TTransform,
) -> Result<String>
where
    T: AsyncExpression
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + serde::Serialize
        + serde::Deserialize<'de>,
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<JsStdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T> + Sync,
    TAllocator: AsyncHeapAllocator<T> + Sync,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
    TMiddleware: Actor<TAction> + Send + 'static,
    TAction: Action + SerializableAction + GraphQlWebServerAction<T> + Send + 'static,
    TEnv: IntoIterator<Item = (String, String)>,
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
    let graph_root = compile_entry_point(
        options.graph_root.as_path(),
        options.syntax,
        env,
        module_loader,
        &compiler_options,
        factory,
        allocator,
    )?;
    let middleware = if options.debug_actions {
        EitherActor::Left(compose_actors(
            LoggerMiddleware::new(JsonActionLogger::stderr()),
            middleware,
        ))
    } else {
        EitherActor::Right(middleware)
    };
    let app = GraphQlWebServer::<TAction>::new(
        graph_root,
        ServerMiddleware::post(middleware),
        compiler_options,
        interpreter_options,
        factory.clone(),
        allocator.clone(),
        query_transform,
        NoopWebSocketGraphQlServerQueryTransform,
        get_http_query_metric_labels,
        get_websocket_connection_metric_labels,
        get_websocket_operation_metric_labels,
    );
    let query = options.query;
    let variables = match options.variables {
        None => serde_json::Value::Object(serde_json::Map::new()),
        Some(variables) => serde_json::from_str(&variables)
            .with_context(|| anyhow!("Invalid query parameters: {}", variables))?,
    };
    let result = app
        .handle_http_request({
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
                .with_context(|| anyhow!("Failed to create GraphQL request payload"))?
        })
        .await;
    let status = result.status();
    let body = result.into_body();
    let bytes = hyper::body::to_bytes(body)
        .await
        .with_context(|| anyhow!("Invalid response body"))?;
    let response = String::from_utf8(bytes.into_iter().collect())
        .with_context(|| anyhow!("Invalid response encoding"))?;
    if status.is_success() {
        Ok(response)
    } else {
        Err(anyhow!("HTTP error {}:\n\n{}", status, response))
    }
}

fn get_http_query_metric_labels(
    _operation: &GraphQlOperationPayload,
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

fn get_websocket_operation_metric_labels(
    _operation_name: Option<&str>,
    _operation: &GraphQlOperationPayload,
) -> Vec<(String, String)> {
    Vec::new()
}
