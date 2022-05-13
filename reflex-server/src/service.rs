// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, sync::Arc, time::SystemTime};

use bytes::Bytes;
use futures::Future;
use http::{header, HeaderMap, Method, Request, Response, StatusCode};
use hyper::{
    service::{service_fn, Service},
    Body,
};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, Reducible, Rewritable},
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_dispatcher::{
    compose_actors,
    scheduler::tokio::{TokioScheduler, TokioSchedulerMetricNames},
    Action, Actor, InboundAction, OutboundAction, Scheduler,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::bytecode_interpreter::{
        BytecodeInterpreter, BytecodeInterpreterAction, BytecodeInterpreterMetricNames,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::{
    middleware::ServerMiddleware,
    server::{
        action::init::InitRuntimeAction,
        actor::{
            http_graphql_server::HttpGraphQlServerQueryTransform,
            websocket_graphql_server::WebSocketGraphQlServerQueryTransform, ServerAction,
            ServerActor, ServerMetricNames,
        },
        playground::handle_playground_http_request,
        utils::{create_http_response, get_cors_headers},
    },
    transport::actor::hyper::{HyperServer, HyperServerAction, HyperServerCommandChannel},
};

pub trait GraphQlWebServerAction<T: Expression>:
    Action
    + ServerAction<T>
    + BytecodeInterpreterAction<T>
    + HyperServerAction
    + InboundAction<InitRuntimeAction>
    + OutboundAction<InitRuntimeAction>
{
}
impl<T: Expression, TAction> GraphQlWebServerAction<T> for TAction where
    Self: Action
        + ServerAction<T>
        + HyperServerAction
        + BytecodeInterpreterAction<T>
        + InboundAction<InitRuntimeAction>
        + OutboundAction<InitRuntimeAction>
{
}

#[derive(Default, Clone, Copy, Debug)]
pub struct GraphQlWebServerMetricNames {
    pub server: ServerMetricNames,
    pub interpreter: BytecodeInterpreterMetricNames,
    pub scheduler: TokioSchedulerMetricNames,
}

pub struct GraphQlWebServer<TAction: Action + Send + 'static> {
    commands: HyperServerCommandChannel,
    _runtime: TokioScheduler<TAction>,
}
impl<TAction: Action + Send + 'static> GraphQlWebServer<TAction> {
    pub fn new<T, TPre, TPost>(
        graph_root: (Program, InstructionPointer),
        middleware: ServerMiddleware<TAction, TPre, TPost>,
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: impl AsyncExpressionFactory<T>,
        allocator: impl AsyncHeapAllocator<T>,
        transform_http: impl HttpGraphQlServerQueryTransform + Send + 'static,
        transform_ws: impl WebSocketGraphQlServerQueryTransform + Send + 'static,
        metric_names: GraphQlWebServerMetricNames,
        get_http_query_metric_labels: impl Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>
            + Send
            + 'static,
        get_websocket_connection_metric_labels: impl Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>
            + Send
            + 'static,
        get_operation_metric_labels: impl Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>
            + Send
            + 'static,
    ) -> Self
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + 'static,
        TPre: Actor<TAction> + Send + 'static,
        TPost: Actor<TAction> + Send + 'static,
        TAction: GraphQlWebServerAction<T> + Send + 'static,
    {
        let server = HyperServer::default();
        let commands = server.command_channel();
        let mut runtime = TokioScheduler::<TAction>::new(
            compose_actors(
                middleware.pre,
                compose_actors(
                    ServerActor::new(
                        factory.clone(),
                        allocator.clone(),
                        transform_http,
                        transform_ws,
                        metric_names.server,
                        get_http_query_metric_labels,
                        get_websocket_connection_metric_labels,
                        get_operation_metric_labels,
                    ),
                    compose_actors(
                        BytecodeInterpreter::new(
                            graph_root,
                            compiler_options,
                            interpreter_options,
                            factory,
                            allocator,
                            metric_names.interpreter,
                        ),
                        compose_actors(server, middleware.post),
                    ),
                ),
            ),
            metric_names.scheduler,
        );
        runtime.dispatch(
            InitRuntimeAction {
                timestamp: SystemTime::now(),
            }
            .into(),
        );
        Self {
            commands,
            _runtime: runtime,
        }
    }
    pub fn handle_http_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>> {
        HyperServer::handle_http_request(&self.commands, request)
    }
    pub fn handle_websocket_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>> {
        HyperServer::handle_websocket_request(&self.commands, request)
    }
}

pub fn graphql_service<TAction: Action + Send + 'static>(
    server: Arc<GraphQlWebServer<TAction>>,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
> {
    service_fn({
        move |req| {
            let server = server.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                // TODO: Handle requests within actor
                let mut response = match req.method() {
                    &Method::POST => server.handle_http_request(req).await,
                    &Method::GET => {
                        if req.headers().contains_key(header::UPGRADE) {
                            server.handle_websocket_request(req).await
                        } else {
                            handle_playground_http_request(req)
                                .await
                                .map(|body| body.into())
                        }
                    }
                    &Method::OPTIONS => handle_cors_preflight_request(req).map(|body| body.into()),
                    _ => method_not_allowed(req).map(|body| body.into()),
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

fn handle_cors_preflight_request<T>(_req: Request<T>) -> Response<Bytes> {
    create_http_response(StatusCode::NO_CONTENT, None, None)
}

fn method_not_allowed<T>(_req: Request<T>) -> Response<Bytes> {
    create_http_response(
        StatusCode::METHOD_NOT_ALLOWED,
        None,
        Some(format!("{}", StatusCode::METHOD_NOT_ALLOWED)),
    )
}
