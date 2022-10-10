// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{convert::Infallible, iter::once, sync::Arc};

use futures::{future, stream, Future, FutureExt, Sink, SinkExt, Stream, StreamExt};
use http::{header, HeaderMap, HeaderValue, Method, Request, Response, StatusCode};
use hyper::{
    service::{service_fn, Service},
    upgrade::Upgraded,
    Body,
};
use hyper_tungstenite::{
    tungstenite::{Error as TungsteniteError, Message},
    HyperWebsocket, WebSocketStream,
};
use reflex::core::{Applicable, Expression, InstructionPointer, Reducible, Rewritable};
use reflex_dispatcher::tokio_task_metrics_export::get_task_monitor;
use reflex_dispatcher::{
    scheduler::tokio::{TokioScheduler, TokioSchedulerMetricNames},
    utils::take_until_final_item::TakeUntilFinalItem,
    Action, Actor, AsyncActionFilter, AsyncActionStream, AsyncDispatchResult, AsyncScheduler,
    AsyncSubscriptionStream, ChainedActor, InboundAction, OutboundAction, PostMiddleware,
    PreMiddleware, Scheduler, SchedulerMiddleware,
};
use reflex_graphql::{
    create_json_error_object,
    stdlib::Stdlib as GraphQlStdlib,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage,
    },
    GraphQlOperation, GraphQlSchema,
};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::bytecode_interpreter::{BytecodeInterpreter, BytecodeInterpreterAction},
    worker::bytecode_worker::BytecodeWorkerMetricNames,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_stdlib::Stdlib;
use uuid::Uuid;

use crate::{
    server::{
        action::{
            http_server::{HttpServerRequestAction, HttpServerResponseAction},
            query_inspector_server::{
                QueryInspectorServerHttpRequestAction, QueryInspectorServerHttpResponseAction,
            },
            session_playback_server::{
                SessionPlaybackServerHttpRequestAction, SessionPlaybackServerHttpResponseAction,
            },
            websocket_server::{
                WebSocketServerConnectAction, WebSocketServerDisconnectAction,
                WebSocketServerReceiveAction, WebSocketServerSendAction,
            },
        },
        actor::{
            http_graphql_server::HttpGraphQlServerQueryTransform,
            server::{ServerAction, ServerActor, ServerMetricNames},
            websocket_graphql_server::WebSocketGraphQlServerQueryTransform,
        },
        playground::handle_playground_http_request,
        utils::{
            clone_http_request_wrapper, clone_http_response, clone_http_response_wrapper,
            create_http_response, create_json_http_response, get_cors_headers,
        },
        SessionPlaybackServerAction,
    },
    utils::server::handle_http_request,
};

pub trait GraphQlWebServerAction<T: Expression>:
    Action
    + ServerAction<T>
    + BytecodeInterpreterAction<T>
    + InboundAction<HttpServerResponseAction>
    + InboundAction<WebSocketServerSendAction>
    + InboundAction<WebSocketServerDisconnectAction>
    + InboundAction<QueryInspectorServerHttpResponseAction>
    + OutboundAction<HttpServerRequestAction>
    + OutboundAction<HttpServerResponseAction>
    + OutboundAction<WebSocketServerConnectAction>
    + OutboundAction<WebSocketServerReceiveAction>
    + OutboundAction<QueryInspectorServerHttpRequestAction>
{
}
impl<T: Expression, TAction> GraphQlWebServerAction<T> for TAction where
    Self: Action
        + ServerAction<T>
        + BytecodeInterpreterAction<T>
        + InboundAction<HttpServerResponseAction>
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>
        + InboundAction<QueryInspectorServerHttpResponseAction>
        + OutboundAction<HttpServerRequestAction>
        + OutboundAction<HttpServerResponseAction>
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerReceiveAction>
        + OutboundAction<QueryInspectorServerHttpRequestAction>
{
}

#[derive(Default, Clone, Copy, Debug)]
pub struct GraphQlWebServerMetricNames {
    pub server: ServerMetricNames,
    pub interpreter: BytecodeWorkerMetricNames,
    pub scheduler: TokioSchedulerMetricNames,
}

pub struct GraphQlWebServer<TAction: Action + Send + 'static> {
    runtime: TokioScheduler<TAction>,
    connection_monitor: tokio_metrics::TaskMonitor,
}
impl<TAction: Action + Send + 'static> GraphQlWebServer<TAction> {
    pub fn new<T, TFactory>(
        graph_root: (CompiledProgram, InstructionPointer),
        schema: Option<GraphQlSchema>,
        actor: impl Actor<TAction, State = impl Send + 'static> + Send + 'static,
        middleware: SchedulerMiddleware<
            impl PreMiddleware<TAction, State = impl Send + 'static> + Send + 'static,
            impl PostMiddleware<TAction, State = impl Send + 'static> + Send + 'static,
            TAction,
        >,
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: TFactory,
        allocator: impl AsyncHeapAllocator<T>,
        transform_http: impl HttpGraphQlServerQueryTransform + Send + 'static,
        transform_ws: impl WebSocketGraphQlServerQueryTransform + Send + 'static,
        metric_names: GraphQlWebServerMetricNames,
        get_graphql_query_label: impl Fn(&GraphQlOperation) -> String + Send + 'static,
        get_http_query_metric_labels: impl Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>
            + Send
            + 'static,
        get_websocket_connection_metric_labels: impl Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>
            + Send
            + 'static,
        get_operation_metric_labels: impl Fn(&GraphQlOperation) -> Vec<(String, String)>
            + Send
            + 'static,
    ) -> Result<Self, String>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::String: Send,
        T::Builtin: Send,
        T::Signal<T>: Send,
        T::SignalList<T>: Send,
        T::StructPrototype<T>: Send,
        T::ExpressionList<T>: Send,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + 'static,
        TFactory: AsyncExpressionFactory<T>,
        TAction: GraphQlWebServerAction<T> + Clone + Send + 'static,
    {
        let connection_monitor = get_task_monitor(
            &metric_names.scheduler.tokio_task_metric_names,
            "graphql_connection",
        );
        let server = ServerActor::new(
            schema,
            factory.clone(),
            allocator.clone(),
            transform_http,
            transform_ws,
            metric_names.server,
            get_graphql_query_label,
            get_http_query_metric_labels,
            get_websocket_connection_metric_labels,
            get_operation_metric_labels,
        )?;
        let runtime = TokioScheduler::<TAction>::new(
            ChainedActor::new(
                server,
                ChainedActor::new(
                    BytecodeInterpreter::new(
                        graph_root,
                        compiler_options,
                        interpreter_options,
                        factory,
                        allocator,
                        metric_names.interpreter,
                    ),
                    actor,
                ),
            ),
            middleware,
            metric_names.scheduler,
        );
        Ok(Self {
            runtime,
            connection_monitor,
        })
    }
    pub fn handle_graphql_http_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>>
    where
        TAction: Clone
            + InboundAction<HttpServerResponseAction>
            + OutboundAction<HttpServerRequestAction>
            + OutboundAction<HttpServerResponseAction>,
    {
        handle_graphql_http_request(&self.runtime, request)
    }
}
impl<TAction: Action + Send + 'static> Scheduler for GraphQlWebServer<TAction> {
    type Action = TAction;
}
impl<TAction> AsyncScheduler for GraphQlWebServer<TAction>
where
    TAction: Action + Clone + Send + 'static,
{
    fn dispatch<TActions: AsyncActionStream<Self::Action>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult {
        self.runtime.dispatch(actions)
    }
    fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<Self::Action, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V> {
        self.runtime.subscribe(transform)
    }
}

pub fn graphql_service<TAction>(
    server: Arc<GraphQlWebServer<TAction>>,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    TAction: Action
        + Clone
        + Send
        + 'static
        + InboundAction<HttpServerResponseAction>
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>
        + OutboundAction<HttpServerRequestAction>
        + OutboundAction<HttpServerResponseAction>
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerSendAction>
        + OutboundAction<WebSocketServerReceiveAction>,
{
    let connection_monitor = server.connection_monitor.clone();
    service_fn({
        move |req: Request<Body>| {
            let server = server.clone();
            let connection_monitor = connection_monitor.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::POST => handle_graphql_http_request(&server.runtime, req).await,
                    &Method::GET => {
                        if hyper_tungstenite::is_upgrade_request(&req) {
                            match handle_graphql_websocket_request(&server.runtime, req).await {
                                Err(response) => response,
                                Ok((response, listen_task)) => {
                                    let _ =
                                        tokio::spawn(connection_monitor.instrument(listen_task));
                                    response
                                }
                            }
                        } else {
                            handle_playground_http_request(req).await
                        }
                    }
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => method_not_allowed(req),
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

pub fn query_inspector_service<TScheduler, TAction>(
    scheduler: Arc<TScheduler>,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    TScheduler: AsyncScheduler<Action = TAction> + Send + Sync,
    TAction: Action
        + Clone
        + Send
        + 'static
        + InboundAction<QueryInspectorServerHttpResponseAction>
        + OutboundAction<QueryInspectorServerHttpRequestAction>
        + OutboundAction<QueryInspectorServerHttpResponseAction>,
{
    service_fn({
        move |req: Request<Body>| {
            let scheduler = scheduler.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => handle_query_inspector_http_request(&scheduler, req).await,
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

pub fn session_playback_service<T, TScheduler, TAction>(
    scheduler: Arc<TScheduler>,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    T: Expression,
    TScheduler: AsyncScheduler<Action = TAction> + Send + Sync,
    TAction: Action + Clone + Send + 'static + SessionPlaybackServerAction<T>,
{
    service_fn({
        move |req: Request<Body>| {
            let scheduler = scheduler.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => handle_session_playback_http_request(&scheduler, req).await,
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

fn handle_graphql_http_request<TScheduler, TAction>(
    scheduler: &TScheduler,
    request: Request<Body>,
) -> impl Future<Output = Response<Body>>
where
    TScheduler: AsyncScheduler<Action = TAction>,
    TAction: Action
        + Send
        + Clone
        + 'static
        + InboundAction<HttpServerResponseAction>
        + OutboundAction<HttpServerRequestAction>
        + OutboundAction<HttpServerResponseAction>,
{
    handle_http_request(
        request,
        scheduler,
        |request_id, request| {
            HttpServerRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            HttpServerResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let HttpServerResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response_wrapper(response).map(|_| response.body().clone()))
        },
    )
}

fn handle_graphql_websocket_request<TAction>(
    scheduler: &TokioScheduler<TAction>,
    request: Request<Body>,
) -> impl Future<Output = Result<(Response<Body>, impl Future<Output = ()>), Response<Body>>>
where
    TAction: Action
        + Send
        + Clone
        + 'static
        + InboundAction<HttpServerResponseAction>
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>
        + OutboundAction<HttpServerResponseAction>
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerSendAction>
        + OutboundAction<WebSocketServerReceiveAction>,
{
    let connection_id = Uuid::new_v4();
    let request_headers = clone_http_request_wrapper(&request);
    match upgrade_websocket(request, "graphql-ws") {
        Err(response) => future::ready(Err(response)).left_future(),
        Ok((response, connection)) => future::ready(Ok((
            response,
            connection.then({
                let subscribe_websocket_responses =
                    create_websocket_response_stream(scheduler, connection_id);
                let commands = scheduler.commands();
                move |connection| match connection {
                    Err(err) => {
                        let err = WebSocketServerSendAction {
                            connection_id,
                            message: GraphQlSubscriptionServerMessage::ConnectionError(
                                create_json_error_object(
                                    format!("Failed to initiate WebSocket connection: {}", err),
                                    None,
                                ),
                            ),
                        };
                        let mut commands = commands;
                        async move {
                            let _ = commands.send(err.into());
                        }
                    }
                    .left_future(),
                    Ok(upgraded_socket) => {
                        let actions = create_websocket_action_stream(
                            WebSocketServerConnectAction {
                                connection_id,
                                request: request_headers,
                            },
                            upgraded_socket,
                            subscribe_websocket_responses,
                        );
                        pipe_stream(actions, commands).right_future()
                    }
                }
            }),
        )))
        .right_future(),
    }
}

fn create_websocket_response_stream<TScheduler, TAction>(
    scheduler: &TScheduler,
    connection_id: Uuid,
) -> impl Future<Output = impl Stream<Item = Message>>
where
    TScheduler: AsyncScheduler<Action = TAction>,
    TAction: Action
        + Clone
        + Send
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>,
{
    scheduler
        .subscribe(move |action: &TAction| {
            if let Some(action) = action.match_type() {
                let WebSocketServerSendAction {
                    connection_id: emitted_connection_id,
                    message,
                } = action;
                if *emitted_connection_id != connection_id {
                    return None;
                }
                let message = message.clone().into_json();
                Some(Message::Text(message.to_string()))
            } else if let Some(action) = action.match_type() {
                let WebSocketServerDisconnectAction {
                    connection_id: emitted_connection_id,
                } = action;
                if *emitted_connection_id != connection_id {
                    return None;
                }
                Some(Message::Close(None))
            } else {
                None
            }
        })
        .map(|stream| {
            TakeUntilFinalItem::new(stream, |message| matches!(message, &Message::Close(_)))
        })
}

fn create_websocket_action_stream<TAction>(
    connect_action: WebSocketServerConnectAction,
    upgraded_socket: WebSocketStream<Upgraded>,
    subscribe_websocket_responses: impl Future<Output = impl Stream<Item = Message>>,
) -> impl Stream<Item = TAction>
where
    TAction: Action
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerSendAction>
        + OutboundAction<WebSocketServerReceiveAction>,
{
    let connection_id = connect_action.connection_id;
    let connect_action = stream::iter(once(connect_action.into()));
    let message_actions = subscribe_websocket_responses
        .map(move |response_stream| {
            let (websocket_tx, websocket_rx) = upgraded_socket.split();
            let request_stream = websocket_rx
                .filter_map(|message| {
                    let parsed_message = message
                        .or_else(|err| match err {
                            TungsteniteError::ConnectionClosed => Ok(Message::Close(None)),
                            err => Err(format!("{}", err)),
                        })
                        .and_then(parse_websocket_message)
                        .transpose();
                    future::ready(parsed_message)
                })
                .chain(stream::iter(once(Ok(
                    GraphQlSubscriptionClientMessage::ConnectionTerminate,
                ))))
                .map(move |parsed_message| match parsed_message {
                    Err(err) => WebSocketServerSendAction {
                        connection_id,
                        message: GraphQlSubscriptionServerMessage::ConnectionError(
                            JsonValue::from(err),
                        ),
                    }
                    .into(),
                    Ok(message) => WebSocketServerReceiveAction {
                        connection_id,
                        message,
                    }
                    .into(),
                });
            let response_stream =
                ignore_stream_results(pipe_stream(response_stream, websocket_tx).into_stream());
            let duplex_stream = stream::select(request_stream, response_stream);
            duplex_stream
        })
        .into_stream()
        .flatten();
    connect_action.chain(message_actions)
}

fn handle_query_inspector_http_request<TScheduler, TAction>(
    scheduler: &TScheduler,
    request: Request<Body>,
) -> impl Future<Output = Response<Body>>
where
    TScheduler: AsyncScheduler<Action = TAction>,
    TAction: Action
        + Send
        + Clone
        + 'static
        + InboundAction<QueryInspectorServerHttpResponseAction>
        + OutboundAction<QueryInspectorServerHttpRequestAction>
        + OutboundAction<QueryInspectorServerHttpResponseAction>,
{
    handle_http_request(
        request,
        scheduler,
        |request_id, request| {
            QueryInspectorServerHttpRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            QueryInspectorServerHttpResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let QueryInspectorServerHttpResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response(response))
        },
    )
}

fn handle_session_playback_http_request<TScheduler, TAction>(
    scheduler: &TScheduler,
    request: Request<Body>,
) -> impl Future<Output = Response<Body>>
where
    TScheduler: AsyncScheduler<Action = TAction>,
    TAction: Action
        + Send
        + Clone
        + 'static
        + InboundAction<SessionPlaybackServerHttpResponseAction>
        + OutboundAction<SessionPlaybackServerHttpRequestAction>
        + OutboundAction<SessionPlaybackServerHttpResponseAction>,
{
    handle_http_request(
        request,
        scheduler,
        |request_id, request| {
            SessionPlaybackServerHttpRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            SessionPlaybackServerHttpResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let SessionPlaybackServerHttpResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response(response))
        },
    )
}

fn handle_cors_preflight_request<T: From<String> + Default>(_req: Request<T>) -> Response<T> {
    create_http_response(StatusCode::NO_CONTENT, None, None)
}

fn method_not_allowed<T: From<String> + Default>(_req: Request<T>) -> Response<T> {
    create_http_response(
        StatusCode::METHOD_NOT_ALLOWED,
        None,
        Some(format!("{}", StatusCode::METHOD_NOT_ALLOWED)),
    )
}

fn upgrade_websocket(
    request: Request<Body>,
    protocol: &'static str,
) -> Result<(Response<Body>, HyperWebsocket), Response<Body>> {
    match hyper_tungstenite::upgrade(request, None) {
        Err(err) => Err(create_json_error_message_response(
            StatusCode::BAD_REQUEST,
            format!("{}", err),
        )),
        Ok((mut response, websocket)) => {
            response.headers_mut().insert(
                header::SEC_WEBSOCKET_PROTOCOL,
                HeaderValue::from_static(protocol),
            );
            Ok((response, websocket))
        }
    }
}

fn parse_websocket_message(
    message: Message,
) -> Result<Option<GraphQlSubscriptionClientMessage>, String> {
    match message {
        Message::Text(data) => deserialize_graphql_client_message(&data).map(Some),
        Message::Binary(_) => Err(String::from("Unsupported message encoding")),
        Message::Close(_) => Ok(Some(GraphQlSubscriptionClientMessage::ConnectionTerminate)),
        Message::Ping(_) | Message::Pong(_) | Message::Frame(_) => Ok(None),
    }
}

fn create_json_error_message_response<T: From<String> + Default>(
    status_code: StatusCode,
    message: String,
) -> Response<T> {
    create_json_http_response(status_code, None, &JsonValue::String(message))
}

fn pipe_stream<T>(
    stream: impl Stream<Item = T>,
    sink: impl Sink<T> + Unpin,
) -> impl Future<Output = ()> {
    stream
        .fold(sink, |mut sink, value| async move {
            let _ = sink.send(value).await;
            sink
        })
        .map(|_| ())
}

fn ignore_stream_results<T, V>(stream: impl Stream<Item = T>) -> impl Stream<Item = V> {
    stream.flat_map(|_| stream::empty())
}
