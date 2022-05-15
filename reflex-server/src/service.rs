// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, iter::once, sync::Arc};

use futures::{future, stream, Future, FutureExt, Sink, SinkExt, Stream, StreamExt};
use http::{header, HeaderMap, HeaderValue, Method, Request, Response, StatusCode};
use hyper::{
    body,
    service::{service_fn, Service},
    upgrade::Upgraded,
    Body,
};
use hyper_tungstenite::{tungstenite::Message, HyperWebsocket, WebSocketStream};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, Reducible, Rewritable},
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_dispatcher::{
    compose_actors,
    scheduler::tokio::{TokioScheduler, TokioSchedulerMetricNames},
    utils::take_until_final_item::TakeUntilFinalItem,
    Action, Actor, InboundAction, OutboundAction,
};
use reflex_graphql::{
    create_json_error_object,
    stdlib::Stdlib as GraphQlStdlib,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage,
    },
    GraphQlOperationPayload,
};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::bytecode_interpreter::{BytecodeInterpreter, BytecodeInterpreterAction},
    worker::bytecode_worker::BytecodeWorkerMetricNames,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use uuid::Uuid;

use crate::{
    middleware::ServerMiddleware,
    server::{
        action::{
            http_server::{HttpServerRequestAction, HttpServerResponseAction},
            websocket_server::{
                WebSocketServerConnectAction, WebSocketServerDisconnectAction,
                WebSocketServerReceiveAction, WebSocketServerSendAction,
            },
        },
        actor::{
            http_graphql_server::HttpGraphQlServerQueryTransform,
            websocket_graphql_server::WebSocketGraphQlServerQueryTransform, ServerAction,
            ServerActor, ServerMetricNames,
        },
        playground::handle_playground_http_request,
        utils::{
            clone_request_wrapper, clone_response_wrapper, create_http_response,
            create_json_http_response, get_cors_headers,
        },
    },
};

pub trait GraphQlWebServerAction<T: Expression>:
    Action
    + ServerAction<T>
    + BytecodeInterpreterAction<T>
    + InboundAction<HttpServerResponseAction>
    + InboundAction<WebSocketServerSendAction>
    + InboundAction<WebSocketServerDisconnectAction>
    + OutboundAction<HttpServerRequestAction>
    + OutboundAction<HttpServerResponseAction>
    + OutboundAction<WebSocketServerConnectAction>
    + OutboundAction<WebSocketServerReceiveAction>
{
}
impl<T: Expression, TAction> GraphQlWebServerAction<T> for TAction where
    Self: Action
        + ServerAction<T>
        + BytecodeInterpreterAction<T>
        + InboundAction<HttpServerResponseAction>
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>
        + OutboundAction<HttpServerRequestAction>
        + OutboundAction<HttpServerResponseAction>
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerReceiveAction>
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
        TPre::State: Send,
        TPost: Actor<TAction> + Send + 'static,
        TPost::State: Send,
        TAction: GraphQlWebServerAction<T> + Send + 'static,
    {
        let runtime = TokioScheduler::<TAction>::new(
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
                        middleware.post,
                    ),
                ),
            ),
            metric_names.scheduler,
        );
        Self { runtime }
    }
    pub fn handle_graphql_http_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>>
    where
        TAction: InboundAction<HttpServerResponseAction>
            + OutboundAction<HttpServerRequestAction>
            + OutboundAction<HttpServerResponseAction>,
    {
        let (headers, body) = request.into_parts();
        let request_id = Uuid::new_v4();
        let dispatch_request = self.runtime.dispatch(Box::pin(
            body::to_bytes(body)
                .map(move |bytes| match bytes {
                    Err(err) => HttpServerResponseAction {
                        request_id,
                        response: create_http_response(
                            StatusCode::BAD_REQUEST,
                            None,
                            Some(format!("Failed to parse incoming request: {}", err)),
                        ),
                    }
                    .into(),
                    Ok(bytes) => {
                        let request = Request::from_parts(headers, bytes);
                        HttpServerRequestAction {
                            request_id,
                            request,
                        }
                        .into()
                    }
                })
                .into_stream(),
        ));
        let subscribe_response_stream = self
            .runtime
            .subscribe(move |action| {
                let HttpServerResponseAction {
                    request_id: response_id,
                    response,
                } = action.match_type()?;
                if *response_id != request_id {
                    return None;
                }
                Some(clone_response_wrapper(response).map(|_| response.body().clone()))
            })
            .map(|stream| stream.take(1));
        subscribe_response_stream.then({
            |response_stream| {
                dispatch_request.then(|_| {
                    response_stream.into_future().map(|(response, _)| {
                        response
                            .map(|response| response.map(Body::from))
                            .unwrap_or_else(|| {
                                Response::builder()
                                    .status(StatusCode::SERVICE_UNAVAILABLE)
                                    .body(Default::default())
                                    .unwrap()
                            })
                    })
                })
            }
        })
    }
    pub fn handle_graphql_websocket_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Result<(Response<Body>, impl Future<Output = ()>), Response<Body>>>
    where
        TAction: InboundAction<HttpServerResponseAction>
            + InboundAction<WebSocketServerSendAction>
            + InboundAction<WebSocketServerDisconnectAction>
            + OutboundAction<HttpServerResponseAction>
            + OutboundAction<WebSocketServerConnectAction>
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<WebSocketServerReceiveAction>,
    {
        let connection_id = Uuid::new_v4();
        let request_headers = clone_request_wrapper(&request);
        match upgrade_websocket(request, "graphql-ws") {
            Err(response) => future::ready(Err(response)).left_future(),
            Ok((response, connection)) => future::ready(Ok((
                response,
                connection.then({
                    let subscribe_websocket_responses =
                        create_websocket_response_stream(&self.runtime, connection_id);
                    let commands = self.runtime.commands();
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
}

fn create_websocket_response_stream<TAction>(
    runtime: &TokioScheduler<TAction>,
    connection_id: Uuid,
) -> impl Future<Output = impl Stream<Item = Message>>
where
    TAction: Action
        + Send
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>,
{
    runtime
        .subscribe(move |action| {
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
            let request_stream = websocket_rx.filter_map(move |message| {
                let parsed_message = message
                    .map_err(|err| format!("{}", err))
                    .and_then(parse_websocket_message)
                    .transpose();
                let result = parsed_message.map(|message| match message {
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
                future::ready(result)
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
    service_fn({
        move |req: Request<Body>| {
            let server = server.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::POST => server.handle_graphql_http_request(req).await,
                    &Method::GET => {
                        if hyper_tungstenite::is_upgrade_request(&req) {
                            match server.handle_graphql_websocket_request(req).await {
                                Err(response) => response,
                                Ok((response, listen_task)) => {
                                    let _ = tokio::spawn(listen_task);
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
