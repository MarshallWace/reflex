// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
};

use bytes::Bytes;
use futures::{future, Future, SinkExt, StreamExt};
use http::{header, HeaderValue, Request, Response, StatusCode};
use hyper::{body::HttpBody, Body};
use hyper_tungstenite::{
    tungstenite::{
        protocol::{frame::coding::CloseCode, CloseFrame},
        Message,
    },
    HyperWebsocket,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OperationStream, OutboundAction,
    ProcessId, StateOperation, StateTransition,
};
use reflex_graphql::subscriptions::{
    deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
    GraphQlSubscriptionServerMessage,
};
use reflex_json::JsonValue;
use tokio::sync::{mpsc, oneshot};
use tokio_stream::wrappers::UnboundedReceiverStream;
use uuid::Uuid;

use crate::server::{
    action::{
        http_server::{HttpServerRequestAction, HttpServerResponseAction},
        init::InitRuntimeAction,
        websocket_server::{
            WebSocketServerConnectAction, WebSocketServerDisconnectAction,
            WebSocketServerReceiveAction, WebSocketServerSendAction,
        },
    },
    utils::{clone_request_wrapper, clone_response_wrapper, create_json_http_response},
};

pub(crate) type HyperServerCommandChannel = mpsc::UnboundedSender<HyperChannelMessage>;

pub(crate) struct HyperServer {
    commands: HyperServerCommandChannel,
    commands_rx: Option<mpsc::UnboundedReceiver<HyperChannelMessage>>,
}
impl Default for HyperServer {
    fn default() -> Self {
        let (commands_tx, commands_rx) = mpsc::unbounded_channel::<HyperChannelMessage>();
        Self {
            commands: commands_tx,
            commands_rx: Some(commands_rx),
        }
    }
}

#[derive(Default)]
struct HyperServerState {
    http_requests: HashMap<Uuid, ActiveHttpRequest>,
    websocket_connections: HashMap<Uuid, ActiveWebSocketConnection>,
}
struct ActiveHttpRequest {
    callback: oneshot::Sender<Response<Bytes>>,
}
struct ActiveWebSocketConnection {
    response: mpsc::UnboundedSender<
        Result<GraphQlSubscriptionServerMessage, Option<CloseFrame<'static>>>,
    >,
}

pub(crate) enum HyperChannelMessage {
    HttpRequest(Uuid, Request<Bytes>, oneshot::Sender<Response<Bytes>>),
    HttpResponse(Uuid, Response<Bytes>),
    WebSocketConnect(
        Uuid,
        Request<()>,
        mpsc::UnboundedSender<
            Result<GraphQlSubscriptionServerMessage, Option<CloseFrame<'static>>>,
        >,
    ),
    WebSocketReceive(Uuid, GraphQlSubscriptionClientMessage),
    WebSocketSend(Uuid, GraphQlSubscriptionServerMessage),
    WebSocketDisconnect(Uuid),
}

impl HyperServer {
    pub(crate) fn command_channel(&self) -> HyperServerCommandChannel {
        self.commands.clone()
    }
}

pub trait HyperServerAction:
    Action
    + InboundAction<InitRuntimeAction>
    + InboundAction<HttpServerResponseAction>
    + InboundAction<WebSocketServerSendAction>
    + InboundAction<WebSocketServerDisconnectAction>
    + OutboundAction<HttpServerRequestAction>
    + OutboundAction<WebSocketServerConnectAction>
    + OutboundAction<WebSocketServerReceiveAction>
{
}
impl<TAction> HyperServerAction for TAction where
    Self: Action
        + InboundAction<InitRuntimeAction>
        + InboundAction<HttpServerResponseAction>
        + InboundAction<WebSocketServerSendAction>
        + InboundAction<WebSocketServerDisconnectAction>
        + OutboundAction<HttpServerRequestAction>
        + OutboundAction<WebSocketServerConnectAction>
        + OutboundAction<WebSocketServerReceiveAction>
{
}

impl<TAction> Actor<TAction> for HyperServer
where
    TAction: HyperServerAction + Send + 'static,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_server_init(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_http_graphql_server_response(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_send(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_disconnect(action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default()
    }
}
impl HyperServer {
    fn handle_server_init<TAction>(
        &mut self,
        _action: &InitRuntimeAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<HttpServerRequestAction>
            + OutboundAction<WebSocketServerConnectAction>
            + OutboundAction<WebSocketServerReceiveAction>,
    {
        let listen_task = self.listen(context.pid())?;
        let task_pid = context.generate_pid();
        let listen_action = StateOperation::Task(task_pid, listen_task);
        Some(StateTransition::new(once(listen_action)))
    }
    fn handle_http_graphql_server_response<TAction>(
        &mut self,
        action: &HttpServerResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let HttpServerResponseAction {
            request_id,
            response,
        } = action;
        let _ = self.commands.send(HyperChannelMessage::HttpResponse(
            *request_id,
            clone_response_wrapper(response).map(|_| response.body().clone()),
        ));
        None
    }
    fn handle_websocket_graphql_server_send<TAction>(
        &mut self,
        action: &WebSocketServerSendAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let WebSocketServerSendAction {
            connection_id,
            message,
        } = action;
        // FIXME: Prevent unnecessary websocket response cloning
        let _ = self.commands.send(HyperChannelMessage::WebSocketSend(
            *connection_id,
            message.clone(),
        ));
        None
    }
    fn handle_websocket_graphql_server_disconnect<TAction>(
        &mut self,
        action: &WebSocketServerDisconnectAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let WebSocketServerDisconnectAction { connection_id } = action;
        let _ = self
            .commands
            .send(HyperChannelMessage::WebSocketDisconnect(*connection_id));
        None
    }
}
impl HyperServer {
    fn listen<TAction>(&mut self, pid: ProcessId) -> Option<OperationStream<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<HttpServerRequestAction>
            + OutboundAction<WebSocketServerConnectAction>
            + OutboundAction<WebSocketServerReceiveAction>,
    {
        self.commands_rx.take().map(|commands_rx| {
            OperationStream::new(
                UnboundedReceiverStream::new(commands_rx)
                    .scan(HyperServerState::default(), move |state, action| {
                        let result = match action {
                            HyperChannelMessage::HttpRequest(request_id, request, callback) => {
                                if let Entry::Vacant(entry) = state.http_requests.entry(request_id)
                                {
                                    entry.insert(ActiveHttpRequest { callback });
                                    Some(StateOperation::Send(
                                        pid,
                                        HttpServerRequestAction {
                                            request_id,
                                            request,
                                        }
                                        .into(),
                                    ))
                                } else {
                                    let _ = callback.send(create_json_error_message_response(
                                        StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("Request ID already exists: {}", request_id),
                                    ));
                                    None
                                }
                            }
                            HyperChannelMessage::HttpResponse(request_id, response) => {
                                if let Entry::Occupied(entry) =
                                    state.http_requests.entry(request_id)
                                {
                                    let ActiveHttpRequest { callback } = entry.remove();
                                    let _ = callback.send(response);
                                    None
                                } else {
                                    None
                                }
                            }
                            HyperChannelMessage::WebSocketConnect(
                                connection_id,
                                request,
                                response,
                            ) => {
                                if let Entry::Vacant(entry) =
                                    state.websocket_connections.entry(connection_id)
                                {
                                    entry.insert(ActiveWebSocketConnection { response });
                                    Some(StateOperation::Send(
                                        pid,
                                        WebSocketServerConnectAction {
                                            connection_id,
                                            request,
                                        }
                                        .into(),
                                    ))
                                } else {
                                    let _ = response.send(Err(Some(CloseFrame {
                                        code: CloseCode::Error,
                                        reason: format!(
                                            "Connection ID already exists: {}",
                                            connection_id
                                        )
                                        .into(),
                                    })));
                                    None
                                }
                            }
                            HyperChannelMessage::WebSocketReceive(connection_id, message) => {
                                if state.websocket_connections.contains_key(&connection_id) {
                                    Some(StateOperation::Send(
                                        pid,
                                        WebSocketServerReceiveAction {
                                            connection_id,
                                            message,
                                        }
                                        .into(),
                                    ))
                                } else {
                                    None
                                }
                            }
                            HyperChannelMessage::WebSocketSend(connection_id, message) => {
                                if let Some(ActiveWebSocketConnection { response }) =
                                    state.websocket_connections.get(&connection_id)
                                {
                                    let _ = response.send(Ok(message));
                                    None
                                } else {
                                    None
                                }
                            }
                            HyperChannelMessage::WebSocketDisconnect(connection_id) => {
                                if let Entry::Occupied(entry) =
                                    state.websocket_connections.entry(connection_id)
                                {
                                    let ActiveWebSocketConnection { response } = entry.remove();
                                    let _ = response.send(Err(None));
                                    None
                                } else {
                                    None
                                }
                            }
                        };
                        future::ready(Some(result))
                    })
                    .filter_map(|operation| future::ready(operation)),
            )
        })
    }
    pub fn handle_http_request(
        commands: &HyperServerCommandChannel,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>> + 'static {
        let commands = commands.clone();
        async move {
            let response = match read_http_request(request).await {
                Err(err) => {
                    create_json_error_message_response(StatusCode::BAD_REQUEST, format!("{}", err))
                }
                Ok(request) => {
                    let (response_tx, response_rx) = oneshot::channel::<Response<Bytes>>();
                    let request_id = Uuid::new_v4();
                    let _ = commands.send(HyperChannelMessage::HttpRequest(
                        request_id,
                        request,
                        response_tx,
                    ));
                    match response_rx.await {
                        Err(err) => create_json_error_message_response(
                            StatusCode::BAD_GATEWAY,
                            format!("{}", err),
                        ),
                        Ok(response) => response,
                    }
                }
            }
            .map(|body| Body::from(body));
            response
        }
    }
    pub fn handle_websocket_request(
        commands: &HyperServerCommandChannel,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>> + 'static {
        let commands = commands.clone();
        let request_wrapper = clone_request_wrapper(&request);
        async move {
            let (response, websocket) = upgrade_websocket(request, "graphql-ws").await;
            if let Some(websocket) = websocket {
                tokio::spawn(listen_websocket_connection(
                    websocket,
                    request_wrapper,
                    commands,
                ));
            }
            response
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

async fn upgrade_websocket(
    request: Request<Body>,
    protocol: &'static str,
) -> (Response<Body>, Option<HyperWebsocket>) {
    match hyper_tungstenite::upgrade(request, None) {
        Err(err) => {
            let response =
                create_json_error_message_response(StatusCode::BAD_REQUEST, format!("{}", err))
                    .map(|body| body.into());
            (response, None)
        }
        Ok((mut response, websocket)) => {
            response.headers_mut().insert(
                header::SEC_WEBSOCKET_PROTOCOL,
                HeaderValue::from_static(protocol),
            );
            (response, Some(websocket))
        }
    }
}

async fn listen_websocket_connection(
    websocket: HyperWebsocket,
    request_wrapper: Request<()>,
    commands: HyperServerCommandChannel,
) -> Result<(), hyper_tungstenite::tungstenite::Error> {
    let websocket = websocket.await?;
    let (mut websocket_tx, mut websocket_rx) = websocket.split();
    let connection_id = Uuid::new_v4();
    let (responses_tx, mut responses_rx) = mpsc::unbounded_channel::<
        Result<GraphQlSubscriptionServerMessage, Option<CloseFrame<'static>>>,
    >();
    let handle_tx = tokio::spawn(async move {
        while let Some(message) = responses_rx.recv().await {
            match message {
                Ok(payload) => {
                    let message = payload.into_json();
                    let _ = websocket_tx.send(Message::Text(message.to_string())).await;
                }
                Err(close_frame) => {
                    let _ = websocket_tx.send(Message::Close(close_frame)).await;
                    let _ = websocket_tx.close().await;
                    break;
                }
            }
        }
    });
    let _ = commands.send(HyperChannelMessage::WebSocketConnect(
        connection_id,
        request_wrapper,
        responses_tx.clone(),
    ));
    while let Some(message) = websocket_rx.next().await {
        let parsed_message = message
            .map_err(|err| format!("{}", err))
            .and_then(parse_websocket_message)
            .transpose();
        if let Some(message) = parsed_message {
            match message {
                Ok(message) => {
                    let _ = commands.send(HyperChannelMessage::WebSocketReceive(
                        connection_id,
                        message,
                    ));
                }
                Err(err) => {
                    let _ = responses_tx.send(Ok(
                        GraphQlSubscriptionServerMessage::ConnectionError(JsonValue::from(err)),
                    ));
                }
            }
        }
    }
    handle_tx.abort();
    Ok(())
}

fn create_json_error_message_response(status_code: StatusCode, message: String) -> Response<Bytes> {
    create_json_http_response(status_code, None, &JsonValue::String(message))
}

async fn read_http_request<T: HttpBody>(request: Request<T>) -> Result<Request<Bytes>, T::Error> {
    let cloned_request = clone_request_wrapper(&request);
    hyper::body::to_bytes(request.into_body())
        .await
        .map(|body| cloned_request.map(|_| body))
}
