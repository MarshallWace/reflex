// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::VecDeque,
    iter::once,
    ops::Deref,
    sync::{Arc, Mutex},
    time::Duration,
};

use async_recursion::async_recursion;
use futures::{
    future,
    stream::{self, SplitSink},
    Future, FutureExt, SinkExt, Stream, StreamExt,
};
use http::{header, Uri};
use hyper::Body;
use reflex::core::Uuid;
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, Matcher, MessageData,
    NoopDisposeCallback, ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition,
    TaskFactory, TaskInbox,
};
use reflex_graphql::subscriptions::{
    deserialize_graphql_server_message, GraphQlSubscriptionClientMessage,
    GraphQlSubscriptionServerMessage,
};
use reflex_json::JsonValue;
use reflex_macros::{dispatcher, task_factory_enum, Named};
use serde::{Deserialize, Serialize};
use tokio::net::TcpStream;
use tokio_tungstenite::{
    tungstenite::{Error as TungsteniteError, Message},
    MaybeTlsStream, WebSocketStream,
};

use crate::{
    action::graphql::{
        GraphQlHandlerHttpConnectionErrorAction, GraphQlHandlerHttpFetchCompleteAction,
        GraphQlHandlerWebSocketClientMessageAction, GraphQlHandlerWebSocketConnectSuccessAction,
        GraphQlHandlerWebSocketConnectionErrorAction,
        GraphQlHandlerWebSocketConnectionTerminateAction,
        GraphQlHandlerWebSocketServerMessageAction,
    },
    utils::fetch::{fetch, parse_fetch_request, FetchRequest},
};

#[derive(PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct GraphQlConnectionUrl(String);
impl GraphQlConnectionUrl {
    pub fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    pub fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl From<String> for GraphQlConnectionUrl {
    fn from(value: String) -> Self {
        Self(value)
    }
}
impl std::fmt::Display for GraphQlConnectionUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

pub trait GraphQlHandlerTaskAction:
    GraphQlHandlerHttpFetchTaskAction + GraphQlHandlerWebSocketConnectionTaskAction
{
}
impl<_Self> GraphQlHandlerTaskAction for _Self where
    Self: GraphQlHandlerHttpFetchTaskAction + GraphQlHandlerWebSocketConnectionTaskAction
{
}

pub trait GraphQlHandlerTask<TConnect>:
    From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    + From<GraphQlHandlerWebSocketConnectionTaskFactory>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
}
impl<_Self, TConnect> GraphQlHandlerTask<TConnect> for _Self
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    Self: From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
        + From<GraphQlHandlerWebSocketConnectionTaskFactory>,
{
}

// TODO: Implement Serialize/Deserialize traits for GraphQlHandlerTaskFactory
task_factory_enum!({
    #[derive(Clone)]
    pub enum GraphQlHandlerTaskFactory<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
        HttpFetch(GraphQlHandlerHttpFetchTaskFactory<TConnect>),
        WebSocketConnection(GraphQlHandlerWebSocketConnectionTaskFactory),
    }

    impl<TConnect, TAction, TTask> TaskFactory<TAction, TTask> for GraphQlHandlerTaskFactory<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + GraphQlHandlerTaskAction + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
    }
});

// TODO: Implement Serialize/Deserialize traits for GraphQlHandlerHttpFetchTaskFactory
#[derive(Named, Clone)]
pub struct GraphQlHandlerHttpFetchTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    pub operation_id: Uuid,
    pub client: hyper::Client<TConnect, Body>,
    pub request: FetchRequest,
    pub caller_pid: ProcessId,
}
impl<TConnect, TAction, TTask> TaskFactory<TAction, TTask>
    for GraphQlHandlerHttpFetchTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + GraphQlHandlerHttpFetchTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = GraphQlHandlerHttpFetchTaskActor<TConnect>;
    fn create(self) -> Self::Actor {
        let Self {
            operation_id,
            client,
            request,
            caller_pid,
        } = self;
        GraphQlHandlerHttpFetchTaskActor {
            operation_id,
            client,
            request,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct GraphQlHandlerHttpFetchTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    operation_id: Uuid,
    client: hyper::Client<TConnect, Body>,
    request: FetchRequest,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct GraphQlHandlerHttpFetchTaskActorState;

dispatcher!({
    pub enum GraphQlHandlerHttpFetchTaskAction {
        Inbox(GraphQlHandlerHttpFetchCompleteAction),
        Inbox(GraphQlHandlerHttpConnectionErrorAction),

        Outbox(GraphQlHandlerHttpFetchCompleteAction),
        Outbox(GraphQlHandlerHttpConnectionErrorAction),
    }

    impl<TConnect, TAction, TTask> Dispatcher<TAction, TTask>
        for GraphQlHandlerHttpFetchTaskActor<TConnect>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = GraphQlHandlerHttpFetchTaskActorState;
        type Events<TInbox: TaskInbox<TAction>> = BoxedActionStream<TInbox::Message>;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Async(Box::pin(self.events(inbox)), None)
        }

        fn accept(&self, _action: &GraphQlHandlerHttpFetchCompleteAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerHttpFetchCompleteAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerHttpFetchCompleteAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_http_fetch_complete(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlHandlerHttpConnectionErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerHttpConnectionErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerHttpConnectionErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_http_connection_error(state, action, metadata, context)
        }
    }
});

impl<TConnect> GraphQlHandlerHttpFetchTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn events<TInbox, TAction>(&self, _inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action
            + From<GraphQlHandlerHttpFetchCompleteAction>
            + From<GraphQlHandlerHttpConnectionErrorAction>,
    {
        match parse_fetch_request(&self.request) {
            Err(err) => future::ready(Err(err)).left_future(),
            Ok(request) => fetch(self.client.clone(), request).right_future(),
        }
        .map({
            let operation_id = self.operation_id;
            let url = self.request.url.clone();
            move |result| match result {
                Ok((status_code, body)) => TAction::from(GraphQlHandlerHttpFetchCompleteAction {
                    operation_id,
                    url,
                    status_code,
                    body,
                }),
                Err(err) => TAction::from(GraphQlHandlerHttpConnectionErrorAction {
                    operation_id,
                    url,
                    message: format_http_error_message(err),
                }),
            }
        })
        .map(|action| TInbox::Message::from(action))
        .into_stream()
    }
    fn handle_graphql_handler_http_fetch_complete<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerHttpFetchTaskActorState,
        _action: &GraphQlHandlerHttpFetchCompleteAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerHttpFetchCompleteAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_graphql_handler_http_connection_error<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerHttpFetchTaskActorState,
        _action: &GraphQlHandlerHttpConnectionErrorAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerHttpConnectionErrorAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
}

pub trait GraphQlHandlerWebSocketConnectionTaskAction:
    GraphQlHandlerWebSocketConnectionTaskEventsAction + GraphQlHandlerWebSocketConnectionTaskActorAction
{
}
impl<_Self> GraphQlHandlerWebSocketConnectionTaskAction for _Self where
    Self: GraphQlHandlerWebSocketConnectionTaskEventsAction
        + GraphQlHandlerWebSocketConnectionTaskActorAction
{
}

pub trait GraphQlHandlerWebSocketConnectionTaskEventsAction:
    Matcher<GraphQlHandlerWebSocketClientMessageAction>
    + From<GraphQlHandlerWebSocketConnectionTerminateAction>
{
}
impl<_Self> GraphQlHandlerWebSocketConnectionTaskEventsAction for _Self where
    Self: Matcher<GraphQlHandlerWebSocketClientMessageAction>
        + From<GraphQlHandlerWebSocketConnectionTerminateAction>
{
}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct GraphQlHandlerWebSocketConnectionTaskFactory {
    pub connection_id: Uuid,
    pub url: GraphQlConnectionUrl,
    pub delay: Option<Duration>,
    pub caller_pid: ProcessId,
}
impl<TAction, TTask> TaskFactory<TAction, TTask> for GraphQlHandlerWebSocketConnectionTaskFactory
where
    TAction: Action + GraphQlHandlerWebSocketConnectionTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = GraphQlHandlerWebSocketConnectionTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            connection_id,
            url,
            delay,
            caller_pid,
        } = self;
        GraphQlHandlerWebSocketConnectionTaskActor {
            connection_id,
            url,
            delay,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct GraphQlHandlerWebSocketConnectionTaskActor {
    connection_id: Uuid,
    url: GraphQlConnectionUrl,
    delay: Option<Duration>,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct GraphQlHandlerWebSocketConnectionTaskActorState;

dispatcher!({
    pub enum GraphQlHandlerWebSocketConnectionTaskActorAction {
        Inbox(GraphQlHandlerWebSocketConnectSuccessAction),
        Inbox(GraphQlHandlerWebSocketServerMessageAction),
        Inbox(GraphQlHandlerWebSocketConnectionTerminateAction),
        Inbox(GraphQlHandlerWebSocketConnectionErrorAction),

        Outbox(GraphQlHandlerWebSocketConnectSuccessAction),
        Outbox(GraphQlHandlerWebSocketServerMessageAction),
        Outbox(GraphQlHandlerWebSocketConnectionErrorAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for GraphQlHandlerWebSocketConnectionTaskActor
    where
        TAction: Action
            + Matcher<GraphQlHandlerWebSocketClientMessageAction>
            + From<GraphQlHandlerWebSocketConnectionTerminateAction>
            + Send
            + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = GraphQlHandlerWebSocketConnectionTaskActorState;
        type Events<TInbox: TaskInbox<TAction>> = BoxedActionStream<TInbox::Message>;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Async(Box::pin(self.events(inbox)), None)
        }

        fn accept(&self, _action: &GraphQlHandlerWebSocketConnectSuccessAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerWebSocketConnectSuccessAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerWebSocketConnectSuccessAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_websocket_connect_success(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlHandlerWebSocketServerMessageAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerWebSocketServerMessageAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerWebSocketServerMessageAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_websocket_server_message(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlHandlerWebSocketConnectionTerminateAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerWebSocketConnectionTerminateAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerWebSocketConnectionTerminateAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_websocket_connection_terminate(
                state, action, metadata, context,
            )
        }

        fn accept(&self, _action: &GraphQlHandlerWebSocketConnectionErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlHandlerWebSocketConnectionErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlHandlerWebSocketConnectionErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_handler_websocket_connection_error(state, action, metadata, context)
        }
    }
});

impl GraphQlHandlerWebSocketConnectionTaskActor {
    fn events<TInbox, TAction>(&self, inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action
            + Matcher<GraphQlHandlerWebSocketClientMessageAction>
            + From<GraphQlHandlerWebSocketConnectionErrorAction>
            + From<GraphQlHandlerWebSocketConnectionErrorAction>
            + From<GraphQlHandlerWebSocketServerMessageAction>
            + From<GraphQlHandlerWebSocketConnectSuccessAction>
            + From<GraphQlHandlerWebSocketConnectionTerminateAction>
            + Send
            + 'static,
    {
        let client_messages = inbox.filter_map({
            let connection_id = self.connection_id;
            move |message| {
                let action = message.deref();
                let result = if let Some(GraphQlHandlerWebSocketClientMessageAction {
                    connection_id: message_connection_id,
                    message,
                }) = action.match_type()
                {
                    if *message_connection_id == connection_id {
                        Some(message.clone())
                    } else {
                        None
                    }
                } else {
                    None
                };
                future::ready(result)
            }
        });
        match create_websocket_connection(self.url.as_str(), self.delay) {
            Err(err) => stream::iter([TAction::from(
                GraphQlHandlerWebSocketConnectionErrorAction {
                    connection_id: self.connection_id,
                    url: String::from(self.url.as_str()),
                    message: err,
                    retryable: false,
                },
            )])
            .left_stream(),
            Ok(connect_task) => {
                enum WebSocketConnectionState {
                    Pending(VecDeque<GraphQlSubscriptionClientMessage>),
                    Connected(
                        Option<SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>>,
                    ),
                    Error,
                }
                impl WebSocketConnectionState {
                    fn take_pending_messages(
                        &mut self,
                    ) -> VecDeque<GraphQlSubscriptionClientMessage> {
                        match self {
                            Self::Pending(messages) => std::mem::take(messages),
                            Self::Connected(_) => Default::default(),
                            Self::Error => Default::default(),
                        }
                    }
                }
                #[async_recursion]
                async fn drain_pending_messages(
                    socket_tx: SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>,
                    connection_state: Arc<Mutex<WebSocketConnectionState>>,
                ) -> Result<(), TungsteniteError> {
                    let pending_messages =
                        if let Some(mut connection_state) = connection_state.lock().ok() {
                            let pending_messages = connection_state.take_pending_messages();
                            if pending_messages.is_empty() {
                                *connection_state =
                                    WebSocketConnectionState::Connected(Some(socket_tx));
                                None
                            } else {
                                Some((pending_messages, socket_tx))
                            }
                        } else {
                            None
                        };
                    if let Some((pending_messages, mut socket_tx)) = pending_messages {
                        let result = socket_tx
                            .send_all(&mut stream::iter(pending_messages.into_iter().map(
                                |message| Ok(Message::Text(message.into_json().to_string())),
                            )))
                            .await;
                        match result {
                            Err(err) => Err(err),
                            Ok(_) => drain_pending_messages(socket_tx, connection_state).await,
                        }
                    } else {
                        Ok(())
                    }
                }
                let connection_state = Arc::new(Mutex::new(WebSocketConnectionState::Pending(
                    Default::default(),
                )));
                let listen_task = {
                    let connection_state = connection_state.clone();
                    let connection_id = self.connection_id;
                    let url = self.url.clone();
                    async move {
                        match connect_task.await {
                            Err(err) => {
                                if let Some(mut connection_state) = connection_state.lock().ok() {
                                    *connection_state = WebSocketConnectionState::Error;
                                }
                                stream::iter([TAction::from(
                                    GraphQlHandlerWebSocketConnectionErrorAction {
                                        connection_id,
                                        url: url.into_string(),
                                        message: format_websocket_error_message(err),
                                        retryable: true,
                                    },
                                )])
                                .left_stream()
                            }
                            Ok(socket) => {
                                let (socket_tx, socket_rx) = socket.split();
                                let _ = drain_pending_messages(socket_tx, connection_state.clone())
                                    .await;
                                let server_messages = socket_rx
                                    .filter_map(|message| {
                                        let message = message.or_else(|err| match err {
                                            TungsteniteError::ConnectionClosed => {
                                                Ok(Message::Close(None))
                                            }
                                            err => Err(format!("{}", err)),
                                        });
                                        let result = match message {
                                            Err(err) => Err(err),
                                            Ok(message) => {
                                                let payload = match message {
                                                    Message::Text(data) => Ok(Some(data)),
                                                    Message::Binary(data) => {
                                                        match String::from_utf8(data) {
                                                            Ok(data) => Ok(Some(data)),
                                                            Err(_) => Err(String::from(
                                                                "Invalid message encoding",
                                                            )),
                                                        }
                                                    }
                                                    Message::Close(_) => {
                                                        Err(String::from("Connection closed"))
                                                    }
                                                    Message::Ping(_)
                                                    | Message::Pong(_)
                                                    | Message::Frame(_) => Ok(None),
                                                };
                                                match payload {
                                                    Err(message) => Err(message),
                                                    Ok(None) => Ok(None),
                                                    Ok(Some(data)) => {
                                                        deserialize_graphql_server_message(&data)
                                                            .map(Some)
                                                    }
                                                }
                                            }
                                        }
                                        .unwrap_or_else(|err| {
                                            Some(GraphQlSubscriptionServerMessage::ConnectionError(
                                                JsonValue::from(format_websocket_error_message(
                                                    err,
                                                )),
                                            ))
                                        });
                                        future::ready(result)
                                    })
                                    .map({
                                        move |message| {
                                            TAction::from(
                                                GraphQlHandlerWebSocketServerMessageAction {
                                                    connection_id,
                                                    message: Arc::new(message),
                                                },
                                            )
                                        }
                                    });
                                stream::iter(once(TAction::from(
                                    GraphQlHandlerWebSocketConnectSuccessAction {
                                        connection_id,
                                        url: url.into_string(),
                                    },
                                )))
                                .chain(server_messages)
                                .right_stream()
                            }
                        }
                    }
                };
                let send_task = {
                    let connection_id = self.connection_id;
                    let url = self.url.clone();
                    async move {
                        let mut client_messages = client_messages;
                        while let Some(message) = client_messages.next().await {
                            let is_terminate_message = matches!(
                                &message,
                                GraphQlSubscriptionClientMessage::ConnectionTerminate
                            );
                            let connection =
                                if let Some(mut connection_state) = connection_state.lock().ok() {
                                    match &mut *connection_state {
                                        WebSocketConnectionState::Pending(queue) => {
                                            queue.push_back(message);
                                            None
                                        }
                                        WebSocketConnectionState::Connected(connection) => {
                                            connection.take().map(|socket_tx| (socket_tx, message))
                                        }
                                        WebSocketConnectionState::Error => None,
                                    }
                                } else {
                                    None
                                };
                            if let Some((mut socket_tx, message)) = connection {
                                let _ = socket_tx
                                    .send(Message::Text(message.into_json().to_string()))
                                    .await;
                                if let Some(mut connection_state) = connection_state.lock().ok() {
                                    if let WebSocketConnectionState::Connected(connection) =
                                        &mut *connection_state
                                    {
                                        connection.replace(socket_tx);
                                    }
                                }
                            }
                            if is_terminate_message {
                                break;
                            }
                        }
                        TAction::from(GraphQlHandlerWebSocketConnectionTerminateAction {
                            connection_id,
                            url: url.into_string(),
                        })
                    }
                };
                let combined_events =
                    stream::select(listen_task.into_stream().flatten(), send_task.into_stream());
                combined_events.right_stream()
            }
        }
        .map(|action| TInbox::Message::from(action))
    }
    fn handle_graphql_handler_websocket_connect_success<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerWebSocketConnectionTaskActorState,
        _action: &GraphQlHandlerWebSocketConnectSuccessAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketConnectSuccessAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_graphql_handler_websocket_server_message<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerWebSocketConnectionTaskActorState,
        _action: &GraphQlHandlerWebSocketServerMessageAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketServerMessageAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
    fn handle_graphql_handler_websocket_connection_terminate<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerWebSocketConnectionTaskActorState,
        _action: &GraphQlHandlerWebSocketConnectionTerminateAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([SchedulerCommand::Kill(
            context.pid(),
        )]))
    }
    fn handle_graphql_handler_websocket_connection_error<TAction, TTask>(
        &self,
        _state: &mut GraphQlHandlerWebSocketConnectionTaskActorState,
        _action: &GraphQlHandlerWebSocketConnectionErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketConnectionErrorAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(context.pid()),
            SchedulerCommand::Forward(self.caller_pid),
        ]))
    }
}

fn create_websocket_connection(
    url: &str,
    delay: Option<Duration>,
) -> Result<
    impl Future<Output = Result<WebSocketStream<MaybeTlsStream<TcpStream>>, TungsteniteError>>,
    String,
> {
    let uri = url
        .parse::<Uri>()
        .map_err(|_| format!("Invalid URL: {}", url))?;
    let handshake_request = create_websocket_handshake_request(&uri, "graphql-ws")
        .map_err(|err| format!("Failed to create WebSocket upgrade request: {}", err))?;
    let connection_delay = match delay {
        None => future::ready(()).left_future(),
        Some(duration) => tokio::time::sleep(duration).right_future(),
    };
    Ok(connection_delay.then(|_| {
        tokio_tungstenite::connect_async(handshake_request).map(|handshake_result| {
            match handshake_result {
                Ok((stream, _handshake_response)) => Ok(stream),
                Err(err) => Err(err),
            }
        })
    }))
}

fn create_websocket_handshake_request(
    uri: &Uri,
    protocol: &'static str,
) -> http::Result<tokio_tungstenite::tungstenite::handshake::client::Request> {
    let host = uri.authority().map(|authority| authority.host());
    // https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers#client_handshake_request
    tokio_tungstenite::tungstenite::handshake::client::Request::builder()
        .header(header::HOST, host.unwrap_or(""))
        .header(header::UPGRADE, "websocket")
        .header(header::CONNECTION, "Upgrade")
        .header(header::SEC_WEBSOCKET_PROTOCOL, protocol)
        .header(header::SEC_WEBSOCKET_KEY, "dGhlIHNhbXBsZSBub25jZQ==")
        .header(header::SEC_WEBSOCKET_VERSION, "13")
        .uri(uri)
        .body(())
}

fn format_http_error_message(err: impl std::fmt::Display) -> String {
    format!("GraphQL fetch error: {}", err)
}

fn format_websocket_error_message(err: impl std::fmt::Display) -> String {
    format!("GraphQL socket error: {}", err)
}
