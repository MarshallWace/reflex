// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    sync::{Arc, Mutex},
    time::Duration,
};

use futures::{future, stream, Future, FutureExt, SinkExt, Stream, StreamExt};
use http::{
    header::{self, HeaderName, CONTENT_TYPE},
    HeaderMap, HeaderValue, Uri,
};
use hyper::Body;
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator,
    RecordTermType, RefType, SignalType, StateToken, StringTermType, StringValue,
    StructPrototypeType,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_graphql::{
    create_json_error_object,
    subscriptions::{
        deserialize_graphql_server_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage,
    },
    GraphQlOperationPayload,
};
use reflex_json::JsonValue;
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction, EffectUpdateBatch,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_utils::reconnect::ReconnectTimeout;
use tokio::{net::TcpStream, sync::mpsc, time::sleep};
use tokio_tungstenite::{
    tungstenite::{handshake::client::Request as HandshakeRequest, Message},
    MaybeTlsStream, WebSocketStream,
};
use uuid::Uuid;

use crate::{
    action::graphql::{
        GraphQlHandlerWebSocketConnectErrorAction, GraphQlHandlerWebSocketConnectSuccessAction,
        GraphQlHandlerWebSocketServerMessageAction,
    },
    utils::fetch::{fetch, FetchError, FetchRequest},
};

pub const EFFECT_TYPE_GRAPHQL: &'static str = "reflex::graphql";

#[derive(Clone, Copy, Debug)]
pub struct GraphQlHandlerMetricNames {
    pub graphql_effect_connection_count: &'static str,
    pub graphql_effect_total_operation_count: &'static str,
    pub graphql_effect_active_operation_count: &'static str,
}
impl GraphQlHandlerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.graphql_effect_connection_count,
            Unit::Count,
            "Active GraphQL effect Web Socket connection count"
        );
        describe_counter!(
            self.graphql_effect_total_operation_count,
            Unit::Count,
            "Total GraphQL effect operation count"
        );
        describe_gauge!(
            self.graphql_effect_active_operation_count,
            Unit::Count,
            "Active GraphQL effect operation count"
        );
        self
    }
}
impl Default for GraphQlHandlerMetricNames {
    fn default() -> Self {
        Self {
            graphql_effect_connection_count: "graphql_effect_connection_count",
            graphql_effect_total_operation_count: "graphql_effect_total_operation_count",
            graphql_effect_active_operation_count: "graphql_effect_active_operation_count",
        }
    }
}

pub trait GraphQlHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + InboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
    + InboundAction<GraphQlHandlerWebSocketConnectErrorAction>
    + InboundAction<GraphQlHandlerWebSocketServerMessageAction>
    + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
    + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>
    + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> GraphQlHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + InboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
        + InboundAction<GraphQlHandlerWebSocketConnectErrorAction>
        + InboundAction<GraphQlHandlerWebSocketServerMessageAction>
        + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
        + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>
        + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>
        + OutboundAction<EffectEmitAction<T>>
{
}

#[derive(Clone)]
pub struct GraphQlHandler<T, TConnect, TFactory, TAllocator, TReconnect>
where
    T: AsyncExpression,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    client: hyper::Client<TConnect, Body>,
    factory: TFactory,
    allocator: TAllocator,
    reconnect_timeout: TReconnect,
    metric_names: GraphQlHandlerMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TConnect, TFactory, TAllocator, TReconnect>
    GraphQlHandler<T, TConnect, TFactory, TAllocator, TReconnect>
where
    T: AsyncExpression,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    pub fn new(
        client: hyper::Client<TConnect, Body>,
        factory: TFactory,
        allocator: TAllocator,
        reconnect_timeout: TReconnect,
        metric_names: GraphQlHandlerMetricNames,
    ) -> Self {
        Self {
            client,
            factory,
            allocator,
            reconnect_timeout,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

pub struct GraphQlHandlerState {
    http_requests: HashMap<StateToken, HttpRequestState>,
    websocket_requests: HashMap<StateToken, GraphQlConnectionId>,
    websocket_connections: HashMap<GraphQlConnectionId, WebSocketConnectionState>,
    websocket_connection_mappings: HashMap<GraphQlConnectionUrl, GraphQlConnectionId>,
}
impl Default for GraphQlHandlerState {
    fn default() -> Self {
        Self {
            http_requests: Default::default(),
            websocket_requests: Default::default(),
            websocket_connections: Default::default(),
            websocket_connection_mappings: Default::default(),
        }
    }
}

struct HttpRequestState {
    task_pid: ProcessId,
    metric_labels: Arc<Mutex<Option<[(&'static str, String); 3]>>>,
}

struct WebSocketConnectionState {
    url: GraphQlConnectionUrl,
    operations: HashMap<StateToken, WebSocketOperationState>,
    effects: HashMap<GraphQlOperationId, StateToken>,
    connection: WebSocketConnection,
}

struct WebSocketOperationState {
    operation_id: GraphQlOperationId,
    metric_labels: [(&'static str, String); 3],
}

enum WebSocketConnection {
    Pending(ProcessId, PendingWebSocketConnection),
    Connected(ProcessId, mpsc::Sender<GraphQlSubscriptionClientMessage>),
}
struct PendingWebSocketConnection {
    socket: Arc<Mutex<Option<WebSocketStream<MaybeTlsStream<TcpStream>>>>>,
    pending_messages: Vec<GraphQlSubscriptionClientMessage>,
    connection_attempt: usize,
}
impl WebSocketConnection {
    fn send<TAction>(
        &mut self,
        messages: impl IntoIterator<Item = GraphQlSubscriptionClientMessage>,
        context: &mut impl HandlerContext,
    ) -> Option<StateOperation<TAction>>
    where
        TAction: Action + Send + 'static,
    {
        match self {
            Self::Pending(_, connection) => {
                connection.pending_messages.extend(messages);
                None
            }
            Self::Connected(_, connection_tx) => {
                let task_pid = context.generate_pid();
                let task = OperationStream::new({
                    let connection_tx = connection_tx.clone();
                    let messages = messages.into_iter().collect::<Vec<_>>();
                    Box::pin(async move {
                        for message in messages {
                            let _ = connection_tx.send(message).await;
                        }
                        StateOperation::Kill(task_pid)
                    })
                    .into_stream()
                });
                Some(StateOperation::Task(task_pid, task))
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
struct GraphQlConnectionUrl(String);
impl GraphQlConnectionUrl {
    fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl std::fmt::Display for GraphQlConnectionUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct GraphQlConnectionId(Uuid);
impl GraphQlConnectionId {
    fn as_uuid(&self) -> Uuid {
        let Self(value) = self;
        *value
    }
}
impl std::fmt::Display for GraphQlConnectionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_uuid(), f)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
struct GraphQlOperationId(Uuid);
impl GraphQlOperationId {
    fn as_uuid(&self) -> Uuid {
        let Self(value) = self;
        *value
    }
}
impl std::fmt::Display for GraphQlOperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_uuid(), f)
    }
}

impl<T, TConnect, TFactory, TAllocator, TAction, TReconnect> Actor<TAction>
    for GraphQlHandler<T, TConnect, TFactory, TAllocator, TReconnect>
where
    T: AsyncExpression,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TReconnect: ReconnectTimeout + Send + Clone,
    TAction: GraphQlHandlerAction<T> + Send + 'static,
{
    type State = GraphQlHandlerState;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: GraphQlHandlerState,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_handler_websocket_connect_success(
                &mut state, action, metadata, context,
            )
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_handler_websocket_connect_error(
                &mut state, action, metadata, context,
            )
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_handler_websocket_server_message(
                &mut state, action, metadata, context,
            )
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TConnect, TFactory, TAllocator, TReconnect>
    GraphQlHandler<T, TConnect, TFactory, TAllocator, TReconnect>
where
    T: AsyncExpression,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<EffectEmitAction<T>>
            + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
            + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>
            + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_GRAPHQL {
            return None;
        }
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .map(|effect| {
                let state_token = effect.id();
                match parse_graphql_effect_args(effect, &self.factory) {
                    Ok(args) => {
                        if is_websocket_url(&args.url) {
                            let (connect_action, subscribe_action) =
                                self.subscribe_websocket_operation(state, effect, args, context);
                            (
                                (
                                    state_token,
                                    create_pending_expression(&self.factory, &self.allocator),
                                ),
                                (connect_action, subscribe_action),
                            )
                        } else {
                            match self.subscribe_http_operation(state, effect, args, context) {
                                Ok(subscribe_action) => {
                                    let connect_action = None;
                                    (
                                        (
                                            state_token,
                                            create_pending_expression(
                                                &self.factory,
                                                &self.allocator,
                                            ),
                                        ),
                                        (connect_action, Some(subscribe_action)),
                                    )
                                }
                                Err(err) => ((state_token, err), (None, None)),
                            }
                        }
                    }
                    Err(err) => (
                        (
                            state_token,
                            create_error_message_expression(err, &self.factory, &self.allocator),
                        ),
                        (None, None),
                    ),
                }
            })
            .unzip();
        let initial_values_action = if initial_values.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_GRAPHQL.into(),
                        updates: initial_values,
                    }],
                }
                .into(),
            ))
        };
        Some(StateTransition::new(
            initial_values_action
                .into_iter()
                .chain(
                    tasks
                        .into_iter()
                        .flat_map(|(connect_task, subscribe_task)| {
                            connect_task.into_iter().chain(subscribe_task)
                        }),
                ),
        ))
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + Send + 'static,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_GRAPHQL {
            return None;
        }
        let actions = effects
            .iter()
            .filter_map(|effect| {
                if state.http_requests.contains_key(&effect.id()) {
                    let unsubscribe_action = None;
                    let disconnect_action = self.unsubscribe_http_operation(state, effect)?;
                    Some((unsubscribe_action, Some(disconnect_action)))
                } else if state.websocket_requests.contains_key(&effect.id()) {
                    let (unsubscribe_action, disconnect_action) =
                        self.unsubscribe_websocket_operation(state, effect, context)?;
                    Some((unsubscribe_action, disconnect_action))
                } else {
                    None
                }
            })
            .flat_map(|(unsubscribe, disconnect)| unsubscribe.into_iter().chain(disconnect));
        Some(StateTransition::new(actions))
    }
    fn handle_graphql_handler_websocket_connect_success<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketConnectSuccessAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction:
            Action + Send + 'static + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>,
    {
        let GraphQlHandlerWebSocketConnectSuccessAction {
            connection_id,
            url: _,
        } = action;
        let connection_id = *connection_id;
        let connection_state = state
            .websocket_connections
            .get_mut(&GraphQlConnectionId(connection_id))?;
        let (socket, pending_messages) = match &mut connection_state.connection {
            WebSocketConnection::Connected(..) => None,
            WebSocketConnection::Pending(_, connection) => {
                let PendingWebSocketConnection {
                    socket,
                    pending_messages,
                    connection_attempt: _,
                } = connection;
                socket
                    .lock()
                    .unwrap()
                    .take()
                    .map(|socket| (socket, std::mem::replace(pending_messages, Vec::new())))
            }
        }?;
        let (mut socket_tx, socket_rx) = socket.split();
        let listen_task = listen_websocket_connection(socket_rx).map({
            let current_pid = context.pid();
            move |message| {
                StateOperation::Send(
                    current_pid,
                    GraphQlHandlerWebSocketServerMessageAction {
                        connection_id,
                        message,
                    }
                    .into(),
                )
            }
        });
        let (client_messages, send_task) = {
            let (messages_tx, mut messages_rx) =
                mpsc::channel::<GraphQlSubscriptionClientMessage>(32);
            let send_task = async move {
                while let Some(message) = messages_rx.recv().await {
                    let payload = message.into_json();
                    let _ = socket_tx.send(Message::Text(payload.to_string())).await;
                }
            }
            .into_stream()
            .flat_map(|_| stream::empty());
            (messages_tx, send_task)
        };
        let combined_task = stream::select(
            listen_task,
            stream::select(send_task, {
                let client_messages = client_messages.clone();
                async move {
                    for message in pending_messages {
                        let _ = client_messages.send(message).await;
                    }
                }
                .into_stream()
                .flat_map(|_| stream::empty())
            }),
        );
        let task_pid = context.generate_pid();
        connection_state.connection = WebSocketConnection::Connected(task_pid, client_messages);
        Some(StateTransition::new(once(StateOperation::Task(
            task_pid,
            OperationStream::new(Box::pin(combined_task)),
        ))))
    }
    fn handle_graphql_handler_websocket_connect_error<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketConnectErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
            + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>
            + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>,
    {
        let GraphQlHandlerWebSocketConnectErrorAction {
            connection_id,
            url: _,
            error: _,
        } = action;
        let connection_id = GraphQlConnectionId(*connection_id);
        let connection_state = state.websocket_connections.get_mut(&connection_id)?;
        let (connection_pid, connection) = match &mut connection_state.connection {
            WebSocketConnection::Connected(..) => None,
            WebSocketConnection::Pending(connection_pid, connection) => {
                Some((connection_pid, connection))
            }
        }?;
        let reconnect_timeout = self
            .reconnect_timeout
            .duration(connection.connection_attempt)?;
        connection.connection_attempt += 1;
        let (task_pid, task) = create_websocket_connect_task(
            connection_id,
            connection_state.url.clone(),
            connection.socket.clone(),
            context,
            if reconnect_timeout.is_zero() {
                None
            } else {
                Some(reconnect_timeout)
            },
        );
        *connection_pid = task_pid;
        Some(StateTransition::new(once(task)))
    }
    fn handle_graphql_handler_websocket_server_message<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketServerMessageAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let GraphQlHandlerWebSocketServerMessageAction {
            connection_id,
            message,
        } = action;
        let connection_id = GraphQlConnectionId(*connection_id);
        let connection_state = state.websocket_connections.get_mut(&connection_id)?;
        match message {
            GraphQlSubscriptionServerMessage::ConnectionError(payload) => self
                .handle_graphql_handler_websocket_server_message_connection_error(
                    connection_state,
                    payload,
                    metadata,
                    context,
                ),
            GraphQlSubscriptionServerMessage::Data(operation_id, payload) => self
                .handle_graphql_handler_websocket_server_message_data(
                    connection_state,
                    operation_id.as_str(),
                    payload,
                    metadata,
                    context,
                ),
            GraphQlSubscriptionServerMessage::Patch(operation_id, payload) => self
                .handle_graphql_handler_websocket_server_message_patch(
                    connection_state,
                    operation_id.as_str(),
                    payload,
                    metadata,
                    context,
                ),
            GraphQlSubscriptionServerMessage::Error(operation_id, payload) => self
                .handle_graphql_handler_websocket_server_message_error(
                    connection_state,
                    operation_id.as_str(),
                    payload,
                    metadata,
                    context,
                ),
            GraphQlSubscriptionServerMessage::Complete(_operation_id) => None,
            GraphQlSubscriptionServerMessage::ConnectionAck
            | GraphQlSubscriptionServerMessage::ConnectionKeepAlive => None,
        }
    }
    fn handle_graphql_handler_websocket_server_message_connection_error<TAction>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        payload: &JsonValue,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        if connection_state.operations.is_empty() {
            None
        } else {
            let value =
                parse_graphql_error_payload(payload.clone(), &self.factory, &self.allocator);
            Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_GRAPHQL.into(),
                        updates: connection_state
                            .effects
                            .values()
                            .map(|effect_id| (*effect_id, value.clone()))
                            .collect(),
                    }],
                }
                .into(),
            ))))
        }
    }
    fn handle_graphql_handler_websocket_server_message_data<TAction>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        payload: &JsonValue,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let effect_id = Uuid::parse_str(operation_id).ok().and_then(|uuid| {
            let operation_id = GraphQlOperationId(uuid);
            connection_state.effects.get_mut(&operation_id)
        })?;
        let value = parse_graphql_data_payload(payload.clone(), &self.factory, &self.allocator);
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_GRAPHQL.into(),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_graphql_handler_websocket_server_message_patch<TAction>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        _payload: &JsonValue,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let effect_id = Uuid::parse_str(operation_id).ok().and_then(|uuid| {
            let operation_id = GraphQlOperationId(uuid);
            connection_state.effects.get_mut(&operation_id)
        })?;
        // TODO: Support patch messages in graphql client handler
        let value = create_error_message_expression(
            format!("GraphQL patch message not implemented"),
            &self.factory,
            &self.allocator,
        );
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_GRAPHQL.into(),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_graphql_handler_websocket_server_message_error<TAction>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        payload: &JsonValue,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let effect_id = Uuid::parse_str(operation_id).ok().and_then(|uuid| {
            let operation_id = GraphQlOperationId(uuid);
            connection_state.effects.get_mut(&operation_id)
        })?;
        let value = parse_graphql_error_payload(payload.clone(), &self.factory, &self.allocator);
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_GRAPHQL.into(),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn subscribe_http_operation<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        effect: &T::Signal<T>,
        operation: GraphQlEffectArgs,
        context: &mut impl HandlerContext,
    ) -> Result<StateOperation<TAction>, T>
    where
        TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let GraphQlEffectArgs {
            url,
            operation,
            headers,
        } = operation;
        let operation_name = operation.operation_name.clone();
        match fetch_http_graphql_request(
            self.client.clone(),
            url.as_str(),
            operation,
            headers,
            &self.factory,
            &self.allocator,
        ) {
            Ok(request) => {
                // TODO: Allow configurable GraphQL effect metric labels
                let metric_labels = [
                    ("type", String::from("http")),
                    ("url", String::from(url.as_str())),
                    (
                        "operation_name",
                        operation_name.unwrap_or_else(|| String::from("<null>")),
                    ),
                ];
                increment_counter!(
                    self.metric_names.graphql_effect_total_operation_count,
                    &metric_labels
                );
                increment_gauge!(
                    self.metric_names.graphql_effect_active_operation_count,
                    1.0,
                    &metric_labels
                );
                let shared_metric_labels = Arc::new(Mutex::new(Some(metric_labels)));
                let task_pid = context.generate_pid();
                state.http_requests.insert(
                    effect.id(),
                    HttpRequestState {
                        task_pid,
                        metric_labels: shared_metric_labels.clone(),
                    },
                );
                Ok(StateOperation::Task(
                    task_pid,
                    OperationStream::new(
                        Box::pin(request.map({
                            let effect_id = effect.id();
                            let current_pid = context.pid();
                            let metric_names = self.metric_names;
                            move |result| {
                                if let Some(metric_labels) = shared_metric_labels
                                    .lock()
                                    .ok()
                                    .and_then(|mut metric_labels| metric_labels.take())
                                {
                                    decrement_gauge!(
                                        metric_names.graphql_effect_active_operation_count,
                                        1.0,
                                        &metric_labels
                                    );
                                }
                                StateOperation::Send(
                                    current_pid,
                                    EffectEmitAction {
                                        effect_types: vec![EffectUpdateBatch {
                                            effect_type: EFFECT_TYPE_GRAPHQL.into(),
                                            updates: vec![(effect_id, result)],
                                        }],
                                    }
                                    .into(),
                                )
                            }
                        }))
                        .into_stream(),
                    ),
                ))
            }
            Err(err) => Err(create_error_message_expression(
                format!("Invalid GraphQL HTTP request: {}", err),
                &self.factory,
                &self.allocator,
            )),
        }
    }
    fn unsubscribe_http_operation<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        effect: &T::Signal<T>,
    ) -> Option<StateOperation<TAction>>
    where
        TAction: Action,
    {
        let HttpRequestState {
            task_pid,
            metric_labels,
        } = state.http_requests.remove(&effect.id())?;
        if let Some(metric_labels) = metric_labels
            .lock()
            .ok()
            .and_then(|mut metric_labels| metric_labels.take())
        {
            decrement_gauge!(
                self.metric_names.graphql_effect_active_operation_count,
                1.0,
                &metric_labels
            );
        }
        Some(StateOperation::Kill(task_pid))
    }
    fn subscribe_websocket_operation<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        effect: &T::Signal<T>,
        operation: GraphQlEffectArgs,
        context: &mut impl HandlerContext,
    ) -> (
        Option<StateOperation<TAction>>,
        Option<StateOperation<TAction>>,
    )
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
            + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>
            + OutboundAction<GraphQlHandlerWebSocketServerMessageAction>,
    {
        let GraphQlEffectArgs {
            url,
            operation,
            headers: _,
        } = operation;
        let connection_id = match state.websocket_connection_mappings.entry(url.clone()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let metric_labels = [("url", String::from(url.as_str()))];
                increment_gauge!(
                    self.metric_names.graphql_effect_connection_count,
                    1.0,
                    &metric_labels
                );
                *entry.insert(GraphQlConnectionId(Uuid::new_v4()))
            }
        };
        state.websocket_requests.insert(effect.id(), connection_id);
        let (connection_state, connect_task) =
            match state.websocket_connections.entry(connection_id) {
                Entry::Occupied(entry) => (entry.into_mut(), None),
                Entry::Vacant(entry) => {
                    let connection = Arc::new(Mutex::new(None));
                    let (task_pid, connect_task) = create_websocket_connect_task(
                        connection_id,
                        url.clone(),
                        connection.clone(),
                        context,
                        None,
                    );
                    let connection_state = entry.insert(WebSocketConnectionState {
                        url,
                        operations: Default::default(),
                        effects: Default::default(),
                        connection: WebSocketConnection::Pending(
                            task_pid,
                            PendingWebSocketConnection {
                                socket: connection,
                                pending_messages: Vec::new(),
                                connection_attempt: 0,
                            },
                        ),
                    });
                    (connection_state, Some(connect_task))
                }
            };
        let operation_id = match connection_state.operations.entry(effect.id()) {
            Entry::Occupied(entry) => entry.get().operation_id.clone(),
            Entry::Vacant(entry) => {
                let operation_id = GraphQlOperationId(Uuid::new_v4());
                // TODO: Allow configurable GraphQL effect metric labels
                let metric_labels = [
                    ("type", String::from("ws")),
                    ("url", String::from(connection_state.url.as_str())),
                    (
                        "operation_name",
                        String::from(
                            operation
                                .operation_name
                                .as_ref()
                                .map(|value| value.as_str())
                                .unwrap_or("<null>"),
                        ),
                    ),
                ];
                increment_counter!(
                    self.metric_names.graphql_effect_total_operation_count,
                    &metric_labels
                );
                increment_gauge!(
                    self.metric_names.graphql_effect_active_operation_count,
                    1.0,
                    &metric_labels
                );
                entry.insert(WebSocketOperationState {
                    operation_id: operation_id.clone(),
                    metric_labels,
                });
                operation_id
            }
        };
        let subscribe_task = {
            let subscribe_message =
                GraphQlSubscriptionClientMessage::start(operation_id.to_string(), operation);
            connection_state
                .connection
                .send(once(subscribe_message), context)
        };
        connection_state.effects.insert(operation_id, effect.id());
        (connect_task, subscribe_task)
    }
    fn unsubscribe_websocket_operation<TAction>(
        &self,
        state: &mut GraphQlHandlerState,
        effect: &T::Signal<T>,
        context: &mut impl HandlerContext,
    ) -> Option<(
        Option<StateOperation<TAction>>,
        Option<StateOperation<TAction>>,
    )>
    where
        TAction: Action + Send + 'static,
    {
        let connection_id = state.websocket_requests.remove(&effect.id())?;
        let (unsubscribe_action, is_final_subscription) = {
            let connection_state = state.websocket_connections.get_mut(&connection_id)?;
            let WebSocketOperationState {
                operation_id,
                metric_labels,
            } = connection_state.operations.remove(&effect.id())?;
            connection_state.effects.remove(&operation_id);
            decrement_gauge!(
                self.metric_names.graphql_effect_active_operation_count,
                1.0,
                &metric_labels
            );
            let is_final_subscription = connection_state.operations.is_empty();
            let unsubscribe_action = {
                let messages = once(GraphQlSubscriptionClientMessage::stop(
                    operation_id.to_string(),
                ))
                .chain(if is_final_subscription {
                    Some(GraphQlSubscriptionClientMessage::connection_terminate())
                } else {
                    None
                });
                connection_state.connection.send(messages, context)
            };
            (unsubscribe_action, is_final_subscription)
        };
        let disconnect_action = if is_final_subscription {
            state
                .websocket_connections
                .remove(&connection_id)
                .map(|connection_state| {
                    let WebSocketConnectionState {
                        url, connection, ..
                    } = connection_state;
                    let connection_pid = match connection {
                        WebSocketConnection::Pending(task_pid, ..) => task_pid,
                        WebSocketConnection::Connected(task_pid, ..) => task_pid,
                    };
                    if let Some(_) = state.websocket_connection_mappings.remove(&url) {
                        let metric_labels = [("url", String::from(url.as_str()))];
                        decrement_gauge!(
                            self.metric_names.graphql_effect_connection_count,
                            1.0,
                            &metric_labels
                        );
                    }
                    StateOperation::Kill(connection_pid)
                })
        } else {
            None
        };
        Some((unsubscribe_action, disconnect_action))
    }
}

fn listen_websocket_connection(
    socket_rx: impl Stream<Item = Result<Message, tokio_tungstenite::tungstenite::Error>> + Unpin,
) -> impl Stream<Item = GraphQlSubscriptionServerMessage> {
    socket_rx.filter_map(|message| {
        let result = match message {
            Err(err) => Err(format!("{}", err)),
            Ok(message) => {
                let payload = match message {
                    Message::Text(data) => Ok(Some(data)),
                    Message::Binary(data) => match String::from_utf8(data) {
                        Ok(data) => Ok(Some(data)),
                        Err(_) => Err(String::from("Invalid WebSocket message encoding")),
                    },
                    // TODO: Handle GraphQL client close message
                    Message::Close(_) => Ok(None),
                    Message::Ping(_) | Message::Pong(_) | Message::Frame(_) => Ok(None),
                };
                match payload {
                    Err(message) => Err(message),
                    Ok(None) => Ok(None),
                    Ok(Some(data)) => deserialize_graphql_server_message(&data).map(Some),
                }
            }
        }
        .unwrap_or_else(|err| {
            Some(GraphQlSubscriptionServerMessage::ConnectionError(
                JsonValue::from(format!("GraphQL socket error: {}", err)),
            ))
        });
        future::ready(result)
    })
}

fn create_websocket_connect_task<TAction>(
    connection_id: GraphQlConnectionId,
    url: GraphQlConnectionUrl,
    result: Arc<Mutex<Option<WebSocketStream<MaybeTlsStream<TcpStream>>>>>,
    context: &mut impl HandlerContext,
    delay: Option<Duration>,
) -> (ProcessId, StateOperation<TAction>)
where
    TAction: Action
        + Send
        + 'static
        + OutboundAction<GraphQlHandlerWebSocketConnectSuccessAction>
        + OutboundAction<GraphQlHandlerWebSocketConnectErrorAction>,
{
    let current_pid = context.pid();
    let task_pid = context.generate_pid();
    let task = StateOperation::Task(
        task_pid,
        OperationStream::new(
            {
                Box::pin({
                    async move {
                        let _ = match delay {
                            Some(duration) => sleep(duration).await,
                            None => (),
                        };
                        let connection =
                            create_websocket_connection(url.as_str())
                                .await
                                .and_then(|socket| match result.lock() {
                                    Ok(mut connection) => {
                                        connection.replace(socket);
                                        Ok(())
                                    }
                                    Err(err) => Err(format!("{}", err)),
                                });
                        [
                            StateOperation::Kill(task_pid),
                            StateOperation::Send(
                                current_pid,
                                match connection {
                                    Ok(_) => GraphQlHandlerWebSocketConnectSuccessAction {
                                        connection_id: connection_id.as_uuid(),
                                        url: url.into_string(),
                                    }
                                    .into(),
                                    Err(message) => GraphQlHandlerWebSocketConnectErrorAction {
                                        connection_id: connection_id.as_uuid(),
                                        url: url.into_string(),
                                        error: message,
                                    }
                                    .into(),
                                },
                            ),
                        ]
                    }
                })
            }
            .into_stream()
            .flat_map(|results| stream::iter(results)),
        ),
    );
    (task_pid, task)
}

fn fetch_http_graphql_request<T: AsyncExpression, TConnect>(
    client: hyper::Client<TConnect, Body>,
    url: &str,
    operation: GraphQlOperationPayload,
    headers: Option<HeaderMap<HeaderValue>>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<impl Future<Output = T>, FetchError>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    let request = FetchRequest {
        url: String::from(url),
        method: String::from("POST"),
        headers: headers
            .into_iter()
            .flatten()
            .filter_map(|(key, value)| key.map(|key| (key, value)))
            .chain(once((
                CONTENT_TYPE,
                HeaderValue::from_static("application/json"),
            )))
            .collect(),
        body: Some(format!("{}", operation.into_json()).into()),
    };
    let factory = factory.clone();
    let allocator = allocator.clone();
    let request = fetch(client, &request)?;
    Ok(async move {
        request
            .await
            .map_err(|err| format!("{}", err))
            .and_then(
                |(_status, body)| match String::from_utf8(body.into_iter().collect()) {
                    Ok(body) => reflex_json::deserialize(&body),
                    Err(_) => Err(String::from("Invalid JSON response")),
                },
            )
            .map_err(|err| {
                vec![create_json_error_object(
                    format!("GraphQL HTTP request failed: {}", err),
                    None,
                )]
            })
            .and_then(|body| parse_graphql_response_payload(body))
            .and_then(|data| {
                reflex_json::hydrate(data, &factory, &allocator).map_err(|message| {
                    vec![create_json_error_object(
                        format!("Failed to parse GraphQL data payload: {}", message),
                        None,
                    )]
                })
            })
            .unwrap_or_else(|errors| {
                let errors = errors.into_iter().map(|payload| {
                    reflex_json::hydrate(payload, &factory, &allocator).unwrap_or_else(|err| {
                        create_error_message_expression(
                            format!("Failed to deserialize GraphQL error: {}", err),
                            &factory,
                            &allocator,
                        )
                    })
                });
                create_aggregate_error_expression(errors, &factory, &allocator)
            })
    })
}

fn parse_graphql_response_payload(data: JsonValue) -> Result<JsonValue, Vec<JsonValue>> {
    let result = match data {
        JsonValue::Object(value) => {
            let (data, errors) = value
                .into_iter()
                .fold((None, None), |results, (key, value)| {
                    let (data, errors) = results;
                    match key.as_str() {
                        "data" => match &value {
                            JsonValue::Object(_) => (Some(value), errors),
                            _ => (data, errors),
                        },
                        "errors" => match value {
                            JsonValue::Array(errors) => (data, Some(errors)),
                            _ => (data, errors),
                        },
                        _ => (data, errors),
                    }
                });
            match (data, errors) {
                (_, Some(errors)) => Some(Err(errors)),
                (Some(data), None) => Some(Ok(data)),
                _ => None,
            }
        }
        _ => None,
    };
    match result {
        Some(result) => result,
        None => Err(vec![create_json_error_object(
            String::from("Invalid GraphQL response payload"),
            None,
        )]),
    }
}

fn parse_graphql_data_payload<T: Expression>(
    payload: JsonValue,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    parse_graphql_response_payload(payload)
        .and_then(|data| {
            reflex_json::hydrate(data, factory, allocator).map_err(|message| {
                vec![create_json_error_object(
                    format!("Failed to parse GraphQL data payload: {}", message),
                    None,
                )]
            })
        })
        .unwrap_or_else(|errors| {
            let errors = errors.into_iter().map(|payload| {
                reflex_json::hydrate(payload, factory, allocator).unwrap_or_else(|err| {
                    create_error_message_expression(
                        format!("Failed to deserialize GraphQL error: {}", err),
                        factory,
                        allocator,
                    )
                })
            });
            create_aggregate_error_expression(errors, factory, allocator)
        })
}

fn parse_graphql_error_payload<T: Expression>(
    value: JsonValue,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    match reflex_json::hydrate(value, factory, allocator) {
        Ok(payload) => create_error_expression(payload, factory, allocator),
        Err(message) => create_error_message_expression(
            format!("Failed to parse GraphQL error payload: {}", message),
            factory,
            allocator,
        ),
    }
}

fn is_websocket_url(url: &GraphQlConnectionUrl) -> bool {
    url.as_str().starts_with("ws")
}

async fn create_websocket_connection(
    url: &str,
) -> Result<WebSocketStream<MaybeTlsStream<TcpStream>>, String> {
    let uri = url
        .parse::<Uri>()
        .map_err(|_| format!("Invalid URL: {}", url))?;
    let handshake_request = create_websocket_handshake_request(&uri, "graphql-ws")
        .map_err(|err| format!("Failed to create WebSocket upgrade request: {}", err))?;
    let (stream, _handshake_response) =
        tokio_tungstenite::connect_async(handshake_request)
            .await
            .map_err(|err| format!("WebSocket connection error: {}", err))?;
    Ok(stream)
}

fn create_websocket_handshake_request(
    uri: &Uri,
    protocol: &'static str,
) -> http::Result<HandshakeRequest> {
    let host = uri.authority().map(|authority| authority.host());
    // https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers#client_handshake_request
    HandshakeRequest::builder()
        .header(header::HOST, host.unwrap_or(""))
        .header(header::UPGRADE, "websocket")
        .header(header::CONNECTION, "Upgrade")
        .header(header::SEC_WEBSOCKET_PROTOCOL, protocol)
        .header(header::SEC_WEBSOCKET_KEY, "dGhlIHNhbXBsZSBub25jZQ==")
        .header(header::SEC_WEBSOCKET_VERSION, "13")
        .uri(uri)
        .body(())
}

struct GraphQlEffectArgs {
    url: GraphQlConnectionUrl,
    operation: GraphQlOperationPayload,
    headers: Option<HeaderMap<HeaderValue>>,
}

fn parse_graphql_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<GraphQlEffectArgs, String> {
    let args = effect.args().as_deref();
    if args.len() != 7 {
        return Err(format!(
            "Invalid graphql signal: Expected 7 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let url = parse_string_arg(remaining_args.next().unwrap(), factory);
    let query = parse_string_arg(remaining_args.next().unwrap(), factory);
    let operation_name = parse_optional_string_arg(remaining_args.next().unwrap(), factory);
    let variables = parse_optional_object_arg(remaining_args.next().unwrap(), factory)?;
    let extensions = parse_optional_object_arg(remaining_args.next().unwrap(), factory)?;
    let headers = parse_optional_headers_arg(remaining_args.next().unwrap(), factory);
    let _token = remaining_args.next().unwrap();
    match (url, query, operation_name, variables, extensions, headers) {
        (
            Some(url),
            Some(query),
            Some(operation_name),
            Some(variables),
            Some(extensions),
            Some(headers),
        ) => Ok(GraphQlEffectArgs {
            url: GraphQlConnectionUrl(url),
            operation: GraphQlOperationPayload {
                query,
                operation_name,
                variables: variables.into_iter().flatten().collect(),
                extensions: extensions.into_iter().flatten().collect(),
            },
            headers,
        }),
        _ => Err(format!(
            "Invalid graphql signal arguments: {}",
            effect
                .args()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
        )),
    }
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_string_term(value) {
        Some(term) => Some(String::from(term.value().as_deref().as_str())),
        _ => None,
    }
}

fn parse_optional_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<String>> {
    match factory.match_string_term(value) {
        Some(term) => Some(Some(String::from(term.value().as_deref().as_str()))),
        _ => match factory.match_nil_term(value) {
            Some(_) => Some(None),
            _ => None,
        },
    }
}

fn parse_headers_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<HeaderMap<HeaderValue>> {
    match factory.match_record_term(value) {
        Some(term) => term
            .prototype()
            .as_deref()
            .keys()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .zip(term.values().as_deref().iter().map(|item| item.as_deref()))
            .map(|(key, value)| {
                Some((
                    HeaderName::try_from(parse_string_arg(key, factory)?).ok()?,
                    HeaderValue::try_from(parse_string_arg(value, factory)?).ok()?,
                ))
            })
            .collect::<Option<HeaderMap<_>>>(),
        _ => None,
    }
}

fn parse_optional_headers_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<HeaderMap<HeaderValue>>> {
    match factory.match_record_term(value) {
        Some(_) => parse_headers_arg(value, factory).map(Some),
        _ => match factory.match_nil_term(value) {
            Some(_) => Some(None),
            _ => None,
        },
    }
}

fn parse_object_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<impl IntoIterator<Item = (String, JsonValue)>>, String> {
    match factory.match_record_term(value) {
        Some(value) => {
            let properties = value
                .prototype()
                .as_deref()
                .keys()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .zip(value.values().as_deref().iter().map(|item| item.as_deref()))
                .filter_map(|(key, value)| {
                    factory.match_string_term(key).map(|key| {
                        reflex_json::sanitize(value)
                            .map(|value| (String::from(key.value().as_deref().as_str()), value))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Some(properties))
        }
        _ => Ok(None),
    }
}

fn parse_optional_object_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<Option<impl IntoIterator<Item = (String, JsonValue)>>>, String> {
    match factory.match_record_term(value) {
        Some(_) => parse_object_arg(value, factory).map(Some),
        _ => match factory.match_nil_term(value) {
            Some(_) => Ok(Some(None)),
            _ => Ok(None),
        },
    }
}

fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn create_error_message_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(
        factory.create_string_term(allocator.create_string(message)),
        factory,
        allocator,
    )
}

fn create_error_expression<T: Expression>(
    payload: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_aggregate_error_expression(once(payload), factory, allocator)
}

fn create_aggregate_error_expression<T: Expression>(
    payload: impl IntoIterator<Item = T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(payload.into_iter().map(|payload| {
            allocator.create_signal(SignalType::Error, allocator.create_unit_list(payload))
        })),
    )
}
