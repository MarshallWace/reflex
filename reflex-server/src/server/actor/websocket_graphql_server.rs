// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{empty, once},
    marker::PhantomData,
    time::Duration,
};

use futures::FutureExt;
use http::{HeaderMap, Request};
use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::core::{Expression, ExpressionFactory, Uuid};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response, create_json_error_object,
    parse_graphql_query, serialize_graphql_result_payload,
    subscriptions::{
        GraphQlSubscriptionClientMessage, GraphQlSubscriptionConnectionInitMessage,
        GraphQlSubscriptionServerMessage, GraphQlSubscriptionStartMessage,
        GraphQlSubscriptionStopMessage, GraphQlSubscriptionUpdateMessage, OperationId,
    },
    validate::validate_graphql_result,
    GraphQlOperation, GraphQlQuery, GraphQlQueryTransform, GraphQlSchemaTypes,
};
use reflex_json::{json_object, JsonMap, JsonNumber, JsonValue};
use tokio::time::sleep;

use crate::{
    server::action::{
        graphql_server::{
            GraphQlServerEmitAction, GraphQlServerModifyAction, GraphQlServerParseErrorAction,
            GraphQlServerSubscribeAction, GraphQlServerUnsubscribeAction,
        },
        websocket_server::{
            WebSocketServerConnectAction, WebSocketServerDisconnectAction,
            WebSocketServerReceiveAction, WebSocketServerSendAction,
            WebSocketServerThrottleTimeoutAction,
        },
    },
    utils::transform::apply_graphql_query_transform,
};

#[derive(Clone, Copy, Debug)]
pub struct WebSocketGraphQlServerMetricNames {
    pub graphql_websocket_connection_count: &'static str,
    pub graphql_websocket_initialized_connection_count: &'static str,
}
impl WebSocketGraphQlServerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.graphql_websocket_connection_count,
            Unit::Count,
            "Active client GraphQL Web Socket connection count"
        );
        describe_gauge!(
            self.graphql_websocket_initialized_connection_count,
            Unit::Count,
            "Active initialized client GraphQL Web Socket connection count"
        );
        self
    }
}
impl Default for WebSocketGraphQlServerMetricNames {
    fn default() -> Self {
        Self {
            graphql_websocket_connection_count: "graphql_websocket_connection_count",
            graphql_websocket_initialized_connection_count:
                "graphql_websocket_initialized_connection_count",
        }
    }
}

pub trait WebSocketGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<()>,
        connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue>;
}
impl<T> WebSocketGraphQlServerQueryTransform for T
where
    T: GraphQlQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        _request: &Request<()>,
        _connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue> {
        apply_graphql_query_transform(operation, self).map_err(|(status, message)| {
            create_json_error_object(
                message,
                [(String::from("status"), JsonValue::from(status.as_u16()))],
            )
        })
    }
}

pub struct NoopWebSocketGraphQlServerQueryTransform;
impl WebSocketGraphQlServerQueryTransform for NoopWebSocketGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperation,
        _request: &Request<()>,
        _connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue> {
        Ok(operation)
    }
}
pub enum EitherWebSocketGraphQlServerQueryTransform<
    T1: WebSocketGraphQlServerQueryTransform,
    T2: WebSocketGraphQlServerQueryTransform,
> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Clone for EitherWebSocketGraphQlServerQueryTransform<T1, T2>
where
    T1: WebSocketGraphQlServerQueryTransform + Clone,
    T2: WebSocketGraphQlServerQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}
impl<T1, T2> WebSocketGraphQlServerQueryTransform
    for EitherWebSocketGraphQlServerQueryTransform<T1, T2>
where
    T1: WebSocketGraphQlServerQueryTransform,
    T2: WebSocketGraphQlServerQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<()>,
        connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue> {
        match self {
            Self::Left(inner) => inner.transform(operation, request, connection_params),
            Self::Right(inner) => inner.transform(operation, request, connection_params),
        }
    }
}
pub struct ChainedWebSocketGraphQlServerQueryTransform<
    T1: WebSocketGraphQlServerQueryTransform,
    T2: WebSocketGraphQlServerQueryTransform,
> {
    pub left: T1,
    pub right: T2,
}
impl<T1, T2> Clone for ChainedWebSocketGraphQlServerQueryTransform<T1, T2>
where
    T1: WebSocketGraphQlServerQueryTransform + Clone,
    T2: WebSocketGraphQlServerQueryTransform + Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2> WebSocketGraphQlServerQueryTransform
    for ChainedWebSocketGraphQlServerQueryTransform<T1, T2>
where
    T1: WebSocketGraphQlServerQueryTransform,
    T2: WebSocketGraphQlServerQueryTransform,
{
    fn transform(
        &self,
        operation: GraphQlOperation,
        request: &Request<()>,
        connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperation, JsonValue> {
        let operation = self.left.transform(operation, request, connection_params)?;
        self.right.transform(operation, request, connection_params)
    }
}

pub trait WebSocketGraphQlServerAction<T: Expression>:
    Action
    + InboundAction<WebSocketServerConnectAction>
    + InboundAction<WebSocketServerReceiveAction>
    + InboundAction<WebSocketServerThrottleTimeoutAction>
    + InboundAction<GraphQlServerParseErrorAction<T>>
    + InboundAction<GraphQlServerEmitAction<T>>
    + OutboundAction<GraphQlServerSubscribeAction<T>>
    + OutboundAction<GraphQlServerModifyAction<T>>
    + OutboundAction<GraphQlServerUnsubscribeAction<T>>
    + OutboundAction<WebSocketServerSendAction>
    + OutboundAction<WebSocketServerDisconnectAction>
    + OutboundAction<WebSocketServerThrottleTimeoutAction>
{
}
impl<T: Expression, TAction> WebSocketGraphQlServerAction<T> for TAction where
    Self: Action
        + InboundAction<WebSocketServerConnectAction>
        + InboundAction<WebSocketServerReceiveAction>
        + InboundAction<WebSocketServerThrottleTimeoutAction>
        + InboundAction<GraphQlServerParseErrorAction<T>>
        + InboundAction<GraphQlServerEmitAction<T>>
        + OutboundAction<GraphQlServerSubscribeAction<T>>
        + OutboundAction<GraphQlServerModifyAction<T>>
        + OutboundAction<GraphQlServerUnsubscribeAction<T>>
        + OutboundAction<WebSocketServerSendAction>
        + OutboundAction<WebSocketServerDisconnectAction>
        + OutboundAction<WebSocketServerThrottleTimeoutAction>
{
}

pub(crate) struct WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
{
    schema_types: Option<GraphQlSchemaTypes<'static, String>>,
    factory: TFactory,
    transform: TTransform,
    metric_names: WebSocketGraphQlServerMetricNames,
    get_connection_metric_labels: TMetricLabels,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TTransform, TMetricLabels>
    WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
{
    pub(crate) fn new(
        schema_types: Option<GraphQlSchemaTypes<'static, String>>,
        factory: TFactory,
        transform: TTransform,
        metric_names: WebSocketGraphQlServerMetricNames,
        get_connection_metric_labels: TMetricLabels,
    ) -> Self {
        Self {
            schema_types,
            factory,
            transform,
            metric_names: metric_names.init(),
            get_connection_metric_labels,
            _expression: Default::default(),
        }
    }
}

pub struct WebSocketGraphQlServerState<T: Expression> {
    // TODO: Use newtypes for state hashmap keys
    connections: HashMap<Uuid, WebSocketGraphQlConnection<T>>,
    _expression: PhantomData<T>,
}
impl<T: Expression> Default for WebSocketGraphQlServerState<T> {
    fn default() -> Self {
        Self {
            connections: Default::default(),
            _expression: Default::default(),
        }
    }
}
struct WebSocketGraphQlConnection<T: Expression> {
    request: Request<()>,
    initialized_state: Option<WebSocketGraphQlInitializedConnectionState>,
    operations: Vec<WebSocketGraphQlOperation<T>>,
}
struct WebSocketGraphQlInitializedConnectionState {
    connection_params: Option<JsonValue>,
    metric_labels: Vec<(String, String)>,
}
struct WebSocketGraphQlOperation<T: Expression> {
    operation_id: OperationId,
    subscription_id: Uuid,
    query: Option<GraphQlQuery>,
    /// Previous result payload if this is a diff stream (empty before first result emitted)
    diff_result: Option<Option<(T, JsonValue)>>,
    /// Throttle duration and active throttle state if this is a throttled stream
    throttle: Option<(Duration, Option<ThrottleState<T>>)>,
}
struct ThrottleState<T: Expression> {
    result: T,
    task_pid: ProcessId,
}
impl<T: Expression> WebSocketGraphQlServerState<T> {
    fn find_subscription_mut(
        &mut self,
        subscription_id: &Uuid,
    ) -> Option<(Uuid, &mut WebSocketGraphQlOperation<T>)> {
        self.connections
            .iter_mut()
            .find_map(|(connection_id, connection)| {
                connection
                    .find_subscription_mut(subscription_id)
                    .map(|subscription| (*connection_id, subscription))
            })
    }
    fn remove_subscription(
        &mut self,
        subscription_id: &Uuid,
    ) -> Option<(Uuid, WebSocketGraphQlOperation<T>)> {
        self.connections
            .iter_mut()
            .find_map(|(connection_id, connection)| {
                connection
                    .remove_subscription(subscription_id)
                    .map(|subscription| (*connection_id, subscription))
            })
    }
}
impl<T: Expression> WebSocketGraphQlConnection<T> {
    fn has_operation(&self, operation_id: &OperationId) -> bool {
        self.find_operation(operation_id).is_some()
    }
    fn find_operation(&self, operation_id: &OperationId) -> Option<&WebSocketGraphQlOperation<T>> {
        self.operations
            .iter()
            .find(|operation| operation.operation_id == *operation_id)
    }
    fn remove_operation(
        &mut self,
        operation_id: &OperationId,
    ) -> Option<WebSocketGraphQlOperation<T>> {
        let index = self
            .operations
            .iter()
            .position(|operation| operation.operation_id == *operation_id)?;
        Some(self.operations.remove(index))
    }
    fn find_subscription_mut(
        &mut self,
        subscription_id: &Uuid,
    ) -> Option<&mut WebSocketGraphQlOperation<T>> {
        self.operations
            .iter_mut()
            .find(|operation| operation.subscription_id == *subscription_id)
    }
    fn remove_subscription(
        &mut self,
        subscription_id: &Uuid,
    ) -> Option<WebSocketGraphQlOperation<T>> {
        let index = self
            .operations
            .iter()
            .position(|operation| operation.subscription_id == *subscription_id)?;
        Some(self.operations.remove(index))
    }
}

impl<T, TFactory, TTransform, TMetricLabels, TAction> Actor<TAction>
    for WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TAction: WebSocketGraphQlServerAction<T> + Send + 'static,
{
    type State = WebSocketGraphQlServerState<T>;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_connect(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_receive(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_parse_error(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_emit_action(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_throttle_timeout_action(
                &mut state, action, metadata, context,
            )
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TTransform, TMetricLabels>
    WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
{
    fn handle_websocket_graphql_server_connect<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerConnectAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let WebSocketServerConnectAction {
            connection_id,
            request,
        } = action;
        let connection_id = *connection_id;
        let entry = match state.connections.entry(connection_id) {
            Entry::Vacant(entry) => Some(entry),
            Entry::Occupied(_) => None,
        }?;
        increment_gauge!(self.metric_names.graphql_websocket_connection_count, 1.0,);
        entry.insert(WebSocketGraphQlConnection {
            request: clone_request_wrapper(request),
            initialized_state: Default::default(),
            operations: Default::default(),
        });
        None
    }
    fn handle_websocket_graphql_server_receive<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerReceiveAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<GraphQlServerSubscribeAction<T>>
            + OutboundAction<GraphQlServerModifyAction<T>>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<WebSocketServerDisconnectAction>,
    {
        match &action.message {
            GraphQlSubscriptionClientMessage::ConnectionInit(message) => self
                .handle_websocket_graphql_server_receive_connection_init(
                    state, message, action, metadata, context,
                ),
            GraphQlSubscriptionClientMessage::Start(message) => self
                .handle_websocket_graphql_server_receive_start(
                    state, message, action, metadata, context,
                ),
            GraphQlSubscriptionClientMessage::Stop(message) => self
                .handle_websocket_graphql_server_receive_stop(
                    state, message, action, metadata, context,
                ),
            GraphQlSubscriptionClientMessage::Update(message) => self
                .handle_websocket_graphql_server_receive_update(
                    state, message, action, metadata, context,
                ),
            GraphQlSubscriptionClientMessage::ConnectionTerminate => self
                .handle_websocket_graphql_server_receive_connection_terminate(
                    state, action, metadata, context,
                ),
        }
    }
    fn handle_websocket_graphql_server_receive_connection_init<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionConnectionInitMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<WebSocketServerSendAction>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        let connection_params = message.payload().cloned();
        let previous_metric_labels = connection
            .initialized_state
            .take()
            .map(|initialized_state| initialized_state.metric_labels);
        let metric_labels = (self.get_connection_metric_labels)(
            connection_params.as_ref(),
            connection.request.headers(),
        );
        if let Some(previous_metric_labels) = previous_metric_labels {
            decrement_gauge!(
                self.metric_names
                    .graphql_websocket_initialized_connection_count,
                1.0,
                &previous_metric_labels
            );
        }
        increment_gauge!(
            self.metric_names
                .graphql_websocket_initialized_connection_count,
            1.0,
            &metric_labels
        );
        connection
            .initialized_state
            .replace(WebSocketGraphQlInitializedConnectionState {
                connection_params,
                metric_labels,
            });
        let connection_ack_action = StateOperation::Send(
            context.pid(),
            WebSocketServerSendAction {
                connection_id: *connection_id,
                message: GraphQlSubscriptionServerMessage::ConnectionAck,
            }
            .into(),
        );
        Some(StateTransition::new(once(connection_ack_action)))
    }
    fn handle_websocket_graphql_server_receive_start<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionStartMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<GraphQlServerSubscribeAction<T>>
            + OutboundAction<WebSocketServerSendAction>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        let operation = if connection.has_operation(message.operation_id()) {
            Err(create_json_error_object(
                format!("Subscription ID already exists: {}", message.operation_id()),
                None,
            ))
        } else {
            let operation = message.payload();
            parse_graphql_query(&operation.query)
                .map_err(|err| create_json_error_object(format!("Invalid query: {}", err), None))
                .and_then(|query| {
                    let operation = GraphQlOperation::new(
                        query,
                        operation.operation_name.clone(),
                        operation.variables.clone(),
                        operation.extensions.clone(),
                    );
                    self.transform.transform(
                        operation,
                        &connection.request,
                        connection
                            .initialized_state
                            .as_ref()
                            .and_then(|initialized_state| {
                                initialized_state.connection_params.as_ref()
                            }),
                    )
                })
        };
        match operation {
            Err(err) => Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: GraphQlSubscriptionServerMessage::ConnectionError(err),
                }
                .into(),
            )))),
            Ok(operation) => {
                let subscription_id = Uuid::new_v4();
                let validation_query = self
                    .schema_types
                    .as_ref()
                    .map(|_| operation.query().clone());
                connection.operations.push(WebSocketGraphQlOperation {
                    operation_id: message.operation_id().clone(),
                    subscription_id,
                    query: validation_query,
                    diff_result: if is_diff_subscription(&operation) {
                        Some(None)
                    } else {
                        None
                    },
                    throttle: get_subscription_throttle_duration(&operation)
                        .map(|duration| (duration, None)),
                });
                Some(StateTransition::new(once(StateOperation::Send(
                    context.pid(),
                    GraphQlServerSubscribeAction {
                        subscription_id,
                        operation,
                        _expression: Default::default(),
                    }
                    .into(),
                ))))
            }
        }
    }
    fn handle_websocket_graphql_server_receive_stop<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionStopMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        if let Some(operation) = connection.remove_operation(message.operation_id()) {
            Some(StateTransition::new([
                StateOperation::Send(
                    context.pid(),
                    GraphQlServerUnsubscribeAction {
                        subscription_id: operation.subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                StateOperation::Send(
                    context.pid(),
                    WebSocketServerSendAction {
                        connection_id: *connection_id,
                        message: GraphQlSubscriptionServerMessage::Complete(operation.operation_id),
                    }
                    .into(),
                ),
            ]))
        } else {
            Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: GraphQlSubscriptionServerMessage::ConnectionError(
                        create_json_error_object(
                            format!(
                                "Subscription ID already unsubscribed: {}",
                                message.operation_id()
                            ),
                            None,
                        ),
                    ),
                }
                .into(),
            ))))
        }
    }
    fn handle_websocket_graphql_server_receive_update<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionUpdateMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<GraphQlServerModifyAction<T>>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        if let Some(operation) = connection.find_operation(message.operation_id()) {
            Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                GraphQlServerModifyAction {
                    subscription_id: operation.subscription_id,
                    variables: message
                        .payload()
                        .iter()
                        .map(|(key, value)| (key.clone(), value.clone()))
                        .collect(),
                    _expression: Default::default(),
                }
                .into(),
            ))))
        } else {
            Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: GraphQlSubscriptionServerMessage::ConnectionError(
                        create_json_error_object(
                            format!("Subscription ID not found: {}", message.operation_id()),
                            None,
                        ),
                    ),
                }
                .into(),
            ))))
        }
    }
    fn handle_websocket_graphql_server_receive_connection_terminate<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>
            + OutboundAction<WebSocketServerDisconnectAction>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let mut connection = state.connections.remove(connection_id)?;
        let previous_metric_labels = connection
            .initialized_state
            .take()
            .map(|initialized_state| initialized_state.metric_labels);
        if let Some(previous_metric_labels) = previous_metric_labels {
            decrement_gauge!(
                self.metric_names
                    .graphql_websocket_initialized_connection_count,
                1.0,
                &previous_metric_labels
            );
        }
        decrement_gauge!(self.metric_names.graphql_websocket_connection_count, 1.0);
        Some(StateTransition::new(
            connection
                .operations
                .into_iter()
                .map(|operation| {
                    StateOperation::Send(
                        context.pid(),
                        GraphQlServerUnsubscribeAction {
                            subscription_id: operation.subscription_id,
                            _expression: Default::default(),
                        }
                        .into(),
                    )
                })
                .chain(once(StateOperation::Send(
                    context.pid(),
                    WebSocketServerDisconnectAction {
                        connection_id: *connection_id,
                    }
                    .into(),
                ))),
        ))
    }
    fn handle_graphql_parse_error<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &GraphQlServerParseErrorAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<WebSocketServerSendAction>,
    {
        let GraphQlServerParseErrorAction {
            subscription_id,
            message,
            operation,
            ..
        } = action;
        let (connection_id, subscription) = state.remove_subscription(subscription_id)?;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            WebSocketServerSendAction {
                connection_id,
                message: GraphQlSubscriptionServerMessage::Error(
                    subscription.operation_id,
                    json_object([
                        (
                            String::from("message"),
                            JsonValue::String(String::from(message)),
                        ),
                        (String::from("operation"), operation.clone().into_json()),
                    ]),
                ),
            }
            .into(),
        ))))
    }
    fn handle_graphql_emit_action<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<WebSocketServerThrottleTimeoutAction>,
    {
        let GraphQlServerEmitAction {
            subscription_id,
            result,
        } = action;
        let (connection_id, subscription) = state.find_subscription_mut(subscription_id)?;
        let is_unchanged = subscription
            .diff_result
            .as_ref()
            .and_then(|previous_result| previous_result.as_ref())
            .map(|(existing, _)| existing.id() == result.id())
            .unwrap_or(false);
        if is_unchanged {
            return None;
        }
        match subscription.throttle.as_mut() {
            None => {
                let update_message = get_subscription_result_payload(
                    result,
                    &subscription.operation_id,
                    subscription.query.as_ref(),
                    self.schema_types.as_ref(),
                    subscription.diff_result.as_mut(),
                    &self.factory,
                )?;
                Some(StateTransition::new(once(StateOperation::Send(
                    context.pid(),
                    WebSocketServerSendAction {
                        connection_id,
                        message: update_message,
                    }
                    .into(),
                ))))
            }
            Some((duration, existing_delay)) => {
                if let Some(ThrottleState { task_pid, .. }) = existing_delay.take() {
                    existing_delay.replace(ThrottleState {
                        result: result.clone(),
                        task_pid,
                    });
                    None
                } else {
                    let (task, task_pid) = create_delayed_action(
                        WebSocketServerThrottleTimeoutAction {
                            subscription_id: *subscription_id,
                        }
                        .into(),
                        *duration,
                        context,
                    );
                    existing_delay.replace(ThrottleState {
                        result: result.clone(),
                        task_pid,
                    });
                    Some(StateTransition::new(once(task)))
                }
            }
        }
    }
    fn handle_websocket_graphql_throttle_timeout_action<TAction>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerThrottleTimeoutAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + 'static
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<WebSocketServerThrottleTimeoutAction>,
    {
        let WebSocketServerThrottleTimeoutAction { subscription_id } = action;
        let (connection_id, subscription) = state.find_subscription_mut(subscription_id)?;
        let ThrottleState { result, task_pid } = subscription
            .throttle
            .as_mut()
            .and_then(|(_, existing_task)| existing_task.take())?;
        let update_message = get_subscription_result_payload(
            &result,
            &subscription.operation_id,
            subscription.query.as_ref(),
            self.schema_types.as_ref(),
            subscription.diff_result.as_mut(),
            &self.factory,
        );
        let update_action = update_message.map(|message| {
            StateOperation::Send(
                context.pid(),
                WebSocketServerSendAction {
                    connection_id,
                    message,
                }
                .into(),
            )
        });
        let dispose_throttle_task_action = StateOperation::Kill(task_pid);
        Some(StateTransition::new(
            once(dispose_throttle_task_action).chain(update_action),
        ))
    }
}

fn get_subscription_result_payload<T: Expression>(
    result: &T,
    operation_id: &OperationId,
    query: Option<&GraphQlQuery>,
    schema_types: Option<&GraphQlSchemaTypes<'static, String>>,
    diff_result: Option<&mut Option<(T, JsonValue)>>,
    factory: &impl ExpressionFactory<T>,
) -> Option<GraphQlSubscriptionServerMessage> {
    let result_payload =
        serialize_graphql_result_payload(result, factory).and_then(|payload| {
            match (query, schema_types) {
                (Some(query), Some(schema_types)) => {
                    validate_graphql_result(&payload, query, schema_types).map(|_| payload)
                }
                _ => Ok(payload),
            }
        });
    // TODO: perform diff on unserialized result payloads to allow fast dirty-checking of branches
    let previous_result_payload = if let Some(previous_result) = diff_result {
        std::mem::replace(
            previous_result,
            result_payload
                .as_ref()
                .ok()
                // TODO: avoid unnecessary cloning of websocket JSON payload for diff results
                .map(|payload| (result.clone(), payload.clone())),
        )
    } else {
        None
    };
    match (result_payload, previous_result_payload) {
        (Ok(result_payload), Some((_, previous_result_payload))) => {
            Some(GraphQlSubscriptionServerMessage::Patch(
                operation_id.clone(),
                create_graphql_success_response(
                    diff_results(&previous_result_payload, &result_payload)
                        .unwrap_or_else(|| json_object(empty())),
                ),
            ))
        }
        (result_payload, _) => Some(GraphQlSubscriptionServerMessage::Data(
            operation_id.clone(),
            match result_payload {
                Ok(result) => create_graphql_success_response(result),
                Err(errors) => create_graphql_error_response(errors),
            },
        )),
    }
}

fn is_diff_subscription(operation: &GraphQlOperation) -> bool {
    operation
        .extension("diff")
        .map(|value| match value {
            JsonValue::Bool(value) => *value,
            _ => false,
        })
        .unwrap_or(false)
}

fn get_subscription_throttle_duration(operation: &GraphQlOperation) -> Option<Duration> {
    operation
        .extension("throttle")
        .and_then(|value| match value {
            JsonValue::Number(value) => {
                parse_json_positive_integer(value).map(Duration::from_millis)
            }
            _ => None,
        })
}

fn parse_json_positive_integer(value: &JsonNumber) -> Option<u64> {
    match value.as_u64() {
        Some(value) if value == 0 => None,
        Some(value) => Some(value),
        None => match value.as_f64() {
            Some(value) if value >= 1.0 => Some(value.trunc() as u64),
            _ => None,
        },
    }
}

fn diff_results(previous: &JsonValue, current: &JsonValue) -> Option<JsonValue> {
    if current == previous {
        return None;
    }
    match previous {
        JsonValue::Object(previous) => match current {
            JsonValue::Object(current) => diff_objects(previous, current),
            _ => Some(current.clone()),
        },
        JsonValue::Array(previous) => match current {
            JsonValue::Array(current) => diff_arrays(previous, current),
            _ => Some(current.clone()),
        },
        _ => Some(current.clone()),
    }
}

fn diff_objects(
    previous: &JsonMap<String, JsonValue>,
    current: &JsonMap<String, JsonValue>,
) -> Option<JsonValue> {
    let previous_entries = previous.iter().collect::<HashMap<_, _>>();
    let updates = json_object(current.iter().filter_map(|(key, current_value)| {
        previous_entries.get(key).and_then(|previous_value| {
            diff_results(previous_value, current_value).map(|value| (key.clone(), value))
        })
    }));
    if is_empty_json_object(&updates) {
        None
    } else {
        Some(updates)
    }
}

fn diff_arrays(previous: &Vec<JsonValue>, current: &Vec<JsonValue>) -> Option<JsonValue> {
    let updates = current
        .iter()
        .zip(previous.iter())
        .map(|(current, previous)| diff_results(previous, current))
        .chain(
            current
                .iter()
                .skip(previous.len())
                .map(|item| Some(item.clone())),
        )
        .collect::<Vec<_>>();
    let updates = json_object(
        updates
            .into_iter()
            .enumerate()
            .filter_map(|(index, item)| item.map(|value| (index.to_string(), value)))
            .chain(if current.len() != previous.len() {
                Some((String::from("length"), JsonValue::from(current.len())))
            } else {
                None
            }),
    );
    if is_empty_json_object(&updates) {
        None
    } else {
        Some(updates)
    }
}

fn is_empty_json_object(value: &JsonValue) -> bool {
    match value {
        JsonValue::Object(value) => value.is_empty(),
        _ => false,
    }
}

fn create_delayed_action<TAction>(
    action: TAction,
    duration: Duration,
    context: &mut impl HandlerContext,
) -> (StateOperation<TAction>, ProcessId)
where
    TAction: Action + Send + 'static,
{
    let task_id = context.generate_pid();
    (
        StateOperation::Task(
            task_id,
            OperationStream::new(Box::pin(
                sleep(duration)
                    .map({
                        let current_pid = context.pid();
                        move |_| StateOperation::Send(current_pid, action)
                    })
                    .into_stream(),
            )),
        ),
        task_id,
    )
}

fn clone_request_wrapper<T>(request: &Request<T>) -> Request<()> {
    let mut result = Request::new(());
    *result.method_mut() = request.method().clone();
    *result.uri_mut() = request.uri().clone();
    *result.version_mut() = request.version();
    let headers = result.headers_mut();
    for (key, value) in request.headers() {
        headers.append(key.clone(), value.clone());
    }
    result
}
