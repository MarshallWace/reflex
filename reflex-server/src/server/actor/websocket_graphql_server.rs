// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{empty, once},
    marker::PhantomData,
    sync::Once,
    time::Duration,
};

use futures::FutureExt;
use http::{HeaderMap, Request};
use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::core::{Expression, ExpressionFactory, Uuid};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OperationStream, OutboundAction,
    ProcessId, StateOperation, StateTransition,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response, create_json_error_object,
    serialize_graphql_result_payload,
    subscriptions::{
        GraphQlSubscriptionClientMessage, GraphQlSubscriptionConnectionInitMessage,
        GraphQlSubscriptionServerMessage, GraphQlSubscriptionStartMessage,
        GraphQlSubscriptionStopMessage, GraphQlSubscriptionUpdateMessage, OperationId,
    },
    GraphQlOperationPayload,
};
use reflex_json::{json_object, JsonMap, JsonNumber, JsonValue};
use tokio::time::sleep;

use crate::server::action::{
    graphql_server::{
        GraphQlServerEmitAction, GraphQlServerModifyAction, GraphQlServerParseErrorAction,
        GraphQlServerSubscribeAction, GraphQlServerUnsubscribeAction,
    },
    websocket_server::{
        WebSocketServerConnectAction, WebSocketServerDisconnectAction,
        WebSocketServerReceiveAction, WebSocketServerSendAction,
        WebSocketServerThrottleTimeoutAction,
    },
};

pub const METRIC_GRAPHQL_WEBSOCKET_CONNECTION_COUNT: &'static str =
    "graphql_websocket_connection_count";
pub const METRIC_GRAPHQL_WEBSOCKET_INITIALIZED_CONNECTION_COUNT: &'static str =
    "graphql_websocket_initialized_connection_count";

static INIT_METRICS: Once = Once::new();

fn init_metrics() {
    INIT_METRICS.call_once(|| {
        describe_gauge!(
            METRIC_GRAPHQL_WEBSOCKET_CONNECTION_COUNT,
            Unit::Count,
            "Active client GraphQL Web Socket connection count"
        );
        describe_gauge!(
            METRIC_GRAPHQL_WEBSOCKET_INITIALIZED_CONNECTION_COUNT,
            Unit::Count,
            "Active initialized client GraphQL Web Socket connection count"
        );
    });
}

pub trait WebSocketGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        request: &Request<()>,
        connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperationPayload, JsonValue>;
}
impl<T> WebSocketGraphQlServerQueryTransform for T
where
    T: Fn(
        GraphQlOperationPayload,
        &Request<()>,
        Option<&JsonValue>,
    ) -> Result<GraphQlOperationPayload, JsonValue>,
{
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        request: &Request<()>,
        connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperationPayload, JsonValue> {
        self(operation, request, connection_params)
    }
}

pub struct NoopWebSocketGraphQlServerQueryTransform;
impl WebSocketGraphQlServerQueryTransform for NoopWebSocketGraphQlServerQueryTransform {
    fn transform(
        &self,
        operation: GraphQlOperationPayload,
        _request: &Request<()>,
        _connection_params: Option<&JsonValue>,
    ) -> Result<GraphQlOperationPayload, JsonValue> {
        Ok(operation)
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
    factory: TFactory,
    transform: TTransform,
    get_connection_metric_labels: TMetricLabels,
    state: WebSocketGraphQlServerState<T>,
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
        factory: TFactory,
        transform: TTransform,
        get_connection_metric_labels: TMetricLabels,
    ) -> Self {
        init_metrics();
        Self {
            factory,
            transform,
            get_connection_metric_labels,
            state: Default::default(),
            _expression: Default::default(),
        }
    }
}

struct WebSocketGraphQlServerState<T: Expression> {
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
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_connect(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_server_receive(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_parse_error(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_emit_action(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_websocket_graphql_throttle_timeout_action(action, metadata, context)
        } else {
            StateTransition::new(None)
        }
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
        &mut self,
        action: &WebSocketServerConnectAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action,
    {
        let WebSocketServerConnectAction {
            connection_id,
            request,
        } = action;
        let connection_id = *connection_id;
        if let Entry::Vacant(entry) = self.state.connections.entry(connection_id) {
            increment_gauge!(METRIC_GRAPHQL_WEBSOCKET_CONNECTION_COUNT, 1.0,);
            entry.insert(WebSocketGraphQlConnection {
                request: clone_request_wrapper(request),
                initialized_state: Default::default(),
                operations: Default::default(),
            });
        }
        StateTransition::new(None)
    }
    fn handle_websocket_graphql_server_receive<TAction>(
        &mut self,
        action: &WebSocketServerReceiveAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
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
                    message, action, metadata, context,
                ),
            GraphQlSubscriptionClientMessage::Start(message) => self
                .handle_websocket_graphql_server_receive_start(message, action, metadata, context),
            GraphQlSubscriptionClientMessage::Stop(message) => self
                .handle_websocket_graphql_server_receive_stop(message, action, metadata, context),
            GraphQlSubscriptionClientMessage::Update(message) => self
                .handle_websocket_graphql_server_receive_update(message, action, metadata, context),
            GraphQlSubscriptionClientMessage::ConnectionTerminate => self
                .handle_websocket_graphql_server_receive_connection_terminate(
                    action, metadata, context,
                ),
        }
    }
    fn handle_websocket_graphql_server_receive_connection_init<TAction>(
        &mut self,
        message: &GraphQlSubscriptionConnectionInitMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + OutboundAction<WebSocketServerSendAction>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let action = match self.state.connections.get_mut(connection_id) {
            None => None,
            Some(connection) => {
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
                        METRIC_GRAPHQL_WEBSOCKET_INITIALIZED_CONNECTION_COUNT,
                        1.0,
                        &previous_metric_labels
                    );
                }
                increment_gauge!(
                    METRIC_GRAPHQL_WEBSOCKET_INITIALIZED_CONNECTION_COUNT,
                    1.0,
                    &metric_labels
                );
                connection
                    .initialized_state
                    .replace(WebSocketGraphQlInitializedConnectionState {
                        connection_params,
                        metric_labels,
                    });
                Some(StateOperation::Send(
                    context.pid(),
                    WebSocketServerSendAction {
                        connection_id: *connection_id,
                        message: GraphQlSubscriptionServerMessage::ConnectionAck,
                    }
                    .into(),
                ))
            }
        };
        StateTransition::new(action)
    }
    fn handle_websocket_graphql_server_receive_start<TAction>(
        &mut self,
        message: &GraphQlSubscriptionStartMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action
            + OutboundAction<GraphQlServerSubscribeAction<T>>
            + OutboundAction<WebSocketServerSendAction>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        match self.state.connections.get_mut(connection_id) {
            None => StateTransition::new(None),
            Some(connection) => {
                let operation = if connection.has_operation(message.operation_id()) {
                    Err(create_json_error_object(
                        format!("Subscription ID already exists: {}", message.operation_id()),
                        None,
                    ))
                } else {
                    let operation = message.payload().clone();
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
                };
                match operation {
                    Err(err) => StateTransition::new(Some(StateOperation::Send(
                        context.pid(),
                        WebSocketServerSendAction {
                            connection_id: *connection_id,
                            message: GraphQlSubscriptionServerMessage::ConnectionError(err),
                        }
                        .into(),
                    ))),
                    Ok(operation) => {
                        let subscription_id = Uuid::new_v4();
                        connection.operations.push(WebSocketGraphQlOperation {
                            operation_id: message.operation_id().clone(),
                            subscription_id,
                            diff_result: if is_diff_subscription(&operation) {
                                Some(None)
                            } else {
                                None
                            },
                            throttle: get_subscription_throttle_duration(&operation)
                                .map(|duration| (duration, None)),
                        });
                        StateTransition::new(Some(StateOperation::Send(
                            context.pid(),
                            GraphQlServerSubscribeAction {
                                subscription_id,
                                operation,
                                _expression: Default::default(),
                            }
                            .into(),
                        )))
                    }
                }
            }
        }
    }
    fn handle_websocket_graphql_server_receive_stop<TAction>(
        &mut self,
        message: &GraphQlSubscriptionStopMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        match self.state.connections.get_mut(connection_id) {
            None => StateTransition::new(None),
            Some(connection) => {
                if let Some(operation) = connection.remove_operation(message.operation_id()) {
                    StateTransition::new([
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
                                message: GraphQlSubscriptionServerMessage::Complete(
                                    operation.operation_id,
                                ),
                            }
                            .into(),
                        ),
                    ])
                } else {
                    StateTransition::new(Some(StateOperation::Send(
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
                    )))
                }
            }
        }
    }
    fn handle_websocket_graphql_server_receive_update<TAction>(
        &mut self,
        message: &GraphQlSubscriptionUpdateMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<GraphQlServerModifyAction<T>>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        match self.state.connections.get_mut(connection_id) {
            None => StateTransition::new(None),
            Some(connection) => {
                if let Some(operation) = connection.find_operation(message.operation_id()) {
                    StateTransition::new(Some(StateOperation::Send(
                        context.pid(),
                        GraphQlServerModifyAction {
                            subscription_id: operation.subscription_id,
                            variables: message.payload().clone(),
                            _expression: Default::default(),
                        }
                        .into(),
                    )))
                } else {
                    StateTransition::new(Some(StateOperation::Send(
                        context.pid(),
                        WebSocketServerSendAction {
                            connection_id: *connection_id,
                            message: GraphQlSubscriptionServerMessage::ConnectionError(
                                create_json_error_object(
                                    format!(
                                        "Subscription ID not found: {}",
                                        message.operation_id()
                                    ),
                                    None,
                                ),
                            ),
                        }
                        .into(),
                    )))
                }
            }
        }
    }
    fn handle_websocket_graphql_server_receive_connection_terminate<TAction>(
        &mut self,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
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
        match self.state.connections.remove(connection_id) {
            None => StateTransition::new(None),
            Some(mut connection) => {
                let previous_metric_labels = connection
                    .initialized_state
                    .take()
                    .map(|initialized_state| initialized_state.metric_labels);
                if let Some(previous_metric_labels) = previous_metric_labels {
                    decrement_gauge!(
                        METRIC_GRAPHQL_WEBSOCKET_INITIALIZED_CONNECTION_COUNT,
                        1.0,
                        &previous_metric_labels
                    );
                }
                decrement_gauge!(METRIC_GRAPHQL_WEBSOCKET_CONNECTION_COUNT, 1.0);
                StateTransition::new(
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
                )
            }
        }
    }
    fn handle_graphql_parse_error<TAction>(
        &mut self,
        action: &GraphQlServerParseErrorAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + OutboundAction<WebSocketServerSendAction>,
    {
        let GraphQlServerParseErrorAction {
            subscription_id,
            message,
            operation,
            ..
        } = action;
        match self.state.remove_subscription(subscription_id) {
            None => StateTransition::new(None),
            Some((connection_id, subscription)) => {
                StateTransition::new(Some(StateOperation::Send(
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
                )))
            }
        }
    }
    fn handle_graphql_emit_action<TAction>(
        &mut self,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
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
        if let Some((connection_id, subscription)) =
            self.state.find_subscription_mut(subscription_id)
        {
            let is_unchanged = subscription
                .diff_result
                .as_ref()
                .and_then(|previous_result| previous_result.as_ref())
                .map(|(existing, _)| existing.id() == result.id())
                .unwrap_or(false);
            if is_unchanged {
                return StateTransition::new(None);
            }
            StateTransition::new(match subscription.throttle.as_mut() {
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
                        Some(task)
                    }
                }
                None => get_subscription_result_payload(
                    result,
                    &subscription.operation_id,
                    subscription.diff_result.as_mut(),
                    &self.factory,
                )
                .map(|message| {
                    StateOperation::Send(
                        context.pid(),
                        WebSocketServerSendAction {
                            connection_id,
                            message,
                        }
                        .into(),
                    )
                }),
            })
        } else {
            StateTransition::new(None)
        }
    }
    fn handle_websocket_graphql_throttle_timeout_action<TAction>(
        &mut self,
        action: &WebSocketServerThrottleTimeoutAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action
            + 'static
            + OutboundAction<WebSocketServerSendAction>
            + OutboundAction<WebSocketServerThrottleTimeoutAction>,
    {
        let WebSocketServerThrottleTimeoutAction { subscription_id } = action;
        if let Some((connection_id, subscription)) =
            self.state.find_subscription_mut(subscription_id)
        {
            if let Some(ThrottleState { result, task_pid }) = subscription
                .throttle
                .as_mut()
                .and_then(|(_, existing_task)| existing_task.take())
            {
                StateTransition::new(
                    once(StateOperation::Kill(task_pid)).chain(
                        get_subscription_result_payload(
                            &result,
                            &subscription.operation_id,
                            subscription.diff_result.as_mut(),
                            &self.factory,
                        )
                        .map(|message| {
                            StateOperation::Send(
                                context.pid(),
                                WebSocketServerSendAction {
                                    connection_id,
                                    message,
                                }
                                .into(),
                            )
                        }),
                    ),
                )
            } else {
                StateTransition::new(None)
            }
        } else {
            StateTransition::new(None)
        }
    }
}

fn get_subscription_result_payload<T: Expression>(
    result: &T,
    operation_id: &OperationId,
    diff_result: Option<&mut Option<(T, JsonValue)>>,
    factory: &impl ExpressionFactory<T>,
) -> Option<GraphQlSubscriptionServerMessage> {
    let result_payload = serialize_graphql_result_payload(result, factory);
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

fn is_diff_subscription(operation: &GraphQlOperationPayload) -> bool {
    operation
        .extension("diff")
        .map(|value| match value {
            JsonValue::Bool(value) => *value,
            _ => false,
        })
        .unwrap_or(false)
}

fn get_subscription_throttle_duration(operation: &GraphQlOperationPayload) -> Option<Duration> {
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
            .filter_map(|(index, item)| item.map(|value| (index.to_string(), value))),
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
