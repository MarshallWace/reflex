// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{empty, once},
    marker::PhantomData,
    time::{Duration, Instant},
};

use http::{HeaderMap, Request};
use metrics::{decrement_gauge, describe_gauge, gauge, increment_gauge, Unit};
use reflex::core::{Expression, ExpressionFactory, Uuid};
use reflex_dispatcher::{
    Action, ActorInitContext, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_graphql::{
    create_graphql_error_response, create_graphql_success_response, create_json_error_object,
    parse_graphql_operation_type, parse_graphql_query, serialize_graphql_result_payload,
    subscriptions::{
        GraphQlSubscriptionClientMessage, GraphQlSubscriptionConnectionInitMessage,
        GraphQlSubscriptionServerMessage, GraphQlSubscriptionStartMessage,
        GraphQlSubscriptionStopMessage, GraphQlSubscriptionUpdateMessage, OperationId,
    },
    validate::validate_graphql_result,
    GraphQlOperation, GraphQlOperationType, GraphQlQuery, GraphQlQueryTransform,
    GraphQlSchemaTypes,
};
use reflex_json::{json_object, JsonMap, JsonNumber, JsonValue};
use reflex_macros::{dispatcher, Named};

use crate::server::{
    actor::graphql_server::GraphQlQueryStatus,
    task::websocket_graphql_server::{
        WebSocketGraphQlServerTask, WebSocketGraphQlServerThrottleTimeoutTaskFactory,
    },
};
use crate::{
    server::{
        action::{
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
        utils::clone_http_request_wrapper,
    },
    utils::transform::apply_graphql_query_transform,
};

#[derive(Clone, Copy, Debug)]
pub struct WebSocketGraphQlServerMetricNames {
    pub graphql_websocket_connection_count: &'static str,
    pub graphql_websocket_initialized_connection_count: &'static str,
    pub graphql_websocket_query_error_duration_micros: &'static str,
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
        describe_gauge!(
            self.graphql_websocket_query_error_duration_micros,
            Unit::Microseconds,
            "Time that a given query has spent in an error state"
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
            graphql_websocket_query_error_duration_micros:
                "graphql_websocket_query_error_duration_micros",
        }
    }
}

pub trait WebSocketGraphQlServerConnectionMetricLabels {
    fn labels(
        &self,
        connection_params: Option<&JsonValue>,
        headers: &HeaderMap,
    ) -> Vec<(String, String)>;
}
impl<T> WebSocketGraphQlServerConnectionMetricLabels for T
where
    Self: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
{
    fn labels(
        &self,
        connection_params: Option<&JsonValue>,
        headers: &HeaderMap,
    ) -> Vec<(String, String)> {
        (self)(connection_params, headers)
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

#[derive(Named, Clone)]
pub struct WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
{
    schema_types: Option<GraphQlSchemaTypes<'static, String>>,
    factory: TFactory,
    transform: TTransform,
    metric_names: WebSocketGraphQlServerMetricNames,
    get_connection_metric_labels: TMetricLabels,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TTransform, TMetricLabels>
    WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
{
    pub(crate) fn new(
        schema_types: Option<GraphQlSchemaTypes<'static, String>>,
        factory: TFactory,
        transform: TTransform,
        metric_names: WebSocketGraphQlServerMetricNames,
        get_connection_metric_labels: TMetricLabels,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            schema_types,
            factory,
            transform,
            metric_names: metric_names.init(),
            get_connection_metric_labels,
            main_pid,
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
    operation_type: GraphQlOperationType,
    // Only necessary if validating query results against a schema
    query: Option<GraphQlQuery>,
    /// Previous result payload if this is a diff stream (empty before first result emitted)
    diff_result: Option<Option<(T, JsonValue)>>,
    /// Throttle duration and active throttle state if this is a throttled stream
    throttle: Option<(Duration, Option<ThrottleState<T>>)>,
    error_metric_tracker: QueryErrorStateTracker,
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

dispatcher!({
    pub enum WebSocketGraphQlServerAction<T: Expression> {
        Inbox(WebSocketServerConnectAction),
        Inbox(WebSocketServerReceiveAction),
        Inbox(WebSocketServerThrottleTimeoutAction),
        Inbox(GraphQlServerParseErrorAction<T>),
        Inbox(GraphQlServerEmitAction<T>),

        Outbox(GraphQlServerSubscribeAction<T>),
        Outbox(GraphQlServerModifyAction<T>),
        Outbox(GraphQlServerUnsubscribeAction<T>),
        Outbox(WebSocketServerSendAction),
        Outbox(WebSocketServerDisconnectAction),
        Outbox(WebSocketServerThrottleTimeoutAction),
    }

    impl<T, TFactory, TTransform, TMetricLabels, TAction, TTask> Dispatcher<TAction, TTask>
        for WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TTransform: WebSocketGraphQlServerQueryTransform,
        TMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask,
    {
        type State = WebSocketGraphQlServerState<T>;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (Default::default(), inbox, Default::default())
        }

        fn accept(&self, _action: &WebSocketServerConnectAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &WebSocketServerConnectAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &WebSocketServerConnectAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_websocket_graphql_server_connect(state, action, metadata, context)
        }

        fn accept(&self, _action: &WebSocketServerReceiveAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &WebSocketServerReceiveAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &WebSocketServerReceiveAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_websocket_graphql_server_receive(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerParseErrorAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerParseErrorAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerParseErrorAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_parse_error(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerEmitAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerEmitAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerEmitAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_emit_action(state, action, metadata, context)
        }

        fn accept(&self, _action: &WebSocketServerThrottleTimeoutAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &WebSocketServerThrottleTimeoutAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &WebSocketServerThrottleTimeoutAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_websocket_graphql_throttle_timeout_action(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TTransform, TMetricLabels>
    WebSocketGraphQlServer<T, TFactory, TTransform, TMetricLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TTransform: WebSocketGraphQlServerQueryTransform,
    TMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
{
    fn handle_websocket_graphql_server_connect<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerConnectAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
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
            request: clone_http_request_wrapper(request),
            initialized_state: Default::default(),
            operations: Default::default(),
        });
        None
    }
    fn handle_websocket_graphql_server_receive<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerReceiveAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<GraphQlServerSubscribeAction<T>>
            + From<GraphQlServerModifyAction<T>>
            + From<GraphQlServerUnsubscribeAction<T>>
            + From<WebSocketServerSendAction>
            + From<WebSocketServerDisconnectAction>,
        TTask: TaskFactory<TAction, TTask>,
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
    fn handle_websocket_graphql_server_receive_connection_init<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionConnectionInitMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<WebSocketServerSendAction>,
        TTask: TaskFactory<TAction, TTask>,
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
        let metric_labels = self
            .get_connection_metric_labels
            .labels(connection_params.as_ref(), connection.request.headers());
        if let Some(previous_metric_labels) = previous_metric_labels {
            decrement_gauge!(
                self.metric_names
                    .graphql_websocket_initialized_connection_count,
                1.0,
                &previous_metric_labels
            );
            connection
                .operations
                .iter_mut()
                .for_each(|operation| operation.error_metric_tracker.record_in_non_error_state());
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
        let connection_ack_action = SchedulerCommand::Send(
            self.main_pid,
            WebSocketServerSendAction {
                connection_id: *connection_id,
                message: GraphQlSubscriptionServerMessage::ConnectionAck,
            }
            .into(),
        );
        Some(SchedulerTransition::new(once(connection_ack_action)))
    }
    fn handle_websocket_graphql_server_receive_start<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionStartMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerSubscribeAction<T>> + From<WebSocketServerSendAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        let operation_id = message.operation_id();
        let operation = if connection.has_operation(operation_id) {
            Err(GraphQlSubscriptionServerMessage::ConnectionError(
                create_json_error_object(
                    format!("Subscription ID already exists: {}", operation_id),
                    None,
                ),
            ))
        } else {
            let operation = message.payload();
            parse_graphql_query(&operation.query)
                .map_err(|err| {
                    GraphQlSubscriptionServerMessage::Error(
                        operation_id.clone(),
                        create_json_error_object(format!("Invalid query: {}", err), None),
                    )
                })
                .and_then(|query| {
                    let operation = GraphQlOperation::new(
                        query,
                        operation.operation_name.clone(),
                        operation.variables.clone(),
                        operation.extensions.clone(),
                    );
                    self.transform
                        .transform(
                            operation,
                            &connection.request,
                            connection
                                .initialized_state
                                .as_ref()
                                .and_then(|initialized_state| {
                                    initialized_state.connection_params.as_ref()
                                }),
                        )
                        .map_err(|err| {
                            GraphQlSubscriptionServerMessage::Error(operation_id.clone(), err)
                        })
                })
                .and_then(|operation| {
                    parse_graphql_operation_type(operation.query(), operation.operation_name())
                        .map_err(|err| {
                            GraphQlSubscriptionServerMessage::Error(
                                operation_id.clone(),
                                create_json_error_object(format!("{}", err), None),
                            )
                        })
                        .map(|operation_type| (operation, operation_type))
                })
                .and_then(|(operation, operation_type)| {
                    let diff_result = is_diff_subscription(&operation);
                    let throttle_duration = get_subscription_throttle_duration(&operation);
                    if diff_result && operation_type != GraphQlOperationType::Subscription {
                        Err(GraphQlSubscriptionServerMessage::Error(
                            operation_id.clone(),
                            create_json_error_object(
                                format!("Result diffing is only valid for subscription operations"),
                                None,
                            ),
                        ))
                    } else if throttle_duration.is_some()
                        && operation_type != GraphQlOperationType::Subscription
                    {
                        Err(GraphQlSubscriptionServerMessage::Error(
                            operation_id.clone(),
                            create_json_error_object(
                                format!(
                                    "@throttle directive is only valid for subscription operations"
                                ),
                                None,
                            ),
                        ))
                    } else {
                        let operation_state = {
                            let subscription_id = Uuid::new_v4();
                            let validation_query = self
                                .schema_types
                                .as_ref()
                                .map(|_| operation.query().clone());
                            let mut operation_metric_labels = connection
                                .initialized_state
                                .as_ref()
                                .map(|connection| connection.metric_labels.clone())
                                .unwrap_or_else(Vec::new);
                            if let Some(operation_name) = operation.operation_name() {
                                operation_metric_labels.push((
                                    "operation_name".to_string(),
                                    operation_name.to_string(),
                                ));
                            }
                            let error_metric_tracker = QueryErrorStateTracker {
                                metric_labels: operation_metric_labels,
                                metric_name: self
                                    .metric_names
                                    .graphql_websocket_query_error_duration_micros,
                                error_state_start_time: None,
                            };
                            WebSocketGraphQlOperation {
                                operation_id: operation_id.clone(),
                                subscription_id,
                                operation_type,
                                query: validation_query,
                                diff_result: if diff_result { Some(None) } else { None },
                                throttle: throttle_duration.map(|duration| (duration, None)),
                                error_metric_tracker,
                            }
                        };
                        Ok((operation, operation_state))
                    }
                })
        };
        match operation {
            Err(err) => Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: err,
                }
                .into(),
            )))),
            Ok((operation, operation_state)) => {
                let subscription_id = operation_state.subscription_id;
                connection.operations.push(operation_state);
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.main_pid,
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
    fn handle_websocket_graphql_server_receive_stop<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionStopMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<WebSocketServerSendAction> + From<GraphQlServerUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        self.unsubscribe_operation(state, connection_id, message.operation_id(), context)
    }
    fn unsubscribe_operation<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        connection_id: &Uuid,
        operation_id: &OperationId,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<WebSocketServerSendAction> + From<GraphQlServerUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let connection = state.connections.get_mut(connection_id)?;
        if let Some(operation) = connection.remove_operation(operation_id) {
            Some(SchedulerTransition::new([
                SchedulerCommand::Send(
                    self.main_pid,
                    GraphQlServerUnsubscribeAction {
                        subscription_id: operation.subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                SchedulerCommand::Send(
                    self.main_pid,
                    WebSocketServerSendAction {
                        connection_id: *connection_id,
                        message: GraphQlSubscriptionServerMessage::Complete(operation.operation_id),
                    }
                    .into(),
                ),
            ]))
        } else {
            Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: GraphQlSubscriptionServerMessage::ConnectionError(
                        create_json_error_object(
                            format!("Subscription ID already unsubscribed: {}", operation_id),
                            None,
                        ),
                    ),
                }
                .into(),
            ))))
        }
    }
    fn handle_websocket_graphql_server_receive_update<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        message: &GraphQlSubscriptionUpdateMessage,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<WebSocketServerSendAction> + From<GraphQlServerModifyAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let WebSocketServerReceiveAction {
            connection_id,
            message: _,
        } = action;
        let connection = state.connections.get_mut(connection_id)?;
        let operation_id = message.operation_id();
        let modify_action = match connection.find_operation(operation_id) {
            None => Err(GraphQlSubscriptionServerMessage::ConnectionError(
                create_json_error_object(
                    format!("Subscription ID not found: {}", message.operation_id()),
                    None,
                ),
            )),
            Some(operation) => match operation.operation_type {
                GraphQlOperationType::Subscription => Ok(GraphQlServerModifyAction {
                    subscription_id: operation.subscription_id,
                    variables: message
                        .payload()
                        .iter()
                        .map(|(key, value)| (key.clone(), value.clone()))
                        .collect(),
                    _expression: Default::default(),
                }
                .into()),
                GraphQlOperationType::Query | GraphQlOperationType::Mutation => {
                    Err(GraphQlSubscriptionServerMessage::Error(
                        operation_id.clone(),
                        create_json_error_object(
                            format!("Unable to update {} variables", operation.operation_type),
                            None,
                        ),
                    ))
                }
            },
        };
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            modify_action.unwrap_or_else(|err| {
                WebSocketServerSendAction {
                    connection_id: *connection_id,
                    message: err,
                }
                .into()
            }),
        ))))
    }
    fn handle_websocket_graphql_server_receive_connection_terminate<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerReceiveAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<WebSocketServerSendAction>
            + From<GraphQlServerUnsubscribeAction<T>>
            + From<WebSocketServerDisconnectAction>,
        TTask: TaskFactory<TAction, TTask>,
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
        Some(SchedulerTransition::new(
            connection
                .operations
                .into_iter()
                .map(|operation| {
                    SchedulerCommand::Send(
                        self.main_pid,
                        GraphQlServerUnsubscribeAction {
                            subscription_id: operation.subscription_id,
                            _expression: Default::default(),
                        }
                        .into(),
                    )
                })
                .chain(once(SchedulerCommand::Send(
                    self.main_pid,
                    WebSocketServerDisconnectAction {
                        connection_id: *connection_id,
                    }
                    .into(),
                ))),
        ))
    }
    fn handle_graphql_parse_error<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &GraphQlServerParseErrorAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<WebSocketServerSendAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerParseErrorAction {
            subscription_id,
            message,
            operation,
            ..
        } = action;
        let (connection_id, subscription) = state.remove_subscription(subscription_id)?;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
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
    fn record_error_duration_metrics(
        &self,
        result: &T,
        subscription: &mut WebSocketGraphQlOperation<T>,
    ) {
        subscription
            .error_metric_tracker
            .record_query_state(result, &self.factory);
    }
    fn handle_graphql_emit_action<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<WebSocketServerSendAction>
            + From<WebSocketServerThrottleTimeoutAction>
            + From<GraphQlServerUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask> + From<WebSocketGraphQlServerThrottleTimeoutTaskFactory>,
    {
        let GraphQlServerEmitAction {
            subscription_id,
            result,
        } = action;
        let (connection_id, subscription) = state.find_subscription_mut(subscription_id)?;
        self.record_error_duration_metrics(result, subscription);

        let is_unchanged = subscription
            .diff_result
            .as_ref()
            .and_then(|previous_result| previous_result.as_ref())
            .map(|(existing, _)| existing.id() == result.id())
            .unwrap_or(false);
        if is_unchanged {
            return None;
        }
        let should_unsubscribe = match subscription.operation_type {
            GraphQlOperationType::Query | GraphQlOperationType::Mutation => true,
            GraphQlOperationType::Subscription => false,
        };
        let emit_actions = match subscription.throttle.as_mut() {
            None => {
                let update_message = get_subscription_result_payload(
                    result,
                    &subscription.operation_id,
                    subscription.query.as_ref(),
                    self.schema_types.as_ref(),
                    subscription.diff_result.as_mut(),
                    &self.factory,
                )?;
                Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.main_pid,
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
                    let (task_pid, task) =
                        create_throttle_timeout_task(*subscription_id, *duration, context);
                    existing_delay.replace(ThrottleState {
                        result: result.clone(),
                        task_pid,
                    });
                    Some(SchedulerTransition::new(once(SchedulerCommand::Task(
                        task_pid,
                        task.into(),
                    ))))
                }
            }
        };
        let unsubscribe_actions = if should_unsubscribe {
            let operation_id = subscription.operation_id.clone();
            self.unsubscribe_operation(state, &connection_id, &operation_id, context)
        } else {
            None
        };
        match (emit_actions, unsubscribe_actions) {
            (None, None) => None,
            (Some(emit_actions), None) => Some(emit_actions),
            (None, Some(unsubscribe_actions)) => Some(unsubscribe_actions),
            (Some(emit_actions), Some(unsubscribe_actions)) => {
                Some(emit_actions.append(unsubscribe_actions))
            }
        }
    }
    fn handle_websocket_graphql_throttle_timeout_action<TAction, TTask>(
        &self,
        state: &mut WebSocketGraphQlServerState<T>,
        action: &WebSocketServerThrottleTimeoutAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction:
            Action + From<WebSocketServerSendAction> + From<WebSocketServerThrottleTimeoutAction>,
        TTask: TaskFactory<TAction, TTask>,
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
            SchedulerCommand::Send(
                self.main_pid,
                WebSocketServerSendAction {
                    connection_id,
                    message,
                }
                .into(),
            )
        });
        let dispose_throttle_task_action = SchedulerCommand::Kill(task_pid);
        Some(SchedulerTransition::new(
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

fn create_throttle_timeout_task(
    subscription_id: Uuid,
    delay: Duration,
    context: &mut impl HandlerContext,
) -> (ProcessId, WebSocketGraphQlServerThrottleTimeoutTaskFactory) {
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = WebSocketGraphQlServerThrottleTimeoutTaskFactory {
        subscription_id,
        delay,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

#[derive(Clone)]
struct QueryErrorStateTracker {
    metric_labels: Vec<(String, String)>,
    metric_name: &'static str,
    error_state_start_time: Option<Instant>,
}
impl QueryErrorStateTracker {
    pub fn record_query_state<T: Expression>(
        &mut self,
        result: &T,
        factory: &impl ExpressionFactory<T>,
    ) {
        match GraphQlQueryStatus::get_state(Some(result), factory) {
            GraphQlQueryStatus::Error => self.record_query_error_state(),
            _ => self.record_in_non_error_state(),
        }
    }
    pub fn record_in_non_error_state(&mut self) {
        // If this was previously in error then remove it, and update the gauge as appropriate, otherwise nothing to do
        if self.error_state_start_time.is_some() {
            gauge!(self.metric_name, 0.0, &self.metric_labels);
            self.error_state_start_time = None;
        }
    }
    fn record_query_error_state(&mut self) {
        let error_start_time = self.error_state_start_time.unwrap_or_else(Instant::now);
        gauge!(
            self.metric_name,
            error_start_time.elapsed().as_micros() as f64,
            &self.metric_labels
        );
        self.error_state_start_time = Some(error_start_time);
    }
}
