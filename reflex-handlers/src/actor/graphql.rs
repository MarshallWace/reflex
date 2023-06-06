// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::{empty, once},
    marker::PhantomData,
    ops::Deref,
    string::FromUtf8Error,
    time::Duration,
};

use bytes::Bytes;
use http::{
    header::{HeaderName, CONTENT_TYPE},
    HeaderMap, HeaderValue, StatusCode,
};
use hyper::Body;
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator, ListTermType,
    RecordTermType, RefType, SignalType, StateToken, StringTermType, StringValue,
    StructPrototypeType, Uuid,
};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_graphql::{
    create_json_error_object,
    subscriptions::{GraphQlSubscriptionClientMessage, GraphQlSubscriptionServerMessage},
    GraphQlOperationPayload,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_macros::{dispatcher, Named};
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction, EffectUpdateBatch,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::{
    action::graphql::{
        GraphQlHandlerHttpConnectionErrorAction, GraphQlHandlerHttpFetchCompleteAction,
        GraphQlHandlerWebSocketClientMessageAction, GraphQlHandlerWebSocketConnectSuccessAction,
        GraphQlHandlerWebSocketConnectionErrorAction,
        GraphQlHandlerWebSocketConnectionTerminateAction,
        GraphQlHandlerWebSocketServerMessageAction,
    },
    task::graphql::{
        GraphQlConnectionUrl, GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerTask,
        GraphQlHandlerWebSocketConnectionTaskFactory,
    },
    utils::fetch::{parse_fetch_request, FetchRequest},
};

pub const EFFECT_TYPE_GRAPHQL: &'static str = "reflex::graphql";

pub fn is_graphql_effect_type<T: Expression>(
    effect_type: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_string_term(effect_type)
        .map(|effect_type| effect_type.value().as_deref().as_str().deref() == EFFECT_TYPE_GRAPHQL)
        .unwrap_or(false)
}

pub fn create_graphql_effect_type<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_string_term(allocator.create_static_string(EFFECT_TYPE_GRAPHQL))
}

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

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
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

#[derive(Named, Clone)]
pub struct GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    client: hyper::Client<TConnect, Body>,
    factory: TFactory,
    allocator: TAllocator,
    reconnect_timeout: TReconnect,
    metric_names: GraphQlHandlerMetricNames,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TConnect, TReconnect>
    GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    pub fn new(
        client: hyper::Client<TConnect, Body>,
        factory: TFactory,
        allocator: TAllocator,
        reconnect_timeout: TReconnect,
        metric_names: GraphQlHandlerMetricNames,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            client,
            factory,
            allocator,
            reconnect_timeout,
            metric_names: metric_names.init(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct GraphQlHandlerState {
    http_requests: HashMap<StateToken, HttpRequestState>,
    http_operation_effect_mappings: HashMap<Uuid, StateToken>,
    websocket_requests: HashMap<StateToken, GraphQlConnectionId>,
    websocket_connections: HashMap<GraphQlConnectionId, WebSocketConnectionState>,
    websocket_connection_mappings: HashMap<GraphQlConnectionUrl, GraphQlConnectionId>,
}
impl Default for GraphQlHandlerState {
    fn default() -> Self {
        Self {
            http_requests: Default::default(),
            http_operation_effect_mappings: Default::default(),
            websocket_requests: Default::default(),
            websocket_connections: Default::default(),
            websocket_connection_mappings: Default::default(),
        }
    }
}
struct HttpRequestState {
    operation_id: Uuid,
    task_pid: ProcessId,
    metric_labels: [(&'static str, String); 3],
}
struct WebSocketConnectionState {
    task_pid: ProcessId,
    url: GraphQlConnectionUrl,
    connection_params: Option<JsonValue>,
    operations: HashMap<StateToken, WebSocketOperationState>,
    effects: HashMap<GraphQlOperationId, StateToken>,
    connection_attempt: usize,
    metric_labels: [(&'static str, String); 1],
}
struct WebSocketOperationState {
    operation_id: GraphQlOperationId,
    operation: GraphQlOperationPayload,
    metric_labels: [(&'static str, String); 3],
}
impl GraphQlHandlerState {
    fn subscribe_http_operation<T, TFactory, TAllocator, TConnect, TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        url: GraphQlConnectionUrl,
        operation: GraphQlOperationPayload,
        headers: Option<HeaderMap>,
        client: &hyper::Client<TConnect, Body>,
        factory: &TFactory,
        allocator: &TAllocator,
        metric_names: &GraphQlHandlerMetricNames,
        context: &mut impl HandlerContext,
    ) -> Result<SchedulerCommand<TAction, TTask>, T>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask> + From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>,
    {
        let operation_name = operation.operation_name.clone();
        let request = FetchRequest {
            url: String::from(url.as_str()),
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
        match parse_fetch_request(&request) {
            Err(err) => Err(create_error_message_expression(
                format!("Invalid GraphQL HTTP request: {}", err),
                factory,
                allocator,
            )),
            Ok(_) => {
                let operation_id = Uuid::new_v4();
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
                    metric_names.graphql_effect_total_operation_count,
                    &metric_labels
                );
                increment_gauge!(
                    metric_names.graphql_effect_active_operation_count,
                    1.0,
                    &metric_labels
                );
                let (task_pid, task) =
                    create_http_fetch_task(operation_id, client.clone(), request, context);
                self.http_requests.insert(
                    effect_id,
                    HttpRequestState {
                        operation_id,
                        task_pid,
                        metric_labels,
                    },
                );
                self.http_operation_effect_mappings
                    .insert(operation_id, effect_id);
                Ok(SchedulerCommand::Task(task_pid, task.into()))
            }
        }
    }
    fn unsubscribe_http_operation<TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        metric_names: &GraphQlHandlerMetricNames,
    ) -> Option<SchedulerCommand<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let HttpRequestState {
            operation_id,
            task_pid,
            metric_labels,
        } = self.http_requests.remove(&effect_id)?;
        self.http_operation_effect_mappings.remove(&operation_id);
        decrement_gauge!(
            metric_names.graphql_effect_active_operation_count,
            1.0,
            &metric_labels
        );
        Some(SchedulerCommand::Kill(task_pid))
    }
    fn subscribe_websocket_operation<TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        url: GraphQlConnectionUrl,
        operation: GraphQlOperationPayload,
        connection_params: Option<JsonValue>,
        metric_names: &GraphQlHandlerMetricNames,
        context: &mut impl HandlerContext,
    ) -> impl Iterator<Item = SchedulerCommand<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketClientMessageAction> + Send + 'static,
        TTask: TaskFactory<TAction, TTask> + From<GraphQlHandlerWebSocketConnectionTaskFactory>,
    {
        let connection_id = match self.websocket_connection_mappings.entry(url.clone()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(GraphQlConnectionId(Uuid::new_v4())),
        };
        self.websocket_requests.insert(effect_id, connection_id);
        let (connection_state, connect_tasks) =
            match self.websocket_connections.entry(connection_id) {
                Entry::Occupied(entry) => (entry.into_mut(), None),
                Entry::Vacant(entry) => {
                    let metric_labels = [("url", String::from(url.as_str()))];
                    increment_gauge!(
                        metric_names.graphql_effect_connection_count,
                        1.0,
                        &metric_labels
                    );
                    let (task_pid, task) =
                        create_websocket_connect_task(connection_id, url.clone(), None, context);
                    let connection_state = entry.insert(WebSocketConnectionState {
                        task_pid,
                        url,
                        connection_params: connection_params.clone(),
                        operations: Default::default(),
                        effects: Default::default(),
                        connection_attempt: 0,
                        metric_labels,
                    });
                    (
                        connection_state,
                        Some(once(SchedulerCommand::Task(task_pid, task.into())).chain(
                            create_graphql_websocket_init_messages(connection_params, empty()).map(
                                move |message| {
                                    SchedulerCommand::Send(
                                        task_pid,
                                        GraphQlHandlerWebSocketClientMessageAction {
                                            connection_id: connection_id.as_uuid(),
                                            message,
                                        }
                                        .into(),
                                    )
                                },
                            ),
                        )),
                    )
                }
            };
        let subscribe_task = match connection_state.operations.entry(effect_id) {
            Entry::Occupied(_) => None,
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
                    metric_names.graphql_effect_total_operation_count,
                    &metric_labels
                );
                increment_gauge!(
                    metric_names.graphql_effect_active_operation_count,
                    1.0,
                    &metric_labels
                );
                entry.insert(WebSocketOperationState {
                    operation_id: operation_id.clone(),
                    operation: operation.clone(),
                    metric_labels,
                });
                connection_state.effects.insert(operation_id, effect_id);
                Some(SchedulerCommand::Send(
                    connection_state.task_pid,
                    GraphQlHandlerWebSocketClientMessageAction {
                        connection_id: connection_id.as_uuid(),
                        message: GraphQlSubscriptionClientMessage::start(
                            operation_id.to_string(),
                            operation,
                        ),
                    }
                    .into(),
                ))
            }
        };
        connect_tasks.into_iter().flatten().chain(subscribe_task)
    }
    fn unsubscribe_websocket_operation<TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        metric_names: &GraphQlHandlerMetricNames,
    ) -> Option<impl Iterator<Item = SchedulerCommand<TAction, TTask>>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketClientMessageAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let connection_id = self.websocket_requests.remove(&effect_id)?;
        let connection_state = self.websocket_connections.get_mut(&connection_id)?;
        let WebSocketOperationState {
            operation_id,
            operation: _,
            metric_labels,
        } = connection_state.operations.remove(&effect_id)?;
        let is_final_subscription = connection_state.operations.is_empty();
        connection_state.effects.remove(&operation_id);
        decrement_gauge!(
            metric_names.graphql_effect_active_operation_count,
            1.0,
            &metric_labels
        );
        let unsubscribe_actions = once(GraphQlSubscriptionClientMessage::stop(
            operation_id.to_string(),
        ))
        .chain(if is_final_subscription {
            // The WebSocket connection task will kill itself once the ConnectionTerminate message has been sent,
            // therefore there's no need for this actor to send a premature 'kill' scheduler command
            Some(GraphQlSubscriptionClientMessage::connection_terminate())
        } else {
            None
        })
        .map({
            let task_pid = connection_state.task_pid;
            move |message| {
                SchedulerCommand::Send(
                    task_pid,
                    GraphQlHandlerWebSocketClientMessageAction {
                        connection_id: connection_id.as_uuid(),
                        message,
                    }
                    .into(),
                )
            }
        });
        if is_final_subscription {
            if let Some(connection_state) = self.websocket_connections.remove(&connection_id) {
                let WebSocketConnectionState {
                    url, metric_labels, ..
                } = connection_state;
                decrement_gauge!(
                    metric_names.graphql_effect_connection_count,
                    1.0,
                    &metric_labels
                );
                self.websocket_connection_mappings.remove(&url);
            }
        }
        Some(unsubscribe_actions)
    }
}

dispatcher!({
    pub enum GraphQlHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(GraphQlHandlerHttpFetchCompleteAction),
        Inbox(GraphQlHandlerHttpConnectionErrorAction),
        Inbox(GraphQlHandlerWebSocketConnectSuccessAction),
        Inbox(GraphQlHandlerWebSocketConnectionErrorAction),
        Inbox(GraphQlHandlerWebSocketServerMessageAction),

        Outbox(EffectEmitAction<T>),
        Outbox(GraphQlHandlerHttpFetchCompleteAction),
        Outbox(GraphQlHandlerHttpConnectionErrorAction),
        Outbox(GraphQlHandlerWebSocketClientMessageAction),
        Outbox(GraphQlHandlerWebSocketConnectionTerminateAction),
    }

    impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask> Dispatcher<TAction, TTask>
        for GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect>
    where
        T: AsyncExpression,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TReconnect: ReconnectTimeout + Send + Clone,
        TAction: Action + Send + 'static,
        TTask: TaskFactory<TAction, TTask> + GraphQlHandlerTask<TConnect>,
    {
        type State = GraphQlHandlerState;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Sync(inbox)
        }

        fn accept(&self, action: &EffectSubscribeAction<T>) -> bool {
            is_graphql_effect_type(&action.effect_type, &self.factory)
        }
        fn schedule(
            &self,
            _action: &EffectSubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectSubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_subscribe(state, action, metadata, context)
        }

        fn accept(&self, action: &EffectUnsubscribeAction<T>) -> bool {
            is_graphql_effect_type(&action.effect_type, &self.factory)
        }
        fn schedule(
            &self,
            _action: &EffectUnsubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectUnsubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_unsubscribe(state, action, metadata, context)
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
    }
});

impl<T, TFactory, TAllocator, TConnect, TReconnect>
    GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<EffectEmitAction<T>>
            + From<GraphQlHandlerWebSocketClientMessageAction>
            + Send
            + 'static,
        TTask: TaskFactory<TAction, TTask>
            + From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
            + From<GraphQlHandlerWebSocketConnectionTaskFactory>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if !is_graphql_effect_type(effect_type, &self.factory) {
            return None;
        }
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_graphql_effect_args(effect, &self.factory) {
                    Ok(args) => {
                        let GraphQlEffectArgs {
                            url,
                            operation,
                            headers,
                        } = args;
                        if is_websocket_url(&url) {
                            let connection_params = headers.map(|headers| {
                                JsonValue::Object(JsonMap::from_iter(headers.into_iter().map(
                                    |(key, value)| {
                                        (key.to_string(), JsonValue::from(value.to_string()))
                                    },
                                )))
                            });
                            let websocket_actions = state.subscribe_websocket_operation(
                                effect.id(),
                                url,
                                operation,
                                connection_params,
                                &self.metric_names,
                                context,
                            );
                            let initial_value =
                                create_pending_expression(&self.factory, &self.allocator);
                            Some((
                                (state_token, initial_value),
                                (Some(websocket_actions), None),
                            ))
                        } else {
                            let headers = headers.and_then(|headers| {
                                let headers = headers
                                    .into_iter()
                                    .filter_map(|(key, value)| match value {
                                        JsonValue::Null => None,
                                        JsonValue::String(value) => Some((
                                            HeaderName::try_from(key).ok()?,
                                            HeaderValue::try_from(value).ok()?,
                                        )),
                                        _ => None,
                                    })
                                    .collect::<HeaderMap<_>>();
                                if headers.is_empty() {
                                    None
                                } else {
                                    Some(headers)
                                }
                            });
                            match state.subscribe_http_operation(
                                effect.id(),
                                url,
                                operation,
                                headers,
                                &self.client,
                                &self.factory,
                                &self.allocator,
                                &self.metric_names,
                                context,
                            ) {
                                Ok(subscribe_action) => {
                                    let http_actions = Some(subscribe_action);
                                    Some((
                                        (
                                            state_token,
                                            create_pending_expression(
                                                &self.factory,
                                                &self.allocator,
                                            ),
                                        ),
                                        (None, http_actions),
                                    ))
                                }
                                Err(err) => Some(((state_token, err), (None, None))),
                            }
                        }
                    }
                    Err(err) => Some((
                        (
                            state_token,
                            create_error_message_expression(err, &self.factory, &self.allocator),
                        ),
                        (None, None),
                    )),
                }
            })
            .unzip();
        let initial_values_action = if initial_values.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                        updates: initial_values,
                    }],
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().flat_map(|(websocket_tasks, http_tasks)| {
                    websocket_tasks.into_iter().flatten().chain(http_tasks)
                })),
        ))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlHandlerWebSocketClientMessageAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if !is_graphql_effect_type(effect_type, &self.factory) {
            return None;
        }
        let unsubscribe_actions = effects.iter().flat_map(|effect| {
            let effect_id = effect.id();
            let http_actions = state.unsubscribe_http_operation(effect_id, &self.metric_names);
            let websocket_actions =
                state.unsubscribe_websocket_operation(effect_id, &self.metric_names);
            http_actions
                .into_iter()
                .chain(websocket_actions.into_iter().flatten())
        });
        Some(SchedulerTransition::new(unsubscribe_actions))
    }
    fn handle_graphql_handler_http_fetch_complete<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerHttpFetchCompleteAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlHandlerHttpFetchCompleteAction {
            operation_id,
            url: _,
            status_code,
            body,
        } = action;
        let effect_id = state
            .http_operation_effect_mappings
            .get(operation_id)
            .copied()?;
        let disconnect_action = state.unsubscribe_http_operation(effect_id, &self.metric_names)?;
        let result =
            parse_graphql_http_response(*status_code, body, &self.factory, &self.allocator);
        Some(SchedulerTransition::new([
            disconnect_action,
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                        updates: vec![(effect_id, result)],
                    }],
                }
                .into(),
            ),
        ]))
    }
    fn handle_graphql_handler_http_connection_error<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerHttpConnectionErrorAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlHandlerHttpConnectionErrorAction {
            operation_id,
            url: _,
            message,
        } = action;
        let effect_id = state
            .http_operation_effect_mappings
            .get(operation_id)
            .copied()?;
        let disconnect_action = state.unsubscribe_http_operation(effect_id, &self.metric_names)?;
        let result =
            create_error_message_expression(message.clone(), &self.factory, &self.allocator);
        Some(SchedulerTransition::new([
            disconnect_action,
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                        updates: vec![(effect_id, result)],
                    }],
                }
                .into(),
            ),
        ]))
    }
    fn handle_graphql_handler_websocket_connect_success<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketConnectSuccessAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlHandlerWebSocketConnectSuccessAction {
            connection_id,
            url: _,
        } = action;
        let connection_id = GraphQlConnectionId(*connection_id);
        let connection_state = state.websocket_connections.get_mut(&connection_id)?;
        connection_state.connection_attempt = 0;
        None
    }
    fn handle_graphql_handler_websocket_connection_error<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketConnectionErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction:
            Action + From<EffectEmitAction<T>> + From<GraphQlHandlerWebSocketClientMessageAction>,
        TTask: TaskFactory<TAction, TTask> + From<GraphQlHandlerWebSocketConnectionTaskFactory>,
    {
        let GraphQlHandlerWebSocketConnectionErrorAction {
            connection_id,
            url: _,
            message,
            retryable,
        } = action;
        let connection_id = GraphQlConnectionId(*connection_id);
        let mut entry = match state.websocket_connections.entry(connection_id) {
            Entry::Occupied(entry) => Some(entry),
            Entry::Vacant(_) => None,
        }?;
        let emit_action = {
            let connection_state = entry.get();
            let value = create_error_message_expression(message, &self.factory, &self.allocator);
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                        updates: connection_state
                            .effects
                            .values()
                            .copied()
                            .map(|effect_id| (effect_id, value.clone()))
                            .collect(),
                    }],
                }
                .into(),
            )
        };
        let reconnect_timeout = if *retryable {
            let connection_state = entry.get();
            self.reconnect_timeout
                .duration(connection_state.connection_attempt)
        } else {
            None
        };
        match reconnect_timeout {
            None => {
                let connection_state = entry.remove();
                Some(SchedulerTransition::new([
                    SchedulerCommand::Kill(connection_state.task_pid),
                    emit_action,
                ]))
            }
            Some(reconnect_timeout) => {
                let delay = if reconnect_timeout.is_zero() {
                    None
                } else {
                    Some(reconnect_timeout)
                };
                let connection_state = entry.get_mut();
                connection_state.connection_attempt += 1;
                let (task_pid, task) = create_websocket_connect_task(
                    connection_id,
                    connection_state.url.clone(),
                    delay,
                    context,
                );
                let previous_pid = std::mem::replace(&mut connection_state.task_pid, task_pid);
                Some(SchedulerTransition::new(
                    [
                        SchedulerCommand::Kill(previous_pid),
                        emit_action,
                        SchedulerCommand::Task(task_pid, task.into()),
                    ]
                    .into_iter()
                    .chain(
                        create_graphql_websocket_init_messages(
                            connection_state.connection_params.clone(),
                            connection_state.operations.values().map(|operation| {
                                (operation.operation_id.clone(), operation.operation.clone())
                            }),
                        )
                        .map(|message| {
                            SchedulerCommand::Send(
                                task_pid,
                                GraphQlHandlerWebSocketClientMessageAction {
                                    connection_id: connection_id.as_uuid(),
                                    message,
                                }
                                .into(),
                            )
                        }),
                    ),
                ))
            }
        }
    }
    fn handle_graphql_handler_websocket_server_message<TAction, TTask>(
        &self,
        state: &mut GraphQlHandlerState,
        action: &GraphQlHandlerWebSocketServerMessageAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlHandlerWebSocketServerMessageAction {
            connection_id,
            message,
        } = action;
        let connection_id = GraphQlConnectionId(*connection_id);
        let connection_state = state.websocket_connections.get_mut(&connection_id)?;
        match message.as_ref() {
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
    fn handle_graphql_handler_websocket_server_message_connection_error<TAction, TTask>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        payload: &JsonValue,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        if connection_state.operations.is_empty() {
            None
        } else {
            let value =
                parse_graphql_error_payload(payload.clone(), &self.factory, &self.allocator);
            Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                        updates: connection_state
                            .effects
                            .values()
                            .copied()
                            .map(|effect_id| (effect_id, value.clone()))
                            .collect(),
                    }],
                }
                .into(),
            ))))
        }
    }
    fn handle_graphql_handler_websocket_server_message_data<TAction, TTask>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        payload: &JsonValue,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let effect_id = Uuid::parse_str(operation_id).ok().and_then(|uuid| {
            let operation_id = GraphQlOperationId(uuid);
            connection_state.effects.get_mut(&operation_id)
        })?;
        let value = parse_graphql_data_payload(payload.clone(), &self.factory, &self.allocator);
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_graphql_handler_websocket_server_message_patch<TAction, TTask>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        _payload: &JsonValue,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_graphql_handler_websocket_server_message_error<TAction, TTask>(
        &self,
        connection_state: &mut WebSocketConnectionState,
        operation_id: &str,
        payload: &JsonValue,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let effect_id = Uuid::parse_str(operation_id).ok().and_then(|uuid| {
            let operation_id = GraphQlOperationId(uuid);
            connection_state.effects.get_mut(&operation_id)
        })?;
        let value = parse_graphql_error_payload(payload.clone(), &self.factory, &self.allocator);
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: create_graphql_effect_type(&self.factory, &self.allocator),
                    updates: vec![(*effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
}

fn create_http_fetch_task<TConnect>(
    operation_id: Uuid,
    client: hyper::Client<TConnect, Body>,
    request: FetchRequest,
    context: &mut impl HandlerContext,
) -> (ProcessId, GraphQlHandlerHttpFetchTaskFactory<TConnect>)
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = GraphQlHandlerHttpFetchTaskFactory {
        operation_id,
        client,
        request,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

fn create_graphql_websocket_init_messages(
    connection_params: Option<JsonValue>,
    operations: impl IntoIterator<Item = (GraphQlOperationId, GraphQlOperationPayload)>,
) -> impl Iterator<Item = GraphQlSubscriptionClientMessage> {
    once(GraphQlSubscriptionClientMessage::connection_init(
        connection_params,
    ))
    .chain(operations.into_iter().map(|(operation_id, operation)| {
        GraphQlSubscriptionClientMessage::start(operation_id.to_string(), operation)
    }))
}

fn create_websocket_connect_task(
    connection_id: GraphQlConnectionId,
    url: GraphQlConnectionUrl,
    delay: Option<Duration>,
    context: &mut impl HandlerContext,
) -> (ProcessId, GraphQlHandlerWebSocketConnectionTaskFactory) {
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = GraphQlHandlerWebSocketConnectionTaskFactory {
        connection_id: connection_id.as_uuid(),
        url,
        delay,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

fn parse_graphql_http_response<T: Expression>(
    status_code: StatusCode,
    body: &Bytes,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    if status_code.is_success() {
        match read_utf8_bytes(body) {
            Err(err) => Err(format!(
                "Failed to deserialize GraphQL HTTP response: {}",
                err
            )),
            Ok(body) => match reflex_json::deserialize(&body) {
                Err(_) => Err(String::from("Invalid JSON response")),
                Ok(data) => Ok(parse_graphql_data_payload(data, factory, allocator)),
            },
        }
    } else {
        Err(format!(
            "HTTP error {} {}",
            status_code,
            status_code.canonical_reason().unwrap_or("Unknown")
        ))
    }
    .unwrap_or_else(|err| create_error_message_expression(err, factory, allocator))
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

struct GraphQlEffectArgs {
    url: GraphQlConnectionUrl,
    operation: GraphQlOperationPayload,
    headers: Option<JsonMap<String, JsonValue>>,
}

fn parse_graphql_effect_args<T: Expression>(
    effect: &T::Signal,
    factory: &impl ExpressionFactory<T>,
) -> Result<GraphQlEffectArgs, String> {
    let payload = effect.payload();
    let payload = payload.as_deref();
    let args = factory
        .match_list_term(payload)
        .filter(|args| args.items().as_deref().len() == 6)
        .ok_or_else(|| {
            format!(
                "Invalid graphql signal: Expected 6 arguments, received {}",
                payload
            )
        })?;
    let args = args.items();
    let mut args = args.as_deref().iter().map(|item| item.as_deref().clone());
    let url = args.next().unwrap();
    let query = args.next().unwrap();
    let operation_name = args.next().unwrap();
    let variables = args.next().unwrap();
    let extensions = args.next().unwrap();
    let headers = args.next().unwrap();
    let url = parse_string_arg(&url, factory);
    let query = parse_string_arg(&query, factory);
    let operation_name = parse_optional_string_arg(&operation_name, factory);
    let variables = parse_optional_object_arg(&variables, factory)?;
    let extensions = parse_optional_object_arg(&extensions, factory)?;
    let headers = parse_optional_object_arg(&headers, factory)?;
    match (url, query, operation_name, variables, extensions, headers) {
        (
            Some(url),
            Some(query),
            Some(operation_name),
            Some(variables),
            Some(extensions),
            Some(headers),
        ) => Ok(GraphQlEffectArgs {
            url: GraphQlConnectionUrl::from(url),
            operation: GraphQlOperationPayload {
                query,
                operation_name,
                variables: variables.into_iter().flatten().collect(),
                extensions: extensions.into_iter().flatten().collect(),
            },
            headers: headers.map(|headers| headers.into_iter().collect()),
        }),
        _ => Err(format!("Invalid graphql signal arguments: {}", payload)),
    }
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_string_term(value) {
        Some(term) => Some(String::from(term.value().as_deref().as_str().deref())),
        _ => None,
    }
}

fn parse_optional_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<String>> {
    match factory.match_string_term(value) {
        Some(term) => Some(Some(String::from(term.value().as_deref().as_str().deref()))),
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
                .zip(value.values().as_deref().iter())
                .filter_map(|(key, value)| {
                    factory.match_string_term(key.as_deref()).map(|key| {
                        reflex_json::sanitize(value.as_deref()).map(|value| {
                            (String::from(key.value().as_deref().as_str().deref()), value)
                        })
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
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Pending,
        factory.create_nil_term(),
        factory.create_nil_term(),
    ))))
}

fn create_error_message_expression<T: Expression>(
    message: impl Into<String>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(
        factory.create_string_term(allocator.create_string(message.into())),
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
            allocator.create_signal(SignalType::Error, payload, factory.create_nil_term())
        })),
    )
}

fn read_utf8_bytes(data: &Bytes) -> Result<String, FromUtf8Error> {
    String::from_utf8(data.into_iter().copied().collect())
}
