// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
    ops::Deref,
    str::FromStr,
    time::Duration,
};

use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use prost::Message;
use reflex::core::{
    create_record, ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator,
    ListTermType, RecordTermType, Reducible, RefType, Rewritable, SignalType, StateToken,
    StringTermType, StringValue, StructPrototypeType, SymbolId, SymbolTermType,
};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_json::JsonValue;
use reflex_macros::{blanket_trait, dispatcher, Named};
use reflex_protobuf::{reflection::DynamicMessage, Bytes, ProtoTranscoder};
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction, EffectUpdateBatch,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_utils::reconnect::ReconnectTimeout;
use tonic::{transport::Endpoint, Status};
use uuid::Uuid;

use crate::{
    action::{
        GrpcHandlerConnectErrorAction, GrpcHandlerConnectSuccessAction,
        GrpcHandlerConnectionTerminateAction, GrpcHandlerErrorResponseAction,
        GrpcHandlerRequestStartAction, GrpcHandlerRequestStopAction,
        GrpcHandlerSuccessResponseAction, GrpcHandlerTransportErrorAction, GrpcMetadata,
    },
    task::{
        GrpcHandlerConnectionTaskAction, GrpcHandlerConnectionTaskActorAction,
        GrpcHandlerConnectionTaskFactory,
    },
    utils::{GrpcMethod, GrpcMethodName, GrpcServiceLibrary, GrpcServiceName, ProtoId},
    GrpcConfig,
};

pub const EFFECT_TYPE_GRPC: &'static str = "reflex::grpc";

// https://developer.mozilla.org/en-US/docs/Web/API/DOMException#error_names
pub const ERROR_TYPE_NETWORK_ERROR: &'static str = "NetworkError";
pub const ERROR_TYPE_SYNTAX_ERROR: &'static str = "SyntaxError";

#[derive(Clone, Copy, Debug)]
pub struct GrpcHandlerMetricNames {
    pub grpc_effect_connection_count: &'static str,
    pub grpc_effect_total_request_count: &'static str,
    pub grpc_effect_active_request_count: &'static str,
}
impl GrpcHandlerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.grpc_effect_connection_count,
            Unit::Count,
            "Active gRPC effect connection count"
        );
        describe_counter!(
            self.grpc_effect_total_request_count,
            Unit::Count,
            "Total gRPC effect operation count"
        );
        describe_gauge!(
            self.grpc_effect_active_request_count,
            Unit::Count,
            "Active gRPC effect operation count"
        );
        self
    }
}
impl Default for GrpcHandlerMetricNames {
    fn default() -> Self {
        Self {
            grpc_effect_connection_count: "grpc_effect_connection_count",
            grpc_effect_total_request_count: "grpc_effect_total_request_count",
            grpc_effect_active_request_count: "grpc_effect_active_request_count",
        }
    }
}

blanket_trait!(
    pub trait GrpcHandlerAction<T: Expression>:
        GrpcHandlerActorAction<T> + GrpcHandlerConnectionTaskAction
    {
    }
);

blanket_trait!(
    pub trait GrpcHandlerTask<T, TFactory, TAllocator, TTranscoder>:
        From<GrpcHandlerConnectionTaskFactory>
    where
        T: AsyncExpression,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TTranscoder: ProtoTranscoder + Send + 'static,
    {
    }
);

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
struct GrpcServiceUrl(String);
impl GrpcServiceUrl {
    fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl std::fmt::Display for GrpcServiceUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct GrpcConnectionId(Uuid);
impl GrpcConnectionId {
    fn as_uuid(&self) -> Uuid {
        let Self(value) = self;
        *value
    }
}
impl std::fmt::Display for GrpcConnectionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_uuid(), f)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct GrpcOperationId(Uuid);
impl GrpcOperationId {
    fn as_uuid(&self) -> Uuid {
        let Self(value) = self;
        *value
    }
}
impl std::fmt::Display for GrpcOperationId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.as_uuid(), f)
    }
}

#[derive(Named, Clone)]
pub struct GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TConfig: GrpcConfig + 'static,
    TReconnect: ReconnectTimeout + Send,
{
    services: GrpcServiceLibrary,
    transcoder: TTranscoder,
    factory: TFactory,
    allocator: TAllocator,
    reconnect_timeout: TReconnect,
    max_operations_per_connection: Option<usize>,
    config: TConfig,
    metric_names: GrpcHandlerMetricNames,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
    GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TConfig: GrpcConfig,
    TReconnect: ReconnectTimeout + Send,
{
    pub fn new(
        services: GrpcServiceLibrary,
        transcoder: TTranscoder,
        factory: TFactory,
        allocator: TAllocator,
        reconnect_timeout: TReconnect,
        max_operations_per_connection: Option<usize>,
        config: TConfig,
        metric_names: GrpcHandlerMetricNames,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            services,
            transcoder,
            factory,
            allocator,
            reconnect_timeout,
            max_operations_per_connection,
            config,
            metric_names: metric_names.init(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct GrpcHandlerState {
    active_requests: HashMap<StateToken, GrpcConnectionId>,
    active_connections: HashMap<GrpcConnectionId, GrpcConnectionState>,
    active_connection_mappings: HashMap<GrpcServiceUrl, HashSet<GrpcConnectionId>>,
}
struct GrpcConnectionState {
    task_pid: ProcessId,
    url: GrpcServiceUrl,
    endpoint: Endpoint,
    operations: HashMap<StateToken, GrpcOperationState>,
    effects: HashMap<GrpcOperationId, StateToken>,
    connection_attempt: usize,
    metric_labels: [(&'static str, String); 1],
}
struct GrpcOperationState {
    operation_id: GrpcOperationId,
    request: GrpcRequest,
    metric_labels: [(&'static str, String); 3],
}
#[derive(Clone, Debug)]
struct GrpcRequest {
    service_name: GrpcServiceName,
    method_name: GrpcMethodName,
    method: GrpcMethod,
    payload: JsonValue,
    metadata: GrpcMetadata,
    message: Bytes,
}
impl Default for GrpcHandlerState {
    fn default() -> Self {
        Self {
            active_requests: Default::default(),
            active_connections: Default::default(),
            active_connection_mappings: Default::default(),
        }
    }
}
impl GrpcHandlerState {
    fn subscribe_grpc_operation<TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        url: GrpcServiceUrl,
        endpoint: Endpoint,
        request: GrpcRequest,
        max_operations_per_connection: Option<usize>,
        metric_names: &GrpcHandlerMetricNames,
        context: &mut impl HandlerContext,
    ) -> impl Iterator<Item = SchedulerCommand<TAction, TTask>>
    where
        TAction: Action + From<GrpcHandlerRequestStartAction>,
        TTask: TaskFactory<TAction, TTask> + From<GrpcHandlerConnectionTaskFactory>,
    {
        let connection_id = match self.active_connection_mappings.entry(url.clone()) {
            Entry::Occupied(mut entry) => {
                // Reuse an existing connection if one exists for this URL and is below its maximum operation limit
                let existing_connection_id = {
                    let mut existing_connection_ids = entry.get().iter().copied();
                    match max_operations_per_connection {
                        None => existing_connection_ids.next(),
                        Some(max_operations) => existing_connection_ids.find(|connection_id| {
                            self.active_connections
                                .get(connection_id)
                                .map(|connection_state| {
                                    connection_state.operations.len() < max_operations
                                })
                                .unwrap_or(false)
                        }),
                    }
                };
                if let Some(connection_id) = existing_connection_id {
                    connection_id
                } else {
                    let connection_id = GrpcConnectionId(Uuid::new_v4());
                    entry.get_mut().insert(connection_id);
                    connection_id
                }
            }
            Entry::Vacant(entry) => {
                let connection_id = GrpcConnectionId(Uuid::new_v4());
                entry.insert(once(connection_id).collect());
                connection_id
            }
        };
        self.active_requests.insert(effect_id, connection_id);
        let (connection_state, connect_task) = match self.active_connections.entry(connection_id) {
            Entry::Occupied(entry) => (entry.into_mut(), None),
            Entry::Vacant(entry) => {
                let metric_labels = [("url", String::from(url.as_str()))];
                increment_gauge!(
                    metric_names.grpc_effect_connection_count,
                    1.0,
                    &metric_labels
                );
                let (task_pid, task) =
                    create_grpc_connect_task(connection_id, endpoint.clone(), None, context);
                let connection_state = entry.insert(GrpcConnectionState {
                    task_pid,
                    url: url.clone(),
                    endpoint,
                    operations: Default::default(),
                    effects: Default::default(),
                    connection_attempt: 0,
                    metric_labels,
                });
                (
                    connection_state,
                    Some(SchedulerCommand::Task(task_pid, task.into())),
                )
            }
        };
        let subscribe_task = match connection_state.operations.entry(effect_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                let operation_id = GrpcOperationId(Uuid::new_v4());
                let GrpcRequest {
                    service_name,
                    method_name,
                    ..
                } = &request;
                // TODO: Allow configurable gRPC effect metric labels
                let metric_labels = [
                    ("url", String::from(url.as_str())),
                    ("service", String::from(service_name.as_str())),
                    ("method", String::from(method_name.as_str())),
                ];
                increment_counter!(metric_names.grpc_effect_total_request_count, &metric_labels);
                increment_gauge!(
                    metric_names.grpc_effect_active_request_count,
                    1.0,
                    &metric_labels
                );
                entry.insert(GrpcOperationState {
                    operation_id: operation_id.clone(),
                    request: request.clone(),
                    metric_labels,
                });
                connection_state.effects.insert(operation_id, effect_id);
                let GrpcRequest {
                    service_name,
                    method_name,
                    method,
                    payload,
                    metadata,
                    message,
                } = request;
                Some(SchedulerCommand::Send(
                    connection_state.task_pid,
                    GrpcHandlerRequestStartAction {
                        connection_id: connection_id.as_uuid(),
                        url: url.into_string(),
                        operation_id: operation_id.as_uuid(),
                        service_name: service_name.into_string(),
                        method_name: method_name.into_string(),
                        method_path: get_grpc_method_path(&method),
                        streaming: method.descriptor.is_server_streaming(),
                        input: payload,
                        metadata,
                        message,
                    }
                    .into(),
                ))
            }
        };
        connect_task.into_iter().chain(subscribe_task)
    }
    fn unsubscribe_grpc_operation<TAction, TTask>(
        &mut self,
        effect_id: StateToken,
        metric_names: &GrpcHandlerMetricNames,
    ) -> Option<impl Iterator<Item = SchedulerCommand<TAction, TTask>>>
    where
        TAction: Action
            + From<GrpcHandlerRequestStopAction>
            + From<GrpcHandlerConnectionTerminateAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let connection_id = self.active_requests.remove(&effect_id)?;
        let (operation_state, url, task_pid, is_final_operation_for_connection) = {
            let connection_state = self.active_connections.get_mut(&connection_id)?;
            let operation_state = connection_state.operations.remove(&effect_id)?;
            let url = String::from(connection_state.url.as_str());
            let task_pid = connection_state.task_pid;
            let is_final_subscription = connection_state.operations.is_empty();
            connection_state
                .effects
                .remove(&operation_state.operation_id);
            (operation_state, url, task_pid, is_final_subscription)
        };
        let GrpcOperationState {
            operation_id,
            request,
            metric_labels,
        } = operation_state;
        decrement_gauge!(
            metric_names.grpc_effect_active_request_count,
            1.0,
            &metric_labels
        );
        let unsubscribe_action = GrpcHandlerRequestStopAction {
            connection_id: connection_id.as_uuid(),
            url,
            operation_id: operation_id.as_uuid(),
            service_name: request.service_name.into_string(),
            method_name: request.method_name.into_string(),
            method_path: get_grpc_method_path(&request.method),
            input: request.payload,
            metadata: request.metadata,
        }
        .into();
        let disconnect_action = if is_final_operation_for_connection {
            if let Some(connection_state) = self.active_connections.remove(&connection_id) {
                let GrpcConnectionState {
                    url, metric_labels, ..
                } = connection_state;
                decrement_gauge!(
                    metric_names.grpc_effect_connection_count,
                    1.0,
                    &metric_labels
                );
                let is_final_connection_for_url = self
                    .active_connection_mappings
                    .get_mut(&url)
                    .map(|remaining_connection_ids| {
                        remaining_connection_ids.remove(&connection_id);
                        remaining_connection_ids.is_empty()
                    })
                    .unwrap_or(false);
                if is_final_connection_for_url {
                    self.active_connection_mappings.remove(&url);
                }
                // The gRPC connection task will kill itself once the ConnectionTerminate message has been sent,
                // therefore there's no need for this actor to send a premature 'kill' scheduler command
                Some(
                    GrpcHandlerConnectionTerminateAction {
                        connection_id: connection_id.as_uuid(),
                        url: url.into_string(),
                    }
                    .into(),
                )
            } else {
                None
            }
        } else {
            None
        };
        Some(
            once(unsubscribe_action)
                .chain(disconnect_action)
                .map(move |message| SchedulerCommand::Send(task_pid, message)),
        )
    }
    fn handle_connection_error<T: Expression, TAction, TTask>(
        &mut self,
        connection_id: GrpcConnectionId,
        error: T,
        reconnect_timeout: &impl ReconnectTimeout,
        main_pid: ProcessId,
        context: &mut impl HandlerContext,
    ) -> Option<impl Iterator<Item = SchedulerCommand<TAction, TTask>>>
    where
        TAction: Action
            + Send
            + 'static
            + From<EffectEmitAction<T>>
            + From<GrpcHandlerRequestStartAction>
            + From<GrpcHandlerRequestStopAction>,
        TTask: TaskFactory<TAction, TTask> + From<GrpcHandlerConnectionTaskFactory>,
    {
        let mut entry = match self.active_connections.entry(connection_id) {
            Entry::Occupied(entry) => Some(entry),
            Entry::Vacant(_) => None,
        }?;
        let emit_action = {
            let connection_state = entry.get();
            SchedulerCommand::Send(
                main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_GRPC.into(),
                        updates: connection_state
                            .effects
                            .values()
                            .copied()
                            .map(|effect_id| (effect_id, error.clone()))
                            .collect(),
                    }],
                }
                .into(),
            )
        };
        let timeout_duration = {
            let connection_state = entry.get();
            reconnect_timeout.duration(connection_state.connection_attempt)
        };
        let (abort_actions, reconnect_actions) = match timeout_duration {
            None => {
                let connection_state = entry.remove();
                (
                    Some([
                        SchedulerCommand::Kill(connection_state.task_pid),
                        emit_action,
                    ]),
                    None,
                )
            }
            Some(reconnect_timeout) => {
                let delay = if reconnect_timeout.is_zero() {
                    None
                } else {
                    Some(reconnect_timeout)
                };
                let connection_state = entry.get_mut();
                connection_state.connection_attempt += 1;
                let (task_pid, task) = create_grpc_connect_task(
                    connection_id,
                    connection_state.endpoint.clone(),
                    delay,
                    context,
                );
                let previous_pid = std::mem::replace(&mut connection_state.task_pid, task_pid);
                let resubscribe_active_operations = connection_state.operations.values().map({
                    let connection_id = connection_id.as_uuid();
                    let url = String::from(connection_state.url.as_str());
                    move |operation| {
                        let GrpcOperationState {
                            operation_id,
                            request,
                            metric_labels: _,
                        } = operation;
                        let GrpcRequest {
                            service_name,
                            method_name,
                            method,
                            payload,
                            metadata,
                            message,
                        } = request;
                        GrpcHandlerRequestStartAction {
                            connection_id,
                            url: url.clone(),
                            operation_id: operation_id.as_uuid(),
                            service_name: service_name.clone().into_string(),
                            method_name: method_name.clone().into_string(),
                            method_path: get_grpc_method_path(&method),
                            streaming: method.descriptor.is_server_streaming(),
                            input: payload.clone(),
                            metadata: metadata.clone(),
                            message: message.clone(),
                        }
                        .into()
                    }
                });
                (
                    None,
                    Some(
                        [
                            SchedulerCommand::Kill(previous_pid),
                            emit_action,
                            SchedulerCommand::Task(task_pid, task.into()),
                        ]
                        .into_iter()
                        .chain(
                            resubscribe_active_operations
                                .map(move |action| SchedulerCommand::Send(task_pid, action)),
                        ),
                    ),
                )
            }
        };
        Some(
            abort_actions
                .into_iter()
                .flatten()
                .chain(reconnect_actions.into_iter().flatten())
                .collect::<Vec<_>>()
                .into_iter(),
        )
    }
}

dispatcher!({
    pub enum GrpcHandlerActorAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(GrpcHandlerConnectSuccessAction),
        Inbox(GrpcHandlerConnectErrorAction),
        Inbox(GrpcHandlerSuccessResponseAction),
        Inbox(GrpcHandlerErrorResponseAction),
        Inbox(GrpcHandlerTransportErrorAction),

        Outbox(EffectEmitAction<T>),
        Outbox(GrpcHandlerRequestStartAction),
        Outbox(GrpcHandlerRequestStopAction),
        Outbox(GrpcHandlerConnectionTerminateAction),
    }

    impl<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect, TAction, TTask>
        Dispatcher<TAction, TTask>
        for GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T>,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TTranscoder: ProtoTranscoder + Clone + Send + 'static,
        TConfig: GrpcConfig + 'static,
        TReconnect: ReconnectTimeout + Send,
        TAction: Action + GrpcHandlerConnectionTaskActorAction + Send + 'static,
        TTask: TaskFactory<TAction, TTask> + GrpcHandlerTask<T, TFactory, TAllocator, TTranscoder>,
    {
        type State = GrpcHandlerState;
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
            action.effect_type.as_str() == EFFECT_TYPE_GRPC
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
            action.effect_type.as_str() == EFFECT_TYPE_GRPC
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

        fn accept(&self, _action: &GrpcHandlerConnectSuccessAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerConnectSuccessAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerConnectSuccessAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_connect_success(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerConnectErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerConnectErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerConnectErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_connect_error(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerSuccessResponseAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerSuccessResponseAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerSuccessResponseAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_success_response(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerErrorResponseAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerErrorResponseAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerErrorResponseAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_error_response(state, action, metadata, context)
        }

        fn accept(&self, _action: &GrpcHandlerTransportErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GrpcHandlerTransportErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GrpcHandlerTransportErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_grpc_handler_transport_error(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
    GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TConfig: GrpcConfig + 'static,
    TReconnect: ReconnectTimeout + Send,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + Send
            + 'static
            + From<EffectEmitAction<T>>
            + From<GrpcHandlerRequestStartAction>,
        TTask: TaskFactory<TAction, TTask> + From<GrpcHandlerConnectionTaskFactory>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_GRPC {
            return None;
        }
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .map(|effect| {
                let state_token = effect.id();
                match parse_grpc_effect_args(effect, &self.factory)
                    .map_err(|err| (err, None))
                    .and_then(|args| {
                        let GrpcEffectArgs {
                            proto_id,
                            url,
                            service_name,
                            method_name,
                            input,
                            metadata,
                        } = args;
                        let deserialized_args: Result<_, String> = (|| {
                            let endpoint = parse_grpc_endpoint(&self.config, &url)?;
                            let method = self
                                .services
                                .get(&proto_id, &service_name, &method_name)
                                .cloned()
                                .ok_or_else(|| {
                                    format_grpc_error_message(
                                        "Unrecognized gRPC method",
                                        &service_name,
                                        format!("{}:{}.{}", proto_id, service_name, method_name),
                                        &input,
                                    )
                                })?;
                            let request_message_type = method.descriptor.input();
                            let message = self
                                .transcoder
                                .serialize_message(
                                    &input,
                                    &request_message_type,
                                    &self.transcoder,
                                    &self.factory,
                                    &self.allocator,
                                )
                                .map(|message| Bytes::from(message.encode_to_vec()))
                                .map_err(|err| {
                                    format_grpc_error_message(
                                        err,
                                        &service_name,
                                        method.descriptor.name(),
                                        &input,
                                    )
                                })?;
                            let payload = reflex_json::sanitize(&input)
                                .unwrap_or_else(|_| JsonValue::Object(Default::default()));
                            Ok((endpoint, method, message, payload))
                        })();
                        match deserialized_args {
                            Ok((endpoint, method, message, payload)) => Ok((
                                endpoint,
                                url,
                                GrpcRequest {
                                    service_name,
                                    method_name,
                                    method,
                                    payload,
                                    metadata,
                                    message,
                                },
                            )),
                            Err(message) => Err((
                                format_grpc_error_message(
                                    message,
                                    &service_name,
                                    &method_name,
                                    input,
                                ),
                                Some(ERROR_TYPE_NETWORK_ERROR),
                            )),
                        }
                    }) {
                    Ok((endpoint, url, request)) => {
                        let async_actions = state.subscribe_grpc_operation(
                            effect.id(),
                            url,
                            endpoint,
                            request,
                            self.max_operations_per_connection,
                            &self.metric_names,
                            context,
                        );
                        let initial_value =
                            create_pending_expression(&self.factory, &self.allocator);
                        ((state_token, initial_value), Some(async_actions))
                    }
                    Err((message, error_type)) => (
                        (
                            state_token,
                            create_error_message_expression(
                                message,
                                error_type,
                                &self.factory,
                                &self.allocator,
                            ),
                        ),
                        None,
                    ),
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
                        effect_type: EFFECT_TYPE_GRPC.into(),
                        updates: initial_values,
                    }],
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().filter_map(|actions| actions).flatten()),
        ))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + Send
            + 'static
            + From<GrpcHandlerRequestStopAction>
            + From<GrpcHandlerConnectionTerminateAction>
            + GrpcHandlerConnectionTaskActorAction,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_GRPC {
            return None;
        }
        let unsubscribe_actions = effects.iter().flat_map(|effect| {
            let effect_id = effect.id();
            let actions = state.unsubscribe_grpc_operation(effect_id, &self.metric_names);
            actions.into_iter().flatten()
        });
        Some(SchedulerTransition::new(unsubscribe_actions))
    }
    fn handle_grpc_handler_connect_success<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &GrpcHandlerConnectSuccessAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + Send + 'static + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GrpcHandlerConnectSuccessAction {
            connection_id,
            url: _,
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let connection_state = state.active_connections.get_mut(&connection_id)?;
        if connection_state.connection_attempt > 0 {
            connection_state.connection_attempt = 0;
            Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_GRPC.into(),
                        updates: connection_state
                            .effects
                            .values()
                            .copied()
                            .map(|effect_id| {
                                (
                                    effect_id,
                                    create_pending_expression(&self.factory, &self.allocator),
                                )
                            })
                            .collect(),
                    }],
                }
                .into(),
            ))))
        } else {
            None
        }
    }
    fn handle_grpc_handler_connect_error<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &GrpcHandlerConnectErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + Send
            + 'static
            + From<EffectEmitAction<T>>
            + From<GrpcHandlerRequestStartAction>
            + From<GrpcHandlerRequestStopAction>
            + GrpcHandlerConnectionTaskActorAction,
        TTask: TaskFactory<TAction, TTask> + From<GrpcHandlerConnectionTaskFactory>,
    {
        let GrpcHandlerConnectErrorAction {
            connection_id,
            url,
            message,
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let error = create_error_message_expression(
            format!("{}: {}", url, message),
            Some(ERROR_TYPE_NETWORK_ERROR),
            &self.factory,
            &self.allocator,
        );
        state
            .handle_connection_error(
                connection_id,
                error,
                &self.reconnect_timeout,
                self.main_pid,
                context,
            )
            .map(SchedulerTransition::new)
    }
    fn handle_grpc_handler_success_response<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &GrpcHandlerSuccessResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + Send + 'static + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GrpcHandlerSuccessResponseAction {
            connection_id,
            operation_id,
            data,
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let operation_id = GrpcOperationId(*operation_id);
        let connection_state = state.active_connections.get(&connection_id)?;
        let effect_id = connection_state.effects.get(&operation_id).copied()?;
        let operation_state = connection_state.operations.get(&effect_id)?;
        let request = &operation_state.request;
        let message_type = request.method.descriptor.output();
        let value = DynamicMessage::decode(message_type, &mut data.clone())
            .map_err(|err| format!("{}", err))
            .and_then(|message| {
                self.transcoder
                    .deserialize_message(&message, &self.transcoder, &self.factory, &self.allocator)
                    .map_err(|err| format!("{}", err))
            })
            .unwrap_or_else(|message| {
                create_grpc_operation_error_message_expression(
                    message,
                    Some(ERROR_TYPE_NETWORK_ERROR),
                    request,
                    &self.factory,
                    &self.allocator,
                )
            });
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_GRPC.into(),
                    updates: vec![(effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_grpc_handler_error_response<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &GrpcHandlerErrorResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + Send + 'static + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GrpcHandlerErrorResponseAction {
            connection_id,
            operation_id,
            status,
            ..
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let operation_id = GrpcOperationId(*operation_id);
        let status = Status::from(status.clone());
        let connection_state = state.active_connections.get(&connection_id)?;
        let effect_id = connection_state.effects.get(&operation_id).copied()?;
        let operation_state = connection_state.operations.get(&effect_id)?;
        let request = &operation_state.request;
        let value = create_grpc_operation_error_message_expression(
            format!("{}", status),
            Some(ERROR_TYPE_NETWORK_ERROR),
            request,
            &self.factory,
            &self.allocator,
        );
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_GRPC.into(),
                    updates: vec![(effect_id, value)],
                }],
            }
            .into(),
        ))))
    }
    fn handle_grpc_handler_transport_error<TAction, TTask>(
        &self,
        state: &mut GrpcHandlerState,
        action: &GrpcHandlerTransportErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + Send
            + 'static
            + From<EffectEmitAction<T>>
            + From<GrpcHandlerRequestStartAction>
            + From<GrpcHandlerRequestStopAction>
            + GrpcHandlerConnectionTaskActorAction,
        TTask: TaskFactory<TAction, TTask> + From<GrpcHandlerConnectionTaskFactory>,
    {
        let GrpcHandlerTransportErrorAction {
            connection_id,
            operation_id,
            status,
            ..
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let operation_id = GrpcOperationId(*operation_id);
        let status = Status::from(status.clone());
        let connection_state = state.active_connections.get(&connection_id)?;
        let effect_id = connection_state.effects.get(&operation_id).copied()?;
        let operation_state = connection_state.operations.get(&effect_id)?;
        let request = &operation_state.request;
        let error = create_grpc_operation_error_message_expression(
            status,
            Some(ERROR_TYPE_NETWORK_ERROR),
            request,
            &self.factory,
            &self.allocator,
        );
        state
            .handle_connection_error(
                connection_id,
                error,
                &self.reconnect_timeout,
                self.main_pid,
                context,
            )
            .map(SchedulerTransition::new)
    }
}
impl<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect, TAction, TTask>
    TaskFactory<TAction, TTask>
    for GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TConfig: GrpcConfig + 'static,
    TReconnect: ReconnectTimeout + Send,
    TAction:
        Action + GrpcHandlerActorAction<T> + GrpcHandlerConnectionTaskActorAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + GrpcHandlerTask<T, TFactory, TAllocator, TTranscoder>,
{
    type Actor = Self;
    fn create(self) -> Self::Actor {
        self
    }
}

fn create_grpc_connect_task(
    connection_id: GrpcConnectionId,
    endpoint: Endpoint,
    delay: Option<Duration>,
    context: &mut impl HandlerContext,
) -> (ProcessId, GrpcHandlerConnectionTaskFactory) {
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = GrpcHandlerConnectionTaskFactory {
        connection_id: connection_id.as_uuid(),
        endpoint,
        delay,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

fn parse_grpc_endpoint(
    config: &impl GrpcConfig,
    url: &GrpcServiceUrl,
) -> Result<tonic::transport::Endpoint, String> {
    match tonic::transport::Endpoint::from_str(url.as_str()) {
        Err(err) => Err(format!("Invalid gRPC endpoint URL: {}", err)),
        Ok(endpoint) => Ok(config.configure(endpoint)),
    }
}

fn get_grpc_method_path(method: &GrpcMethod) -> String {
    format!(
        "/{}/{}",
        method.descriptor.parent_service().full_name(),
        method.descriptor.name()
    )
}

fn create_grpc_operation_error_message_expression<T: Expression>(
    message: impl std::fmt::Display,
    error_type: Option<&'static str>,
    request: &GrpcRequest,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_message_expression(
        format_grpc_error_message(
            message,
            &request.service_name,
            &request.method_name,
            &request.payload,
        ),
        error_type,
        factory,
        allocator,
    )
}

struct GrpcEffectArgs<T: Expression> {
    proto_id: ProtoId,
    url: GrpcServiceUrl,
    service_name: GrpcServiceName,
    method_name: GrpcMethodName,
    input: T,
    metadata: GrpcMetadata,
}

fn parse_grpc_effect_args<T: AsyncExpression>(
    effect: &T::Signal,
    factory: &impl ExpressionFactory<T>,
) -> Result<GrpcEffectArgs<T>, String> {
    let payload = effect.payload();
    let payload = payload.as_deref();
    let args = factory
        .match_list_term(payload)
        .filter(|args| args.items().as_deref().len() == 6)
        .ok_or_else(|| {
            format!(
                "Invalid grpc signal: Expected 6 arguments, received {}",
                payload
            )
        })?;
    let args = args.items();
    let mut args = args.as_deref().iter().map(|item| item.as_deref().clone());
    let proto_id = args.next().unwrap();
    let url = args.next().unwrap();
    let service = args.next().unwrap();
    let method = args.next().unwrap();
    let input = args.next().unwrap();
    let metadata = args.next().unwrap();
    let proto_id = parse_symbol_arg(&proto_id, factory);
    let url = parse_string_arg(&url, factory);
    let service = parse_string_arg(&service, factory);
    let method = parse_string_arg(&method, factory);
    let metadata = parse_optional_object_arg(&metadata, factory)?;
    match (proto_id, url, service, method, input, metadata) {
        (
            Some(protocol),
            Some(url),
            Some(service_name),
            Some(method_name),
            input,
            Some(metadata),
        ) => Ok(GrpcEffectArgs {
            proto_id: ProtoId::from(protocol),
            url: GrpcServiceUrl(url),
            service_name: GrpcServiceName(service_name),
            method_name: GrpcMethodName(method_name),
            input,
            metadata: metadata
                .into_iter()
                .flatten()
                .filter_map(|(key, value)| match value {
                    JsonValue::String(value) => Some((key, value)),
                    _ => None,
                })
                .collect(),
        }),
        _ => Err(format!("Invalid grpc signal arguments: {}", payload)),
    }
}

fn parse_symbol_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<SymbolId> {
    match factory.match_symbol_term(value) {
        Some(term) => Some(term.id()),
        _ => None,
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

fn format_grpc_error_message(
    message: impl std::fmt::Display,
    service: &GrpcServiceName,
    method_name: impl std::fmt::Display,
    input: impl std::fmt::Display,
) -> String {
    format!("{}.{}({}): {}", service, method_name, input, message)
}

fn create_error_message_expression<T: Expression>(
    message: impl Into<String>,
    error_type: Option<&'static str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(
        create_error_response(message, error_type, factory, allocator),
        factory,
        allocator,
    )
}

fn create_error_response<T: Expression>(
    message: impl Into<String>,
    error_type: Option<&'static str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_record(
        [
            Some((
                factory.create_string_term(allocator.create_static_string("message")),
                factory.create_string_term(allocator.create_string(message.into())),
            )),
            error_type.map(|error_type| {
                (
                    factory.create_string_term(allocator.create_static_string("name")),
                    factory.create_string_term(allocator.create_static_string(error_type)),
                )
            }),
        ]
        .into_iter()
        .filter_map(|field| field),
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
