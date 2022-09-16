// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    str::FromStr,
    sync::{Arc, Mutex},
    time::Duration,
};

use futures::{future, stream, Future, FutureExt, Stream, StreamExt};
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::{
    cache::SubstitutionCache,
    core::{
        create_record, ConditionType, Expression, ExpressionFactory, ExpressionListType,
        HeapAllocator, Reducible, RefType, Rewritable, SignalType, StateToken, StringTermType,
        StringValue, SymbolId, SymbolTermType,
    },
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_json::JsonValue;
use reflex_protobuf::{
    reflection::{DynamicMessage, MessageDescriptor, MethodDescriptor},
    Message, ProtoTranscoder,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_utils::{
    partition_results,
    reconnect::{FibonacciReconnectTimeout, ReconnectTimeout},
};
use tokio::time::sleep;
use tonic::{
    codec::{Codec, DecodeBuf, Decoder, EncodeBuf, Encoder},
    Code, Status,
};
use uuid::Uuid;

use crate::{
    action::{
        GrpcHandlerConnectErrorAction, GrpcHandlerConnectSuccessAction,
        GrpcHandlerTransportErrorAction,
    },
    utils::{
        get_transport_error, ignore_repeated_grpc_errors, GrpcMethod, GrpcMethodName,
        GrpcServiceLibrary, GrpcServiceName, ProtoId,
    },
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

pub trait GrpcHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + InboundAction<GrpcHandlerConnectSuccessAction>
    + InboundAction<GrpcHandlerConnectErrorAction>
    + InboundAction<GrpcHandlerTransportErrorAction>
    + OutboundAction<GrpcHandlerConnectSuccessAction>
    + OutboundAction<GrpcHandlerConnectErrorAction>
    + OutboundAction<GrpcHandlerTransportErrorAction>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> GrpcHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + InboundAction<GrpcHandlerConnectSuccessAction>
        + InboundAction<GrpcHandlerConnectErrorAction>
        + InboundAction<GrpcHandlerTransportErrorAction>
        + OutboundAction<GrpcHandlerConnectSuccessAction>
        + OutboundAction<GrpcHandlerConnectErrorAction>
        + OutboundAction<GrpcHandlerTransportErrorAction>
        + OutboundAction<EffectEmitAction<T>>
{
}

type TonicClient = tonic::client::Grpc<tonic::transport::Channel>;

#[derive(Clone, Debug)]
struct GrpcClient(TonicClient);
impl GrpcClient {
    fn execute(
        &self,
        method: &GrpcMethod,
        request: DynamicMessage,
    ) -> Result<impl Stream<Item = Result<DynamicMessage, Status>> + Unpin, Status> {
        let client = self.clone();
        match method.descriptor.is_server_streaming() {
            true => execute_streaming_grpc_request(client, method, request).map(|response| {
                response
                    .map(|result| match result {
                        Err(err) => stream::iter(once(Err(err))).left_stream(),
                        Ok(response) => {
                            let results = response.into_inner();
                            // It appears that when the underlying HTTP connection is broken, the tonic response stream
                            // will emit a deluge of errors (maybe one for every time the underlying stream is polled?),
                            // so we need to filter out duplicate errors to prevent them from overloading the event bus
                            let results = ignore_repeated_grpc_errors(
                                results,
                                FibonacciReconnectTimeout {
                                    units: Duration::from_secs(1),
                                    max_timeout: Duration::from_secs(30),
                                },
                            );
                            return take_while_inclusive(results, |result| match &result {
                                Err(status) if get_transport_error(status).is_some() => false,
                                _ => true,
                            })
                            .right_stream();
                        }
                    })
                    .into_stream()
                    .flatten()
                    .left_stream()
            }),
            false => execute_unary_grpc_request(client, method, request).map({
                |response| {
                    response
                        .map(|result| result.map(|response| response.into_inner()))
                        .into_stream()
                        .right_stream()
                }
            }),
        }
        .map(Box::pin)
    }
    fn into_inner(self) -> impl Future<Output = Result<TonicClient, Status>> {
        let Self(mut client) = self;
        async move {
            let _ = client.ready().await.map_err(|err| {
                Status::new(
                    Code::Unknown,
                    format!("Failed to initialize gRPC client: {}", err),
                )
            })?;
            Ok(client)
        }
    }
}

#[derive(Clone)]
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
            _expression: Default::default(),
        }
    }
}

pub struct GrpcHandlerState<T>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
{
    active_requests: HashMap<StateToken, GrpcRequestState>,
    active_connections: HashMap<GrpcConnectionId, GrpcConnectionState<T>>,
    active_connection_mappings: HashMap<(ProtoId, GrpcServiceUrl), Vec<GrpcConnectionId>>,
}
impl<T> Default for GrpcHandlerState<T>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
{
    fn default() -> Self {
        Self {
            active_requests: Default::default(),
            active_connections: Default::default(),
            active_connection_mappings: Default::default(),
        }
    }
}

struct GrpcRequestState {
    connection_id: GrpcConnectionId,
    metric_labels: [(&'static str, String); 2],
}

struct GrpcConnectionState<T>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
{
    protocol: ProtoId,
    url: GrpcServiceUrl,
    connection: GrpcConnection,
    operations: HashMap<StateToken, GrpcOperation<T>>,
}
enum GrpcConnection {
    Pending(PendingGrpcConnection),
    Connected(GrpcClient),
    Error(String),
}
struct PendingGrpcConnection {
    task_pid: ProcessId,
    result: Arc<Mutex<Option<GrpcClient>>>,
    connection_attempt: usize,
}
impl GrpcConnection {
    fn dispose(self) -> Option<ProcessId> {
        match self {
            Self::Pending(connection_state) => Some(connection_state.task_pid),
            Self::Connected(client) => {
                std::mem::drop(client);
                None
            }
            Self::Error(..) => None,
        }
    }
}
struct GrpcOperation<T>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
{
    request: GrpcRequest<T>,
    status: GrpcOperationStatus,
    accumulate: Option<T>,
}
enum GrpcOperationStatus {
    Queued,
    Active(ProcessId),
    Error,
}

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

#[derive(Clone, Debug)]
struct GrpcRequest<T: Expression> {
    method: GrpcMethod,
    input: T,
    message: DynamicMessage,
}
impl<T: Expression> GrpcRequest<T> {
    fn into_parts(self) -> (GrpcMethod, T, DynamicMessage) {
        let Self {
            method,
            input,
            message,
        } = self;
        (method, input, message)
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

impl<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect, TAction> Actor<TAction>
    for GrpcHandler<T, TFactory, TAllocator, TTranscoder, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TConfig: GrpcConfig,
    TReconnect: ReconnectTimeout + Send,
    TAction: GrpcHandlerAction<T> + Send + 'static,
{
    type State = GrpcHandlerState<T>;
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
            self.handle_effect_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_grpc_handler_connect_success(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_grpc_handler_connect_error(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_grpc_handler_transport_error(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
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
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<EffectEmitAction<T>>
            + OutboundAction<GrpcHandlerConnectSuccessAction>
            + OutboundAction<GrpcHandlerConnectErrorAction>
            + OutboundAction<GrpcHandlerTransportErrorAction>,
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
                    .and_then(|args| {
                        let proto_id = args.proto_id;
                        let url = args.url;
                        let service_name = args.service;
                        let method_name = args.method;
                        let input = args.message;
                        let method = self
                            .services
                            .get(&proto_id, &service_name, &method_name)
                            .cloned()
                            .ok_or_else(|| {
                                format_grpc_error_message(
                                    "Unrecognized gRPC method",
                                    &url,
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
                            .map_err(|err| {
                                format_grpc_error_message(
                                    err,
                                    &url,
                                    method.descriptor.full_name(),
                                    &input,
                                )
                            })?;
                        let request = GrpcRequest {
                            method,
                            input,
                            message,
                        };
                        let actions = self.subscribe_grpc_operation(
                            state,
                            proto_id,
                            url,
                            effect.id(),
                            request,
                            args.accumulate,
                            context,
                        );
                        Ok(actions)
                    })
                    .map_err(|message| {
                        create_error_message_expression(
                            message,
                            Some(ERROR_TYPE_NETWORK_ERROR),
                            &self.factory,
                            &self.allocator,
                        )
                    }) {
                    Ok(actions) => (
                        (
                            state_token,
                            create_pending_expression(&self.factory, &self.allocator),
                        ),
                        Some(actions),
                    ),
                    Err(err) => ((state_token, err), None),
                }
            })
            .unzip();
        let initial_values_action = if initial_values.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                EffectEmitAction {
                    updates: initial_values,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().filter_map(|actions| actions).flatten()),
        ))
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_GRPC {
            return None;
        }
        let actions = effects.iter().flat_map(|effect| {
            if state.active_requests.contains_key(&effect.id()) {
                self.unsubscribe_grpc_operation(state, effect)
            } else {
                None
            }
            .into_iter()
            .flatten()
        });
        Some(StateTransition::new(actions))
    }
    fn handle_grpc_handler_connect_success<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        action: &GrpcHandlerConnectSuccessAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<EffectEmitAction<T>>
            + OutboundAction<GrpcHandlerTransportErrorAction>,
    {
        let GrpcHandlerConnectSuccessAction {
            connection_id,
            url: _,
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let connection_state = state.active_connections.get_mut(&connection_id)?;
        let client = match &mut connection_state.connection {
            GrpcConnection::Connected(..) => None,
            GrpcConnection::Pending(pending_connection) => {
                pending_connection.result.lock().unwrap().take()
            }
            GrpcConnection::Error(..) => None,
        }?;
        let pending_operations =
            connection_state
                .operations
                .iter_mut()
                .filter_map(|(effect_id, operation)| match operation.status {
                    GrpcOperationStatus::Queued | GrpcOperationStatus::Error => {
                        Some((*effect_id, operation))
                    }
                    GrpcOperationStatus::Active(_) => None,
                });
        let (operation_tasks, operation_errors): (Vec<_>, Vec<_>) =
            partition_results(pending_operations.map({
                |(effect_id, operation_state)| match listen_grpc_operation(
                    connection_id,
                    &connection_state.url,
                    client.clone(),
                    effect_id,
                    operation_state.request.clone(),
                    operation_state.accumulate.clone(),
                    &self.transcoder,
                    &self.factory,
                    &self.allocator,
                    context,
                ) {
                    Err(state_update) => {
                        operation_state.status = GrpcOperationStatus::Error;
                        Err(state_update)
                    }
                    Ok((operation_task, task_pid)) => {
                        operation_state.status = GrpcOperationStatus::Active(task_pid);
                        Ok(operation_task)
                    }
                }
            }));
        connection_state.connection = GrpcConnection::Connected(client);
        let error_actions = if operation_errors.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                EffectEmitAction {
                    updates: operation_errors,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(
            error_actions.into_iter().chain(operation_tasks),
        ))
    }
    fn handle_grpc_handler_connect_error<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        action: &GrpcHandlerConnectErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GrpcHandlerConnectSuccessAction>
            + OutboundAction<GrpcHandlerConnectErrorAction>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let GrpcHandlerConnectErrorAction {
            connection_id,
            url: _,
            error: _,
        } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let connection_state = state.active_connections.get_mut(&connection_id)?;
        let pending_connection = match &mut connection_state.connection {
            GrpcConnection::Connected(..) | GrpcConnection::Error(..) => None,
            GrpcConnection::Pending(pending_connection) => Some(pending_connection),
        }?;
        let reconnect_timeout = self
            .reconnect_timeout
            .duration(pending_connection.connection_attempt)?;
        pending_connection.connection_attempt += 1;
        let reconnect_task = create_grpc_connect_task(
            connection_id,
            &self.config,
            connection_state.url.clone(),
            pending_connection.result.clone(),
            context,
            if reconnect_timeout.is_zero() {
                None
            } else {
                Some(reconnect_timeout)
            },
        );
        match reconnect_task {
            Ok((task_pid, task)) => {
                pending_connection.task_pid = task_pid;
                Some(StateTransition::new(once(task)))
            }
            Err(message) => {
                connection_state.connection = GrpcConnection::Error(message.clone());
                Some(StateTransition::new(once(StateOperation::Send(
                    context.pid(),
                    EffectEmitAction {
                        updates: connection_state
                            .operations
                            .keys()
                            .cloned()
                            .map({
                                let error = create_error_message_expression(
                                    message,
                                    Some(ERROR_TYPE_NETWORK_ERROR),
                                    &self.factory,
                                    &self.allocator,
                                );
                                move |effect_id| (effect_id, error.clone())
                            })
                            .collect(),
                    }
                    .into(),
                ))))
            }
        }
    }
    fn handle_grpc_handler_transport_error<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        action: &GrpcHandlerTransportErrorAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GrpcHandlerConnectSuccessAction>
            + OutboundAction<GrpcHandlerConnectErrorAction>
            + OutboundAction<GrpcHandlerTransportErrorAction>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let GrpcHandlerTransportErrorAction { connection_id, .. } = action;
        let connection_id = GrpcConnectionId(*connection_id);
        let (disconnect_action, (proto_id, url, operations)) =
            self.unsubscribe_grpc_connection(state, &connection_id)?;
        let unsubscribe_operation_actions = operations
            .iter()
            .filter_map(|(effect_id, operation)| {
                let GrpcRequestState {
                    connection_id: _,
                    metric_labels,
                } = state.active_requests.remove(effect_id)?;
                decrement_gauge!(
                    self.metric_names.grpc_effect_active_request_count,
                    1.0,
                    &metric_labels
                );
                match operation.status {
                    GrpcOperationStatus::Queued | GrpcOperationStatus::Error => None,
                    GrpcOperationStatus::Active(task_id) => Some(StateOperation::Kill(task_id)),
                }
            })
            .collect::<Vec<_>>();
        let subscribe_operation_actions =
            operations.into_iter().flat_map(|(effect_id, operation)| {
                self.subscribe_grpc_operation(
                    state,
                    proto_id,
                    url.clone(),
                    effect_id,
                    operation.request,
                    operation.accumulate,
                    context,
                )
            });
        Some(StateTransition::new(
            disconnect_action
                .into_iter()
                .chain(unsubscribe_operation_actions)
                .chain(subscribe_operation_actions),
        ))
    }
    fn subscribe_grpc_operation<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        proto_id: ProtoId,
        url: GrpcServiceUrl,
        effect_id: StateToken,
        request: GrpcRequest<T>,
        accumulate: Option<T>,
        context: &mut impl HandlerContext,
    ) -> impl Iterator<Item = StateOperation<TAction>> + '_
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GrpcHandlerConnectSuccessAction>
            + OutboundAction<GrpcHandlerConnectErrorAction>
            + OutboundAction<GrpcHandlerTransportErrorAction>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let connection_id = match state
            .active_connection_mappings
            .entry((proto_id.clone(), url.clone()))
        {
            Entry::Occupied(mut entry) => {
                let connection_ids = entry.get_mut();
                let existing_connection_id = connection_ids
                    .iter()
                    .find(|connection_id| {
                        if let Some(max_operations) = self.max_operations_per_connection {
                            state
                                .active_connections
                                .get(connection_id)
                                .map(|connection_state| {
                                    connection_state.operations.len() < max_operations
                                })
                                .unwrap_or(false)
                        } else {
                            true
                        }
                    })
                    .copied();
                match existing_connection_id {
                    Some(connection_id) => connection_id,
                    None => {
                        let connection_id = GrpcConnectionId(Uuid::new_v4());
                        connection_ids.push(connection_id);
                        connection_id
                    }
                }
            }
            Entry::Vacant(entry) => {
                let connection_id = GrpcConnectionId(Uuid::new_v4());
                entry.insert(vec![connection_id]);
                connection_id
            }
        };
        let metric_labels = [
            ("url", String::from(url.as_str())),
            (
                "method",
                String::from(request.method.descriptor.full_name()),
            ),
        ];
        increment_counter!(
            self.metric_names.grpc_effect_total_request_count,
            &metric_labels
        );
        increment_gauge!(
            self.metric_names.grpc_effect_active_request_count,
            1.0,
            &metric_labels
        );
        state.active_requests.insert(
            effect_id,
            GrpcRequestState {
                connection_id,
                metric_labels,
            },
        );
        let (connection_state, connect_action) = match state.active_connections.entry(connection_id)
        {
            Entry::Occupied(entry) => (entry.into_mut(), None),
            Entry::Vacant(entry) => {
                let client = Arc::new(Mutex::new(None));
                let connection = create_grpc_connect_task(
                    connection_id,
                    &self.config,
                    url.clone(),
                    client.clone(),
                    context,
                    None,
                );
                let (connection, connect_action) = match connection {
                    Ok((task_pid, connect_task)) => (
                        GrpcConnection::Pending(PendingGrpcConnection {
                            task_pid,
                            result: client,
                            connection_attempt: 0,
                        }),
                        connect_task,
                    ),
                    Err(message) => {
                        let error = create_error_message_expression(
                            message.clone(),
                            Some(ERROR_TYPE_NETWORK_ERROR),
                            &self.factory,
                            &self.allocator,
                        );
                        let error_task = StateOperation::Send(
                            context.pid(),
                            EffectEmitAction {
                                updates: vec![(effect_id, error)],
                            }
                            .into(),
                        );
                        (GrpcConnection::Error(message), error_task)
                    }
                };
                let metric_labels = [("url", String::from(url.as_str()))];
                increment_gauge!(
                    self.metric_names.grpc_effect_connection_count,
                    1.0,
                    &metric_labels
                );
                let connection_state = entry.insert(GrpcConnectionState {
                    protocol: proto_id,
                    url,
                    connection,
                    operations: Default::default(),
                });
                (connection_state, Some(connect_action))
            }
        };
        let operation_state = match connection_state.operations.entry(effect_id) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(GrpcOperation {
                status: GrpcOperationStatus::Queued,
                request,
                accumulate,
            }),
        };
        let subscribe_action = {
            let (status, action) = match &mut connection_state.connection {
                GrpcConnection::Pending(_) => (GrpcOperationStatus::Queued, None),
                GrpcConnection::Connected(client) => {
                    let (status, task) = match listen_grpc_operation(
                        connection_id,
                        &connection_state.url,
                        client.clone(),
                        effect_id,
                        operation_state.request.clone(),
                        operation_state.accumulate.clone(),
                        &self.transcoder,
                        &self.factory,
                        &self.allocator,
                        context,
                    ) {
                        Err(state_update) => {
                            let emit_action = StateOperation::Send(
                                context.pid(),
                                EffectEmitAction {
                                    updates: vec![state_update],
                                }
                                .into(),
                            );
                            (GrpcOperationStatus::Error, Some(emit_action))
                        }
                        Ok((execute_action, task_pid)) => {
                            (GrpcOperationStatus::Active(task_pid), Some(execute_action))
                        }
                    };
                    (status, task)
                }
                GrpcConnection::Error(_) => (GrpcOperationStatus::Error, None),
            };
            operation_state.status = status;
            action
        };
        connect_action.into_iter().chain(subscribe_action)
    }
    fn unsubscribe_grpc_operation<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        effect: &T::Signal<T>,
    ) -> Option<impl Iterator<Item = StateOperation<TAction>> + '_>
    where
        TAction: Action + 'static,
    {
        let GrpcRequestState {
            connection_id,
            metric_labels,
        } = state.active_requests.remove(&effect.id())?;
        decrement_gauge!(
            self.metric_names.grpc_effect_active_request_count,
            1.0,
            &metric_labels
        );
        let (unsubscribe_action, is_final_subscription) = {
            let connection_state = state.active_connections.get_mut(&connection_id)?;
            let operation_state = connection_state.operations.remove(&effect.id())?;
            let is_final_subscription = connection_state.operations.is_empty();
            let task_pid = match operation_state.status {
                GrpcOperationStatus::Queued | GrpcOperationStatus::Error => None,
                GrpcOperationStatus::Active(task_id) => Some(task_id),
            };
            let unsubscribe_action = task_pid.map(StateOperation::Kill);
            (unsubscribe_action, is_final_subscription)
        };
        let disconnect_action = if is_final_subscription {
            self.unsubscribe_grpc_connection(state, &connection_id)
                .and_then(|(disconnect_action, _disposed_connection)| disconnect_action)
        } else {
            None
        };
        Some(unsubscribe_action.into_iter().chain(disconnect_action))
    }
    fn unsubscribe_grpc_connection<TAction>(
        &self,
        state: &mut GrpcHandlerState<T>,
        connection_id: &GrpcConnectionId,
    ) -> Option<(
        Option<StateOperation<TAction>>,
        (
            ProtoId,
            GrpcServiceUrl,
            HashMap<StateToken, GrpcOperation<T>>,
        ),
    )>
    where
        TAction: Action + 'static,
    {
        let connection_state = state.active_connections.remove(&connection_id)?;
        let GrpcConnectionState {
            protocol,
            url,
            connection,
            operations,
        } = connection_state;
        let metric_labels = [("url", String::from(url.as_str()))];
        let mut connections_entry = match state
            .active_connection_mappings
            .entry((protocol, url.clone()))
        {
            Entry::Vacant(_) => None,
            Entry::Occupied(entry) => Some(entry),
        }?;
        decrement_gauge!(
            self.metric_names.grpc_effect_connection_count,
            1.0,
            &metric_labels
        );
        let num_remaining_connections_for_service = {
            let connection_ids = connections_entry.get_mut();
            if let Some(index) = connection_ids
                .iter()
                .position(|existing_connection_id| existing_connection_id == connection_id)
            {
                connection_ids.remove(index);
            }
            connection_ids.len()
        };
        if num_remaining_connections_for_service == 0 {
            connections_entry.remove();
        }
        let connect_task_pid = connection.dispose();
        Some((
            connect_task_pid.map(StateOperation::Kill),
            (protocol, url, operations),
        ))
    }
}

fn create_grpc_connect_task<TConfig, TAction>(
    connection_id: GrpcConnectionId,
    config: &TConfig,
    url: GrpcServiceUrl,
    result: Arc<Mutex<Option<GrpcClient>>>,
    context: &mut impl HandlerContext,
    delay: Option<Duration>,
) -> Result<(ProcessId, StateOperation<TAction>), String>
where
    TConfig: GrpcConfig + 'static,
    TAction: Action
        + Send
        + 'static
        + OutboundAction<GrpcHandlerConnectSuccessAction>
        + OutboundAction<GrpcHandlerConnectErrorAction>,
{
    let current_pid = context.pid();
    let task_pid = context.generate_pid();
    let connection = create_grpc_connection(config, &url)?;
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
                        let connection = connection.await.and_then(|client| match result.lock() {
                            Ok(mut result) => {
                                result.replace(client);
                                Ok(())
                            }
                            Err(err) => Err(format!("{}", err)),
                        });
                        [
                            StateOperation::Kill(task_pid),
                            StateOperation::Send(
                                current_pid,
                                match connection {
                                    Ok(_) => GrpcHandlerConnectSuccessAction {
                                        connection_id: connection_id.as_uuid(),
                                        url: url.into_string(),
                                    }
                                    .into(),
                                    Err(message) => GrpcHandlerConnectErrorAction {
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
    Ok((task_pid, task))
}

fn listen_grpc_operation<T, TFactory, TAllocator, TTranscoder, TAction>(
    connection_id: GrpcConnectionId,
    url: &GrpcServiceUrl,
    client: GrpcClient,
    effect_id: StateToken,
    request: GrpcRequest<T>,
    accumulate: Option<T>,
    transcoder: &TTranscoder,
    factory: &TFactory,
    allocator: &TAllocator,
    context: &mut impl HandlerContext,
) -> Result<(StateOperation<TAction>, ProcessId), (StateToken, T)>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TTranscoder: ProtoTranscoder + Clone + Send + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: Action
        + Send
        + 'static
        + OutboundAction<EffectEmitAction<T>>
        + OutboundAction<GrpcHandlerTransportErrorAction>,
{
    let (method, input, message) = request.into_parts();
    match client.execute(&method, message) {
        Err(status) => {
            let error = create_grpc_operation_error_message_expression(
                &status,
                Some(ERROR_TYPE_NETWORK_ERROR),
                &url,
                &method,
                &input,
                factory,
                allocator,
            );
            Err((effect_id, error))
        }
        Ok(operation) => {
            let current_pid = context.pid();
            let task_pid = context.generate_pid();
            Ok((
                StateOperation::Task(
                    task_pid,
                    OperationStream::new({
                        let results = operation.map({
                            let transcoder = transcoder.clone();
                            let factory = factory.clone();
                            let allocator = allocator.clone();
                            let url = url.clone();
                            let method = method.clone();
                            let input = input.clone();
                            move |result| match result {
                                Ok(message) => match transcoder.deserialize_message(
                                    &message,
                                    &transcoder,
                                    &factory,
                                    &allocator,
                                ) {
                                    Ok(payload) => Ok(payload),
                                    Err(err) => Ok(create_grpc_operation_error_message_expression(
                                        &err,
                                        Some(ERROR_TYPE_SYNTAX_ERROR),
                                        &url,
                                        &method,
                                        &input,
                                        &factory,
                                        &allocator,
                                    )),
                                },
                                Err(status) => {
                                    let is_transport_error = get_transport_error(&status).is_some();
                                    let error = create_grpc_operation_error_message_expression(
                                        &status,
                                        if is_transport_error {
                                            Some(ERROR_TYPE_NETWORK_ERROR)
                                        } else {
                                            None
                                        },
                                        &url,
                                        &method,
                                        &input,
                                        &factory,
                                        &allocator,
                                    );
                                    if is_transport_error {
                                        Err((status, error))
                                    } else {
                                        Ok(error)
                                    }
                                }
                            }
                        });
                        // FIXME: Deprecate gRPC handler accumulate option in favour of scan handler
                        let results = if let Some(accumulator) = accumulate {
                            results
                                .scan(factory.create_nil_term(), {
                                    let factory = factory.clone();
                                    let allocator = allocator.clone();
                                    move |state, value| {
                                        let result = value.map(|value| {
                                            match factory.match_signal_term(&value) {
                                                Some(_effect) => value,
                                                None => {
                                                    let result = factory.create_application_term(
                                                        accumulator.clone(),
                                                        allocator.create_pair(state.clone(), value),
                                                    );
                                                    let result = result
                                                        .reduce(
                                                            &factory,
                                                            &allocator,
                                                            &mut SubstitutionCache::new(),
                                                        )
                                                        .unwrap_or(result);
                                                    *state = result.clone();
                                                    result
                                                }
                                            }
                                        });
                                        future::ready(Some(result))
                                    }
                                })
                                .left_stream()
                        } else {
                            results.right_stream()
                        };
                        results.flat_map({
                            let url = url.clone();
                            let method = method.clone();
                            move |result| {
                                let (emit_action, error_action) = match result {
                                    Ok(value) => (
                                        EffectEmitAction {
                                            updates: vec![(effect_id, value)],
                                        }
                                        .into(),
                                        None,
                                    ),
                                    Err((status, error)) => (
                                        EffectEmitAction {
                                            updates: vec![(effect_id, error)],
                                        }
                                        .into(),
                                        Some(
                                            GrpcHandlerTransportErrorAction {
                                                connection_id: connection_id.as_uuid(),
                                                url: String::from(url.as_str()),
                                                method: String::from(method.descriptor.full_name()),
                                                input: reflex_json::sanitize(&input)
                                                    .ok()
                                                    .unwrap_or(JsonValue::Object(
                                                        Default::default(),
                                                    )),
                                                error: format!("{}", status),
                                            }
                                            .into(),
                                        ),
                                    ),
                                };
                                stream::iter(
                                    once(emit_action).chain(error_action).map(move |action| {
                                        StateOperation::Send(current_pid, action)
                                    }),
                                )
                            }
                        })
                    }),
                ),
                task_pid,
            ))
        }
    }
}

fn create_grpc_connection(
    config: &impl GrpcConfig,
    url: &GrpcServiceUrl,
) -> Result<impl Future<Output = Result<GrpcClient, String>>, String> {
    match tonic::transport::Endpoint::from_str(url.as_str()) {
        Err(err) => Err(format!("Invalid gRPC endpoint URL: {}", err)),
        Ok(endpoint) => {
            let endpoint = config.configure(endpoint);
            let connection = async move { endpoint.connect().await };
            Ok(connection.map(|channel| match channel {
                Err(err) => Err(format!("gRPC connection error: {}", err)),
                Ok(channel) => Ok(GrpcClient(tonic::client::Grpc::new(channel))),
            }))
        }
    }
}

fn execute_unary_grpc_request(
    client: GrpcClient,
    method: &GrpcMethod,
    request: impl tonic::IntoRequest<DynamicMessage>,
) -> Result<impl Future<Output = Result<tonic::Response<DynamicMessage>, Status>>, Status> {
    let path = get_grpc_method_path(method)?;
    Ok(client
        .into_inner()
        .map({
            let descriptor = method.descriptor.clone();
            move |result| match result {
                Err(err) => future::ready(Err(err)).left_future(),
                Ok(mut client) => {
                    let codec = DynamicMessageCodec::new(descriptor);
                    async move { client.unary(request.into_request(), path, codec).await }
                }
                .right_future(),
            }
        })
        .flatten())
}

fn execute_streaming_grpc_request(
    client: GrpcClient,
    method: &GrpcMethod,
    request: impl tonic::IntoRequest<DynamicMessage>,
) -> Result<
    impl Future<Output = Result<tonic::Response<tonic::codec::Streaming<DynamicMessage>>, Status>>,
    Status,
> {
    let path = get_grpc_method_path(method)?;
    Ok(client
        .into_inner()
        .map({
            let descriptor = method.descriptor.clone();
            move |result| match result {
                Err(err) => future::ready(Err(err)).left_future(),
                Ok(mut client) => {
                    let codec = DynamicMessageCodec::new(descriptor);
                    async move {
                        client
                            .server_streaming(request.into_request(), path, codec)
                            .await
                    }
                }
                .right_future(),
            }
        })
        .flatten())
}

#[derive(Debug, Clone)]
struct DynamicMessageCodec {
    descriptor: MethodDescriptor,
}
impl DynamicMessageCodec {
    fn new(descriptor: MethodDescriptor) -> Self {
        Self { descriptor }
    }
}
impl Codec for DynamicMessageCodec {
    type Encode = DynamicMessage;
    type Decode = DynamicMessage;
    type Encoder = DynamicMessageEncoder;
    type Decoder = DynamicMessageDecoder;
    fn encoder(&mut self) -> Self::Encoder {
        DynamicMessageEncoder
    }
    fn decoder(&mut self) -> Self::Decoder {
        DynamicMessageDecoder(self.descriptor.output())
    }
}
#[derive(Debug, Clone)]
struct DynamicMessageEncoder;
impl Encoder for DynamicMessageEncoder {
    type Item = DynamicMessage;
    type Error = Status;
    fn encode(&mut self, item: Self::Item, buf: &mut EncodeBuf<'_>) -> Result<(), Self::Error> {
        item.encode(buf)
            .map_err(|err| Status::new(Code::Unknown, format!("{}", err)))
    }
}
#[derive(Debug, Clone)]
struct DynamicMessageDecoder(MessageDescriptor);
impl Decoder for DynamicMessageDecoder {
    type Item = DynamicMessage;
    type Error = Status;
    fn decode(&mut self, buf: &mut DecodeBuf<'_>) -> Result<Option<Self::Item>, Self::Error> {
        let Self(descriptor) = &self;
        let descriptor = descriptor.clone();
        let item = DynamicMessage::decode(descriptor, buf)
            .map(Option::Some)
            .map_err(|err| Status::new(Code::Internal, err.to_string()))?;
        Ok(item)
    }
}

fn get_grpc_method_path(
    method: &GrpcMethod,
) -> Result<tonic::codegen::http::uri::PathAndQuery, Status> {
    let method_path = format!(
        "/{}/{}",
        method.descriptor.parent_service().full_name(),
        method.descriptor.name()
    );
    tonic::codegen::http::uri::PathAndQuery::try_from(method_path.as_str()).map_err(|_| {
        Status::new(
            Code::Unknown,
            format!("Invalid gRPC method: {}", method_path),
        )
    })
}

fn create_grpc_operation_error_message_expression<T: AsyncExpression>(
    message: impl std::fmt::Display,
    error_type: Option<&'static str>,
    url: &GrpcServiceUrl,
    method: &GrpcMethod,
    input: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_message_expression(
        format_grpc_error_message(message, url, method.descriptor.full_name(), input),
        error_type,
        factory,
        allocator,
    )
}

struct GrpcEffectArgs<T: AsyncExpression> {
    proto_id: ProtoId,
    url: GrpcServiceUrl,
    service: GrpcServiceName,
    method: GrpcMethodName,
    message: T,
    accumulate: Option<T>,
}

fn parse_grpc_effect_args<T: AsyncExpression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<GrpcEffectArgs<T>, String> {
    let args = effect.args().as_deref();
    if args.len() != 7 {
        return Err(format!(
            "Invalid grpc signal: Expected 7 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.iter().map(|item| item.as_deref());
    let proto_id = parse_symbol_arg(args.next().unwrap(), factory);
    let url = parse_string_arg(args.next().unwrap(), factory);
    let service = parse_string_arg(args.next().unwrap(), factory);
    let method = parse_string_arg(args.next().unwrap(), factory);
    let message = args.next().unwrap().clone();
    let accumulate = args
        .next()
        .cloned()
        .filter(|value| match_null_expression(value, factory).is_none());
    let _token = args.next().unwrap();
    match (proto_id, url, service, method, message, accumulate) {
        (Some(protocol), Some(url), Some(service), Some(method), message, accumulate) => {
            Ok(GrpcEffectArgs {
                proto_id: ProtoId::from(protocol),
                url: GrpcServiceUrl(url),
                service: GrpcServiceName(service),
                method: GrpcMethodName(method),
                message,
                accumulate,
            })
        }
        _ => Err(format!(
            "Invalid grpc signal arguments: {}",
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
        Some(term) => Some(String::from(term.value().as_deref().as_str())),
        _ => None,
    }
}

fn match_null_expression<'a, T: Expression>(
    value: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<&'a T> {
    match factory.match_nil_term(value) {
        Some(_) => Some(value),
        _ => None,
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

fn format_grpc_error_message(
    message: impl std::fmt::Display,
    url: &GrpcServiceUrl,
    method_path: impl std::fmt::Display,
    input: impl std::fmt::Display,
) -> String {
    format!("{}: {}({}): {}", url, method_path, input, message)
}

fn create_error_message_expression<T: Expression>(
    message: impl Into<String>,
    error_type: Option<&'static str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(
        create_error_payload(message, error_type, factory, allocator),
        factory,
        allocator,
    )
}

fn create_error_payload<T: Expression>(
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
            allocator.create_signal(SignalType::Error, allocator.create_unit_list(payload))
        })),
    )
}

fn take_while_inclusive<T>(
    stream: impl Stream<Item = T>,
    predicate: impl Fn(&T) -> bool + 'static,
) -> impl Stream<Item = T> {
    stream
        .flat_map(move |result| {
            let tombstone = if predicate(&result) {
                None
            } else {
                Some(Err(()))
            };
            stream::iter(once(Ok(result)).chain(tombstone))
        })
        .take_while(|result| future::ready(result.is_ok()))
        .map(|result| result.unwrap())
}
