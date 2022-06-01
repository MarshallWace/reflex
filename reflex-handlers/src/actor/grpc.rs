// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    pin::Pin,
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
        Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal, SignalType,
        StateToken, StringValue,
    },
    lang::{create_struct, ValueTerm},
    stdlib::Stdlib,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, StateUpdate,
};
use reflex_utils::{partition_results, reconnect::ReconnectTimeout};
use tokio::time::sleep;
use tonic::Status;
use uuid::Uuid;

use crate::action::grpc::{GrpcHandlerConnectErrorAction, GrpcHandlerConnectSuccessAction};

pub use schema::GrpcServiceId;

pub use ::tonic;

pub mod imports;
pub mod loader;
pub mod schema;

pub const EFFECT_TYPE_GRPC: &'static str = "reflex::grpc";

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

pub trait GrpcConfig {
    fn configure(&self, endpoint: tonic::transport::Endpoint) -> tonic::transport::Endpoint;
}

#[derive(Clone, Copy, Debug)]
pub struct DefaultGrpcConfig;
impl GrpcConfig for DefaultGrpcConfig {
    fn configure(&self, endpoint: tonic::transport::Endpoint) -> tonic::transport::Endpoint {
        endpoint
            .keep_alive_while_idle(true)
            .tcp_keepalive(Some(Duration::from_secs(30)))
            .http2_keep_alive_interval(Duration::from_secs(30))
            .keep_alive_timeout(Duration::from_secs(20))
            .concurrency_limit(100)
    }
}

pub trait GrpcHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + InboundAction<GrpcHandlerConnectSuccessAction>
    + InboundAction<GrpcHandlerConnectErrorAction>
    + OutboundAction<GrpcHandlerConnectSuccessAction>
    + OutboundAction<GrpcHandlerConnectErrorAction>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> GrpcHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + InboundAction<GrpcHandlerConnectSuccessAction>
        + InboundAction<GrpcHandlerConnectErrorAction>
        + OutboundAction<GrpcHandlerConnectSuccessAction>
        + OutboundAction<GrpcHandlerConnectErrorAction>
        + OutboundAction<EffectEmitAction<T>>
{
}

#[derive(Clone)]
pub struct GrpcHandler<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
    TConfig: GrpcConfig + 'static,
    TReconnect: ReconnectTimeout + Send,
{
    services: HashMap<GrpcServiceId, TService>,
    factory: TFactory,
    allocator: TAllocator,
    reconnect_timeout: TReconnect,
    config: TConfig,
    metric_names: GrpcHandlerMetricNames,
    _expression: PhantomData<T>,
    _client: PhantomData<TClient>,
}
impl<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
    GrpcHandler<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
    TConfig: GrpcConfig,
    TReconnect: ReconnectTimeout + Send,
{
    pub fn new(
        services: impl IntoIterator<Item = TService>,
        factory: TFactory,
        allocator: TAllocator,
        reconnect_timeout: TReconnect,
        config: TConfig,
        metric_names: GrpcHandlerMetricNames,
    ) -> Self {
        Self {
            services: services
                .into_iter()
                .map(|service| (service.id(), service))
                .collect(),
            factory,
            allocator,
            reconnect_timeout,
            config,
            metric_names: metric_names.init(),
            _expression: Default::default(),
            _client: Default::default(),
        }
    }
}

pub struct GrpcHandlerState<T, TService, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
{
    active_requests: HashMap<StateToken, GrpcRequestState>,
    active_connections: HashMap<GrpcConnectionId, GrpcConnectionState<T, TService, TClient>>,
    active_connection_mappings: HashMap<(GrpcServiceId, GrpcServiceUrl), GrpcConnectionId>,
}
impl<T, TService, TClient> Default for GrpcHandlerState<T, TService, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
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

struct GrpcConnectionState<T, TService, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
{
    protocol: GrpcServiceId,
    service: TService,
    url: GrpcServiceUrl,
    operations: HashMap<StateToken, GrpcOperationState>,
    connection: GrpcConnection<T, TClient>,
}

enum GrpcOperationState {
    Queued,
    Active(ProcessId),
}

enum GrpcConnection<T, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TClient: GrpcClient + 'static,
{
    Pending(ProcessId, PendingGrpcConnection<T, TClient>),
    Connected(TClient),
    Error(String),
}
struct PendingGrpcConnection<T, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TClient: GrpcClient + 'static,
{
    client: Arc<Mutex<Option<TClient>>>,
    pending_operations: Vec<PendingGrpcOperation<T>>,
    connection_attempt: usize,
}
struct PendingGrpcOperation<T: AsyncExpression> {
    effect_id: StateToken,
    request: GrpcRequest<T>,
    accumulate: Option<T>,
}
impl<T, TClient> GrpcConnection<T, TClient>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TClient: GrpcClient + 'static,
{
    fn dispose(self) -> Option<ProcessId> {
        match self {
            Self::Pending(task_id, ..) => Some(task_id),
            Self::Connected(client) => {
                std::mem::drop(client);
                None
            }
            Self::Error(..) => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct GrpcServiceUrl(String);
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

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct GrpcRequest<T: AsyncExpression> {
    pub method: GrpcMethodName,
    pub message: T,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct GrpcMethodName(String);
impl GrpcMethodName {
    pub fn as_str(&self) -> &str {
        let Self(value) = self;
        value.as_str()
    }
    pub fn into_string(self) -> String {
        let Self(value) = self;
        value
    }
}
impl std::fmt::Display for GrpcMethodName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

pub fn create_grpc_exports<'a, T: Expression, TService, TClient>(
    services: impl IntoIterator<Item = &'a TService>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Vec<(GrpcServiceId, T)>
where
    TService: GrpcService<TClient> + 'a,
    TClient: GrpcClient + 'a,
    T::Builtin: From<Stdlib>,
{
    services
        .into_iter()
        .map(|service| {
            (
                service.id(),
                create_default_module_export(
                    service.factory(factory, allocator),
                    factory,
                    allocator,
                ),
            )
        })
        .collect::<Vec<_>>()
}

fn create_default_module_export<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_struct(once((String::from("default"), value)), factory, allocator)
}

pub trait GrpcService<TClient: GrpcClient>: Send + Clone {
    fn id(&self) -> GrpcServiceId;
    fn factory<T: Expression>(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> T
    where
        T::Builtin: From<Stdlib>;
    fn connect(
        &self,
        url: tonic::transport::Endpoint,
    ) -> Pin<Box<dyn Future<Output = Result<TClient, String>> + Send>>;
}

pub trait GrpcClient: Send + Clone {
    fn execute<T: AsyncExpression>(
        self,
        request: GrpcRequest<T>,
        factory: &impl AsyncExpressionFactory<T>,
        allocator: &impl AsyncHeapAllocator<T>,
    ) -> Result<GrpcResponse<T>, String>;
}

pub struct GrpcResponse<T: AsyncExpression>(
    Pin<Box<dyn Stream<Item = Result<T, String>> + Send + 'static>>,
);
impl<T: AsyncExpression> GrpcResponse<T> {
    pub fn new(stream: Pin<Box<dyn Stream<Item = Result<T, String>> + Send + 'static>>) -> Self {
        Self(stream)
    }
    pub fn unary<V>(
        result: impl Future<Output = Result<tonic::Response<V>, tonic::Status>> + Send + 'static,
        parse: impl Fn(V) -> Result<T, String> + Send + 'static,
    ) -> Self {
        Self::new(Box::pin(result.into_stream().map({
            move |result| match result {
                Ok(result) => parse(result.into_inner()),
                Err(error) => Err(format_grpc_error_message(error)),
            }
        })))
    }
    pub fn stream<V: 'static>(
        result: impl Future<Output = Result<tonic::Response<tonic::codec::Streaming<V>>, tonic::Status>>
            + Send
            + 'static,
        parse: impl Fn(V) -> Result<T, String> + Send + 'static,
    ) -> Self {
        Self::new(Box::pin({
            {
                async move {
                    match result.await {
                        Err(error) => {
                            stream::iter(once(Err(format_grpc_error_message(error)))).left_stream()
                        }
                        Ok(stream) => stream
                            .into_inner()
                            .map(move |result| match result {
                                Ok(result) => parse(result),
                                Err(error) => Err(format_grpc_error_message(error)),
                            })
                            .right_stream(),
                    }
                }
            }
            .into_stream()
            .flatten()
        }))
    }
    pub fn into_stream(self) -> impl Stream<Item = Result<T, String>> + 'static {
        self.0
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

impl<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect, TAction> Actor<TAction>
    for GrpcHandler<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
    TConfig: GrpcConfig,
    TReconnect: ReconnectTimeout + Send,
    TAction: GrpcHandlerAction<T> + Send + 'static,
{
    type State = GrpcHandlerState<T, TService, TClient>;
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
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
    GrpcHandler<T, TFactory, TAllocator, TService, TClient, TConfig, TReconnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
    TConfig: GrpcConfig,
    TReconnect: ReconnectTimeout + Send,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut GrpcHandlerState<T, TService, TClient>,
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
            + OutboundAction<GrpcHandlerConnectErrorAction>,
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
                    .map_err(|message| {
                        create_error_message_expression(message, &self.factory, &self.allocator)
                    })
                    .and_then(|args| {
                        let url = args.url;
                        let request = GrpcRequest {
                            method: args.method,
                            message: args.message,
                        };
                        self.subscribe_grpc_operation(
                            state,
                            effect,
                            args.protocol,
                            url.clone(),
                            request.clone(),
                            args.accumulate,
                            context,
                        )
                        .map_err(|message| {
                            create_grpc_error_message_expression(
                                &url,
                                &request,
                                &message,
                                &self.factory,
                                &self.allocator,
                            )
                        })
                    }) {
                    Ok((connect_action, subscribe_action)) => (
                        (
                            state_token,
                            StateUpdate::Value(create_pending_expression(
                                &self.factory,
                                &self.allocator,
                            )),
                        ),
                        (connect_action, subscribe_action),
                    ),
                    Err(err) => ((state_token, StateUpdate::Value(err)), (None, None)),
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
        state: &mut GrpcHandlerState<T, TService, TClient>,
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
        let actions = effects
            .iter()
            .filter_map(|effect| {
                if state.active_requests.contains_key(&effect.id()) {
                    let (unsubscribe_action, disconnect_action) =
                        self.unsubscribe_grpc_operation(state, effect)?;
                    Some((unsubscribe_action, disconnect_action))
                } else {
                    None
                }
            })
            .flat_map(|(unsubscribe, disconnect)| unsubscribe.into_iter().chain(disconnect));
        Some(StateTransition::new(actions))
    }
    fn handle_grpc_handler_connect_success<TAction>(
        &self,
        state: &mut GrpcHandlerState<T, TService, TClient>,
        action: &GrpcHandlerConnectSuccessAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let GrpcHandlerConnectSuccessAction {
            connection_id,
            url: _,
        } = action;
        let connection_id = *connection_id;
        let connection_state = state
            .active_connections
            .get_mut(&GrpcConnectionId(connection_id))?;
        let (client, pending_operations) = match &mut connection_state.connection {
            GrpcConnection::Connected(..) => None,
            GrpcConnection::Pending(_, connection) => {
                connection.client.lock().unwrap().take().map(|client| {
                    (
                        client,
                        std::mem::replace(&mut connection.pending_operations, Vec::new()),
                    )
                })
            }
            GrpcConnection::Error(..) => None,
        }?;
        let (operation_tasks, operation_errors): (Vec<_>, Vec<_>) =
            partition_results(pending_operations.into_iter().filter_map({
                |operation| {
                    let effect_id = operation.effect_id;
                    match listen_grpc_operation(
                        &connection_state.url,
                        client.clone(),
                        operation,
                        &self.factory,
                        &self.allocator,
                        context,
                    ) {
                        Err(update) => Some(Err(update)),
                        Ok((operation_task, task_pid)) => connection_state
                            .operations
                            .get_mut(&effect_id)
                            .map(|operation_state| {
                                *operation_state = GrpcOperationState::Active(task_pid);
                                Ok(operation_task)
                            }),
                    }
                }
            }));
        connection_state.connection = GrpcConnection::Connected(client);
        let error_action = if operation_errors.is_empty() {
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
            error_action.into_iter().chain(operation_tasks),
        ))
    }
    fn handle_grpc_handler_connect_error<TAction>(
        &self,
        state: &mut GrpcHandlerState<T, TService, TClient>,
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
        let (connection_pid, connection) = match &mut connection_state.connection {
            GrpcConnection::Connected(..) | GrpcConnection::Error(..) => None,
            GrpcConnection::Pending(connection_pid, connection) => {
                Some((connection_pid, connection))
            }
        }?;
        let reconnect_timeout = self
            .reconnect_timeout
            .duration(connection.connection_attempt)?;
        connection.connection_attempt += 1;
        let reconnect_task = create_grpc_connect_task(
            connection_id,
            connection_state.service.clone(),
            &self.config,
            connection_state.url.clone(),
            connection.client.clone(),
            context,
            if reconnect_timeout.is_zero() {
                None
            } else {
                Some(reconnect_timeout)
            },
        );
        match reconnect_task {
            Ok((task_pid, task)) => {
                *connection_pid = task_pid;
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
                                    &self.factory,
                                    &self.allocator,
                                );
                                move |effect_id| (effect_id, StateUpdate::Value(error.clone()))
                            })
                            .collect(),
                    }
                    .into(),
                ))))
            }
        }
    }
    fn subscribe_grpc_operation<TAction>(
        &self,
        state: &mut GrpcHandlerState<T, TService, TClient>,
        effect: &Signal<T>,
        protocol: GrpcServiceId,
        url: GrpcServiceUrl,
        request: GrpcRequest<T>,
        accumulate: Option<T>,
        context: &mut impl HandlerContext,
    ) -> Result<
        (
            Option<StateOperation<TAction>>,
            Option<StateOperation<TAction>>,
        ),
        String,
    >
    where
        TAction: Action
            + Send
            + 'static
            + OutboundAction<GrpcHandlerConnectSuccessAction>
            + OutboundAction<GrpcHandlerConnectErrorAction>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let service = self
            .services
            .get(&protocol)
            .cloned()
            .ok_or_else(|| format!("Unrecognized gRPC service identifier: {}", protocol))?;
        let connection_id = match state
            .active_connection_mappings
            .entry((protocol.clone(), url.clone()))
        {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(GrpcConnectionId(Uuid::new_v4())),
        };
        let metric_labels = [
            ("url", String::from(url.as_str())),
            ("method", String::from(request.method.as_str())),
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
            effect.id(),
            GrpcRequestState {
                connection_id,
                metric_labels,
            },
        );
        let (connection_state, connect_task) = match state.active_connections.entry(connection_id) {
            Entry::Occupied(entry) => (entry.into_mut(), None),
            Entry::Vacant(entry) => {
                let client = Arc::new(Mutex::new(None));
                let connection = create_grpc_connect_task(
                    connection_id,
                    service.clone(),
                    &self.config,
                    url.clone(),
                    client.clone(),
                    context,
                    None,
                );
                let (connection, connect_action) = match connection {
                    Ok((task_pid, connect_task)) => (
                        GrpcConnection::Pending(
                            task_pid,
                            PendingGrpcConnection {
                                client,
                                pending_operations: Vec::new(),
                                connection_attempt: 0,
                            },
                        ),
                        connect_task,
                    ),
                    Err(message) => {
                        let error = create_error_message_expression(
                            message.clone(),
                            &self.factory,
                            &self.allocator,
                        );
                        let error_task = StateOperation::Send(
                            context.pid(),
                            EffectEmitAction {
                                updates: vec![(effect.id(), StateUpdate::Value(error))],
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
                    service,
                    protocol,
                    url,
                    operations: Default::default(),
                    connection,
                });
                (connection_state, Some(connect_action))
            }
        };
        let operation = PendingGrpcOperation {
            effect_id: effect.id(),
            request,
            accumulate,
        };
        let operation_state = match connection_state.operations.entry(effect.id()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(GrpcOperationState::Queued),
        };
        let subscribe_task = {
            match &mut connection_state.connection {
                GrpcConnection::Pending(_, connection) => {
                    connection.pending_operations.push(operation);
                    None
                }
                GrpcConnection::Connected(client) => Some(
                    match listen_grpc_operation(
                        &connection_state.url,
                        client.clone(),
                        operation,
                        &self.factory,
                        &self.allocator,
                        context,
                    ) {
                        Err(update) => StateOperation::Send(
                            context.pid(),
                            EffectEmitAction {
                                updates: vec![update],
                            }
                            .into(),
                        ),
                        Ok((operation_task, task_pid)) => {
                            *operation_state = GrpcOperationState::Active(task_pid);
                            operation_task
                        }
                    },
                ),
                GrpcConnection::Error(_) => None,
            }
        };
        Ok((connect_task, subscribe_task))
    }
    fn unsubscribe_grpc_operation<TAction>(
        &self,
        state: &mut GrpcHandlerState<T, TService, TClient>,
        effect: &Signal<T>,
    ) -> Option<(
        Option<StateOperation<TAction>>,
        Option<StateOperation<TAction>>,
    )>
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
            let task_pid = match operation_state {
                GrpcOperationState::Queued => None,
                GrpcOperationState::Active(task_id) => Some(task_id),
            };
            let unsubscribe_action = task_pid.map(StateOperation::Kill);
            (unsubscribe_action, is_final_subscription)
        };
        let disconnect_action = if is_final_subscription {
            state
                .active_connections
                .remove(&connection_id)
                .and_then(|connection_state| {
                    let GrpcConnectionState {
                        protocol,
                        url,
                        connection,
                        ..
                    } = connection_state;
                    let metric_labels = [("url", String::from(url.as_str()))];
                    decrement_gauge!(
                        self.metric_names.grpc_effect_connection_count,
                        1.0,
                        &metric_labels
                    );
                    state.active_connection_mappings.remove(&(protocol, url));
                    let connection_pid = connection.dispose();
                    connection_pid.map(StateOperation::Kill)
                })
        } else {
            None
        };
        Some((unsubscribe_action, disconnect_action))
    }
}

fn listen_grpc_operation<T, TFactory, TAllocator, TClient, TAction>(
    url: &GrpcServiceUrl,
    client: TClient,
    operation: PendingGrpcOperation<T>,
    factory: &TFactory,
    allocator: &TAllocator,
    context: &mut impl HandlerContext,
) -> Result<(StateOperation<TAction>, ProcessId), (StateToken, StateUpdate<T>)>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TClient: GrpcClient + 'static,
    TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
{
    let PendingGrpcOperation {
        effect_id,
        request,
        accumulate,
    } = operation;
    match client.clone().execute(request.clone(), factory, allocator) {
        Ok(operation) => {
            let current_pid = context.pid();
            let task_pid = context.generate_pid();
            Ok((
                StateOperation::Task(
                    task_pid,
                    OperationStream::new({
                        let results = operation.into_stream().map({
                            let factory = factory.clone();
                            let allocator = allocator.clone();
                            let url = url.clone();
                            move |result| match result {
                                Ok(result) => result,
                                Err(message) => create_grpc_error_message_expression(
                                    &url, &request, &message, &factory, &allocator,
                                ),
                            }
                        });
                        // FIXME: Deprecate gRPC handler accumulate option in favour of scan handler
                        let results = if let Some(accumulator) = accumulate {
                            results
                                .scan(factory.create_value_term(ValueTerm::Null), {
                                    let factory = factory.clone();
                                    let allocator = allocator.clone();
                                    // TODO: Garbage-collect cache used for gRPC allocator iteratee
                                    let mut cache = Mutex::new(SubstitutionCache::new());
                                    move |state, value| {
                                        let result = match factory.match_signal_term(&value) {
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
                                                        cache.get_mut().unwrap(),
                                                    )
                                                    .unwrap_or(result);
                                                *state = result.clone();
                                                result
                                            }
                                        };
                                        future::ready(Some(result))
                                    }
                                })
                                .left_stream()
                        } else {
                            results.right_stream()
                        };
                        results.map({
                            move |result| {
                                StateOperation::Send(
                                    current_pid,
                                    EffectEmitAction {
                                        updates: vec![(effect_id, StateUpdate::Value(result))],
                                    }
                                    .into(),
                                )
                            }
                        })
                    }),
                ),
                task_pid,
            ))
        }
        Err(message) => Err((
            effect_id,
            StateUpdate::Value(create_grpc_error_message_expression(
                &url, &request, &message, factory, allocator,
            )),
        )),
    }
}

fn create_grpc_connect_task<TService, TClient, TConfig, TAction>(
    connection_id: GrpcConnectionId,
    service: TService,
    config: &TConfig,
    url: GrpcServiceUrl,
    result: Arc<Mutex<Option<TClient>>>,
    context: &mut impl HandlerContext,
    delay: Option<Duration>,
) -> Result<(ProcessId, StateOperation<TAction>), String>
where
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
    TConfig: GrpcConfig + 'static,
    TAction: Action
        + Send
        + 'static
        + OutboundAction<GrpcHandlerConnectSuccessAction>
        + OutboundAction<GrpcHandlerConnectErrorAction>,
{
    let current_pid = context.pid();
    let task_pid = context.generate_pid();
    let connection = create_grpc_connection(service, config, &url)?;
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
                        let connection = connection.await.and_then(|socket| match result.lock() {
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

fn create_grpc_connection<TService, TClient>(
    service: TService,
    config: &impl GrpcConfig,
    url: &GrpcServiceUrl,
) -> Result<impl Future<Output = Result<TClient, String>>, String>
where
    TService: GrpcService<TClient> + 'static,
    TClient: GrpcClient + 'static,
{
    match tonic::transport::Endpoint::from_str(url.as_str()) {
        Err(err) => Err(format!("Invalid gRPC endpoint URL: {}", err)),
        Ok(endpoint) => {
            let endpoint = config.configure(endpoint);
            Ok(service.connect(endpoint))
        }
    }
}

fn format_grpc_error_message(error: Status) -> String {
    format!("gRPC error: {}", error)
}

struct GrpcEffectArgs<T: AsyncExpression> {
    protocol: GrpcServiceId,
    url: GrpcServiceUrl,
    method: GrpcMethodName,
    message: T,
    accumulate: Option<T>,
}

fn parse_grpc_effect_args<T: AsyncExpression>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<GrpcEffectArgs<T>, String> {
    let mut args = effect.args().into_iter();
    if args.len() < 5 {
        return Err(format!(
            "Invalid grpc signal: Expected 5 or more arguments, received {}",
            args.len()
        ));
    }
    let protocol = parse_integer_arg(args.next().unwrap(), factory);
    let url = parse_string_arg(args.next().unwrap(), factory);
    let method = parse_string_arg(args.next().unwrap(), factory);
    let message = args.next().unwrap().clone();
    let accumulate = args
        .next()
        .cloned()
        .filter(|value| match_null_expression(value, factory).is_none());
    match (protocol, url, method, message, accumulate) {
        (Some(protocol), Some(url), Some(method), message, accumulate) => Ok(GrpcEffectArgs {
            protocol: GrpcServiceId(protocol),
            url: GrpcServiceUrl(url),
            method: GrpcMethodName(method),
            message,
            accumulate,
        }),
        _ => Err(format!(
            "Invalid grpc signal arguments: {}",
            effect
                .args()
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
        )),
    }
}

fn parse_integer_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<i32> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Int(value)) => Some(*value),
        _ => None,
    }
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_value_term(value) {
        Some(ValueTerm::String(value)) => Some(String::from(value.as_str())),
        _ => None,
    }
}

fn match_null_expression<'a, T: Expression>(
    value: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<&'a T> {
    Some(value).filter(|value| {
        factory
            .match_value_term(value)
            .map(|value| value.match_null().is_some())
            .unwrap_or(false)
    })
}

fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn create_grpc_error_message_expression<T: AsyncExpression>(
    url: &GrpcServiceUrl,
    request: &GrpcRequest<T>,
    message: &str,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_message_expression(
        format!(
            "{}: {}({}): {}",
            url,
            request.method,
            reflex_json::stringify(&request.message).unwrap_or_else(|_| String::from("...")),
            message
        ),
        factory,
        allocator,
    )
}

fn create_error_message_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(
        factory.create_value_term(ValueTerm::String(message.into())),
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
