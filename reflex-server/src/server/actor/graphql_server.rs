// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::time::SystemTime;
use std::{iter::once, marker::PhantomData, time::Instant};

use metrics::{
    counter, decrement_gauge, describe_counter, describe_gauge, gauge, histogram,
    increment_counter, increment_gauge, Unit,
};
use opentelemetry::trace::StatusCode;
use opentelemetry::{
    trace::{Span, SpanId, TraceContextExt, TraceId, Tracer},
    Context, Key, KeyValue, Value,
};
use reflex::core::{
    ConditionListType, ConditionType, EvaluationResult, Expression, ExpressionFactory,
    HeapAllocator, RecordTermType, RefType, SignalTermType, SignalType, StringTermType,
    StringValue, Uuid,
};
use reflex::hash::{HashId, IntMap};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_graphql::{
    graphql_variables_are_equal, stdlib::Stdlib as GraphQlStdlib, GraphQlOperation,
};
use reflex_json::JsonValue;
use reflex_macros::{dispatcher, Named};
use reflex_runtime::action::evaluate::{
    EvaluateResultAction, EvaluateStartAction, EvaluateUpdateAction,
};
use reflex_runtime::action::query::{
    QueryEmitAction, QuerySubscribeAction, QueryUnsubscribeAction,
};
use reflex_runtime::actor::query_manager::create_query_evaluate_effect;
use reflex_stdlib::Stdlib;

use crate::server::action::graphql_server::{
    GraphQlServerEmitAction, GraphQlServerModifyAction, GraphQlServerParseErrorAction,
    GraphQlServerParseSuccessAction, GraphQlServerSubscribeAction, GraphQlServerUnsubscribeAction,
};

#[derive(Clone, Copy, Debug)]
pub struct GraphQlServerMetricNames {
    pub graphql_total_operation_count: &'static str,
    pub graphql_active_operation_count: &'static str,
    pub graphql_error_payload_count: &'static str,
    pub graphql_success_payload_count: &'static str,
    pub graphql_initial_response_duration: &'static str,
    pub graphql_active_query_success_count: &'static str,
    pub graphql_active_query_pending_count: &'static str,
    pub graphql_active_query_error_count: &'static str,
}
impl GraphQlServerMetricNames {
    fn init(self) -> Self {
        describe_counter!(
            self.graphql_total_operation_count,
            Unit::Count,
            "Total number of GraphQL operations"
        );
        describe_gauge!(
            self.graphql_active_operation_count,
            Unit::Count,
            "Active GraphQL operation count"
        );
        describe_counter!(
            self.graphql_error_payload_count,
            Unit::Count,
            "Total number of GraphQL error payloads emitted"
        );
        describe_counter!(
            self.graphql_success_payload_count,
            Unit::Count,
            "Total number of GraphQL success payloads emitted"
        );
        describe_gauge!(
            self.graphql_initial_response_duration,
            Unit::Milliseconds,
            "GraphQL initial response duration (ms)"
        );
        describe_gauge!(
            self.graphql_active_query_success_count,
            Unit::Count,
            "Number of active GraphQL queries successfully returning results"
        );
        describe_gauge!(
            self.graphql_active_query_error_count,
            Unit::Count,
            "Number of active GraphQL queries returning error results"
        );
        describe_gauge!(
            self.graphql_active_query_pending_count,
            Unit::Count,
            "Number of active GraphQL queries that have yet to return results"
        );
        self
    }
}
impl Default for GraphQlServerMetricNames {
    fn default() -> Self {
        Self {
            graphql_total_operation_count: "graphql_total_operation_count",
            graphql_active_operation_count: "graphql_active_operation_count",
            graphql_error_payload_count: "graphql_error_payload_count",
            graphql_success_payload_count: "graphql_success_payload_count",
            graphql_initial_response_duration: "graphql_initial_response_duration",
            graphql_active_query_success_count: "graphql_active_query_success_count",
            graphql_active_query_pending_count: "graphql_active_query_pending_count",
            graphql_active_query_error_count: "graphql_active_query_error_count",
        }
    }
}

#[derive(Default)]
struct QueryStatusMetrics {
    success: usize,
    error: usize,
    pending: usize,
}
impl QueryStatusMetrics {
    fn register_query(&mut self, status: GraphQlQueryStatus) {
        match status {
            GraphQlQueryStatus::Blocked | GraphQlQueryStatus::Pending => {
                self.pending += 1;
            }
            GraphQlQueryStatus::Success => {
                self.success += 1;
            }
            GraphQlQueryStatus::Error => {
                self.error += 1;
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum GraphQlOperationPhase {
    Queued,
    Busy,
    Blocked,
    Awaiting,
}
impl GraphQlOperationPhase {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Queued => "queued",
            Self::Busy => "busy",
            Self::Blocked => "blocked",
            Self::Awaiting => "awaiting",
        }
    }
}

pub trait GraphQlServerQueryLabel {
    fn label(&self, operation: &GraphQlOperation) -> String;
}
impl<_Self> GraphQlServerQueryLabel for _Self
where
    Self: Fn(&GraphQlOperation) -> String,
{
    fn label(&self, operation: &GraphQlOperation) -> String {
        (self)(operation)
    }
}

pub trait GraphQlServerOperationMetricLabels {
    fn labels(&self, operation: &GraphQlOperation) -> Vec<(String, String)>;
}
impl<_Self> GraphQlServerOperationMetricLabels for _Self
where
    Self: Fn(&GraphQlOperation) -> Vec<(String, String)>,
{
    fn labels(&self, operation: &GraphQlOperation) -> Vec<(String, String)> {
        (self)(operation)
    }
}

#[derive(Named, Clone)]
pub struct GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: GraphQlServerQueryLabel,
    TMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: GraphQlServerMetricNames,
    get_graphql_query_label: TQueryLabel,
    get_operation_metric_labels: TMetricLabels,
    tracer: TTracer,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
    GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: GraphQlServerQueryLabel,
    TMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: GraphQlServerMetricNames,
        get_graphql_query_label: TQueryLabel,
        get_operation_metric_labels: TMetricLabels,
        tracer: TTracer,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            get_graphql_query_label,
            get_operation_metric_labels,
            tracer,
            main_pid,
            _expression: Default::default(),
        }
    }
    fn start_transaction(
        &self,
        operation: &GraphQlOperation,
        attributes: &[(String, String)],
        operation_phase: GraphQlOperationPhase,
    ) -> GraphQlTransactionTrace<TTracer::Span> {
        let trace_label = operation
            .operation_name()
            .map(String::from)
            .unwrap_or_else(|| String::from("<unknown>"));
        let span_attributes = attributes
            .iter()
            .map(|(key, value)| KeyValue::new(Key::from(key.clone()), Value::from(value.clone())));
        let root = create_trace(trace_label, span_attributes, &self.tracer);
        let initial_span = create_span(
            get_span_label(operation_phase),
            get_span_attributes(operation_phase, []),
            &self.tracer,
            &root,
        );
        GraphQlTransactionTrace {
            root,
            active_span: Some(initial_span),
        }
    }
}

fn get_span_label(operation_phase: GraphQlOperationPhase) -> String {
    format!("worker:{}", operation_phase.as_str())
}

fn get_span_attributes(
    operation_phase: GraphQlOperationPhase,
    attributes: impl IntoIterator<Item = KeyValue>,
) -> impl IntoIterator<Item = KeyValue> {
    once(KeyValue::new(
        Key::from("phase"),
        Value::from(operation_phase.as_str()),
    ))
    .chain(attributes)
}

fn create_trace<TTracer: Tracer<Span = impl Span + Send + Sync + 'static>>(
    label: impl Into<Cow<'static, str>>,
    attributes: impl IntoIterator<Item = KeyValue>,
    tracer: &TTracer,
) -> Context {
    let context = Context::new();
    context.with_span(create_span(label, attributes, tracer, &context))
}

fn create_span<TTracer: Tracer<Span = impl Span + Send + Sync + 'static>>(
    label: impl Into<Cow<'static, str>>,
    attributes: impl IntoIterator<Item = KeyValue>,
    tracer: &TTracer,
    trace: &Context,
) -> TTracer::Span {
    tracer
        .span_builder(label)
        .with_attributes(attributes.into_iter().collect())
        .with_start_time(SystemTime::now())
        .start_with_context(tracer, trace)
}

pub struct GraphQlServerState<T: Expression, TSpan: Span> {
    // TODO: Use newtypes for state hashmap keys
    // Active operations, keyed by query hash
    operations: IntMap<HashId, GraphQlOperationState<T, TSpan>>,
    // Mapping from subscription IDs to active operation query hash
    subscription_operation_mappings: HashMap<Uuid, HashId>,
    // Mapping from query evaluate effect ID to active operation query hash
    evaluate_effect_mappings: IntMap<HashId, HashId>,
}
impl<T: Expression, TSpan: Span> Default for GraphQlServerState<T, TSpan> {
    fn default() -> Self {
        Self {
            operations: Default::default(),
            subscription_operation_mappings: Default::default(),
            evaluate_effect_mappings: Default::default(),
        }
    }
}
struct GraphQlOperationState<T: Expression, TSpan: Span> {
    query: T,
    label: String,
    evaluate_effect: T::Signal<T>,
    operation_phase: Option<GraphQlOperationPhase>,
    metric_labels: Vec<(String, String)>,
    start_time: Option<Instant>,
    // Latest emitted result
    result: Option<EvaluationResult<T>>,
    // Mapping from subscription IDs to active subcription state
    subscriptions: HashMap<Uuid, GraphQlSubscriptionState<TSpan>>,
}
struct GraphQlSubscriptionState<TSpan: Span> {
    operation: GraphQlOperation,
    trace: Option<GraphQlTransactionTrace<TSpan>>,
}

struct GraphQlTransactionTrace<TSpan: Span> {
    root: Context,
    active_span: Option<TSpan>,
}
impl<TSpan: Span + Send + Sync + 'static> GraphQlTransactionTrace<TSpan> {
    fn enter_span(
        &mut self,
        operation_phase: GraphQlOperationPhase,
        attributes: impl IntoIterator<Item = KeyValue>,
        tracer: &impl Tracer<Span = TSpan>,
    ) -> (TraceId, SpanId) {
        self.end_active_span(None);
        let span = create_span(
            get_span_label(operation_phase),
            get_span_attributes(operation_phase, attributes),
            tracer,
            &self.root,
        );
        let span_context = span.span_context();
        let trace_id = span_context.trace_id();
        let span_id = span_context.span_id();
        self.active_span = Some(span);
        (trace_id, span_id)
    }
    fn end_active_span(
        &mut self,
        end_event: Option<GraphQlTraceEvent>,
    ) -> Option<(TraceId, SpanId)> {
        let mut span = self.active_span.take()?;
        if let Some(GraphQlTraceEvent {
            event_type,
            attributes,
        }) = end_event
        {
            span.add_event(event_type.as_str(), attributes.clone());
            self.root.span().add_event(event_type.as_str(), attributes);
        }
        let span_context = span.span_context();
        let trace_id = span_context.trace_id();
        let span_id = span_context.span_id();
        span.end();
        Some((trace_id, span_id))
    }
    fn end(mut self, status: Result<(), String>) {
        self.end_active_span(None);
        let root_span = self.root.span();
        let (status_code, message) = match status {
            Ok(_) => (StatusCode::Ok, Default::default()),
            Err(err) => (StatusCode::Error, err),
        };
        root_span.set_status(status_code, message);
        drop(self)
    }
}

#[derive(PartialEq, Clone, Debug)]
struct GraphQlTraceEvent {
    event_type: GraphQlTraceEventType,
    attributes: Vec<KeyValue>,
}
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum GraphQlTraceEventType {
    Emit,
    Error,
    Abort,
}
impl GraphQlTraceEventType {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Emit => "graphql:emit",
            Self::Error => "graphql:error",
            Self::Abort => "graphql:abort",
        }
    }
}

dispatcher!({
    pub enum GraphQlServerAction<T: Expression> {
        Inbox(GraphQlServerSubscribeAction<T>),
        Inbox(GraphQlServerUnsubscribeAction<T>),
        Inbox(GraphQlServerModifyAction<T>),
        Inbox(QueryEmitAction<T>),
        Inbox(EvaluateStartAction<T>),
        Inbox(EvaluateResultAction<T>),
        Inbox(EvaluateUpdateAction<T>),

        Outbox(GraphQlServerParseSuccessAction<T>),
        Outbox(GraphQlServerEmitAction<T>),
        Outbox(QuerySubscribeAction<T>),
        Outbox(QueryUnsubscribeAction<T>),
        Outbox(GraphQlServerParseErrorAction<T>),
        Outbox(GraphQlServerUnsubscribeAction<T>),
    }

    impl<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer, TAction, TTask>
        Dispatcher<TAction, TTask>
        for GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
    where
        T: Expression,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TQueryLabel: GraphQlServerQueryLabel,
        TMetricLabels: GraphQlServerOperationMetricLabels,
        TTracer: Tracer,
        TTracer::Span: Send + Sync + 'static,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = GraphQlServerState<T, TTracer::Span>;
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

        fn accept(&self, _action: &GraphQlServerSubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerSubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerSubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_subscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerUnsubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerUnsubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerUnsubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_unsubscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerModifyAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerModifyAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerModifyAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_modify(state, action, metadata, context)
        }

        fn accept(&self, _action: &QueryEmitAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &QueryEmitAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &QueryEmitAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_query_emit(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateStartAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateStartAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateStartAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_start(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateResultAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateResultAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateResultAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_result(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateUpdateAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateUpdateAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateUpdateAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_update(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
    GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels, TTracer>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: GraphQlServerQueryLabel,
    TMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn handle_graphql_subscribe<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &GraphQlServerSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<QuerySubscribeAction<T>>
            + From<GraphQlServerParseSuccessAction<T>>
            + From<GraphQlServerParseErrorAction<T>>
            + From<GraphQlServerUnsubscribeAction<T>>
            + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            _expression: _,
        } = action;
        let subscription_id = *subscription_id;
        match reflex_graphql::parse_graphql_operation(operation, &self.factory, &self.allocator) {
            Err(err) => Some(SchedulerTransition::new([
                SchedulerCommand::Send(
                    self.main_pid,
                    GraphQlServerParseErrorAction {
                        subscription_id,
                        message: err,
                        operation: operation.clone(),
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                SchedulerCommand::Send(
                    self.main_pid,
                    GraphQlServerUnsubscribeAction {
                        subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
            ])),
            Ok(query) => match state.operations.entry(query.id()) {
                Entry::Occupied(mut entry) => {
                    let trace = {
                        let GraphQlOperationState {
                            operation_phase,
                            metric_labels,
                            ..
                        } = entry.get();
                        self.start_transaction(
                            operation,
                            metric_labels,
                            operation_phase
                                .as_ref()
                                .copied()
                                .unwrap_or(GraphQlOperationPhase::Queued),
                        )
                    };
                    let subscription_state = GraphQlSubscriptionState {
                        operation: operation.clone(),
                        trace: Some(trace),
                    };
                    entry
                        .get_mut()
                        .subscriptions
                        .insert(subscription_id, subscription_state);
                    state
                        .subscription_operation_mappings
                        .insert(subscription_id, query.id());
                    let GraphQlOperationState { query, result, .. } = entry.get();
                    let transition = SchedulerTransition::new(
                        once(SchedulerCommand::Send(
                            self.main_pid,
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: query.clone(),
                            }
                            .into(),
                        ))
                        .chain(result.as_ref().map(|result| {
                            SchedulerCommand::Send(
                                self.main_pid,
                                GraphQlServerEmitAction {
                                    subscription_id,
                                    result: result.result().clone(),
                                }
                                .into(),
                            )
                        })),
                    );
                    self.update_graphql_query_status_metrics(state, []);
                    Some(transition)
                }
                Entry::Vacant(entry) => {
                    let label = self.get_graphql_query_label.label(operation);
                    let metric_labels = self.get_operation_metric_labels.labels(operation);
                    let evaluate_effect = create_query_evaluate_effect(
                        label.clone(),
                        query.clone(),
                        &self.factory,
                        &self.allocator,
                    );
                    increment_counter!(
                        self.metric_names.graphql_total_operation_count,
                        &metric_labels
                    );
                    increment_gauge!(
                        self.metric_names.graphql_active_operation_count,
                        1.0,
                        &metric_labels
                    );
                    counter!(
                        self.metric_names.graphql_success_payload_count,
                        0,
                        &metric_labels
                    );
                    counter!(
                        self.metric_names.graphql_error_payload_count,
                        0,
                        &metric_labels
                    );
                    let operation_phase = GraphQlOperationPhase::Queued;
                    let trace = self.start_transaction(operation, &metric_labels, operation_phase);
                    let subscription_state = GraphQlSubscriptionState {
                        operation: operation.clone(),
                        trace: Some(trace),
                    };
                    state
                        .evaluate_effect_mappings
                        .insert(evaluate_effect.id(), query.id());
                    state
                        .subscription_operation_mappings
                        .insert(subscription_id, query.id());
                    entry.insert(GraphQlOperationState {
                        label: label.clone(),
                        query: query.clone(),
                        evaluate_effect,
                        operation_phase: Some(operation_phase),
                        metric_labels,
                        start_time: Some(Instant::now()),
                        result: None,
                        subscriptions: HashMap::from([(subscription_id, subscription_state)]),
                    });
                    let transition = SchedulerTransition::new([
                        SchedulerCommand::Send(
                            self.main_pid,
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: query.clone(),
                            }
                            .into(),
                        ),
                        SchedulerCommand::Send(
                            self.main_pid,
                            QuerySubscribeAction { query, label }.into(),
                        ),
                    ]);
                    self.update_graphql_query_status_metrics(state, []);
                    Some(transition)
                }
            },
        }
    }
    fn handle_graphql_unsubscribe<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &GraphQlServerUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerUnsubscribeAction {
            subscription_id,
            _expression,
        } = action;
        let operation_id = state
            .subscription_operation_mappings
            .remove(subscription_id)?;
        let mut operation_state_entry = match state.operations.entry(operation_id) {
            Entry::Occupied(entry) => Some(entry),
            Entry::Vacant(_) => None,
        }?;
        let removed_subscription_state = operation_state_entry
            .get_mut()
            .subscriptions
            .remove(subscription_id)?;
        let has_remaining_subscriptions = !operation_state_entry.get().subscriptions.is_empty();
        let mut removed_operation_state = if has_remaining_subscriptions {
            None
        } else {
            Some(operation_state_entry.remove())
        };
        if let Some(operation_state) = removed_operation_state.as_ref() {
            decrement_gauge!(
                self.metric_names.graphql_active_operation_count,
                1.0,
                &operation_state.metric_labels
            );
            state
                .evaluate_effect_mappings
                .remove(&operation_state.evaluate_effect.id());
        }
        let GraphQlSubscriptionState {
            operation: _,
            trace,
        } = removed_subscription_state;
        if let Some(mut trace) = trace {
            trace.end_active_span(Some(GraphQlTraceEvent {
                event_type: GraphQlTraceEventType::Abort,
                attributes: Default::default(),
            }));
            trace.end(Ok(()));
        }
        let removed_operation_labels = removed_operation_state
            .as_mut()
            .map(|operation_state| std::mem::take(&mut operation_state.metric_labels));
        let transition = if let Some(operation_state) = removed_operation_state {
            let GraphQlOperationState { query, label, .. } = operation_state;
            Some(SchedulerTransition::new(Some(SchedulerCommand::Send(
                self.main_pid,
                QueryUnsubscribeAction { query, label }.into(),
            ))))
        } else {
            None
        };
        if let Some(metric_labels) = removed_operation_labels {
            self.update_graphql_query_status_metrics(state, [metric_labels]);
        } else {
            self.update_graphql_query_status_metrics(state, []);
        }
        transition
    }
    fn handle_graphql_modify<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &GraphQlServerModifyAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<QuerySubscribeAction<T>>
            + From<GraphQlServerParseSuccessAction<T>>
            + From<GraphQlServerParseErrorAction<T>>
            + From<GraphQlServerUnsubscribeAction<T>>
            + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerModifyAction {
            subscription_id,
            variables,
            _expression: _,
        } = action;
        let operation_id = state.subscription_operation_mappings.get(subscription_id)?;
        let mut existing_operation_state_entry = match state.operations.entry(*operation_id) {
            Entry::Occupied(entry) => Some(entry),
            Entry::Vacant(_) => None,
        }?;
        let updated_operation = {
            existing_operation_state_entry
                .get()
                .subscriptions
                .get(subscription_id)
                .and_then(|subscription_state| {
                    let existing_operation = &subscription_state.operation;
                    if graphql_variables_are_equal(variables, existing_operation.variables()) {
                        return None;
                    }
                    Some(GraphQlOperation::new(
                        existing_operation.query().clone(),
                        existing_operation.operation_name().map(String::from),
                        variables.clone(),
                        existing_operation.extensions().clone(),
                    ))
                })
        }?;
        match reflex_graphql::parse_graphql_operation(
            &updated_operation,
            &self.factory,
            &self.allocator,
        ) {
            Err(err) => Some(SchedulerTransition::new([
                SchedulerCommand::Send(
                    self.main_pid,
                    GraphQlServerParseErrorAction {
                        subscription_id: *subscription_id,
                        message: err,
                        operation: updated_operation,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                SchedulerCommand::Send(
                    self.main_pid,
                    GraphQlServerUnsubscribeAction {
                        subscription_id: *subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
            ])),
            Ok(updated_query) => {
                let query_is_unaffected =
                    existing_operation_state_entry.get().query.id() == updated_query.id();
                if query_is_unaffected {
                    // If the query variables modification had no effect on the query that was generated (e.g. if the
                    // variables were not used anywhere in the query), update the subsription state but do nothing
                    let subscription_state = existing_operation_state_entry
                        .get_mut()
                        .subscriptions
                        .get_mut(subscription_id)?;
                    subscription_state.operation = updated_operation;
                    None
                } else {
                    // Otherwise this subscription now maps to a new query, so we need to relocate it within the hashmap
                    let previous_subscription_state = existing_operation_state_entry
                        .get_mut()
                        .subscriptions
                        .remove(subscription_id)?;
                    let has_remaining_subscriptions = !existing_operation_state_entry
                        .get()
                        .subscriptions
                        .is_empty();
                    let removed_operation_state = if !has_remaining_subscriptions {
                        Some(existing_operation_state_entry.remove())
                    } else {
                        None
                    };
                    if let Some(operation_state) = removed_operation_state.as_ref() {
                        decrement_gauge!(
                            self.metric_names.graphql_active_operation_count,
                            1.0,
                            &operation_state.metric_labels
                        );
                        state
                            .evaluate_effect_mappings
                            .remove(&operation_state.evaluate_effect.id());
                    }
                    let GraphQlSubscriptionState {
                        operation: _,
                        trace: existing_trace,
                    } = previous_subscription_state;
                    let subscription_id = *subscription_id;
                    state
                        .subscription_operation_mappings
                        .insert(subscription_id, updated_query.id());
                    let abort_event_attributes =
                        updated_operation.variables().iter().map(|(key, value)| {
                            KeyValue::new(format!("variables.{}", key), value.to_string())
                        });
                    let action = match state.operations.entry(updated_query.id()) {
                        Entry::Occupied(mut entry) => {
                            let operation_state = entry.get_mut();
                            let operation_phase = operation_state
                                .operation_phase
                                .as_ref()
                                .copied()
                                .unwrap_or(GraphQlOperationPhase::Queued);
                            let trace = match existing_trace {
                                Some(mut trace) => {
                                    trace.end_active_span(Some(GraphQlTraceEvent {
                                        event_type: GraphQlTraceEventType::Abort,
                                        attributes: abort_event_attributes.into_iter().collect(),
                                    }));
                                    trace.enter_span(operation_phase, [], &self.tracer);
                                    trace
                                }
                                None => self.start_transaction(
                                    &updated_operation,
                                    &operation_state.metric_labels,
                                    operation_phase,
                                ),
                            };
                            let subscription_state = GraphQlSubscriptionState {
                                operation: updated_operation,
                                trace: Some(trace),
                            };
                            operation_state
                                .subscriptions
                                .insert(subscription_id, subscription_state);
                            operation_state.result.as_ref().map(|result| {
                                SchedulerCommand::Send(
                                    self.main_pid,
                                    GraphQlServerEmitAction {
                                        subscription_id,
                                        result: result.result().clone(),
                                    }
                                    .into(),
                                )
                            })
                        }
                        Entry::Vacant(entry) => {
                            let updated_label =
                                self.get_graphql_query_label.label(&updated_operation);
                            let metric_labels =
                                self.get_operation_metric_labels.labels(&updated_operation);
                            let evaluate_effect = create_query_evaluate_effect(
                                updated_label.clone(),
                                updated_query.clone(),
                                &self.factory,
                                &self.allocator,
                            );
                            increment_gauge!(
                                self.metric_names.graphql_active_operation_count,
                                1.0,
                                &metric_labels
                            );
                            state
                                .evaluate_effect_mappings
                                .insert(evaluate_effect.id(), updated_query.id());
                            let operation_phase = GraphQlOperationPhase::Queued;
                            let trace = match existing_trace {
                                Some(mut trace) => {
                                    trace.end_active_span(Some(GraphQlTraceEvent {
                                        event_type: GraphQlTraceEventType::Abort,
                                        attributes: abort_event_attributes.into_iter().collect(),
                                    }));
                                    trace.enter_span(operation_phase, [], &self.tracer);
                                    trace
                                }
                                None => self.start_transaction(
                                    &updated_operation,
                                    &metric_labels,
                                    operation_phase,
                                ),
                            };
                            let subscription_state = GraphQlSubscriptionState {
                                operation: updated_operation,
                                trace: Some(trace),
                            };
                            entry.insert(GraphQlOperationState {
                                label: updated_label.clone(),
                                query: updated_query.clone(),
                                evaluate_effect,
                                operation_phase: Some(operation_phase),
                                metric_labels,
                                start_time: Some(Instant::now()),
                                result: None,
                                subscriptions: HashMap::from([(
                                    subscription_id,
                                    subscription_state,
                                )]),
                            });
                            Some(SchedulerCommand::Send(
                                self.main_pid,
                                QuerySubscribeAction {
                                    query: updated_query.clone(),
                                    label: updated_label,
                                }
                                .into(),
                            ))
                        }
                    };
                    if let Some(operation_state) = removed_operation_state {
                        let GraphQlOperationState { metric_labels, .. } = operation_state;
                        self.update_graphql_query_status_metrics(state, [metric_labels]);
                    } else {
                        self.update_graphql_query_status_metrics(state, []);
                    }
                    Some(SchedulerTransition::new(
                        once(SchedulerCommand::Send(
                            self.main_pid,
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: updated_query,
                            }
                            .into(),
                        ))
                        .chain(action),
                    ))
                }
            }
        }
    }
    fn handle_query_emit<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &QueryEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryEmitAction { query, result } = action;
        let operation_state = state.operations.get_mut(&query.id())?;
        let previous_result = operation_state.result.replace(result.clone());
        let is_unchanged = previous_result
            .as_ref()
            .map(|existing| existing.result().id() == result.result().id())
            .unwrap_or(false);
        if is_unchanged {
            return None;
        }
        let duration = operation_state
            .start_time
            .take()
            .map(|start_time| start_time.elapsed());
        if let Some(duration) = duration {
            histogram!(
                self.metric_names.graphql_initial_response_duration,
                duration.as_millis() as f64,
                &operation_state.metric_labels
            );
        }
        let error_result = match_error_expression(result.result(), &self.factory);
        let is_error_result = error_result.is_some();
        if is_error_result {
            increment_counter!(
                self.metric_names.graphql_error_payload_count,
                &operation_state.metric_labels
            );
        } else {
            increment_counter!(
                self.metric_names.graphql_success_payload_count,
                &operation_state.metric_labels
            );
        }
        let result_status = if let Some(err) = error_result {
            Err(String::from(
                parse_error_message(err, &self.factory, &self.allocator).unwrap_or("Error"),
            ))
        } else {
            Ok(())
        };
        for trace in operation_state
            .subscriptions
            .values_mut()
            .filter_map(|subscription_state| subscription_state.trace.take())
        {
            trace.end(result_status.clone());
        }
        let should_recalculate_metrics = {
            // We only need to recalculate subscription metrics if the query's status has changed as a result of this update
            let previous_value = previous_result.as_ref().map(|result| result.result());
            let updated_value = Some(result.result());
            let previous_query_state = GraphQlQueryStatus::get_state(previous_value, &self.factory);
            let updated_query_state = GraphQlQueryStatus::get_state(updated_value, &self.factory);
            updated_query_state != previous_query_state
        };
        let transition =
            SchedulerTransition::new(operation_state.subscriptions.keys().copied().map(
                |subscription_id| {
                    SchedulerCommand::Send(
                        self.main_pid,
                        GraphQlServerEmitAction {
                            subscription_id,
                            result: result.result().clone(),
                        }
                        .into(),
                    )
                },
            ));
        if should_recalculate_metrics {
            self.update_graphql_query_status_metrics(state, []);
        }
        Some(transition)
    }
    fn handle_evaluate_start<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateStartAction { cache_id, .. } = action;
        let query_id = state.evaluate_effect_mappings.get(cache_id)?;
        let operation_state = state.operations.get_mut(query_id)?;
        let active_traces = operation_state
            .subscriptions
            .values_mut()
            .filter_map(|subscription| subscription.trace.as_mut());
        for trace in active_traces {
            trace.enter_span(
                GraphQlOperationPhase::Busy,
                [
                    KeyValue::new(
                        Key::from("state_index"),
                        Value::from(JsonValue::Null.to_string()),
                    ),
                    KeyValue::new(Key::from("state_updates"), Value::from(0)),
                ],
                &self.tracer,
            );
        }
        None
    }
    fn handle_evaluate_update<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &EvaluateUpdateAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateUpdateAction {
            cache_id,
            state_index,
            state_updates,
        } = action;
        let query_id = state.evaluate_effect_mappings.get(cache_id)?;
        let operation_state = state.operations.get_mut(query_id)?;
        let active_traces = operation_state
            .subscriptions
            .values_mut()
            .filter_map(|subscription| subscription.trace.as_mut());
        for trace in active_traces {
            trace.enter_span(
                GraphQlOperationPhase::Busy,
                [
                    KeyValue::new(
                        Key::from("state_index"),
                        Value::from(
                            match state_index.as_ref() {
                                Some(index) => JsonValue::Number(usize::from(*index).into()),
                                None => JsonValue::Null,
                            }
                            .to_string(),
                        ),
                    ),
                    KeyValue::new(
                        Key::from("state_updates"),
                        Value::from(state_updates.len() as i64),
                    ),
                ],
                &self.tracer,
            );
        }
        None
    }
    fn handle_evaluate_result<TAction, TTask>(
        &self,
        state: &mut GraphQlServerState<T, TTracer::Span>,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<GraphQlServerEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateResultAction {
            cache_id, result, ..
        } = action;
        let query_id = state.evaluate_effect_mappings.get(cache_id)?;
        let operation_state = state.operations.get_mut(query_id)?;
        let query_status = GraphQlQueryStatus::get_state(Some(result.result()), &self.factory);
        let updated_phase = match query_status {
            GraphQlQueryStatus::Blocked => Some(GraphQlOperationPhase::Blocked),
            GraphQlQueryStatus::Pending => Some(GraphQlOperationPhase::Awaiting),
            GraphQlQueryStatus::Success | GraphQlQueryStatus::Error => None,
        };
        operation_state.operation_phase = updated_phase;
        let active_traces = operation_state
            .subscriptions
            .values_mut()
            .filter_map(|subscription| subscription.trace.as_mut());
        if let Some(updated_phase) = updated_phase {
            let span_attributes = match updated_phase {
                GraphQlOperationPhase::Blocked => {
                    let effect_type_counts =
                        parse_custom_effect_types(result.result(), &self.factory)
                            .into_iter()
                            .fold(
                                HashMap::<String, usize>::new(),
                                |mut results, effect_type| {
                                    match results.entry(effect_type) {
                                        Entry::Occupied(mut entry) => {
                                            *entry.get_mut() += 1;
                                        }
                                        Entry::Vacant(entry) => {
                                            entry.insert(1);
                                        }
                                    }
                                    results
                                },
                            );
                    let total_effect_count: usize = effect_type_counts.values().sum();
                    Some(
                        once(KeyValue::new("effects", format!("{}", total_effect_count))).chain(
                            effect_type_counts.into_iter().map(|(effect_type, count)| {
                                KeyValue::new(
                                    format!("effect:{}", effect_type),
                                    format!("{}", count),
                                )
                            }),
                        ),
                    )
                }
                _ => None,
            }
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
            for trace in active_traces {
                trace.enter_span(updated_phase, span_attributes.clone(), &self.tracer);
            }
        } else {
            let result_event = Some(if is_error_result_payload(result.result(), &self.factory) {
                GraphQlTraceEvent {
                    event_type: GraphQlTraceEventType::Error,
                    attributes: Default::default(),
                }
            } else {
                GraphQlTraceEvent {
                    event_type: GraphQlTraceEventType::Emit,
                    attributes: Default::default(),
                }
            });
            for trace in active_traces {
                trace.end_active_span(result_event.clone());
            }
        }
        None
    }
    fn update_graphql_query_status_metrics<const NUM_DISPOSED: usize>(
        &self,
        state: &GraphQlServerState<T, TTracer::Span>,
        disposed_metrics: [Vec<(String, String)>; NUM_DISPOSED],
    ) {
        let query_status_metrics = state
            .operations
            .values()
            .map(|operation_state| {
                let result = operation_state
                    .result
                    .as_ref()
                    .map(|result| result.result());
                let status = GraphQlQueryStatus::get_state(result, &self.factory);
                (&operation_state.metric_labels, status)
            })
            .fold(
                HashMap::<&Vec<(String, String)>, QueryStatusMetrics>::from_iter(
                    disposed_metrics
                        .iter()
                        .map(|metric_labels| (metric_labels, QueryStatusMetrics::default())),
                ),
                |mut results, (metric_labels, status)| {
                    let query_metrics = results.entry(metric_labels).or_default();
                    query_metrics.register_query(status);
                    results
                },
            );
        for (metric_labels, metric_counts) in query_status_metrics {
            self.update_graphql_query_status_metric(metric_labels, metric_counts);
        }
    }
    fn update_graphql_query_status_metric(
        &self,
        metric_labels: &Vec<(String, String)>,
        metric_values: QueryStatusMetrics,
    ) {
        let QueryStatusMetrics {
            pending,
            error,
            success,
        } = metric_values;
        gauge!(
            self.metric_names.graphql_active_query_pending_count,
            pending as f64,
            metric_labels
        );
        gauge!(
            self.metric_names.graphql_active_query_error_count,
            error as f64,
            metric_labels
        );
        gauge!(
            self.metric_names.graphql_active_query_success_count,
            success as f64,
            metric_labels
        );
    }
}

fn parse_custom_effect_types<T: Expression>(
    result: &T,
    factory: &impl ExpressionFactory<T>,
) -> impl IntoIterator<Item = String> {
    factory
        .match_signal_term(result)
        .into_iter()
        .flat_map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .filter_map(|effect| match effect.signal_type() {
                    SignalType::Custom(effect_type) => Some(effect_type),
                    _ => None,
                })
        })
        .collect::<HashSet<_>>()
}

fn match_error_expression<'a, T: Expression>(
    expression: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T::Signal<T>> {
    factory.match_signal_term(expression).and_then(|term| {
        term.signals()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .find_map(|effect| {
                if matches!(effect.signal_type(), SignalType::Error) {
                    Some(effect)
                } else {
                    None
                }
            })
    })
}

fn parse_error_message<'a, T: Expression + 'a>(
    error: &'a T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<&'a str> {
    error
        .args()
        .map(|item| item.as_deref())
        .next()
        .and_then(|arg| parse_error_payload_message(arg, factory, allocator))
}

fn parse_error_payload_message<'a, T: Expression + 'a>(
    value: &'a T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<&'a str> {
    if let Some(term) = factory.match_string_term(value) {
        Some(term.value().as_deref().as_str())
    } else if let Some(term) = factory.match_record_term(value) {
        parse_error_object_payload_message(term, factory, allocator)
    } else {
        None
    }
}

fn parse_error_object_payload_message<'a, T: Expression + 'a>(
    value: &'a T::RecordTerm<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<&'a str> {
    let message = value
        .get(&factory.create_string_term(allocator.create_static_string("message")))
        .map(|value| value.as_deref())?;
    let term = factory.match_string_term(message)?;
    Some(term.value().as_deref().as_str())
}

fn is_error_result_payload<T: Expression>(
    expression: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    match_error_expression(expression, factory).is_some()
}

fn is_blocked_result_payload<T: Expression>(
    expression: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_signal_term(expression)
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .any(|effect| matches!(effect.signal_type(), SignalType::Custom(_)))
        })
        .unwrap_or(false)
}

fn is_pending_result_payload<T: Expression>(
    expression: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_signal_term(expression)
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .any(|effect| matches!(effect.signal_type(), SignalType::Pending))
        })
        .unwrap_or(false)
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub(super) enum GraphQlQueryStatus {
    Blocked,
    Pending,
    Success,
    Error,
}

impl GraphQlQueryStatus {
    pub(super) fn get_state<T: Expression>(
        value: Option<&T>,
        factory: &impl ExpressionFactory<T>,
    ) -> Self {
        match value {
            None => Self::Pending,
            Some(value) if is_error_result_payload(value, factory) => Self::Error,
            Some(value) if is_blocked_result_payload(value, factory) => Self::Blocked,
            Some(value) if is_pending_result_payload(value, factory) => Self::Pending,
            _ => Self::Success,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};
    use std::iter::empty;

    use metrics_exporter_prometheus::PrometheusHandle;

    use opentelemetry::trace::noop::{NoopSpan, NoopTracer};
    use reflex::core::{uuid, DependencyList};
    use reflex_dispatcher::{Handler, MessageOffset, ProcessId, SchedulerTransition};
    use reflex_graphql::{
        parse_graphql_operation, parse_graphql_query, GraphQlVariables, NoopGraphQlQueryTransform,
    };
    use reflex_handlers::utils::tls::hyper_tls;
    use reflex_json::{JsonMap, JsonValue};
    use reflex_lang::allocator::DefaultAllocator;
    use reflex_lang::{CachedSharedTerm, ExpressionList, SharedTermFactory};
    use reflex_utils::reconnect::NoopReconnectTimeout;

    use crate::action::ServerCliAction;
    use crate::builtins::ServerBuiltins;
    use crate::cli::reflex_server::GraphQlWebServerMetricLabels;
    use crate::cli::task::ServerCliTaskFactory;

    use super::*;

    #[test]
    fn status_of_operation_state_is_correct() {
        let (factory, allocator, _) = harness(generate_metric_labels_for_operation);
        //       error maps to error
        let error_result = dummy_error_result(&factory, &allocator);
        assert_eq!(
            GraphQlQueryStatus::Error,
            GraphQlQueryStatus::get_state(Some(error_result.result()), &factory)
        );
        //       pending maps to pending
        let pending_result = dummy_pending_result(&factory, &allocator);
        assert_eq!(
            GraphQlQueryStatus::Pending,
            GraphQlQueryStatus::get_state(Some(pending_result.result()), &factory)
        );
        //       blocked-awaiting-effect maps to blocked
        let blocked_result = dummy_blocked_result(&factory, &allocator);
        assert_eq!(
            GraphQlQueryStatus::Blocked,
            GraphQlQueryStatus::get_state(Some(blocked_result.result()), &factory)
        );
        //       random other thing maps to success
        let success_result = dummy_success_result(&factory);
        assert_eq!(
            GraphQlQueryStatus::Success,
            GraphQlQueryStatus::get_state(Some(success_result.result()), &factory)
        );
        //       error and pending maps to error
        let mixed_result = EvaluationResult::new(
            factory.create_signal_term(allocator.create_signal_list(vec![
                allocator.create_signal(SignalType::Pending, ExpressionList::new(empty())),
                allocator.create_signal(SignalType::Error, ExpressionList::new(empty())),
            ])),
            DependencyList::empty(),
        );
        assert_eq!(
            GraphQlQueryStatus::Error,
            GraphQlQueryStatus::get_state(Some(mixed_result.result()), &factory)
        );
        //       missing result maps to pending
        assert_eq!(
            GraphQlQueryStatus::Pending,
            GraphQlQueryStatus::get_state::<CachedSharedTerm<ServerBuiltins>>(None, &factory)
        );
    }

    #[test]
    fn subscription_results_in_pending_state() {
        type T = CachedSharedTerm<ServerBuiltins>;
        type TFactory = SharedTermFactory<ServerBuiltins>;
        type TAllocator = DefaultAllocator<T>;
        type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
        type TReconnect = NoopReconnectTimeout;
        type TTracer = NoopTracer;
        type TAction = ServerCliAction<T>;
        type TTask = ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            TTracer,
        >;
        let (_factory, _allocator, server) = harness(generate_metric_labels_for_operation);
        let subscribed_operation_id = uuid!("b60cd922-45f2-47b2-8929-e6b43befbe3c");
        let subscribed_operation = graphql_operation_with_uuid(subscribed_operation_id);
        let subscribe_action = {
            let operation_id = subscribed_operation_id;
            let operation = subscribed_operation.clone();
            let subscription_id = operation_id;
            graphql_subscribe_action(subscription_id, operation)
        };
        let mut state = GraphQlServerState::default();

        let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
            &server,
            &mut state,
            &subscribe_action,
            &message_data(),
            &mut DummyContext,
        );

        assert_eq!(state.operations.len(), 1);
        let updated_operation_state = state.operations.values().next().unwrap();
        assert_eq!(updated_operation_state.result, None);
        let active_subscription_ids = updated_operation_state
            .subscriptions
            .keys()
            .copied()
            .collect::<HashSet<_>>();
        assert_eq!(
            active_subscription_ids,
            HashSet::from_iter([subscribed_operation_id])
        );
    }

    #[test]
    fn new_subscription_metrics_are_recorded_alongside_existing_subscription_metrics() {
        type T = CachedSharedTerm<ServerBuiltins>;
        type TFactory = SharedTermFactory<ServerBuiltins>;
        type TAllocator = DefaultAllocator<T>;
        type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
        type TReconnect = NoopReconnectTimeout;
        type TTracer = NoopTracer;
        type TAction = ServerCliAction<T>;
        type TTask = ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            TTracer,
        >;
        reflex_test_utils::run_metrics_test(|handle| {
            let (factory, allocator, server) = harness(generate_metric_labels_for_operation);

            // These unique IDs which are embedded in the corresponding GraphQL queries via the variables,
            // which ensures unique state entries are created for the various operations
            let success_operation_id = uuid!("92e81759-a478-496c-a261-31f5980c21f7");
            let error_operation_id = uuid!("fd11c0a3-c1c8-425d-a17d-989319ed425a");
            let pending_operation_id = uuid!("40be68e9-52b0-4f09-815c-26473c5f4412");
            let success_operation = graphql_operation_with_uuid(success_operation_id);
            let error_operation = graphql_operation_with_uuid(error_operation_id);
            let pending_operation = graphql_operation_with_uuid(pending_operation_id);

            let mut state = graphql_server_state([
                {
                    let operation_id = success_operation_id;
                    let operation = success_operation.clone();
                    let result = Some(dummy_success_result(&factory));
                    let query = parse_graphql_operation(&operation, &factory, &allocator).unwrap();
                    let query_label = GraphQlWebServerMetricLabels.label(&operation);
                    let evaluate_effect = create_query_evaluate_effect(
                        query_label.clone(),
                        query.clone(),
                        &factory,
                        &allocator,
                    );
                    let metric_labels = generate_metric_labels_for_operation(&operation);
                    GraphQlOperationState {
                        query,
                        label: query_label,
                        evaluate_effect,
                        metric_labels,
                        operation_phase: Default::default(),
                        start_time: None,
                        result,
                        subscriptions: [{
                            let subscription_id = operation_id;
                            (
                                subscription_id,
                                GraphQlSubscriptionState {
                                    operation,
                                    trace: None,
                                },
                            )
                        }]
                        .into_iter()
                        .collect(),
                    }
                },
                {
                    let operation_id = error_operation_id;
                    let operation = error_operation.clone();
                    let result = Some(dummy_error_result(&factory, &allocator));
                    let query = parse_graphql_operation(&operation, &factory, &allocator).unwrap();
                    let query_label = GraphQlWebServerMetricLabels.label(&operation);
                    let evaluate_effect = create_query_evaluate_effect(
                        query_label.clone(),
                        query.clone(),
                        &factory,
                        &allocator,
                    );
                    let metric_labels = generate_metric_labels_for_operation(&operation);
                    GraphQlOperationState {
                        query,
                        label: query_label,
                        evaluate_effect,
                        metric_labels,
                        operation_phase: Default::default(),
                        start_time: None,
                        result,
                        subscriptions: [{
                            let subscription_id = operation_id;
                            (
                                subscription_id,
                                GraphQlSubscriptionState {
                                    operation,
                                    trace: None,
                                },
                            )
                        }]
                        .into_iter()
                        .collect(),
                    }
                },
                {
                    let operation_id = pending_operation_id;
                    let operation = pending_operation.clone();
                    let result = Some(dummy_pending_result(&factory, &allocator));
                    let query = parse_graphql_operation(&operation, &factory, &allocator).unwrap();
                    let query_label = GraphQlWebServerMetricLabels.label(&operation);
                    let evaluate_effect = create_query_evaluate_effect(
                        query_label.clone(),
                        query.clone(),
                        &factory,
                        &allocator,
                    );
                    let metric_labels = generate_metric_labels_for_operation(&operation);
                    GraphQlOperationState {
                        query,
                        label: query_label,
                        evaluate_effect,
                        metric_labels,
                        operation_phase: Default::default(),
                        start_time: None,
                        result,
                        subscriptions: [{
                            let subscription_id = operation_id;
                            (
                                subscription_id,
                                GraphQlSubscriptionState {
                                    operation,
                                    trace: None,
                                },
                            )
                        }]
                        .into_iter()
                        .collect(),
                    }
                },
            ]);
            // Keep a copy of the query ids from the the initial operation state (used for detecting newly-added queries)
            let existing_query_ids = state.operations.keys().cloned().collect::<HashSet<_>>();

            // Flush the initial metrics
            server.update_graphql_query_status_metrics(&state, []);
            let initial_metrics = get_metrics(&handle);

            // Assert the correctness of the initial metrics
            let success_operation_label = retrieve_metric_labels_for_operation(&success_operation);
            let error_operation_label = retrieve_metric_labels_for_operation(&error_operation);
            let pending_operation_label = retrieve_metric_labels_for_operation(&pending_operation);

            assert_metric(
                &initial_metrics,
                &success_metric_name(&success_operation_label),
                1.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&success_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&success_operation_label),
                0.0,
            );

            assert_metric(
                &initial_metrics,
                &success_metric_name(&error_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&error_operation_label),
                1.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&error_operation_label),
                0.0,
            );

            assert_metric(
                &initial_metrics,
                &success_metric_name(&pending_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&pending_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&pending_operation_label),
                1.0,
            );

            // Simulate a subscribe action, with a unique operation ID to ensure a new state entry
            let subscribed_operation_id = uuid!("32a57a5a-bc82-4307-9be3-4e318774c29f");
            let subscribed_operation = graphql_operation_with_uuid(subscribed_operation_id);
            let subscribe_action = {
                let operation_id = subscribed_operation_id;
                let operation = subscribed_operation.clone();
                let subscription_id = operation_id;
                graphql_subscribe_action(subscription_id, operation)
            };
            let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
                &server,
                &mut state,
                &subscribe_action,
                &message_data(),
                &mut DummyContext,
            );

            // Ensure that the newly-subscribed operation state was added correctly
            assert_eq!(state.operations.len(), 4);
            let added_operations = state
                .operations
                .iter()
                .filter(|(query_id, _)| !existing_query_ids.contains(query_id))
                .map(|(_, operation_state)| operation_state)
                .collect::<Vec<_>>();
            assert_eq!(added_operations.len(), 1);
            let added_operation_state = added_operations.into_iter().next().unwrap();
            assert_eq!(added_operation_state.result, None);
            assert_eq!(added_operation_state.subscriptions.len(), 1);
            let active_subscription_ids = added_operation_state
                .subscriptions
                .keys()
                .copied()
                .collect::<HashSet<_>>();
            assert_eq!(
                active_subscription_ids,
                HashSet::from_iter([subscribed_operation_id])
            );

            // Ensure that the newly-added subscription is present in the metrics
            let updated_metrics = get_metrics(&handle);

            let added_subscription_label =
                retrieve_metric_labels_for_operation(&subscribed_operation);

            assert_metric(
                &updated_metrics,
                &success_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&added_subscription_label),
                1.0,
            );

            assert_metric(
                &updated_metrics,
                &success_metric_name(&success_operation_label),
                1.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&success_operation_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&success_operation_label),
                0.0,
            );

            assert_metric(
                &updated_metrics,
                &success_metric_name(&error_operation_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&error_operation_label),
                1.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&error_operation_label),
                0.0,
            );

            assert_metric(
                &updated_metrics,
                &success_metric_name(&pending_operation_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&pending_operation_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&pending_operation_label),
                1.0,
            );
        });
    }

    #[test]
    fn unsubscription_removes_subscription_from_state() {
        type T = CachedSharedTerm<ServerBuiltins>;
        type TFactory = SharedTermFactory<ServerBuiltins>;
        type TAllocator = DefaultAllocator<T>;
        type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
        type TReconnect = NoopReconnectTimeout;
        type TTracer = NoopTracer;
        type TAction = ServerCliAction<T>;
        type TTask = ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            TTracer,
        >;
        let (_factory, _allocator, server) = harness(generate_metric_labels_for_operation);
        // ensure subscription exists
        let subscribed_operation_id = uuid!("03422820-ac5a-4271-a834-3c5163e25aba");
        let subscribed_operation = graphql_operation_with_uuid(subscribed_operation_id);
        let subscribe_action = {
            let operation_id = subscribed_operation_id;
            let operation = subscribed_operation.clone();
            let subscription_id = operation_id;
            graphql_subscribe_action(subscription_id, operation)
        };
        let mut state = GraphQlServerState::default();
        let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
            &server,
            &mut state,
            &subscribe_action,
            &message_data(),
            &mut DummyContext,
        );

        let unsubscribe_action = graphql_unsubscribe_action(subscribed_operation_id);
        let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
            &server,
            &mut state,
            &unsubscribe_action,
            &message_data(),
            &mut DummyContext,
        );
        assert_eq!(state.operations.len(), 0);
    }

    #[test]
    fn unsubscription_updates_metrics_for_subscriptions() {
        type T = CachedSharedTerm<ServerBuiltins>;
        type TFactory = SharedTermFactory<ServerBuiltins>;
        type TAllocator = DefaultAllocator<T>;
        type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
        type TReconnect = NoopReconnectTimeout;
        type TTracer = NoopTracer;
        type TAction = ServerCliAction<T>;
        type TTask = ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            TTracer,
        >;
        reflex_test_utils::run_metrics_test(|handle| {
            let (_factory, _allocator, server) = harness(generate_metric_labels_for_operation);
            // ensure subscription exists
            let subscribed_operation_id = uuid!("f137fc55-cea8-46ec-a807-67d0402a25aa");
            let subscribed_operation = graphql_operation_with_uuid(subscribed_operation_id);
            let subscribe_action = {
                let operation_id = subscribed_operation_id;
                let operation = subscribed_operation.clone();
                let subscription_id = operation_id;
                graphql_subscribe_action(subscription_id, operation)
            };
            let mut state = GraphQlServerState::default();
            let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
                &server,
                &mut state,
                &subscribe_action,
                &message_data(),
                &mut DummyContext,
            );

            let label = retrieve_metric_labels_for_operation(&subscribed_operation);
            let metrics = get_metrics(&handle);
            assert_eq!(metrics[&success_metric_name(&label)], 0.0);
            assert_eq!(metrics[&error_metric_name(&label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&label)], 1.0);

            let unsubscribe_action = graphql_unsubscribe_action(subscribed_operation_id);
            let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
                &server,
                &mut state,
                &unsubscribe_action,
                &message_data(),
                &mut DummyContext,
            );
            assert_eq!(state.operations.len(), 0);
            let label = retrieve_metric_labels_for_operation(&subscribed_operation);
            let metrics = get_metrics(&handle);
            assert_eq!(metrics[&success_metric_name(&label)], 0.0);
            assert_eq!(metrics[&error_metric_name(&label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&label)], 0.0);
        });
    }

    #[test]
    fn graphql_query_modification_merges_subscriptions_with_shared_queries() {
        type T = CachedSharedTerm<ServerBuiltins>;
        type TFactory = SharedTermFactory<ServerBuiltins>;
        type TAllocator = DefaultAllocator<T>;
        type TConnect = hyper_tls::HttpsConnector<hyper::client::HttpConnector>;
        type TReconnect = NoopReconnectTimeout;
        type TTracer = NoopTracer;
        type TAction = ServerCliAction<T>;
        type TTask = ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            NoopGraphQlQueryTransform,
            NoopGraphQlQueryTransform,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            GraphQlWebServerMetricLabels,
            TTracer,
        >;
        reflex_test_utils::run_metrics_test(|handle| {
            let (factory, allocator, server) = harness(generate_metric_labels_for_operation);

            let success_operation_id = uuid!("39002291-ca3b-4043-b403-5e0e50e91c0a");
            let success_operation = graphql_operation_with_uuid(success_operation_id);
            let mut state = graphql_server_state([{
                let operation_id = success_operation_id;
                let operation = success_operation.clone();
                let result = Some(dummy_success_result(&factory));
                let query = parse_graphql_operation(&operation, &factory, &allocator).unwrap();
                let query_label = GraphQlWebServerMetricLabels.label(&operation);
                let evaluate_effect = create_query_evaluate_effect(
                    query_label.clone(),
                    query.clone(),
                    &factory,
                    &allocator,
                );
                let metric_labels = generate_metric_labels_for_operation(&operation);
                GraphQlOperationState {
                    query,
                    label: query_label,
                    evaluate_effect,
                    metric_labels,
                    operation_phase: Default::default(),
                    start_time: None,
                    result,
                    subscriptions: [{
                        let subscription_id = operation_id;
                        (
                            subscription_id,
                            GraphQlSubscriptionState {
                                operation,
                                trace: None,
                            },
                        )
                    }]
                    .into_iter()
                    .collect(),
                }
            }]);
            // Keep a copy of the query ids from the the initial operation state (used for detecting newly-added queries)
            let existing_query_ids = state.operations.keys().cloned().collect::<HashSet<_>>();

            // Flush the initial metrics
            server.update_graphql_query_status_metrics(&state, []);
            let initial_metrics = get_metrics(&handle);

            // Assert the correctness of the initial metrics
            let success_operation_label = retrieve_metric_labels_for_operation(&success_operation);

            assert_metric(
                &initial_metrics,
                &success_metric_name(&success_operation_label),
                1.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&success_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&success_operation_label),
                0.0,
            );

            // Simulate a subscribe action, with a unique operation ID to ensure a new state entry
            let subscribed_operation_id = uuid!("2c8a3933-346a-423e-b9f4-a6fa69815509");
            let subscribed_operation = graphql_operation_with_uuid(subscribed_operation_id);
            let subscribe_action = {
                let operation_id = subscribed_operation_id;
                let operation = subscribed_operation.clone();
                let subscription_id = operation_id;
                graphql_subscribe_action(subscription_id, operation)
            };
            let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
                &server,
                &mut state,
                &subscribe_action,
                &message_data(),
                &mut DummyContext,
            );

            // Ensure that the newly-subscribed operation state was added correctly
            assert_eq!(state.operations.len(), 2);
            let added_operations = state
                .operations
                .iter()
                .filter(|(query_id, _)| !existing_query_ids.contains(query_id))
                .map(|(_, operation_state)| operation_state)
                .collect::<Vec<_>>();
            assert_eq!(added_operations.len(), 1);
            let added_operation_state = added_operations.into_iter().next().unwrap();
            assert_eq!(added_operation_state.result, None);
            assert_eq!(added_operation_state.subscriptions.len(), 1);
            let active_subscription_ids = added_operation_state
                .subscriptions
                .keys()
                .copied()
                .collect::<HashSet<_>>();
            assert_eq!(
                active_subscription_ids,
                HashSet::from([subscribed_operation_id])
            );

            // Ensure that the newly-added subscription is present in the metrics
            let updated_metrics = get_metrics(&handle);
            let added_subscription_label =
                retrieve_metric_labels_for_operation(&subscribed_operation);
            assert_metric(
                &updated_metrics,
                &success_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&added_subscription_label),
                1.0,
            );

            // Ensure that metrics still exist for any existing subscriptions
            assert_metric(
                &initial_metrics,
                &success_metric_name(&success_operation_label),
                1.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&success_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&success_operation_label),
                0.0,
            );

            // Keep a copy of the query ids from the the current operation state (used for detecting newly-added queries)
            let existing_query_ids = state.operations.keys().cloned().collect::<HashSet<_>>();

            // Modify the newly-added query variables so that it becomes identical to the original query
            let modify_action = graphql_modify_action(
                subscribed_operation_id,
                GraphQlVariables::from_iter([(
                    "id".into(),
                    success_operation_id.to_string().into(),
                )]),
            );
            let _ = Handler::<TAction, SchedulerTransition<TAction, TTask>>::handle(
                &server,
                &mut state,
                &modify_action,
                &message_data(),
                &mut DummyContext,
            );

            // Ensure that the subscriptions were consolidated into a single operation state
            assert_eq!(state.operations.len(), 1);
            let retained_operations = state
                .operations
                .iter()
                .filter(|(query_id, _)| existing_query_ids.contains(query_id))
                .map(|(_, operation_state)| operation_state)
                .collect::<Vec<_>>();
            assert_eq!(retained_operations.len(), 1);
            let retained_operation_state = retained_operations.into_iter().next().unwrap();
            assert_eq!(
                retained_operation_state.result,
                Some(dummy_success_result(&factory))
            );
            let active_subscription_ids = retained_operation_state
                .subscriptions
                .keys()
                .copied()
                .collect::<HashSet<_>>();
            assert_eq!(
                active_subscription_ids,
                HashSet::from_iter([success_operation_id, subscribed_operation_id])
            );

            // Ensure that the outdated subscription has been removed from the metrics
            let updated_metrics = get_metrics(&handle);
            let added_subscription_label =
                retrieve_metric_labels_for_operation(&subscribed_operation);
            assert_metric(
                &updated_metrics,
                &success_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &error_metric_name(&added_subscription_label),
                0.0,
            );
            assert_metric(
                &updated_metrics,
                &pending_metric_name(&added_subscription_label),
                0.0,
            );

            // Ensure that metrics still exist for any existing subscriptions
            assert_metric(
                &initial_metrics,
                &success_metric_name(&success_operation_label),
                1.0,
            );
            assert_metric(
                &initial_metrics,
                &error_metric_name(&success_operation_label),
                0.0,
            );
            assert_metric(
                &initial_metrics,
                &pending_metric_name(&success_operation_label),
                0.0,
            );
        });
    }

    fn error_metric_name(label: &str) -> String {
        format!("graphql_active_query_error_count{{{}}}", label)
    }

    fn pending_metric_name(label: &str) -> String {
        format!("graphql_active_query_pending_count{{{}}}", label)
    }

    fn success_metric_name(label: &str) -> String {
        format!("graphql_active_query_success_count{{{}}}", label)
    }

    fn harness<TMetricLabels>(
        get_operation_metric_labels: TMetricLabels,
    ) -> (
        SharedTermFactory<ServerBuiltins>,
        DefaultAllocator<CachedSharedTerm<ServerBuiltins>>,
        GraphQlServer<
            CachedSharedTerm<ServerBuiltins>,
            SharedTermFactory<ServerBuiltins>,
            DefaultAllocator<CachedSharedTerm<ServerBuiltins>>,
            GraphQlWebServerMetricLabels,
            TMetricLabels,
            NoopTracer,
        >,
    )
    where
        TMetricLabels: GraphQlServerOperationMetricLabels,
    {
        let factory = SharedTermFactory::<ServerBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let metric_names = GraphQlServerMetricNames::default();
        let server = GraphQlServer::new(
            factory.clone(),
            allocator.clone(),
            metric_names,
            GraphQlWebServerMetricLabels,
            get_operation_metric_labels,
            NoopTracer::default(),
            ProcessId::default(),
        );
        (factory, allocator, server)
    }

    fn assert_metric(metrics: &HashMap<String, f64>, label: &str, expected: f64) {
        assert_eq!(metrics.get(label).copied(), Some(expected), "{}", label);
    }

    fn get_metrics(handle: &PrometheusHandle) -> HashMap<String, f64> {
        let metrics = handle.render();
        metrics
            .split("\n")
            .filter(|x| !x.starts_with("#") && !x.is_empty())
            .map(|x| {
                let pieces: Vec<&str> = x.split(" ").collect();
                (pieces[0].to_string(), pieces[1].parse().unwrap())
            })
            .collect()
    }

    fn graphql_subscribe_action(
        subscription_id: Uuid,
        operation: GraphQlOperation,
    ) -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            _expression: PhantomData::<CachedSharedTerm<ServerBuiltins>>::default(),
        }
        .into()
    }

    fn graphql_unsubscribe_action(
        subscription_id: Uuid,
    ) -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerUnsubscribeAction {
            subscription_id,
            _expression: PhantomData::<CachedSharedTerm<ServerBuiltins>>::default(),
        }
        .into()
    }

    fn graphql_modify_action(
        subscription_id: Uuid,
        variables: GraphQlVariables,
    ) -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerModifyAction {
            subscription_id,
            variables,
            _expression: PhantomData::<CachedSharedTerm<ServerBuiltins>>::default(),
        }
        .into()
    }

    fn dummy_error_result(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
    ) -> EvaluationResult<CachedSharedTerm<ServerBuiltins>> {
        EvaluationResult::new(
            factory.create_signal_term(allocator.create_signal_list(vec![
                allocator.create_signal(SignalType::Error, ExpressionList::new(empty())),
            ])),
            DependencyList::empty(),
        )
    }

    fn dummy_success_result(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
    ) -> EvaluationResult<CachedSharedTerm<ServerBuiltins>> {
        EvaluationResult::new(factory.create_int_term(3), DependencyList::empty())
    }

    fn dummy_pending_result(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
    ) -> EvaluationResult<CachedSharedTerm<ServerBuiltins>> {
        EvaluationResult::new(
            factory.create_signal_term(allocator.create_signal_list(vec![
                allocator.create_signal(SignalType::Pending, ExpressionList::new(empty())),
            ])),
            DependencyList::empty(),
        )
    }

    fn dummy_blocked_result(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
    ) -> EvaluationResult<CachedSharedTerm<ServerBuiltins>> {
        EvaluationResult::new(
            factory.create_signal_term(allocator.create_signal_list(vec![
                allocator.create_signal(
                    SignalType::Custom(String::from("foo")),
                    ExpressionList::new(empty()),
                ),
            ])),
            DependencyList::empty(),
        )
    }

    fn graphql_operation_with_uuid(operation_id: Uuid) -> GraphQlOperation {
        let operation_name = format!("Query{:x}", operation_id.as_u128());
        let query_with_variables = parse_graphql_query(&format!(
            "subscription {} ($id: String!) {{\n  foo(id: $id)\n}}\n",
            operation_name
        ))
        .unwrap();
        GraphQlOperation::new(
            query_with_variables,
            Some(operation_name),
            JsonMap::from_iter([(
                "id".to_string(),
                JsonValue::String(operation_id.to_string()),
            )]),
            JsonMap::default(),
        )
    }

    fn retrieve_metric_labels_for_operation(operation: &GraphQlOperation) -> String {
        generate_metric_labels_for_operation(operation)
            .into_iter()
            .map(|(key, value)| format!("{}=\"{}\"", key, value))
            .collect::<Vec<_>>()
            .join(",")
    }

    fn generate_metric_labels_for_operation(operation: &GraphQlOperation) -> Vec<(String, String)> {
        operation
            .variables()
            .iter()
            .map(|(key, value)| {
                (
                    key.clone(),
                    match value {
                        JsonValue::String(value) => value.clone(),
                        value => value.to_string(),
                    },
                )
            })
            .collect()
    }

    fn graphql_server_state(
        operations: impl IntoIterator<
            Item = GraphQlOperationState<CachedSharedTerm<ServerBuiltins>, NoopSpan>,
        >,
    ) -> GraphQlServerState<CachedSharedTerm<ServerBuiltins>, NoopSpan> {
        operations
            .into_iter()
            .fold(Default::default(), |mut state, operation_state| {
                if let Some(previous_operation_state) = state
                    .operations
                    .insert(operation_state.query.id(), operation_state)
                {
                    panic!(
                        "Operation state already exists for query ID {}",
                        previous_operation_state.query.id()
                    )
                };
                state
            })
    }

    fn message_data() -> MessageData {
        MessageData {
            offset: MessageOffset::from(0),
            parent: None,
            timestamp: Instant::now(),
        }
    }

    struct DummyContext;
    impl HandlerContext for DummyContext {
        fn pid(&self) -> ProcessId {
            ProcessId::from(0)
        }

        fn generate_pid(&mut self) -> ProcessId {
            ProcessId::from(0)
        }
    }
}
