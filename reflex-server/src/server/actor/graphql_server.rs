// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, marker::PhantomData, time::Instant};

use metrics::{
    counter, decrement_gauge, describe_counter, describe_gauge, histogram, increment_counter,
    increment_gauge, Unit,
};
use reflex::{
    core::{EvaluationResult, Expression, ExpressionFactory, HeapAllocator, SignalType, Uuid},
    stdlib::Stdlib,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_graphql::{
    graphql_variables_are_equal, stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload,
};
use reflex_runtime::action::query::{
    QueryEmitAction, QuerySubscribeAction, QueryUnsubscribeAction,
};

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
        }
    }
}

pub(crate) struct GraphQlServer<T, TFactory, TAllocator, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: GraphQlServerMetricNames,
    get_operation_metric_labels: TMetricLabels,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TMetricLabels> GraphQlServer<T, TFactory, TAllocator, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
{
    pub(crate) fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: GraphQlServerMetricNames,
        get_operation_metric_labels: TMetricLabels,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            get_operation_metric_labels,
            _expression: Default::default(),
        }
    }
}

pub struct GraphQlServerState<T: Expression> {
    operations: Vec<GraphQlOperationState<T>>,
}
impl<T: Expression> Default for GraphQlServerState<T> {
    fn default() -> Self {
        Self {
            operations: Default::default(),
        }
    }
}
struct GraphQlOperationState<T: Expression> {
    operation: GraphQlOperationPayload,
    query: T,
    result: Option<EvaluationResult<T>>,
    subscriptions: Vec<GraphQlSubscriptionState>,
}
struct GraphQlSubscriptionState {
    subscription_id: Uuid,
    operation_name: Option<String>,
    start_time: Option<Instant>,
    metric_labels: Vec<(String, String)>,
}

pub trait GraphQlServerAction<T: Expression>:
    Action
    + InboundAction<GraphQlServerSubscribeAction<T>>
    + InboundAction<GraphQlServerUnsubscribeAction<T>>
    + InboundAction<GraphQlServerModifyAction<T>>
    + InboundAction<GraphQlServerEmitAction<T>>
    + InboundAction<QueryEmitAction<T>>
    + OutboundAction<GraphQlServerParseSuccessAction<T>>
    + OutboundAction<GraphQlServerEmitAction<T>>
    + OutboundAction<QuerySubscribeAction<T>>
    + OutboundAction<QueryUnsubscribeAction<T>>
    + OutboundAction<GraphQlServerParseErrorAction<T>>
    + OutboundAction<GraphQlServerUnsubscribeAction<T>>
{
}
impl<T: Expression, TAction> GraphQlServerAction<T> for TAction where
    Self: Action
        + InboundAction<GraphQlServerSubscribeAction<T>>
        + InboundAction<GraphQlServerUnsubscribeAction<T>>
        + InboundAction<GraphQlServerModifyAction<T>>
        + InboundAction<GraphQlServerEmitAction<T>>
        + InboundAction<QueryEmitAction<T>>
        + OutboundAction<GraphQlServerParseSuccessAction<T>>
        + OutboundAction<GraphQlServerEmitAction<T>>
        + OutboundAction<QuerySubscribeAction<T>>
        + OutboundAction<QueryUnsubscribeAction<T>>
        + OutboundAction<GraphQlServerParseErrorAction<T>>
        + OutboundAction<GraphQlServerUnsubscribeAction<T>>
{
}

impl<T, TFactory, TAllocator, TAction, TMetricLabels> Actor<TAction>
    for GraphQlServer<T, TFactory, TAllocator, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
    TAction: GraphQlServerAction<T>,
{
    type State = GraphQlServerState<T>;
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
            self.handle_graphql_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_modify(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_emit(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_query_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator, TMetricLabels> GraphQlServer<T, TFactory, TAllocator, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
{
    fn handle_graphql_subscribe<TAction>(
        &self,
        state: &mut GraphQlServerState<T>,
        action: &GraphQlServerSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<QuerySubscribeAction<T>>
            + OutboundAction<GraphQlServerParseSuccessAction<T>>
            + OutboundAction<GraphQlServerParseErrorAction<T>>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>,
    {
        let GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            _expression: _,
        } = action;
        let subscription_id = *subscription_id;
        let operation_name = operation.operation_name().map(String::from);
        let metric_labels = (self.get_operation_metric_labels)(
            operation_name
                .as_ref()
                .map(|operation_name| operation_name.as_str()),
            operation,
        );
        match reflex_graphql::parse_graphql_operation(operation, &self.factory, &self.allocator) {
            Err(err) => Some(StateTransition::new([
                StateOperation::Send(
                    context.pid(),
                    GraphQlServerParseErrorAction {
                        subscription_id,
                        message: err,
                        operation: operation.clone(),
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                StateOperation::Send(
                    context.pid(),
                    GraphQlServerUnsubscribeAction {
                        subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
            ])),
            Ok(query) => {
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
                if let Some(existing_entry) = state
                    .operations
                    .iter_mut()
                    .find(|entry| entry.query.id() == query.id())
                {
                    existing_entry.subscriptions.push(GraphQlSubscriptionState {
                        subscription_id,
                        operation_name,
                        start_time: None,
                        metric_labels,
                    });
                    Some(StateTransition::new(once(StateOperation::Send(
                        context.pid(),
                        GraphQlServerParseSuccessAction {
                            subscription_id,
                            query: existing_entry.query.clone(),
                        }
                        .into(),
                    ))))
                } else {
                    let sanitized_operation = GraphQlOperationPayload::new(
                        operation.query().clone(),
                        None,
                        operation
                            .variables()
                            .map(|(key, value)| (String::from(key), value.clone())),
                        None,
                    );
                    state.operations.push(GraphQlOperationState {
                        operation: sanitized_operation,
                        query: query.clone(),
                        result: None,
                        subscriptions: vec![GraphQlSubscriptionState {
                            subscription_id,
                            operation_name,
                            start_time: Some(Instant::now()),
                            metric_labels,
                        }],
                    });
                    Some(StateTransition::new([
                        StateOperation::Send(
                            context.pid(),
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: query.clone(),
                            }
                            .into(),
                        ),
                        StateOperation::Send(context.pid(), QuerySubscribeAction { query }.into()),
                    ]))
                }
            }
        }
    }
    fn handle_graphql_unsubscribe<TAction>(
        &self,
        state: &mut GraphQlServerState<T>,
        action: &GraphQlServerUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryUnsubscribeAction<T>>,
    {
        let GraphQlServerUnsubscribeAction {
            subscription_id,
            _expression,
        } = action;
        let (operation_index, subscription_index, existing_operation_state) = state
            .operations
            .iter_mut()
            .enumerate()
            .find_map(|(entry_index, entry)| {
                entry
                    .subscriptions
                    .iter()
                    .position(|subscription| subscription.subscription_id == *subscription_id)
                    .map(|subscription_index| (entry_index, subscription_index, entry))
            })?;
        let removed_subscription = existing_operation_state
            .subscriptions
            .remove(subscription_index);
        decrement_gauge!(
            self.metric_names.graphql_active_operation_count,
            1.0,
            &removed_subscription.metric_labels
        );
        let has_remaining_subscriptions = !existing_operation_state.subscriptions.is_empty();
        if has_remaining_subscriptions {
            return None;
        }
        let removed_operation_state = state.operations.remove(operation_index);
        Some(StateTransition::new(Some(StateOperation::Send(
            context.pid(),
            QueryUnsubscribeAction {
                query: removed_operation_state.query,
            }
            .into(),
        ))))
    }
    fn handle_graphql_modify<TAction>(
        &self,
        state: &mut GraphQlServerState<T>,
        action: &GraphQlServerModifyAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<QuerySubscribeAction<T>>
            + OutboundAction<GraphQlServerParseSuccessAction<T>>
            + OutboundAction<GraphQlServerParseErrorAction<T>>
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>,
    {
        let GraphQlServerModifyAction {
            subscription_id,
            variables,
            _expression: _,
        } = action;
        let (operation_index, subscription_index, updated_operation) = state
            .operations
            .iter()
            .enumerate()
            .find_map(|(entry_index, entry)| {
                entry
                    .subscriptions
                    .iter()
                    .enumerate()
                    .find_map(|(index, subscription)| {
                        if subscription.subscription_id == *subscription_id {
                            Some(index)
                        } else {
                            None
                        }
                    })
                    .map(|subscription_index| (entry, entry_index, subscription_index))
            })
            .and_then(|(entry, entry_index, subscription_index)| {
                if graphql_variables_are_equal(
                    variables.iter().map(|(key, value)| (key.as_str(), value)),
                    entry.operation.variables(),
                ) {
                    None
                } else {
                    let updated_variables = variables
                        .iter()
                        .map(|(key, value)| (key.clone(), value.clone()));
                    Some((
                        entry_index,
                        subscription_index,
                        GraphQlOperationPayload::new(
                            entry.operation.query().clone(),
                            None,
                            updated_variables,
                            None,
                        ),
                    ))
                }
            })?;
        match reflex_graphql::parse_graphql_operation(
            &updated_operation,
            &self.factory,
            &self.allocator,
        ) {
            Err(err) => Some(StateTransition::new([
                StateOperation::Send(
                    context.pid(),
                    GraphQlServerParseErrorAction {
                        subscription_id: *subscription_id,
                        message: err,
                        operation: updated_operation,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
                StateOperation::Send(
                    context.pid(),
                    GraphQlServerUnsubscribeAction {
                        subscription_id: *subscription_id,
                        _expression: Default::default(),
                    }
                    .into(),
                ),
            ])),
            Ok(query) => {
                let existing_expression = &state.operations[operation_index].query;
                if existing_expression.id() == query.id() {
                    None
                } else {
                    let existing_subscription = state.operations[operation_index]
                        .subscriptions
                        .remove(subscription_index);
                    let GraphQlSubscriptionState {
                        subscription_id,
                        operation_name,
                        metric_labels: previous_metric_labels,
                        start_time: _,
                    } = existing_subscription;
                    let metric_labels = (self.get_operation_metric_labels)(
                        operation_name
                            .as_ref()
                            .map(|operation_name| operation_name.as_str()),
                        &updated_operation,
                    );
                    decrement_gauge!(
                        self.metric_names.graphql_active_operation_count,
                        1.0,
                        &previous_metric_labels
                    );
                    increment_gauge!(
                        self.metric_names.graphql_active_operation_count,
                        1.0,
                        &metric_labels
                    );
                    if state.operations[operation_index].subscriptions.is_empty() {
                        state.operations.remove(operation_index);
                    }
                    if let Some(existing_entry) = state
                        .operations
                        .iter_mut()
                        .find(|entry| entry.query.id() == query.id())
                    {
                        existing_entry.subscriptions.push(GraphQlSubscriptionState {
                            subscription_id,
                            operation_name,
                            start_time: None,
                            metric_labels,
                        });
                        Some(StateTransition::new(Some(StateOperation::Send(
                            context.pid(),
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: existing_entry.query.clone(),
                            }
                            .into(),
                        ))))
                    } else {
                        state.operations.push(GraphQlOperationState {
                            operation: updated_operation,
                            query: query.clone(),
                            result: None,
                            subscriptions: vec![GraphQlSubscriptionState {
                                subscription_id,
                                operation_name,
                                start_time: Some(Instant::now()),
                                metric_labels,
                            }],
                        });
                        Some(StateTransition::new([
                            StateOperation::Send(
                                context.pid(),
                                GraphQlServerParseSuccessAction {
                                    subscription_id,
                                    query: query.clone(),
                                }
                                .into(),
                            ),
                            StateOperation::Send(
                                context.pid(),
                                QuerySubscribeAction { query }.into(),
                            ),
                        ]))
                    }
                }
            }
        }
    }
    fn handle_query_emit<TAction>(
        &self,
        state: &mut GraphQlServerState<T>,
        action: &QueryEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<GraphQlServerEmitAction<T>>,
    {
        let updated_queries = state
            .operations
            .iter_mut()
            .map(|connection| {
                let previous_result = connection.result.replace(action.result.clone());
                (connection, previous_result)
            })
            .filter(|(connection, previous_result)| {
                if connection.query.id() != action.query.id() {
                    return false;
                }
                let is_unchanged = previous_result
                    .as_ref()
                    .map(|existing| existing.result().id() == action.result.result().id())
                    .unwrap_or(false);
                if is_unchanged {
                    return false;
                }
                return true;
            })
            .flat_map(|(connection, _)| {
                connection
                    .subscriptions
                    .iter()
                    .map(|subscription| subscription.subscription_id)
            })
            .collect::<Vec<_>>();
        if updated_queries.is_empty() {
            return None;
        }
        let actions = updated_queries.into_iter().map(|subscription_id| {
            StateOperation::Send(
                context.pid(),
                GraphQlServerEmitAction {
                    subscription_id,
                    result: action.result.result().clone(),
                }
                .into(),
            )
        });
        Some(StateTransition::new(actions))
    }
    fn handle_graphql_emit<TAction>(
        &self,
        state: &mut GraphQlServerState<T>,
        action: &GraphQlServerEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<QuerySubscribeAction<T>>
            + OutboundAction<GraphQlServerParseSuccessAction<T>>
            + OutboundAction<GraphQlServerParseErrorAction<T>>,
    {
        let GraphQlServerEmitAction {
            subscription_id,
            result,
        } = action;
        let (metric_labels, duration) = state.operations.iter_mut().find_map(|operation| {
            operation
                .subscriptions
                .iter_mut()
                .find(|subscription| subscription.subscription_id == *subscription_id)
                .map(|subscription| {
                    let GraphQlSubscriptionState {
                        subscription_id: _,
                        operation_name: _,
                        start_time,
                        metric_labels,
                    } = subscription;
                    (
                        metric_labels,
                        start_time.take().map(|start_time| start_time.elapsed()),
                    )
                })
        })?;
        if let Some(duration) = duration {
            histogram!(
                self.metric_names.graphql_initial_response_duration,
                duration.as_millis() as f64,
                &*metric_labels
            );
        }
        if is_error_result_payload(result, &self.factory) {
            increment_counter!(
                self.metric_names.graphql_error_payload_count,
                &*metric_labels
            );
        } else {
            increment_counter!(
                self.metric_names.graphql_success_payload_count,
                &*metric_labels
            );
        }
        None
    }
}

fn is_error_result_payload<T: Expression>(
    expression: &T,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_signal_term(expression)
        .map(|term| {
            term.signals()
                .iter()
                .any(|effect| matches!(effect.signal_type(), SignalType::Error))
        })
        .unwrap_or(false)
}
