// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashMap;
use std::{iter::once, marker::PhantomData, time::Instant};

use metrics::{
    counter, decrement_gauge, describe_counter, describe_gauge, gauge, histogram,
    increment_counter, increment_gauge, Unit,
};

use reflex::core::{
    ConditionListType, ConditionType, EvaluationResult, Expression, ExpressionFactory,
    HeapAllocator, RefType, SignalTermType, SignalType, Uuid,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_graphql::{
    graphql_variables_are_equal, stdlib::Stdlib as GraphQlStdlib, GraphQlOperation,
};
use reflex_runtime::action::query::{
    QueryEmitAction, QuerySubscribeAction, QueryUnsubscribeAction,
};
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

pub(crate) struct GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: GraphQlServerMetricNames,
    get_graphql_query_label: TQueryLabel,
    get_operation_metric_labels: TMetricLabels,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
    GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
{
    pub(crate) fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: GraphQlServerMetricNames,
        get_graphql_query_label: TQueryLabel,
        get_operation_metric_labels: TMetricLabels,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            get_graphql_query_label,
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
    operation: GraphQlOperation,
    query: T,
    label: String,
    result: Option<EvaluationResult<T>>,
    subscriptions: Vec<GraphQlSubscriptionState>,
}
struct GraphQlSubscriptionState {
    subscription_id: Uuid,
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

impl<T, TFactory, TAllocator, TAction, TQueryLabel, TMetricLabels> Actor<TAction>
    for GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
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
            let return_value = self.handle_graphql_subscribe(&mut state, action, metadata, context);
            self.record_graphql_query_status_metrics(&state);
            return_value
        } else if let Some(action) = action.match_type() {
            let return_value =
                self.handle_graphql_unsubscribe(&mut state, action, metadata, context);
            self.record_graphql_query_status_metrics(&state);
            return_value
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_modify(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_server_emit(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            let return_value = self.handle_query_emit(&mut state, action, metadata, context);
            self.record_graphql_query_status_metrics(&state);
            return_value
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
    GraphQlServer<T, TFactory, TAllocator, TQueryLabel, TMetricLabels>
where
    T: Expression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
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
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>
            + OutboundAction<GraphQlServerEmitAction<T>>,
    {
        let GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            _expression: _,
        } = action;
        let subscription_id = *subscription_id;
        let metric_labels = (self.get_operation_metric_labels)(operation);
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
                        start_time: None,
                        metric_labels,
                    });
                    Some(StateTransition::new(
                        once(StateOperation::Send(
                            context.pid(),
                            GraphQlServerParseSuccessAction {
                                subscription_id,
                                query: existing_entry.query.clone(),
                            }
                            .into(),
                        ))
                        .chain(existing_entry.result.as_ref().map(
                            |result| {
                                StateOperation::Send(
                                    context.pid(),
                                    GraphQlServerEmitAction {
                                        subscription_id,
                                        result: result.result().clone(),
                                    }
                                    .into(),
                                )
                            },
                        )),
                    ))
                } else {
                    let label = (self.get_graphql_query_label)(operation);
                    state.operations.push(GraphQlOperationState {
                        label: label.clone(),
                        operation: operation.clone(),
                        query: query.clone(),
                        result: None,
                        subscriptions: vec![GraphQlSubscriptionState {
                            subscription_id,
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
                            QuerySubscribeAction { query, label }.into(),
                        ),
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
            None
        } else {
            let removed_operation_state = state.operations.remove(operation_index);
            Some(StateTransition::new(Some(StateOperation::Send(
                context.pid(),
                QueryUnsubscribeAction {
                    query: removed_operation_state.query,
                    label: removed_operation_state.label,
                }
                .into(),
            ))))
        }
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
            + OutboundAction<GraphQlServerUnsubscribeAction<T>>
            + OutboundAction<GraphQlServerEmitAction<T>>,
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
                if graphql_variables_are_equal(variables, entry.operation.variables()) {
                    None
                } else {
                    Some((
                        entry_index,
                        subscription_index,
                        GraphQlOperation::new(
                            entry.operation.query().clone(),
                            entry.operation.operation_name().map(String::from),
                            variables.clone(),
                            entry.operation.extensions().clone(),
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
                        metric_labels: previous_metric_labels,
                        start_time: _,
                    } = existing_subscription;
                    let metric_labels = (self.get_operation_metric_labels)(&updated_operation);
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
                            start_time: None,
                            metric_labels,
                        });
                        Some(StateTransition::new(
                            once(StateOperation::Send(
                                context.pid(),
                                GraphQlServerParseSuccessAction {
                                    subscription_id,
                                    query: existing_entry.query.clone(),
                                }
                                .into(),
                            ))
                            .chain(
                                existing_entry.result.as_ref().map(|result| {
                                    StateOperation::Send(
                                        context.pid(),
                                        GraphQlServerEmitAction {
                                            subscription_id,
                                            result: result.result().clone(),
                                        }
                                        .into(),
                                    )
                                }),
                            ),
                        ))
                    } else {
                        let label = (self.get_graphql_query_label)(&updated_operation);
                        state.operations.push(GraphQlOperationState {
                            label: label.clone(),
                            operation: updated_operation,
                            query: query.clone(),
                            result: None,
                            subscriptions: vec![GraphQlSubscriptionState {
                                subscription_id,
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
                                QuerySubscribeAction { query, label }.into(),
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
            .filter(|operation_state| operation_state.query.id() == action.query.id())
            .filter_map(|operation_state| {
                let previous_result = operation_state.result.replace(action.result.clone());
                let is_unchanged = previous_result
                    .as_ref()
                    .map(|existing| existing.result().id() == action.result.result().id())
                    .unwrap_or(false);
                if is_unchanged {
                    None
                } else {
                    Some(operation_state)
                }
            })
            .flat_map(|connection| {
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
    fn handle_graphql_server_emit<TAction>(
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
    fn record_graphql_query_status_metrics(&self, state: &GraphQlServerState<T>) {
        let mut labels_to_metrics_mapping: HashMap<&Vec<(String, String)>, (f64, f64, f64)> =
            HashMap::new();

        for operation in &state.operations {
            for subscription in &operation.subscriptions {
                let status = GraphQlQueryStatus::get_state(&operation, &self.factory);
                let (success_count, error_count, pending_count) = labels_to_metrics_mapping
                    .entry(&subscription.metric_labels)
                    .or_insert_with(|| (0.0, 0.0, 0.0));

                match status {
                    GraphQlQueryStatus::Pending => {
                        *pending_count += 1.0;
                    }
                    GraphQlQueryStatus::Success => {
                        *success_count += 1.0;
                    }
                    GraphQlQueryStatus::Error => {
                        *error_count += 1.0;
                    }
                }
            }
        }

        for (metric_labels, (success_count, error_count, pending_count)) in
            labels_to_metrics_mapping
        {
            gauge!(
                self.metric_names.graphql_active_query_pending_count,
                pending_count,
                metric_labels
            );
            gauge!(
                self.metric_names.graphql_active_query_error_count,
                error_count,
                metric_labels
            );
            gauge!(
                self.metric_names.graphql_active_query_success_count,
                success_count,
                metric_labels
            );
        }
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
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .any(|effect| matches!(effect.signal_type(), SignalType::Error))
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

#[derive(Eq, PartialEq, Debug)]
enum GraphQlQueryStatus {
    Pending,
    Success,
    Error,
}

impl GraphQlQueryStatus {
    fn get_state<T: Expression>(
        operation_state: &GraphQlOperationState<T>,
        factory: &impl ExpressionFactory<T>,
    ) -> Self {
        let evaluation_result = operation_state
            .result
            .as_ref()
            .map(|result| result.result());
        match evaluation_result {
            Some(expr) if is_error_result_payload(expr, factory) => Self::Error,
            Some(expr) if is_pending_result_payload(expr, factory) => Self::Pending,
            None => Self::Pending,
            _ => Self::Success,
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::iter::empty;

    use metrics_exporter_prometheus::{PrometheusBuilder, PrometheusHandle};

    use reflex::core::DependencyList;
    use reflex_dispatcher::{MessageOffset, ProcessId};
    use reflex_graphql::ast::query::Document;
    use reflex_json::JsonMap;
    use reflex_lang::allocator::DefaultAllocator;
    use reflex_lang::{CachedSharedTerm, ExpressionList, SharedTermFactory};

    use crate::action::ServerCliAction;
    use crate::builtins::ServerBuiltins;
    use crate::cli::reflex_server::get_graphql_query_label;

    use super::*;

    #[test]
    fn status_of_operation_state_is_correct() {
        let (_, factory, allocator, _) = harness();
        //       error maps to error
        let (error, _) = error_state(&allocator, &factory);
        assert_eq!(
            GraphQlQueryStatus::Error,
            GraphQlQueryStatus::get_state(&error, &factory)
        );

        //       pending maps to pending
        let (pending, _) = pending_state(&allocator, &factory);
        assert_eq!(
            GraphQlQueryStatus::Pending,
            GraphQlQueryStatus::get_state(&pending, &factory)
        );
        //       random other thing maps to success
        let (success, _) = success_state(&factory);
        assert_eq!(
            GraphQlQueryStatus::Success,
            GraphQlQueryStatus::get_state(&success, &factory)
        );
        //       error and pending maps to error
        let signals = allocator.create_signal_list(vec![
            allocator.create_signal(SignalType::Pending, ExpressionList::new(empty())),
            allocator.create_signal(SignalType::Error, ExpressionList::new(empty())),
        ]);
        let result: Option<EvaluationResult<CachedSharedTerm<ServerBuiltins>>> = Some(
            EvaluationResult::new(factory.create_signal_term(signals), DependencyList::empty()),
        );
        let (sub_state, _) = subscription_state();
        let mixed_state_operation = GraphQlOperationState {
            operation: graphql_operation(),
            query: factory.create_nil_term(),
            label: "".to_string(),
            result,
            subscriptions: vec![sub_state],
        };
        assert_eq!(
            GraphQlQueryStatus::Error,
            GraphQlQueryStatus::get_state(&mixed_state_operation, &factory)
        );
    }

    #[tokio::test]
    async fn status_metrics_subscribe_action() {
        let (handle, factory, allocator, server) = harness();
        for action in [
            graphql_subscribe_action(),
            graphql_unsubscribe_action(),
            graphql_query_emit_action(),
        ] {
            //     one error state -> (0, 1, 0)
            let (state, label) = error_state(&allocator, &factory);
            server.handle(
                graphql_server_state(vec![state]),
                &action,
                &message_data(),
                &mut DummyContext,
            );
            let metrics = get_metrics(&handle);
            assert_eq!(metrics[&success_metric_name(&label)], 0.0);
            assert_eq!(metrics[&error_metric_name(&label)], 1.0);
            assert_eq!(metrics[&pending_metric_name(&label)], 0.0);
            //     one pending state -> (0, 0, 1)
            let (state, label) = pending_state(&allocator, &factory);
            server.handle(
                graphql_server_state(vec![state]),
                &action,
                &message_data(),
                &mut DummyContext,
            );
            let metrics = get_metrics(&handle);
            assert_eq!(metrics[&success_metric_name(&label)], 0.0);
            assert_eq!(metrics[&error_metric_name(&label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&label)], 1.0);
            //     one success state -> (1, 0, 0)
            let (state, label) = success_state(&factory);
            server.handle(
                graphql_server_state(vec![state]),
                &action,
                &message_data(),
                &mut DummyContext,
            );
            let metrics = get_metrics(&handle);
            assert_eq!(metrics[&success_metric_name(&label)], 1.0);
            assert_eq!(metrics[&error_metric_name(&label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&label)], 0.0);
            //     one success, one pending states with same label -> (0, 1, 1)
            let uuid = Uuid::new_v4();
            let (success, success_label) = success_state_with_uuid(&factory, uuid.clone());
            let (pending, pending_label) =
                pending_state_with_uuid(&allocator, &factory, uuid.clone());
            server.handle(
                graphql_server_state(vec![success, pending]),
                &action,
                &message_data(),
                &mut DummyContext,
            );
            let metrics = get_metrics(&handle);
            assert_eq!(pending_label, success_label);
            assert_eq!(metrics[&success_metric_name(&pending_label)], 1.0);
            assert_eq!(metrics[&error_metric_name(&pending_label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&pending_label)], 1.0);
            //     two states with different label -> (1,0,0) + (1,0,0)
            let (success, success_label) = success_state(&factory);
            let (success2, success_label2) = success_state(&factory);
            server.handle(
                graphql_server_state(vec![success, success2]),
                &action,
                &message_data(),
                &mut DummyContext,
            );
            let metrics = get_metrics(&handle);
            assert_ne!(success_label, success_label2);
            assert_eq!(metrics[&success_metric_name(&success_label)], 1.0);
            assert_eq!(metrics[&error_metric_name(&success_label)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&success_label)], 0.0);
            assert_eq!(metrics[&success_metric_name(&success_label2)], 1.0);
            assert_eq!(metrics[&error_metric_name(&success_label2)], 0.0);
            assert_eq!(metrics[&pending_metric_name(&success_label2)], 0.0);
        }
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
    fn harness() -> (
        PrometheusHandle,
        SharedTermFactory<ServerBuiltins>,
        DefaultAllocator<CachedSharedTerm<ServerBuiltins>>,
        GraphQlServer<
            CachedSharedTerm<ServerBuiltins>,
            SharedTermFactory<ServerBuiltins>,
            DefaultAllocator<CachedSharedTerm<ServerBuiltins>>,
            impl Fn(&GraphQlOperation) -> String,
            impl Fn(&GraphQlOperation) -> Vec<(String, String)>,
        >,
    ) {
        let handle = PrometheusBuilder::new()
            .install_recorder()
            .expect("failed to install recorder/exporter");
        let factory = SharedTermFactory::<ServerBuiltins>::default();
        let allocator = DefaultAllocator::default();
        let metric_names = GraphQlServerMetricNames::default();
        fn get_operation_metric_labels(_op: &GraphQlOperation) -> Vec<(String, String)> {
            Vec::new()
        }

        let server = GraphQlServer::new(
            factory.clone(),
            allocator.clone(),
            metric_names,
            get_graphql_query_label,
            get_operation_metric_labels,
        );
        (handle, factory, allocator, server)
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

    fn error_state(
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
    ) -> (
        GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>,
        String,
    ) {
        let signals = allocator.create_signal_list(vec![
            allocator.create_signal(SignalType::Error, ExpressionList::new(empty()))
        ]);
        let result: Option<EvaluationResult<CachedSharedTerm<ServerBuiltins>>> = Some(
            EvaluationResult::new(factory.create_signal_term(signals), DependencyList::empty()),
        );
        let (sub_state, sub_label) = subscription_state();
        (
            GraphQlOperationState {
                operation: graphql_operation(),
                query: factory.create_nil_term(),
                label: "".to_string(),
                result,
                subscriptions: vec![sub_state],
            },
            sub_label,
        )
    }
    fn success_state_with_uuid(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
        uuid: Uuid,
    ) -> (
        GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>,
        String,
    ) {
        let result: Option<EvaluationResult<CachedSharedTerm<ServerBuiltins>>> = Some(
            EvaluationResult::new(factory.create_nil_term(), DependencyList::empty()),
        );
        let (sub_state, sub_label) = subscription_state_with_uuid(uuid);
        (
            GraphQlOperationState {
                operation: graphql_operation(),
                query: factory.create_nil_term(),
                label: "".to_string(),
                result,
                subscriptions: vec![sub_state],
            },
            sub_label,
        )
    }
    fn success_state(
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
    ) -> (
        GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>,
        String,
    ) {
        success_state_with_uuid(factory, Uuid::new_v4())
    }
    fn pending_state_with_uuid(
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
        uuid: Uuid,
    ) -> (
        GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>,
        String,
    ) {
        let signals = allocator.create_signal_list(vec![
            allocator.create_signal(SignalType::Pending, ExpressionList::new(empty()))
        ]);
        let result: Option<EvaluationResult<CachedSharedTerm<ServerBuiltins>>> = Some(
            EvaluationResult::new(factory.create_signal_term(signals), DependencyList::empty()),
        );
        let (sub_state, sub_label) = subscription_state_with_uuid(uuid);
        (
            GraphQlOperationState {
                operation: graphql_operation(),
                query: factory.create_nil_term(),
                label: "".to_string(),
                result,
                subscriptions: vec![sub_state],
            },
            sub_label,
        )
    }
    fn pending_state(
        allocator: &impl HeapAllocator<CachedSharedTerm<ServerBuiltins>>,
        factory: &impl ExpressionFactory<CachedSharedTerm<ServerBuiltins>>,
    ) -> (
        GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>,
        String,
    ) {
        pending_state_with_uuid(allocator, factory, Uuid::new_v4())
    }
    fn subscription_state() -> (GraphQlSubscriptionState, String) {
        let subscription_id = Uuid::new_v4();
        subscription_state_with_uuid(subscription_id)
    }
    fn subscription_state_with_uuid(subscription_id: Uuid) -> (GraphQlSubscriptionState, String) {
        let label = format!("subscription_id=\"{}\"", subscription_id);
        (
            GraphQlSubscriptionState {
                subscription_id,
                start_time: None,
                metric_labels: vec![(
                    "subscription_id".to_string(),
                    format!("{}", subscription_id),
                )],
            },
            label,
        )
    }
    fn graphql_server_state(
        mut op_states: Vec<GraphQlOperationState<CachedSharedTerm<ServerBuiltins>>>,
    ) -> GraphQlServerState<CachedSharedTerm<ServerBuiltins>> {
        let mut state = GraphQlServerState::default();
        state.operations.append(&mut op_states);
        state
    }
    fn graphql_operation() -> GraphQlOperation {
        GraphQlOperation::new(
            Document {
                definitions: Vec::new(),
            },
            None,
            JsonMap::default(),
            JsonMap::default(),
        )
    }
    fn graphql_subscribe_action() -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerSubscribeAction {
            subscription_id: Uuid::new_v4(),
            operation: graphql_operation(),
            _expression: PhantomData::default(),
        }
        .into()
    }
    fn graphql_unsubscribe_action() -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerSubscribeAction {
            subscription_id: Uuid::new_v4(),
            operation: graphql_operation(),
            _expression: PhantomData::default(),
        }
        .into()
    }
    fn graphql_query_emit_action() -> ServerCliAction<CachedSharedTerm<ServerBuiltins>> {
        GraphQlServerSubscribeAction {
            subscription_id: Uuid::new_v4(),
            operation: graphql_operation(),
            _expression: PhantomData::default(),
        }
        .into()
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

        fn caller_pid(&self) -> Option<ProcessId> {
            Some(ProcessId::from(0))
        }

        fn generate_pid(&mut self) -> ProcessId {
            ProcessId::from(0)
        }
    }
}
