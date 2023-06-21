// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
};

use metrics::{describe_counter, increment_counter, Unit};
use reflex::{
    core::{
        DependencyList, EvaluationResult, Expression, ExpressionFactory, HeapAllocator, Signal,
        SignalType, StateToken,
    },
    hash::HashId,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, NamedAction,
    OutboundAction, StateOperation, StateTransition,
};
use reflex_graphql::GraphQlOperationPayload;
use reflex_json::JsonValue;
use reflex_runtime::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        evaluate::EvaluateResultAction,
    },
    actor::evaluate_handler::{create_evaluate_effect, parse_evaluate_effect_query},
    QueryEvaluationMode, QueryInvalidationStrategy, StateUpdate,
};
use reflex_utils::partition_results;
use uuid::Uuid;

use crate::{
    middleware::action::telemetry::{
        TelemetryMiddlewareTransactionEndAction, TelemetryMiddlewareTransactionStartAction,
        TelemetryTransaction,
    },
    server::action::graphql_server::{
        GraphQlServerParseSuccessAction, GraphQlServerSubscribeAction,
        GraphQlServerUnsubscribeAction,
    },
    utils::{
        sanitize::sanitize_json_value,
        traceparent::{parse_traceparent, Traceparent},
    },
};

#[derive(Clone, Copy, Debug)]
pub struct TelemetryMiddlewareMetricNames {
    pub total_action_count: &'static str,
}
impl TelemetryMiddlewareMetricNames {
    fn init(self) -> Self {
        describe_counter!(self.total_action_count, Unit::Count, "Total action count");
        self
    }
}
impl Default for TelemetryMiddlewareMetricNames {
    fn default() -> Self {
        Self {
            total_action_count: "total_action_count",
        }
    }
}

pub trait TelemetryMiddlewareAction<T: Expression>:
    Action
    + NamedAction
    + InboundAction<GraphQlServerSubscribeAction<T>>
    + InboundAction<GraphQlServerUnsubscribeAction<T>>
    + InboundAction<GraphQlServerParseSuccessAction<T>>
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + InboundAction<EvaluateResultAction<T>>
    + OutboundAction<TelemetryMiddlewareTransactionStartAction>
    + OutboundAction<TelemetryMiddlewareTransactionEndAction>
{
}
impl<T: Expression, TAction> TelemetryMiddlewareAction<T> for TAction where
    Self: Action
        + NamedAction
        + InboundAction<GraphQlServerSubscribeAction<T>>
        + InboundAction<GraphQlServerUnsubscribeAction<T>>
        + InboundAction<GraphQlServerParseSuccessAction<T>>
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + OutboundAction<TelemetryMiddlewareTransactionStartAction>
        + OutboundAction<TelemetryMiddlewareTransactionEndAction>
{
}

pub struct TelemetryMiddleware<T, TFactory, TAllocator, TOperationLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TOperationLabels: Fn(&GraphQlOperationPayload) -> (String, Vec<(String, String)>),
{
    factory: TFactory,
    allocator: TAllocator,
    get_operation_transaction_labels: TOperationLabels,
    metric_names: TelemetryMiddlewareMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TOperationLabels>
    TelemetryMiddleware<T, TFactory, TAllocator, TOperationLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TOperationLabels: Fn(&GraphQlOperationPayload) -> (String, Vec<(String, String)>),
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        get_operation_transaction_labels: TOperationLabels,
        metric_names: TelemetryMiddlewareMetricNames,
    ) -> Self {
        Self {
            factory,
            allocator,
            get_operation_transaction_labels,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

pub struct TelemetryMiddlewareState<T: Expression> {
    active_queries: HashMap<Uuid, TelemetryMiddlewareQueryState>,
    effect_mappings: HashMap<HashId, TelemetryMiddlewareEffectState<T>>,
}
impl<T: Expression> Default for TelemetryMiddlewareState<T> {
    fn default() -> Self {
        Self {
            active_queries: Default::default(),
            effect_mappings: Default::default(),
        }
    }
}
struct TelemetryMiddlewareEffectState<T: Expression> {
    effect: Signal<T>,
    transaction_id: Option<Traceparent>,
    parent_transactions: HashSet<Traceparent>,
    subscription_count: usize,
    latest_value: Option<T>,
    query_result: Option<Option<EvaluationResult<T>>>,
}
impl<T: Expression> TelemetryMiddlewareEffectState<T> {
    fn end_transaction(&mut self) -> Option<Traceparent> {
        if let Some(transaction_id) = self.transaction_id.take() {
            self.parent_transactions.clear();
            Some(transaction_id)
        } else {
            None
        }
    }
}
struct TelemetryMiddlewareQueryState {
    transaction_id: Traceparent,
    effect_id: Option<HashId>,
}
impl TelemetryMiddlewareQueryState {
    fn generate_transaction_id(&self) -> Traceparent {
        self.transaction_id.generate_child()
    }
}

impl<T, TFactory, TAllocator, TOperationLabels, TAction> Actor<TAction>
    for TelemetryMiddleware<T, TFactory, TAllocator, TOperationLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TOperationLabels: Fn(&GraphQlOperationPayload) -> (String, Vec<(String, String)>),
    TAction: TelemetryMiddlewareAction<T>,
{
    type State = TelemetryMiddlewareState<T>;
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
        let metric_labels = [("type", action.name())];
        increment_counter!(self.metric_names.total_action_count, &metric_labels);
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_graphql_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_graphql_parse(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_result(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator, TOperationLabels>
    TelemetryMiddleware<T, TFactory, TAllocator, TOperationLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TOperationLabels: Fn(&GraphQlOperationPayload) -> (String, Vec<(String, String)>),
{
    fn handle_graphql_subscribe<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<TelemetryMiddlewareTransactionStartAction>,
    {
        let GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            ..
        } = action;
        let entry = match state.active_queries.entry(*subscription_id) {
            Entry::Vacant(entry) => Some(entry),
            Entry::Occupied(_) => None,
        }?;
        let traceparent = parse_graphql_operation_traceparent_extensions(operation);
        let transaction_id = traceparent
            .map(|transaction_id| transaction_id.generate_child())
            .unwrap_or_else(|| Traceparent::generate());
        let (transaction_name, transaction_attributes) =
            (self.get_operation_transaction_labels)(operation);
        entry.insert(TelemetryMiddlewareQueryState {
            transaction_id,
            effect_id: None,
        });
        let transaction_start_action = StateOperation::Send(
            context.pid(),
            TelemetryMiddlewareTransactionStartAction {
                transactions: vec![TelemetryTransaction {
                    transaction_id,
                    parent_ids: traceparent.into_iter().collect(),
                    name: transaction_name,
                    attributes: transaction_attributes,
                }],
            }
            .into(),
        );
        Some(StateTransition::new(once(transaction_start_action)))
    }
    fn handle_graphql_parse<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerParseSuccessAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let GraphQlServerParseSuccessAction {
            subscription_id,
            query,
        } = action;
        let evaluate_effect = create_evaluate_effect(
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        let query_state = state.active_queries.get_mut(subscription_id)?;
        let effect_id = evaluate_effect.id();
        let previous_effect_id = query_state.effect_id.replace(effect_id);
        let is_unchanged = previous_effect_id
            .map(|previous_effect_id| previous_effect_id == effect_id)
            .unwrap_or(false);
        if is_unchanged {
            return None;
        }
        if let Some(effect_id) = previous_effect_id {
            if let Some(effect_state) = state.effect_mappings.get_mut(&effect_id) {
                effect_state
                    .parent_transactions
                    .remove(&query_state.transaction_id);
            }
        }
        match state.effect_mappings.entry(effect_id) {
            Entry::Vacant(entry) => {
                entry.insert(TelemetryMiddlewareEffectState {
                    effect: evaluate_effect,
                    transaction_id: Some(query_state.generate_transaction_id()),
                    query_result: Some(None),
                    parent_transactions: once(query_state.transaction_id).collect(),
                    subscription_count: 0,
                    latest_value: None,
                });
            }
            Entry::Occupied(mut entry) => {
                entry
                    .get_mut()
                    .parent_transactions
                    .insert(query_state.transaction_id);
            }
        }
        None
    }
    fn handle_graphql_unsubscribe<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<TelemetryMiddlewareTransactionEndAction>,
    {
        let GraphQlServerUnsubscribeAction {
            subscription_id, ..
        } = action;
        let query_state = state.active_queries.remove(subscription_id)?;
        let TelemetryMiddlewareQueryState {
            transaction_id,
            effect_id,
            ..
        } = query_state;
        if let Some(effect_id) = effect_id {
            if let Some(effect_state) = state.effect_mappings.get_mut(&effect_id) {
                effect_state.parent_transactions.remove(&transaction_id);
            }
        }
        let transaction_end_action = StateOperation::Send(
            context.pid(),
            TelemetryMiddlewareTransactionEndAction {
                transaction_ids: vec![transaction_id],
            }
            .into(),
        );
        Some(StateTransition::new(once(transaction_end_action)))
    }
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<TelemetryMiddlewareTransactionStartAction>
            + OutboundAction<TelemetryMiddlewareTransactionEndAction>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let subscribed_transactions = effects
            .iter()
            .filter_map(|effect| match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(entry) => {
                    let transaction_id = Traceparent::generate();
                    let effect_name = format_effect_label(effect);
                    let effect_attributes = format_effect_attributes(effect);
                    entry.insert(TelemetryMiddlewareEffectState {
                        effect: effect.clone(),
                        transaction_id: Some(transaction_id),
                        parent_transactions: Default::default(),
                        query_result: parse_evaluate_effect_query(effect, &self.factory)
                            .map(|_| None),
                        subscription_count: 1,
                        latest_value: None,
                    });
                    Some(TelemetryTransaction {
                        transaction_id,
                        parent_ids: Default::default(),
                        name: effect_name,
                        attributes: effect_attributes,
                    })
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().subscription_count += 1;
                    let effect_state = entry.get();
                    let is_first_subscription = effect_state.subscription_count == 1;
                    if is_first_subscription {
                        match effect_state.transaction_id {
                            Some(transaction_id) => Some(TelemetryTransaction {
                                transaction_id,
                                parent_ids: effect_state
                                    .parent_transactions
                                    .iter()
                                    .cloned()
                                    .collect(),
                                name: format_effect_label(effect),
                                attributes: format_effect_attributes(effect),
                            }),
                            None => None,
                        }
                    } else {
                        None
                    }
                }
            })
            .collect::<Vec<_>>();
        let transaction_start_action = if subscribed_transactions.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                TelemetryMiddlewareTransactionStartAction {
                    transactions: subscribed_transactions,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(transaction_start_action))
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<TelemetryMiddlewareTransactionEndAction>,
    {
        let EffectUnsubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let unsubscribed_transaction_ids = effects
            .iter()
            .filter_map(|effect| match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(_) => None,
                Entry::Occupied(mut entry) => {
                    entry.get_mut().subscription_count -= 1;
                    if entry.get().subscription_count == 0 {
                        let effect_state = entry.remove();
                        effect_state.transaction_id
                    } else {
                        None
                    }
                }
            })
            .collect::<Vec<_>>();
        let transaction_end_action = if unsubscribed_transaction_ids.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: unsubscribed_transaction_ids,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(transaction_end_action))
    }
    fn handle_evaluate_result<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EvaluateResultAction { query, result, .. } = action;
        let evaluate_effect = create_evaluate_effect(
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        let (query_transaction_id, previous_result) = state
            .effect_mappings
            .get_mut(&evaluate_effect.id())
            .map(|query_effect_state| {
                let previous_result = query_effect_state
                    .query_result
                    .replace(Some(result.clone()));
                (query_effect_state.transaction_id, previous_result)
            })?;
        for effect in get_query_result_effects(result.result(), &self.factory) {
            match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(entry) => {
                    let effect_transaction_id = query_transaction_id
                        .map(|transaction_id| transaction_id.generate_child())
                        .unwrap_or_else(|| Traceparent::generate());
                    entry.insert(TelemetryMiddlewareEffectState {
                        effect: effect.clone(),
                        transaction_id: Some(effect_transaction_id),
                        parent_transactions: query_transaction_id.into_iter().collect(),
                        query_result: parse_evaluate_effect_query(effect, &self.factory)
                            .map(|_| None),
                        subscription_count: 0,
                        latest_value: None,
                    });
                }
                Entry::Occupied(mut entry) => {
                    let effect_state = entry.get_mut();
                    if let Some(query_transaction_id) = query_transaction_id {
                        effect_state
                            .parent_transactions
                            .insert(query_transaction_id);
                    }
                }
            }
        }
        let transaction_id = query_transaction_id?;
        let previous_dependencies = previous_result.and_then(|result| {
            result.map(|result| {
                let (_, dependencies) = result.into_parts();
                dependencies
            })
        });
        let Diff {
            added: added_dependencies,
            removed: removed_dependencies,
        } = get_dependency_diff(Some(result.dependencies()), previous_dependencies.as_ref());
        for state_token in removed_dependencies {
            if let Some(effect_state) = state.effect_mappings.get_mut(&state_token) {
                effect_state.parent_transactions.remove(&transaction_id);
            }
        }
        for state_token in added_dependencies {
            if let Some(effect_state) = state.effect_mappings.get_mut(&state_token) {
                effect_state.parent_transactions.insert(transaction_id);
            }
        }
        None
    }
    fn handle_effect_emit<TAction>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<TelemetryMiddlewareTransactionStartAction>
            + OutboundAction<TelemetryMiddlewareTransactionEndAction>,
    {
        let EffectEmitAction { updates } = action;
        let (completed_transaction_ids, reemitted_effects): (Vec<_>, Vec<_>) =
            partition_results(updates.iter().filter_map(|(effect_id, update)| {
                let update = state
                    .effect_mappings
                    .get_mut(effect_id)
                    .map(|effect_state| {
                        let value = match update {
                            StateUpdate::Value(value) => value.clone(),
                            StateUpdate::Patch(updater) => {
                                updater(effect_state.latest_value.as_ref())
                            }
                        };
                        effect_state.latest_value.replace(value);
                        if effect_state.query_result.is_some() {
                            let completed_transaction_id = effect_state.end_transaction();
                            Ok(completed_transaction_id)
                        } else {
                            match effect_state.end_transaction() {
                                Some(completed_transaction_id) => {
                                    Ok(Some(completed_transaction_id))
                                }
                                None => Err(effect_state.effect.clone()),
                            }
                        }
                    })?;
                match update {
                    Err(effect) => Some(Err(effect)),
                    Ok(completed_transaction_id) => match completed_transaction_id {
                        None => None,
                        Some(completed_transaction_id) => {
                            for effect_state in state.effect_mappings.values_mut() {
                                effect_state
                                    .parent_transactions
                                    .remove(&completed_transaction_id);
                            }
                            Some(Ok(completed_transaction_id))
                        }
                    },
                }
            }));
        let completed_transactions_end_action = if completed_transaction_ids.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: completed_transaction_ids,
                }
                .into(),
            ))
        };
        let (effect_start_transactions, effect_end_transaction_ids) = if reemitted_effects
            .is_empty()
        {
            (Vec::new(), Vec::new())
        } else {
            let update_transaction_id = Traceparent::generate();
            let update_transaction = TelemetryTransaction {
                transaction_id: update_transaction_id,
                parent_ids: Default::default(),
                name: format_async_update_batch_label(reemitted_effects.len()),
                attributes: Default::default(),
            };
            let (updated_effect_transaction_ids, effect_transactions): (Vec<_>, Vec<_>) =
                reemitted_effects
                    .into_iter()
                    .map(move |effect| {
                        let effect_transaction_id = update_transaction_id.generate_child();
                        (
                            (effect.id(), effect_transaction_id),
                            TelemetryTransaction {
                                transaction_id: effect_transaction_id,
                                parent_ids: once(update_transaction_id).collect(),
                                name: format_effect_label(&effect),
                                attributes: format_effect_attributes(&effect),
                            },
                        )
                    })
                    .unzip();
            let dependent_query_transactions =
                state.active_queries.values().filter_map(|query_state| {
                    let query_effect_id = query_state.effect_id?;
                    let query_effect_state = state.effect_mappings.get_mut(&query_effect_id)?;
                    let latest_query_result = query_effect_state.query_result.as_ref()?.as_ref()?;
                    let query_dependencies = latest_query_result.dependencies();
                    if query_dependencies.is_empty() {
                        return None;
                    }
                    // TODO: [perf] Investigate less brute-force approaches for determining affected queries
                    let updated_dependency_ids = updated_effect_transaction_ids
                        .iter()
                        .filter_map(|(effect_id, transaction_id)| {
                            if query_dependencies.contains(*effect_id) {
                                Some(*transaction_id)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    query_effect_state
                        .parent_transactions
                        .extend(updated_dependency_ids.iter().copied());
                    if query_effect_state.transaction_id.is_none() {
                        let query_transaction_id = updated_dependency_ids.first()?.generate_child();
                        query_effect_state.transaction_id = Some(query_transaction_id);
                        Some(TelemetryTransaction {
                            transaction_id: query_transaction_id,
                            parent_ids: updated_dependency_ids,
                            name: format_effect_label(&query_effect_state.effect),
                            attributes: format_effect_attributes(&query_effect_state.effect),
                        })
                    } else {
                        return None;
                    }
                });
            let end_transaction_ids = effect_transactions
                .iter()
                .chain(once(&update_transaction))
                .map(|transaction| transaction.transaction_id)
                .collect::<Vec<_>>();
            let start_transactions = once(update_transaction)
                .chain(effect_transactions)
                .chain(dependent_query_transactions)
                .collect::<Vec<_>>();
            (start_transactions, end_transaction_ids)
        };
        let effect_transaction_start_action = if effect_start_transactions.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                TelemetryMiddlewareTransactionStartAction {
                    transactions: effect_start_transactions,
                }
                .into(),
            ))
        };
        // TODO: Defer completion of effect update transaction until all dependent queries have either emitted a result or removed current effect from dependencies
        let effect_transaction_end_action = if effect_end_transaction_ids.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: effect_end_transaction_ids,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(
            completed_transactions_end_action
                .into_iter()
                .chain(effect_transaction_start_action)
                .chain(effect_transaction_end_action),
        ))
    }
}

fn get_query_result_effects<'a, T: Expression>(
    result: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> impl Iterator<Item = &'a Signal<T>> + 'a {
    factory
        .match_signal_term(result)
        .map(|term| {
            term.signals()
                .iter()
                .filter(|signal| matches!(signal.signal_type(), SignalType::Custom(_)))
        })
        .into_iter()
        .flatten()
}

struct Diff<T1, T2> {
    added: T1,
    removed: T2,
}
fn get_dependency_diff<'a>(
    current_dependencies: Option<&'a DependencyList>,
    previous_dependencies: Option<&'a DependencyList>,
) -> Diff<impl IntoIterator<Item = StateToken> + 'a, impl IntoIterator<Item = StateToken> + 'a> {
    let added_dependencies = {
        let (left, right) = match (previous_dependencies, current_dependencies) {
            (None, Some(current_dependencies)) => (Some(current_dependencies), None),
            (Some(previous_dependencies), Some(current_dependencies)) => (
                None,
                Some(
                    current_dependencies
                        .iter()
                        .filter(|state_token| !previous_dependencies.contains(*state_token)),
                ),
            ),
            _ => (None, None),
        };
        left.into_iter()
            .flatten()
            .chain(right.into_iter().flatten())
    };
    let removed_dependencies = {
        let (left, right) = match (previous_dependencies, current_dependencies) {
            (Some(previous_dependencies), None) => (Some(previous_dependencies), None),
            (Some(previous_dependencies), Some(current_dependencies)) => (
                None,
                Some(
                    previous_dependencies
                        .iter()
                        .filter(|state_token| !current_dependencies.contains(*state_token)),
                ),
            ),
            _ => (None, None),
        };
        left.into_iter()
            .flatten()
            .chain(right.into_iter().flatten())
    };
    Diff {
        added: added_dependencies,
        removed: removed_dependencies,
    }
}

fn parse_graphql_operation_traceparent_extensions(
    operation: &GraphQlOperationPayload,
) -> Option<Traceparent> {
    match operation.extension("traceparent") {
        Some(JsonValue::String(value)) => parse_traceparent(value),
        _ => None,
    }
}

fn format_effect_label<T: Expression>(effect: &Signal<T>) -> String {
    match effect.signal_type() {
        SignalType::Custom(signal_type) => format!("{}", signal_type),
        signal_type => format!("{}", signal_type),
    }
}

fn format_async_update_batch_label(batch_size: usize) -> String {
    if batch_size > 1 {
        format!("Async update x{}", batch_size)
    } else {
        String::from("Async update")
    }
}

fn format_effect_attributes<T: Expression>(effect: &Signal<T>) -> Vec<(String, String)> {
    vec![
        (String::from("effect.id"), format!("{}", effect.id())),
        (String::from("effect.type"), format_effect_label(effect)),
        (
            String::from("effect.args"),
            sanitize_json_value(JsonValue::Array(
                effect
                    .args()
                    .iter()
                    .map(|arg| {
                        reflex_json::sanitize(arg).unwrap_or_else(|_| {
                            JsonValue::String(format!("<expression:{}>", arg.id()))
                        })
                    })
                    .collect(),
            ))
            .to_string(),
        ),
    ]
}
