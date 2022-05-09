// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, HashSet, VecDeque},
    iter::once,
    marker::PhantomData,
    sync::Once,
    time::Instant,
};

use metrics::{
    counter, decrement_gauge, describe_counter, describe_gauge, describe_histogram, gauge,
    histogram, increment_gauge, Unit,
};
use reflex::{
    core::{
        DependencyList, DynamicState, EvaluationResult, Expression, ExpressionFactory,
        HeapAllocator, Signal, SignalType, StateCache, StateToken,
    },
    lang::ValueTerm,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, MessageOffset, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_utils::partition_results;

use crate::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        evaluate::{EvaluateResultAction, EvaluateStartAction, EvaluateStopAction},
    },
    QueryEvaluationMode, QueryInvalidationStrategy, StateUpdate,
};

pub const EFFECT_TYPE_EVALUATE: &'static str = "reflex::core::evaluate";

pub const METRIC_STATE_ENTRY_COUNT: &'static str = "state_entry_count";
pub const METRIC_QUEUED_UPDATE_BATCH_COUNT: &'static str = "state_pending_update_batch_count";
pub const METRIC_STATE_GC_DURATION: &'static str = "state_gc_duration";
pub const METRIC_TOTAL_EFFECT_COUNT: &'static str = "total_effect_count";
pub const METRIC_ACTIVE_EFFECT_COUNT: &'static str = "active_effect_count";
pub const METRIC_ACTIVE_QUERY_WORKER_COUNT: &'static str = "active_query_worker_count";
pub const METRIC_PENDING_QUERY_WORKER_COUNT: &'static str = "pending_query_worker_count";
pub const METRIC_ERROR_QUERY_WORKER_COUNT: &'static str = "error_query_worker_count";
pub const METRIC_BLOCKED_QUERY_WORKER_COUNT: &'static str = "blocked_query_worker_count";
pub const METRIC_QUERY_WORKER_CACHE_ENTRY_COUNT: &'static str =
    "active_query_worker_cache_entry_count";

static INIT_METRICS: Once = Once::new();

fn init_metrics() {
    INIT_METRICS.call_once(|| {
        describe_gauge!(
            METRIC_STATE_ENTRY_COUNT,
            Unit::Count,
            "Active global state entry count"
        );
        describe_gauge!(
            METRIC_QUEUED_UPDATE_BATCH_COUNT,
            Unit::Count,
            "Queued worker state update batch count"
        );
        describe_histogram!(
            METRIC_STATE_GC_DURATION,
            Unit::Seconds,
            "Global state garbage collection duration (seconds)"
        );
        describe_counter!(METRIC_TOTAL_EFFECT_COUNT, Unit::Count, "Total effect count");
        describe_gauge!(
            METRIC_ACTIVE_EFFECT_COUNT,
            Unit::Count,
            "Active effect count"
        );
        describe_gauge!(
            METRIC_ACTIVE_QUERY_WORKER_COUNT,
            Unit::Count,
            "Active query worker count"
        );
        describe_gauge!(
            METRIC_PENDING_QUERY_WORKER_COUNT,
            Unit::Count,
            "Pending query worker count"
        );
        describe_gauge!(
            METRIC_ERROR_QUERY_WORKER_COUNT,
            Unit::Count,
            "Errored query worker count"
        );
        describe_gauge!(
            METRIC_BLOCKED_QUERY_WORKER_COUNT,
            Unit::Count,
            "Blocked query worker count"
        );
        describe_gauge!(
            METRIC_QUERY_WORKER_CACHE_ENTRY_COUNT,
            Unit::Count,
            "Active query worker cache entry count"
        );
    });
}

pub fn create_evaluate_effect<T: Expression>(
    query: T,
    evaluation_mode: QueryEvaluationMode,
    invalidation_strategy: QueryInvalidationStrategy,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Signal<T> {
    allocator.create_signal(
        SignalType::Custom(String::from(EFFECT_TYPE_EVALUATE)),
        allocator.create_triple(
            query,
            evaluation_mode.serialize(factory),
            invalidation_strategy.serialize(factory),
        ),
    )
}

pub fn parse_evaluate_effect_query<'a, T: Expression>(
    effect: &'a Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Option<(&'a T, QueryEvaluationMode, QueryInvalidationStrategy)> {
    if effect.args().len() != 3 {
        return None;
    }
    let mut args = effect.args().iter();
    let query = args.next().unwrap();
    let evaluation_mode = args.next().unwrap();
    let invalidation_strategy = args.next().unwrap();
    match (
        QueryEvaluationMode::deserialize(evaluation_mode, factory),
        QueryInvalidationStrategy::deserialize(invalidation_strategy, factory),
    ) {
        (Some(evaluation_mode), Some(invalidation_strategy)) => {
            Some((query, evaluation_mode, invalidation_strategy))
        }
        _ => None,
    }
}

fn create_evaluate_effect_result<T: Expression>(
    result: &EvaluationResult<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_tuple_term(
        allocator.create_pair(
            result.result().clone(),
            factory.create_vector_term(
                allocator.create_list(
                    result
                        .dependencies()
                        .iter()
                        .map(|state_token| factory.create_value_term(ValueTerm::Hash(state_token))),
                ),
            ),
        ),
    )
}

pub fn parse_evaluate_effect_result<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<EvaluationResult<T>> {
    let tuple = factory.match_tuple_term(value)?;
    let value = tuple.get(0)?;
    let dependencies = factory
        .match_vector_term(tuple.get(1)?)?
        .items()
        .iter()
        .filter_map(|dependency| {
            factory
                .match_value_term(dependency)
                .and_then(|dependency| dependency.match_hash())
        });
    Some(EvaluationResult::new(
        value.clone(),
        DependencyList::from(dependencies),
    ))
}

pub trait EvaluateHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + InboundAction<EvaluateResultAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + OutboundAction<EffectSubscribeAction<T>>
    + OutboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
    + OutboundAction<EvaluateStartAction<T>>
    + OutboundAction<EvaluateStopAction<T>>
{
}
impl<T: Expression, TAction> EvaluateHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + InboundAction<EvaluateResultAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + OutboundAction<EffectSubscribeAction<T>>
        + OutboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
        + OutboundAction<EvaluateStartAction<T>>
        + OutboundAction<EvaluateStopAction<T>>
{
}

pub(crate) struct EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    state: EvaluateHandlerState<T>,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub(crate) fn new(factory: TFactory, allocator: TAllocator) -> Self {
        init_metrics();
        Self {
            factory,
            allocator,
            state: Default::default(),
            _expression: Default::default(),
        }
    }
}

struct EvaluateHandlerState<T: Expression> {
    workers: HashMap<StateToken, WorkerState<T>>,
    // TODO: Use expressions as state tokens, removing need to map state tokens back to originating effects
    effects: HashMap<StateToken, Signal<T>>,
    state_cache: GlobalStateCache<T>,
}
impl<T: Expression> Default for EvaluateHandlerState<T> {
    fn default() -> Self {
        Self {
            workers: Default::default(),
            effects: Default::default(),
            state_cache: Default::default(),
        }
    }
}
struct WorkerState<T: Expression> {
    subscription_count: usize,
    effect: Signal<T>,
    query: T,
    evaluation_mode: QueryEvaluationMode,
    invalidation_strategy: QueryInvalidationStrategy,
    status: WorkerStatus<T>,
    state_index: Option<MessageOffset>,
    state_values: HashMap<StateToken, T>,
    metric_labels: [(&'static str, String); 1],
}
struct GlobalStateCache<T: Expression> {
    state_index: Option<MessageOffset>,
    combined_state: StateCache<T>,
    update_batches: VecDeque<(MessageOffset, Vec<(StateToken, T)>)>,
}
impl<T: Expression> Default for GlobalStateCache<T> {
    fn default() -> Self {
        Self {
            state_index: Default::default(),
            combined_state: Default::default(),
            update_batches: Default::default(),
        }
    }
}
enum WorkerStatus<T: Expression> {
    Busy {
        previous_result: Option<(Option<MessageOffset>, EvaluationResult<T>)>,
    },
    Idle {
        latest_result: (Option<MessageOffset>, EvaluationResult<T>),
    },
}
enum WorkerResultStatus {
    Active,
    Pending,
    Error,
    Blocked,
}
impl<T: Expression> EvaluateHandlerState<T> {
    fn combined_effects<'a>(&'a self) -> impl Iterator<Item = &'a Signal<T>> + 'a {
        self.workers.values().flat_map(|worker| {
            once(&worker.effect).into_iter().chain(
                worker
                    .dependencies_iter()
                    .flat_map(|state_token| self.effects.get(&state_token)),
            )
        })
    }
    fn gc_worker_state_history(&mut self) {
        let oldest_active_state_index = self
            .workers
            .values()
            .filter_map(|worker| worker.state_index)
            .min();
        self.state_cache
            .delete_outdated_update_batches(oldest_active_state_index);
    }
    fn update_worker_status_metrics(&self, factory: &impl ExpressionFactory<T>) {
        let (num_pending_workers, num_error_workers, num_blocked_workers) =
            self.workers.iter().fold(
                (0, 0, 0),
                |(num_pending, num_error, num_blocked), (_, worker_state)| match worker_state
                    .current_result_status(factory)
                {
                    WorkerResultStatus::Active => (num_pending, num_error, num_blocked),
                    WorkerResultStatus::Pending => (num_pending + 1, num_error, num_blocked),
                    WorkerResultStatus::Error => (num_pending, num_error + 1, num_blocked),
                    WorkerResultStatus::Blocked => (num_pending, num_error, num_blocked + 1),
                },
            );
        gauge!(
            METRIC_PENDING_QUERY_WORKER_COUNT,
            num_pending_workers as f64
        );
        gauge!(METRIC_ERROR_QUERY_WORKER_COUNT, num_error_workers as f64);
        gauge!(
            METRIC_BLOCKED_QUERY_WORKER_COUNT,
            num_blocked_workers as f64
        );
    }
}
impl<T: Expression> WorkerState<T> {
    fn latest_result(&self) -> Option<&EvaluationResult<T>> {
        match &self.status {
            WorkerStatus::Busy {
                previous_result: Some((_, result)),
            } => Some(result),
            WorkerStatus::Idle {
                latest_result: (_, result),
            } => Some(result),
            _ => None,
        }
    }
    fn current_result_status(&self, factory: &impl ExpressionFactory<T>) -> WorkerResultStatus {
        match self.latest_result() {
            None => WorkerResultStatus::Blocked,
            Some(result) => match factory.match_signal_term(result.result()) {
                None => WorkerResultStatus::Active,
                Some(signal) => {
                    if signal
                        .signals()
                        .iter()
                        .any(|signal| matches!(&signal.signal_type(), SignalType::Error))
                    {
                        WorkerResultStatus::Error
                    } else if signal
                        .signals()
                        .iter()
                        .any(|signal| matches!(&signal.signal_type(), SignalType::Pending))
                    {
                        WorkerResultStatus::Pending
                    } else {
                        WorkerResultStatus::Blocked
                    }
                }
            },
        }
    }
    fn dependencies(&self) -> Option<&DependencyList> {
        self.latest_result().map(|result| result.dependencies())
    }
    fn dependencies_iter(&self) -> impl Iterator<Item = StateToken> + '_ {
        self.dependencies()
            .into_iter()
            .flat_map(|dependencies| dependencies)
    }
    fn update_state_cache<'a>(
        &'a mut self,
        state_index: Option<MessageOffset>,
        updates: impl IntoIterator<
            Item = (StateToken, T),
            IntoIter = impl Iterator<Item = (StateToken, T)> + 'a,
        >,
    ) -> impl Iterator<Item = (StateToken, T)> + 'a {
        self.state_index = state_index;
        updates.into_iter().filter_map(|(state_token, value)| {
            match self.state_values.entry(state_token) {
                Entry::Occupied(mut entry) => {
                    if entry.get().id() == value.id() {
                        None
                    } else {
                        entry.insert(value.clone());
                        Some((state_token, value))
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(value.clone());
                    Some((state_token, value))
                }
            }
        })
    }
}
impl<T: Expression> GlobalStateCache<T> {
    fn apply_batch(&mut self, state_index: MessageOffset, updates: Vec<(StateToken, T)>) {
        self.state_index = Some(state_index);
        for (key, value) in updates.iter() {
            self.combined_state.set(*key, value.clone());
        }
        self.update_batches.push_back((state_index, updates));
        self.update_state_cache_metrics();
    }
    fn get_current_value(&self, state_token: StateToken) -> Option<(StateToken, T)> {
        self.combined_state
            .get(&state_token)
            .map(|value| (state_token, value.clone()))
    }
    fn get_updates_since<'a>(
        &'a self,
        state_index: Option<MessageOffset>,
        dependencies: &'a DependencyList,
    ) -> impl Iterator<Item = (StateToken, T)> + 'a {
        self.update_batches
            .iter()
            .skip_while(move |(offset, _)| {
                if let Some(state_index) = state_index {
                    *offset <= state_index
                } else {
                    false
                }
            })
            .flat_map(|(_, updates)| {
                updates
                    .iter()
                    .filter(|(state_token, _)| dependencies.contains(*state_token))
                    .map(|(state_token, value)| (*state_token, value.clone()))
            })
    }
    fn delete_outdated_update_batches(&mut self, active_state_index: Option<MessageOffset>) {
        if let Some(state_index) = active_state_index {
            while self
                .update_batches
                .get(0)
                .map(|(cached_index, _)| *cached_index <= state_index)
                .unwrap_or(false)
            {
                self.update_batches.pop_front();
            }
        } else {
            self.update_batches.clear();
        }
        self.update_state_cache_metrics();
    }
    fn gc(&mut self, retained_keys: impl IntoIterator<Item = StateToken>) {
        // TODO: [perf] Compare performance of rebuilding new state cache vs removing keys from existing cache
        let start_time = Instant::now();
        self.combined_state = retained_keys
            .into_iter()
            .filter_map(|state_token| {
                self.combined_state
                    .get(&state_token)
                    .map(|value| (state_token, value.clone()))
            })
            .collect();
        let elapsed_time = start_time.elapsed();
        histogram!(METRIC_STATE_GC_DURATION, elapsed_time.as_secs_f64());
        self.update_state_cache_metrics();
    }
    fn update_state_cache_metrics(&self) {
        gauge!(METRIC_STATE_ENTRY_COUNT, self.combined_state.len() as f64);
        gauge!(
            METRIC_QUEUED_UPDATE_BATCH_COUNT,
            self.update_batches.len() as f64
        );
    }
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: EvaluateHandlerAction<T>,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_evaluate_result(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default()
    }
}
impl<T, TFactory, TAllocator> EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &mut self,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction:
            Action + OutboundAction<EvaluateStartAction<T>> + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        let metric_labels = [("effect_type", String::from(effect_type))];
        counter!(
            METRIC_TOTAL_EFFECT_COUNT,
            effects.len() as u64,
            &metric_labels
        );
        increment_gauge!(
            METRIC_ACTIVE_EFFECT_COUNT,
            effects.len() as f64,
            &metric_labels
        );
        if effect_type != EFFECT_TYPE_EVALUATE {
            return None;
        }
        let queries = effects.iter().filter_map(|effect| {
            parse_evaluate_effect_query(effect, &self.factory).map(|query| (effect, query))
        });
        let current_pid = context.pid();
        let (evaluate_start_actions, existing_results): (Vec<_>, Vec<_>) =
            partition_results(queries.filter_map(
                |(effect, (query, evaluation_mode, invalidation_strategy))| {
                    let cache_key = effect.id();
                    match self.state.workers.entry(cache_key) {
                        // For any queries that are already subscribed, re-emit the latest cached value if one exists
                        // (this is necessary because the caller that triggered this action might be expecting a result)
                        Entry::Occupied(mut entry) => {
                            let worker = entry.get_mut();
                            worker.subscription_count += 1;
                            worker.latest_result().map(|result| {
                                Err((cache_key, StateUpdate::Value(result.result().clone())))
                            })
                        }
                        // For any queries that are not yet subscribed, kick off evaluation of that query
                        Entry::Vacant(entry) => {
                            increment_gauge!(METRIC_ACTIVE_QUERY_WORKER_COUNT, 1.0);
                            let metric_labels = [("worker_id", format!("{}", effect.id()))];
                            gauge!(METRIC_QUERY_WORKER_CACHE_ENTRY_COUNT, 0.0, &metric_labels);
                            entry.insert(WorkerState {
                                subscription_count: 1,
                                effect: effect.clone(),
                                query: query.clone(),
                                evaluation_mode,
                                invalidation_strategy,
                                status: WorkerStatus::Busy {
                                    previous_result: None,
                                },
                                state_index: None,
                                state_values: Default::default(),
                                metric_labels,
                            });
                            Some(Ok(StateOperation::Send(
                                current_pid,
                                EvaluateStartAction {
                                    cache_key,
                                    query: query.clone(),
                                    evaluation_mode,
                                    invalidation_strategy,
                                    state_index: None,
                                    state_updates: Default::default(),
                                }
                                .into(),
                            )))
                        }
                    }
                },
            ));
        let emit_cached_results_action = if existing_results.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                current_pid,
                EffectEmitAction {
                    updates: existing_results,
                }
                .into(),
            ))
        };
        Some(StateTransition::new(
            emit_cached_results_action
                .into_iter()
                .chain(evaluate_start_actions),
        ))
    }
    fn handle_effect_unsubscribe<TAction>(
        &mut self,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<EvaluateStopAction<T>>
            + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        decrement_gauge!(METRIC_ACTIVE_EFFECT_COUNT, effects.len() as f64, "effect_type" => String::from(effect_type));
        if effect_type != EFFECT_TYPE_EVALUATE {
            return None;
        }
        let unsubscribed_workers = effects
            .iter()
            .flat_map(|effect| {
                let cache_key = effect.id();
                let mut existing_entry = match self.state.workers.entry(cache_key) {
                    Entry::Occupied(entry) => Some(entry),
                    _ => None,
                }?;
                let updated_subscription_count = {
                    let mut worker = existing_entry.get_mut();
                    worker.subscription_count -= 1;
                    worker.subscription_count
                };
                if updated_subscription_count == 0 {
                    let (_, subscription) = existing_entry.remove_entry();
                    decrement_gauge!(METRIC_ACTIVE_QUERY_WORKER_COUNT, 1.0);
                    gauge!(
                        METRIC_QUERY_WORKER_CACHE_ENTRY_COUNT,
                        0.0,
                        &subscription.metric_labels,
                    );
                    Some((cache_key, subscription))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let remaining_effect_ids = self
            .state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let removed_worker_effects = unsubscribed_workers
            .iter()
            .map(|(_, worker)| &worker.effect)
            .filter(|effect| !remaining_effect_ids.contains(&effect.id()));
        for effect in removed_worker_effects {
            self.state.effects.remove(&effect.id());
        }
        let removed_effects = unsubscribed_workers
            .iter()
            .flat_map(|(_, worker)| worker.dependencies_iter())
            .filter(|state_token| !remaining_effect_ids.contains(&state_token))
            .filter_map(|removed_effect_id| self.state.effects.remove(&removed_effect_id));
        let removed_queries = unsubscribed_workers
            .iter()
            .map(|(cache_key, worker)| (*cache_key, worker.query.clone()));
        let stop_actions = removed_queries.map(|(cache_key, query)| {
            StateOperation::Send(
                context.pid(),
                EvaluateStopAction { cache_key, query }.into(),
            )
        });
        let unsubscribe_actions =
            group_effects_by_type(removed_effects).map(|(effect_type, effects)| {
                StateOperation::Send(
                    context.pid(),
                    EffectUnsubscribeAction {
                        effect_type,
                        effects,
                    }
                    .into(),
                )
            });
        let actions = stop_actions.chain(unsubscribe_actions).collect::<Vec<_>>();
        let has_unsubscribed_effects = !actions.is_empty();
        if has_unsubscribed_effects {
            self.state.state_cache.gc(remaining_effect_ids);
        }
        if !unsubscribed_workers.is_empty() {
            self.state.gc_worker_state_history();
            self.state.update_worker_status_metrics(&self.factory);
        }
        Some(StateTransition::new(actions))
    }
    fn handle_evaluate_result<TAction>(
        &mut self,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<EffectSubscribeAction<T>>
            + OutboundAction<EffectUnsubscribeAction<T>>
            + OutboundAction<EvaluateStartAction<T>>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let EvaluateResultAction {
            cache_key,
            query: _,
            state_index,
            result,
        } = action;
        let worker_state_index = self
            .state
            .workers
            .get(cache_key)
            .map(|worker| worker.state_index)?;
        let is_outdated_action = match (state_index, worker_state_index) {
            (Some(state_index), Some(worker_state_index)) => worker_state_index > *state_index,
            _ => false,
        };
        if is_outdated_action {
            return None;
        }
        let existing_effect_ids = self
            .state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let worker = self.state.workers.get_mut(cache_key)?;
        let added_effects = parse_expression_effects(result.result(), &self.factory)
            .filter(|effect| !existing_effect_ids.contains(&effect.id()))
            .cloned()
            .collect::<Vec<_>>();
        for effect in added_effects.iter() {
            self.state.effects.insert(effect.id(), effect.clone());
        }
        let updated_status = WorkerStatus::Idle {
            latest_result: (worker_state_index, result.clone()),
        };
        let previous_status = std::mem::replace(&mut worker.status, updated_status);
        let previous_dependencies = match previous_status {
            WorkerStatus::Idle {
                latest_result: (_, result),
            } => {
                let (_, dependencies) = result.into_parts();
                Some(dependencies)
            }
            WorkerStatus::Busy { previous_result } => previous_result.map(|(_, result)| {
                let (_, dependencies) = result.into_parts();
                dependencies
            }),
        };
        let reevaluate_action = {
            let reevaluate_action = update_worker_state(
                worker,
                match previous_dependencies {
                    None => WorkerStateUpdateType::FirstResult,
                    Some(dependencies) => WorkerStateUpdateType::SubsequentResult {
                        previous_dependencies: dependencies,
                    },
                },
                &mut self.state.state_cache,
                context,
            );
            self.state.gc_worker_state_history();
            reevaluate_action
        };
        let effect_emit_action: Option<StateOperation<TAction>> =
            if is_pending_result(result.result(), &self.factory) {
                None
            } else {
                Some(StateOperation::Send(
                    context.pid(),
                    EffectEmitAction {
                        updates: vec![(
                            *cache_key,
                            StateUpdate::Value(create_evaluate_effect_result(
                                result,
                                &self.factory,
                                &self.allocator,
                            )),
                        )],
                    }
                    .into(),
                ))
            };
        let updated_effect_ids = self
            .state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let removed_effects = existing_effect_ids
            .iter()
            .filter(|effect_id| !updated_effect_ids.contains(effect_id))
            .filter_map(|effect_id| self.state.effects.remove(&effect_id))
            .collect::<Vec<_>>();
        let effect_subscribe_actions =
            group_effects_by_type(added_effects).map(|(effect_type, effects)| {
                StateOperation::Send(
                    context.pid(),
                    EffectSubscribeAction {
                        effect_type,
                        effects,
                    }
                    .into(),
                )
            });
        let effect_unsubscribe_actions =
            group_effects_by_type(removed_effects).map(|(effect_type, effects)| {
                StateOperation::Send(
                    context.pid(),
                    EffectUnsubscribeAction {
                        effect_type,
                        effects,
                    }
                    .into(),
                )
            });
        let actions = effect_emit_action
            .into_iter()
            .chain(reevaluate_action)
            .chain(effect_subscribe_actions)
            .chain(effect_unsubscribe_actions)
            .collect::<Vec<_>>();
        self.state.update_worker_status_metrics(&self.factory);
        Some(StateTransition::new(actions))
    }
    fn handle_effect_emit<TAction>(
        &mut self,
        action: &EffectEmitAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EvaluateStartAction<T>>,
    {
        let EffectEmitAction { updates } = action;
        let (updated_state_tokens, updates) = if updates.is_empty() {
            (HashSet::<StateToken>::default(), Vec::default())
        } else {
            let existing_state = &self.state.state_cache.combined_state;
            let updates = updates.iter().filter_map(|(state_token, update)| {
                let updated_value = match update {
                    StateUpdate::Value(value) => value.clone(),
                    StateUpdate::Patch(updater) => {
                        updater(self.state.state_cache.combined_state.get(state_token))
                    }
                };
                let is_unchanged = existing_state
                    .get(state_token)
                    .map(|existing_value| updated_value.id() == existing_value.id())
                    .unwrap_or(false);
                if is_unchanged {
                    None
                } else {
                    Some((*state_token, updated_value))
                }
            });
            let (updated_state_tokens, updates) = updates
                .map(|(state_token, update)| (state_token, (state_token, update)))
                .unzip();
            (updated_state_tokens, updates)
        };
        if updated_state_tokens.is_empty() {
            return None;
        }
        let state_index = metadata.offset;
        self.state.state_cache.apply_batch(state_index, updates);
        let invalidated_workers =
            self.state
                .workers
                .values_mut()
                .filter(|worker| match &worker.status {
                    WorkerStatus::Idle {
                        latest_result: (_, result),
                    } => result
                        .dependencies()
                        .iter()
                        .any(|state_token| updated_state_tokens.contains(&state_token)),
                    _ => false,
                });
        let worker_update_actions = invalidated_workers
            .filter_map(|worker| {
                update_worker_state(
                    worker,
                    WorkerStateUpdateType::DependencyUpdate,
                    &mut self.state.state_cache,
                    context,
                )
            })
            .collect::<Vec<_>>();
        self.state.gc_worker_state_history();
        Some(StateTransition::new(worker_update_actions))
    }
}

enum WorkerStateUpdateType {
    FirstResult,
    SubsequentResult {
        previous_dependencies: DependencyList,
    },
    DependencyUpdate,
}
fn update_worker_state<T: Expression, TAction>(
    worker: &mut WorkerState<T>,
    update_type: WorkerStateUpdateType,
    global_state: &mut GlobalStateCache<T>,
    context: &impl HandlerContext,
) -> Option<StateOperation<TAction>>
where
    TAction: Action + OutboundAction<EvaluateStartAction<T>>,
{
    let dependencies = worker.dependencies().cloned()?;
    let updates = match update_type {
        WorkerStateUpdateType::FirstResult => {
            // Insert existing global state values for all dependencies
            worker
                .update_state_cache(
                    global_state.state_index,
                    dependencies
                        .iter()
                        .filter_map(|state_token| global_state.get_current_value(state_token)),
                )
                .collect::<Vec<_>>()
        }
        WorkerStateUpdateType::SubsequentResult {
            previous_dependencies,
        } => {
            // Insert state values for any newly-added dependencies not present in the last evaluation,
            // as well as any dependencies whose values have changed since the last evaluation
            let added_dependencies = dependencies
                .iter()
                .filter(|state_token| !previous_dependencies.contains(*state_token));
            let added_state_values = added_dependencies
                .filter_map(|state_token| global_state.get_current_value(state_token));
            let updated_state_values =
                global_state.get_updates_since(worker.state_index, &dependencies);
            worker
                .update_state_cache(
                    global_state.state_index,
                    added_state_values.chain(updated_state_values),
                )
                .collect::<Vec<_>>()
        }
        WorkerStateUpdateType::DependencyUpdate => {
            // Update any dependencies whose values have changed since the last evaluation
            let updates = global_state.get_updates_since(worker.state_index, &dependencies);
            worker
                .update_state_cache(global_state.state_index, updates)
                .collect::<Vec<_>>()
        }
    };
    if updates.is_empty() {
        return None;
    }
    gauge!(
        METRIC_QUERY_WORKER_CACHE_ENTRY_COUNT,
        worker.state_values.len() as f64,
        &worker.metric_labels
    );
    let existing_status = std::mem::replace(
        &mut worker.status,
        WorkerStatus::Busy {
            previous_result: None,
        },
    );
    worker.status = match existing_status {
        WorkerStatus::Busy { previous_result } => WorkerStatus::Busy { previous_result },
        WorkerStatus::Idle { latest_result } => WorkerStatus::Busy {
            previous_result: Some(latest_result),
        },
    };
    Some(StateOperation::Send(
        context.pid(),
        EvaluateStartAction {
            cache_key: worker.effect.id(),
            query: worker.query.clone(),
            evaluation_mode: worker.evaluation_mode,
            invalidation_strategy: worker.invalidation_strategy,
            state_index: worker.state_index,
            state_updates: updates,
        }
        .into(),
    ))
}

fn group_effects_by_type<T: Expression>(
    signals: impl IntoIterator<Item = Signal<T>>,
) -> impl Iterator<Item = (String, Vec<Signal<T>>)> {
    signals
        .into_iter()
        .filter(|signal| matches!(signal.signal_type(), SignalType::Custom(_)))
        .fold(
            HashMap::<String, Vec<Signal<T>>>::new(),
            |mut result, signal| {
                let existing_signals = get_custom_signal_type(&signal)
                    .and_then(|signal_type| result.get_mut(signal_type));
                if let Some(existing_signals) = existing_signals {
                    existing_signals.push(signal);
                } else if let Some(signal_type) = get_custom_signal_type(&signal) {
                    result.insert(String::from(signal_type), vec![signal]);
                }
                result
            },
        )
        .into_iter()
}

fn get_custom_signal_type<T: Expression>(signal: &Signal<T>) -> Option<&String> {
    match signal.signal_type() {
        SignalType::Custom(signal_type) => Some(signal_type),
        _ => None,
    }
}

fn is_pending_result<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> bool {
    factory
        .match_signal_term(value)
        .map(|signal| {
            signal
                .signals()
                .iter()
                .any(|signal| is_pending_signal(signal))
        })
        .unwrap_or(false)
}

fn is_pending_signal<T: Expression>(value: &Signal<T>) -> bool {
    match value.signal_type() {
        SignalType::Error => false,
        SignalType::Pending | SignalType::Custom(_) => true,
    }
}

fn parse_expression_effects<'a, T: Expression>(
    value: &'a T,
    factory: &'a impl ExpressionFactory<T>,
) -> impl Iterator<Item = &'a Signal<T>> + 'a {
    factory
        .match_signal_term(value)
        .map(|term| {
            term.signals()
                .iter()
                .filter(|effect| matches!(effect.signal_type(), SignalType::Custom(_)))
        })
        .into_iter()
        .flatten()
}
