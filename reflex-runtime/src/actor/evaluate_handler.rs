// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap, HashSet, VecDeque},
    iter::once,
    marker::PhantomData,
    ops::Deref,
    time::{Duration, Instant},
};

use metrics::{
    counter, decrement_gauge, describe_counter, describe_gauge, describe_histogram, gauge,
    histogram, increment_counter, increment_gauge, SharedString, Unit,
};
use reflex::{
    core::{
        ConditionListType, ConditionType, DependencyList, DynamicState, EvaluationResult,
        Expression, ExpressionFactory, ExpressionListType, HeapAllocator, ListTermType, RefType,
        SignalTermType, SignalType, StateCache, StateToken, StringTermType, StringValue,
        SymbolTermType,
    },
    hash::{IntMap, IntSet},
};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, MessageOffset, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};
use reflex_utils::partition_results;

use crate::{
    action::{
        effect::{
            EffectEmitAction, EffectSubscribeAction, EffectThrottleEmitAction,
            EffectUnsubscribeAction, EffectUpdateBatch,
        },
        evaluate::{
            EvaluateResultAction, EvaluateStartAction, EvaluateStopAction, EvaluateUpdateAction,
        },
    },
    task::evaluate_handler::EffectThrottleTaskFactory,
    QueryEvaluationMode, QueryInvalidationStrategy,
};

pub const EFFECT_TYPE_EVALUATE: &'static str = "reflex::core::evaluate";

#[derive(Clone, Copy, Debug)]
pub struct EvaluateHandlerMetricNames {
    pub state_entry_count: &'static str,
    pub state_pending_update_batch_count: &'static str,
    pub state_gc_duration: &'static str,
    pub total_effect_count: &'static str,
    pub active_effect_count: &'static str,
    pub total_effect_emissions_count: &'static str,
    pub total_effect_updates_count: &'static str,
    pub active_query_worker_count: &'static str,
    pub pending_query_worker_count: &'static str,
    pub error_query_worker_count: &'static str,
    pub blocked_query_worker_count: &'static str,
    pub active_query_worker_cache_entry_count: &'static str,
}
impl EvaluateHandlerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.state_entry_count,
            Unit::Count,
            "Active global state entry count"
        );
        describe_gauge!(
            self.state_pending_update_batch_count,
            Unit::Count,
            "Queued worker state update batch count"
        );
        describe_histogram!(
            self.state_gc_duration,
            Unit::Seconds,
            "Global state garbage collection duration (seconds)"
        );
        describe_counter!(self.total_effect_count, Unit::Count, "Total effect count");
        describe_gauge!(self.active_effect_count, Unit::Count, "Active effect count");
        describe_counter!(
            self.total_effect_emissions_count,
            Unit::Count,
            "Number of effect update batches emitted"
        );
        describe_counter!(
            self.total_effect_updates_count,
            Unit::Count,
            "Number of individual effect value updates"
        );
        describe_gauge!(
            self.active_query_worker_count,
            Unit::Count,
            "Active query worker count"
        );
        describe_gauge!(
            self.pending_query_worker_count,
            Unit::Count,
            "Pending query worker count"
        );
        describe_gauge!(
            self.error_query_worker_count,
            Unit::Count,
            "Errored query worker count"
        );
        describe_gauge!(
            self.blocked_query_worker_count,
            Unit::Count,
            "Blocked query worker count"
        );
        describe_gauge!(
            self.active_query_worker_cache_entry_count,
            Unit::Count,
            "Active query worker cache entry count"
        );
        self
    }
}
impl Default for EvaluateHandlerMetricNames {
    fn default() -> Self {
        Self {
            state_entry_count: "state_entry_count",
            state_pending_update_batch_count: "state_pending_update_batch_count",
            state_gc_duration: "state_gc_duration",
            total_effect_count: "total_effect_count",
            active_effect_count: "active_effect_count",
            total_effect_emissions_count: "total_effect_emissions_count",
            total_effect_updates_count: "total_effect_updates_count",
            active_query_worker_count: "active_query_worker_count",
            pending_query_worker_count: "pending_query_worker_count",
            error_query_worker_count: "error_query_worker_count",
            blocked_query_worker_count: "blocked_query_worker_count",
            active_query_worker_cache_entry_count: "active_query_worker_cache_entry_count",
        }
    }
}

pub fn create_evaluate_effect<T: Expression>(
    label: String,
    query: T,
    evaluation_mode: QueryEvaluationMode,
    invalidation_strategy: QueryInvalidationStrategy,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T::Signal {
    allocator.create_signal(
        SignalType::Custom(String::from(EFFECT_TYPE_EVALUATE)),
        factory.create_list_term(allocator.create_list([
            factory.create_string_term(allocator.create_string(label)),
            query,
            evaluation_mode.serialize(factory),
            invalidation_strategy.serialize(factory),
        ])),
        factory.create_nil_term(),
    )
}

pub fn parse_evaluate_effect_query<T: Expression>(
    effect: &T::Signal,
    factory: &impl ExpressionFactory<T>,
) -> Option<(String, T, QueryEvaluationMode, QueryInvalidationStrategy)> {
    let payload = effect.payload();
    let payload = payload.as_deref();
    let args = factory
        .match_list_term(payload)
        .filter(|args| args.items().as_deref().len() == 4)?;
    let args = args.items();
    let mut args = args.as_deref().iter().map(|item| item.as_deref().clone());
    let label = args.next().unwrap();
    let query = args.next().unwrap();
    let evaluation_mode = args.next().unwrap();
    let invalidation_strategy = args.next().unwrap();
    match (
        factory.match_string_term(&label),
        QueryEvaluationMode::deserialize(&evaluation_mode, factory),
        QueryInvalidationStrategy::deserialize(&invalidation_strategy, factory),
    ) {
        (Some(label), Some(evaluation_mode), Some(invalidation_strategy)) => Some((
            String::from(label.value().as_deref().as_str().deref()),
            query.clone(),
            evaluation_mode,
            invalidation_strategy,
        )),
        _ => None,
    }
}

fn create_evaluate_effect_result<T: Expression>(
    result: &EvaluationResult<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_list_term(
        allocator.create_pair(
            result.result().clone(),
            factory.create_list_term(
                allocator.create_list(
                    result
                        .dependencies()
                        .iter()
                        .map(|state_token| factory.create_symbol_term(state_token)),
                ),
            ),
        ),
    )
}

pub fn parse_evaluate_effect_result<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<EvaluationResult<T>> {
    let evalution_result = factory.match_list_term(value)?;
    let items = evalution_result.items();
    let value = items.as_deref().get(0)?;
    let dependencies = items.as_deref().get(1)?;
    let dependencies = factory.match_list_term(dependencies.as_deref())?.items();
    let dependencies = dependencies.as_deref().iter().filter_map(|dependency| {
        factory
            .match_symbol_term(dependency.as_deref())
            .map(|term| term.id())
    });
    Some(EvaluationResult::new(
        value.as_deref().clone(),
        DependencyList::from_iter(dependencies),
    ))
}

#[derive(Named, Clone)]
pub struct EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    throttle: Option<Duration>,
    metric_names: EvaluateHandlerMetricNames,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub(crate) fn new(
        factory: TFactory,
        allocator: TAllocator,
        throttle: Option<Duration>,
        metric_names: EvaluateHandlerMetricNames,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory,
            allocator,
            throttle,
            metric_names: metric_names.init(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct EvaluateHandlerState<T: Expression> {
    // TODO: Use newtypes for state hashmap keys
    workers: IntMap<StateToken, WorkerState<T>>,
    // TODO: Use expressions as state tokens, removing need to map state tokens back to originating effects
    effects: IntMap<StateToken, T::Signal>,
    state_cache: GlobalStateCache<T>,
    /// Whitelist to keep track of effects that must be processed immediately to ensure exactly-once processing guarantees (non-whitelisted effect updates are eligible for throttling)
    immediate_effects: IntSet<StateToken>,
    /// Accumulated set of pending deferred updates, to be applied at the next throttle timeout
    deferred_updates: Option<IntMap<StateToken, T>>,
}
impl<T: Expression> Default for EvaluateHandlerState<T> {
    fn default() -> Self {
        Self {
            workers: Default::default(),
            effects: Default::default(),
            state_cache: Default::default(),
            immediate_effects: Default::default(),
            deferred_updates: Default::default(),
        }
    }
}
struct WorkerState<T: Expression> {
    subscription_count: usize,
    effect: T::Signal,
    status: WorkerStatus<T>,
    state_index: Option<MessageOffset>,
    state_values: HashMap<StateToken, T>,
    metric_labels: [(&'static str, String); 1],
}
struct GlobalStateCache<T: Expression> {
    state_index: Option<MessageOffset>,
    combined_state: StateCache<T>,
    /// Queued updates waiting to be sent to query evaluation workers
    /// Batches are removed from the queue at the point at which all workers have been sent the update
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
    fn apply_batch<TAction, TTask>(
        &mut self,
        state_index: MessageOffset,
        updates: Vec<(StateToken, T)>,
        main_pid: ProcessId,
        metric_names: EvaluateHandlerMetricNames,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EvaluateUpdateAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let updated_state_tokens = updates
            .iter()
            .map(|(state_token, _)| *state_token)
            .collect::<IntSet<_>>();
        self.state_cache.state_index = Some(state_index);
        self.state_cache
            .update_batches
            .push_back((state_index, updates));
        self.state_cache.update_state_cache_metrics(metric_names);
        let invalidated_workers = self
            .workers
            .values_mut()
            .filter_map(|worker| match &mut worker.status {
                WorkerStatus::Idle {
                    latest_result: (_, result),
                } => {
                    let has_invalidated_dependencies = result
                        .dependencies()
                        .iter()
                        .any(|state_token| updated_state_tokens.contains(&state_token));
                    if has_invalidated_dependencies {
                        Some(worker)
                    } else {
                        worker.state_index = Some(state_index);
                        None
                    }
                }
                _ => None,
            });
        let worker_update_actions = invalidated_workers
            .filter_map(|worker| {
                update_worker_state(
                    worker,
                    WorkerStateUpdateType::DependencyUpdate,
                    &mut self.state_cache,
                    metric_names,
                )
                .map(|action| SchedulerCommand::Send(main_pid, TAction::from(action)))
            })
            .collect::<Vec<_>>();
        self.gc_worker_state_history(metric_names);
        if worker_update_actions.is_empty() {
            None
        } else {
            Some(SchedulerTransition::new(worker_update_actions))
        }
    }
    fn combined_effects<'a>(&'a self) -> impl Iterator<Item = &'a T::Signal> + 'a {
        self.workers.values().flat_map(|worker| {
            once(&worker.effect).into_iter().chain(
                worker
                    .dependencies_iter()
                    .flat_map(|state_token| self.effects.get(&state_token)),
            )
        })
    }
    fn gc_worker_state_history(&mut self, metric_names: EvaluateHandlerMetricNames) {
        let oldest_active_state_index = self
            .workers
            .values()
            .filter_map(|worker| worker.state_index)
            .min();
        self.state_cache
            .delete_outdated_update_batches(oldest_active_state_index, metric_names);
    }
    fn update_worker_status_metrics(
        &self,
        factory: &impl ExpressionFactory<T>,
        metric_names: EvaluateHandlerMetricNames,
    ) {
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
            metric_names.pending_query_worker_count,
            num_pending_workers as f64
        );
        gauge!(
            metric_names.error_query_worker_count,
            num_error_workers as f64
        );
        gauge!(
            metric_names.blocked_query_worker_count,
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
            Some(result) => {
                match factory.match_signal_term(result.result()) {
                    None => WorkerResultStatus::Active,
                    Some(signal) => {
                        if signal.signals().as_deref().iter().any(|signal| {
                            matches!(&signal.as_deref().signal_type(), SignalType::Error)
                        }) {
                            WorkerResultStatus::Error
                        } else if signal.signals().as_deref().iter().any(|signal| {
                            matches!(&signal.as_deref().signal_type(), SignalType::Pending)
                        }) {
                            WorkerResultStatus::Pending
                        } else {
                            WorkerResultStatus::Blocked
                        }
                    }
                }
            }
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
    fn delete_outdated_update_batches(
        &mut self,
        active_state_index: Option<MessageOffset>,
        metric_names: EvaluateHandlerMetricNames,
    ) {
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
        self.update_state_cache_metrics(metric_names);
    }
    fn gc(
        &mut self,
        retained_keys: impl IntoIterator<Item = StateToken>,
        metric_names: EvaluateHandlerMetricNames,
    ) {
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
        histogram!(metric_names.state_gc_duration, elapsed_time.as_secs_f64());
        self.update_state_cache_metrics(metric_names);
    }
    fn update_state_cache_metrics(&self, metric_names: EvaluateHandlerMetricNames) {
        gauge!(
            metric_names.state_entry_count,
            self.combined_state.len() as f64
        );
        gauge!(
            metric_names.state_pending_update_batch_count,
            self.update_batches.len() as f64
        );
    }
}

dispatcher!({
    pub enum EvaluateHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(EvaluateResultAction<T>),
        Inbox(EffectEmitAction<T>),
        Inbox(EffectThrottleEmitAction),

        Outbox(EffectSubscribeAction<T>),
        Outbox(EffectUnsubscribeAction<T>),
        Outbox(EffectEmitAction<T>),
        Outbox(EvaluateStartAction<T>),
        Outbox(EvaluateUpdateAction<T>),
        Outbox(EvaluateStopAction),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for EvaluateHandler<T, TFactory, TAllocator>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask> + From<EffectThrottleTaskFactory>,
    {
        type State = EvaluateHandlerState<T>;
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

        fn accept(&self, _action: &EffectSubscribeAction<T>) -> bool {
            true
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

        fn accept(&self, _action: &EffectUnsubscribeAction<T>) -> bool {
            true
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

        fn accept(&self, _action: &EffectEmitAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectEmitAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectEmitAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_emit(state, action, metadata, context)
        }

        fn accept(&self, _action: &EffectThrottleEmitAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectThrottleEmitAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectThrottleEmitAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_throttle_emit(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator> EvaluateHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut EvaluateHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EvaluateStartAction<T>> + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        let metric_labels = [("effect_type", String::from(effect_type))];
        counter!(
            self.metric_names.total_effect_count,
            effects.len() as u64,
            &metric_labels
        );
        increment_gauge!(
            self.metric_names.active_effect_count,
            effects.len() as f64,
            &metric_labels
        );
        if effect_type != EFFECT_TYPE_EVALUATE {
            return None;
        }
        let queries = effects.iter().filter_map(|effect| {
            parse_evaluate_effect_query(effect, &self.factory).map(|query| (effect, query))
        });
        let (evaluate_start_actions, existing_results): (Vec<_>, Vec<_>) =
            partition_results(queries.filter_map(
                |(effect, (label, query, evaluation_mode, invalidation_strategy))| {
                    let state_token = effect.id();
                    // Certain queries guarantee that every update is processed individually.
                    // These need to be whitelisted to skip being batched by throttling.
                    if let QueryInvalidationStrategy::Exact = invalidation_strategy {
                        state.immediate_effects.insert(state_token);
                    }
                    match state.workers.entry(state_token) {
                        // For any queries that are already subscribed, re-emit the latest cached value if one exists
                        // (this is necessary because the caller that triggered this action might be expecting a result)
                        Entry::Occupied(mut entry) => {
                            let worker = entry.get_mut();
                            worker.subscription_count += 1;
                            worker
                                .latest_result()
                                .filter(|result| !is_unresolved_result(result, &self.factory))
                                .map(|result| Err((state_token, result.result().clone())))
                        }
                        // For any queries that are not yet subscribed, kick off evaluation of that query
                        Entry::Vacant(entry) => {
                            increment_gauge!(self.metric_names.active_query_worker_count, 1.0);
                            let metric_labels = [("worker_id", format!("{}", effect.id()))];
                            gauge!(
                                self.metric_names.active_query_worker_cache_entry_count,
                                0.0,
                                &metric_labels
                            );
                            entry.insert(WorkerState {
                                subscription_count: 1,
                                effect: effect.clone(),
                                status: WorkerStatus::Busy {
                                    previous_result: None,
                                },
                                state_index: None,
                                state_values: Default::default(),
                                metric_labels,
                            });
                            Some(Ok(SchedulerCommand::Send(
                                self.main_pid,
                                EvaluateStartAction {
                                    cache_id: state_token,
                                    query,
                                    label,
                                    evaluation_mode,
                                    invalidation_strategy,
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
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_EVALUATE.into(),
                        updates: existing_results,
                    }],
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            emit_cached_results_action
                .into_iter()
                .chain(evaluate_start_actions),
        ))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut EvaluateHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EvaluateStopAction> + From<EffectUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        decrement_gauge!(self.metric_names.active_effect_count, effects.len() as f64, "effect_type" => String::from(effect_type));
        if effect_type != EFFECT_TYPE_EVALUATE {
            return None;
        }
        let unsubscribed_workers = effects
            .iter()
            .flat_map(|effect| {
                let state_token = effect.id();
                let mut existing_entry = match state.workers.entry(state_token) {
                    Entry::Occupied(entry) => Some(entry),
                    _ => None,
                }?;
                let updated_subscription_count = {
                    let mut worker = existing_entry.get_mut();
                    worker.subscription_count -= 1;
                    worker.subscription_count
                };
                if updated_subscription_count == 0 {
                    state.immediate_effects.remove(&state_token);
                    if let Some(pending_updates) = state.deferred_updates.as_mut() {
                        pending_updates.remove(&state_token);
                    }
                    let (_, subscription) = existing_entry.remove_entry();
                    decrement_gauge!(self.metric_names.active_query_worker_count, 1.0);
                    gauge!(
                        self.metric_names.active_query_worker_cache_entry_count,
                        0.0,
                        &subscription.metric_labels,
                    );
                    Some((state_token, subscription))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let remaining_effect_ids = state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let removed_worker_effects = unsubscribed_workers
            .iter()
            .map(|(_, worker)| &worker.effect)
            .filter(|effect| !remaining_effect_ids.contains(&effect.id()));
        for effect in removed_worker_effects {
            state.effects.remove(&effect.id());
        }
        let removed_effects = unsubscribed_workers
            .iter()
            .flat_map(|(_, worker)| worker.dependencies_iter())
            .filter(|state_token| !remaining_effect_ids.contains(&state_token))
            .filter_map(|removed_effect_id| state.effects.remove(&removed_effect_id));
        let removed_queries = unsubscribed_workers.iter().map(|(cache_key, _)| *cache_key);
        let stop_actions = removed_queries.map(|cache_key| {
            SchedulerCommand::Send(
                self.main_pid,
                EvaluateStopAction {
                    cache_id: cache_key,
                }
                .into(),
            )
        });
        let unsubscribe_actions =
            group_effects_by_type(removed_effects).map(|(effect_type, effects)| {
                SchedulerCommand::Send(
                    self.main_pid,
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
            state
                .state_cache
                .gc(remaining_effect_ids, self.metric_names);
        }
        let has_unsubscribed_workers = !unsubscribed_workers.is_empty();
        if has_unsubscribed_workers {
            state.gc_worker_state_history(self.metric_names);
            state.update_worker_status_metrics(&self.factory, self.metric_names);
        }
        Some(SchedulerTransition::new(actions))
    }
    fn handle_evaluate_result<TAction, TTask>(
        &self,
        state: &mut EvaluateHandlerState<T>,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<EffectSubscribeAction<T>>
            + From<EffectUnsubscribeAction<T>>
            + From<EvaluateUpdateAction<T>>
            + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateResultAction {
            cache_id: cache_key,
            state_index,
            result,
        } = action;
        let worker_state_index = state
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
        let existing_effect_ids = state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let worker = state.workers.get_mut(cache_key)?;
        let added_effects = parse_newly_added_expression_effects(
            result.result(),
            &existing_effect_ids,
            &self.factory,
        );
        for effect in added_effects.iter() {
            state.effects.insert(effect.id(), effect.clone());
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
                &mut state.state_cache,
                self.metric_names,
            )
            .map(|action| SchedulerCommand::Send(self.main_pid, TAction::from(action)));
            state.gc_worker_state_history(self.metric_names);
            reevaluate_action
        };
        let effect_emit_action: Option<SchedulerCommand<TAction, TTask>> =
            if is_unresolved_result(&result, &self.factory) {
                None
            } else {
                Some(SchedulerCommand::Send(
                    self.main_pid,
                    EffectEmitAction {
                        effect_types: vec![EffectUpdateBatch {
                            effect_type: EFFECT_TYPE_EVALUATE.into(),
                            updates: vec![(
                                *cache_key,
                                create_evaluate_effect_result(
                                    result,
                                    &self.factory,
                                    &self.allocator,
                                ),
                            )],
                        }],
                    }
                    .into(),
                ))
            };
        let updated_effect_ids = state
            .combined_effects()
            .map(|effect| effect.id())
            .collect::<HashSet<_>>();
        let removed_effects = existing_effect_ids
            .iter()
            .filter(|effect_id| !updated_effect_ids.contains(effect_id))
            .filter_map(|effect_id| state.effects.remove(&effect_id))
            .collect::<Vec<_>>();
        let effect_subscribe_actions =
            group_effects_by_type(added_effects).map(|(effect_type, effects)| {
                SchedulerCommand::Send(
                    self.main_pid,
                    EffectSubscribeAction {
                        effect_type,
                        effects,
                    }
                    .into(),
                )
            });
        let effect_unsubscribe_actions =
            group_effects_by_type(removed_effects).map(|(effect_type, effects)| {
                SchedulerCommand::Send(
                    self.main_pid,
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
        state.update_worker_status_metrics(&self.factory, self.metric_names);
        Some(SchedulerTransition::new(actions))
    }
    fn handle_effect_emit<TAction, TTask>(
        &self,
        state: &mut EvaluateHandlerState<T>,
        action: &EffectEmitAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EvaluateUpdateAction<T>>,
        TTask: TaskFactory<TAction, TTask> + From<EffectThrottleTaskFactory>,
    {
        let EffectEmitAction { effect_types } = action;
        let updates = if effect_types.is_empty() {
            Vec::default()
        } else {
            let existing_state = &state.state_cache.combined_state;
            effect_types
                .iter()
                .flat_map(|batch| {
                    let metric_names = match &batch.effect_type {
                        Cow::Borrowed(effect_type) => {
                            [("effect_type", SharedString::borrowed(effect_type))]
                        }
                        Cow::Owned(effect_type) => [(
                            "effect_type",
                            SharedString::owned(String::from(effect_type)),
                        )],
                    };
                    increment_counter!(
                        self.metric_names.total_effect_emissions_count,
                        &metric_names,
                    );
                    counter!(
                        self.metric_names.total_effect_updates_count,
                        batch.updates.len() as u64,
                        &metric_names,
                    );
                    batch.updates.iter()
                })
                .filter_map(|(state_token, update)| {
                    let is_unchanged = existing_state
                        .get(state_token)
                        .map(|existing_value| update.id() == existing_value.id())
                        .unwrap_or(false);
                    if is_unchanged {
                        None
                    } else {
                        Some((*state_token, update.clone()))
                    }
                })
                .collect()
        };
        if updates.is_empty() {
            return None;
        }
        let (immediate_updates, throttle_operation) = match self.throttle {
            None => (updates, None),
            Some(throttle_duration) => {
                let (immediate_updates, deferred_updates): (Vec<_>, Vec<_>) =
                    partition_results(updates.into_iter().map(|(state_token, value)| {
                        let is_immediate_update = state.immediate_effects.contains(&state_token)
                            || match state.state_cache.combined_state.get(&state_token) {
                                None => true,
                                Some(existing_value) => {
                                    self.factory.match_signal_term(existing_value).is_some()
                                }
                            };
                        state
                            .state_cache
                            .combined_state
                            .set(state_token, value.clone());
                        if is_immediate_update {
                            Ok((state_token, value))
                        } else {
                            Err((state_token, value))
                        }
                    }));
                let throttle_operation = if deferred_updates.is_empty() {
                    None
                } else if let Some(pending_updates) = &mut state.deferred_updates {
                    pending_updates.extend(deferred_updates);
                    None
                } else {
                    state.deferred_updates = Some(deferred_updates.into_iter().collect());
                    let task_pid = context.generate_pid();
                    Some(SchedulerCommand::Task(
                        task_pid,
                        TTask::from(EffectThrottleTaskFactory {
                            timeout: throttle_duration,
                            caller_pid: context.pid(),
                        }),
                    ))
                };
                (immediate_updates, throttle_operation)
            }
        };
        let update_actions = if immediate_updates.is_empty() {
            None
        } else {
            let state_index = metadata.offset;
            state.apply_batch(
                state_index,
                immediate_updates,
                self.main_pid,
                self.metric_names,
            )
        };
        match (update_actions, throttle_operation) {
            (Some(update_actions), Some(throttle_operation)) => {
                let mut combined_actions = update_actions;
                combined_actions.push(throttle_operation);
                Some(combined_actions)
            }
            (Some(update_actions), None) => Some(update_actions),
            (None, Some(throttle_operation)) => {
                Some(SchedulerTransition::new([throttle_operation]))
            }
            (None, None) => None,
        }
    }
    fn handle_effect_throttle_emit<TAction, TTask>(
        &self,
        state: &mut EvaluateHandlerState<T>,
        _action: &EffectThrottleEmitAction,
        metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EvaluateUpdateAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let updates = state.deferred_updates.take()?;
        if updates.is_empty() {
            None
        } else {
            let state_index = metadata.offset;
            state.apply_batch(
                state_index,
                updates.into_iter().collect(),
                self.main_pid,
                self.metric_names,
            )
        }
    }
}

enum WorkerStateUpdateType {
    FirstResult,
    SubsequentResult {
        previous_dependencies: DependencyList,
    },
    DependencyUpdate,
}
fn update_worker_state<T: Expression>(
    worker: &mut WorkerState<T>,
    update_type: WorkerStateUpdateType,
    global_state: &mut GlobalStateCache<T>,
    metric_names: EvaluateHandlerMetricNames,
) -> Option<EvaluateUpdateAction<T>> {
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
        metric_names.active_query_worker_cache_entry_count,
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
    Some(EvaluateUpdateAction {
        cache_id: worker.effect.id(),
        state_index: worker.state_index,
        state_updates: updates,
    })
}

fn group_effects_by_type<T: Expression<Signal = V>, V: ConditionType<T>>(
    effects: impl IntoIterator<Item = V>,
) -> impl Iterator<Item = (String, Vec<V>)> {
    effects
        .into_iter()
        .filter(|signal| matches!(signal.signal_type(), SignalType::Custom(_)))
        .fold(HashMap::<String, Vec<V>>::new(), |mut result, signal| {
            let existing_signals = get_custom_signal_type(&signal)
                .and_then(|signal_type| result.get_mut(&signal_type));
            if let Some(existing_signals) = existing_signals {
                existing_signals.push(signal);
            } else if let Some(signal_type) = get_custom_signal_type(&signal) {
                result.insert(String::from(signal_type), vec![signal]);
            }
            result
        })
        .into_iter()
}

fn get_custom_signal_type<T: Expression<Signal = V>, V: ConditionType<T>>(
    effect: &V,
) -> Option<String> {
    match effect.signal_type() {
        SignalType::Custom(signal_type) => Some(signal_type),
        _ => None,
    }
}

fn is_unresolved_result<T: Expression>(
    result: &EvaluationResult<T>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    factory
        .match_signal_term(result.result())
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .any(|effect| is_unresolved_effect(effect.as_deref()))
        })
        .unwrap_or(false)
}

fn is_unresolved_effect<T: Expression<Signal = V>, V: ConditionType<T>>(effect: &V) -> bool {
    match effect.signal_type() {
        SignalType::Error => false,
        SignalType::Pending | SignalType::Custom(_) => true,
    }
}

fn parse_newly_added_expression_effects<T: Expression>(
    value: &T,
    existing_effect_ids: &HashSet<StateToken>,
    factory: &impl ExpressionFactory<T>,
) -> Vec<T::Signal> {
    match factory.match_signal_term(value) {
        None => Vec::new(),
        Some(term) => term
            .signals()
            .as_deref()
            .iter()
            .filter(|effect| {
                let effect = effect.as_deref();
                matches!(effect.signal_type(), SignalType::Custom(_))
                    && !existing_effect_ids.contains(&effect.id())
            })
            .map(|effect| effect.as_deref().clone())
            .collect(),
    }
}
