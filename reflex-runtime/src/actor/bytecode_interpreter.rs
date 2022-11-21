// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    iter::once,
    marker::PhantomData,
    sync::Arc,
};

use metrics::{describe_gauge, describe_histogram, SharedString, Unit};
use reflex::{
    core::{
        Applicable, ConditionListType, ConditionType, EvaluationResult, Expression,
        ExpressionFactory, InstructionPointer, Reducible, RefType, Rewritable, SignalTermType,
        SignalType, StateToken,
    },
    hash::{HashId, IntMap, IntSet},
};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, MessageOffset, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_macros::{dispatcher, Named};

use crate::{
    action::{
        bytecode_interpreter::{
            BytecodeInterpreterEvaluateAction, BytecodeInterpreterGcAction,
            BytecodeInterpreterGcCompleteAction, BytecodeInterpreterInitAction,
            BytecodeInterpreterResultAction, BytecodeWorkerStatistics,
        },
        evaluate::{
            EvaluateResultAction, EvaluateStartAction, EvaluateStopAction, EvaluateUpdateAction,
        },
    },
    task::bytecode_worker::{
        BytecodeWorkerMetricNames, BytecodeWorkerTask, BytecodeWorkerTaskFactory,
    },
    utils::quantiles::{
        generate_quantile_metric_labels, publish_quantile_bucketed_metric, QuantileBucket,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryInvalidationStrategy,
};

// TODO: Allow tweaking bytecode interpreter GC trigger
const MAX_UPDATES_WITHOUT_GC: usize = 3;

#[derive(Clone, Copy, Debug)]
pub struct BytecodeInterpreterMetricNames {
    pub query_worker_compile_duration: &'static str,
    pub query_worker_evaluate_duration: &'static str,
    pub query_worker_gc_duration: &'static str,
    pub query_worker_state_dependency_count: &'static str,
    pub query_worker_evaluation_cache_entry_count: &'static str,
    pub query_worker_evaluation_cache_deep_size: &'static str,
}
impl BytecodeInterpreterMetricNames {
    pub fn init(self) -> Self {
        describe_histogram!(
            self.query_worker_compile_duration,
            Unit::Seconds,
            "Worker query compilation duration (seconds)"
        );
        describe_histogram!(
            self.query_worker_evaluate_duration,
            Unit::Seconds,
            "Worker query evaluation duration (seconds)"
        );
        describe_histogram!(
            self.query_worker_gc_duration,
            Unit::Seconds,
            "Worker garbage collection duration (seconds)"
        );
        describe_gauge!(
            self.query_worker_state_dependency_count,
            Unit::Count,
            "The number of state dependencies for the most recent worker result"
        );
        describe_gauge!(
            self.query_worker_evaluation_cache_entry_count,
            Unit::Count,
            "The number of entries in the query worker evaluation cache"
        );
        describe_gauge!(
            self.query_worker_evaluation_cache_deep_size,
            Unit::Count,
            "A full count of the number of graph nodes in all entries in the query worker evaluation cache"
        );
        self
    }
}
impl Default for BytecodeInterpreterMetricNames {
    fn default() -> Self {
        Self {
            query_worker_compile_duration: "query_worker_compile_duration",
            query_worker_evaluate_duration: "query_worker_evaluate_duration",
            query_worker_gc_duration: "query_worker_gc_duration",
            query_worker_state_dependency_count: "query_worker_state_dependency_count",
            query_worker_evaluation_cache_deep_size: "query_worker_evaluation_cache_deep_size",
            query_worker_evaluation_cache_entry_count: "query_worker_evaluation_cache_entry_count",
        }
    }
}

pub trait BytecodeInterpreterMetricLabels {
    fn labels(&self, query_name: &str) -> Vec<(SharedString, SharedString)>;
}
impl<_Self> BytecodeInterpreterMetricLabels for _Self
where
    Self: Fn(&str) -> Vec<(SharedString, SharedString)>,
{
    fn labels(&self, query_name: &str) -> Vec<(SharedString, SharedString)> {
        (self)(query_name)
    }
}

#[derive(Named, Clone)]
pub struct BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TMetricLabels: BytecodeInterpreterMetricLabels,
{
    graph_root: Arc<(CompiledProgram, InstructionPointer)>,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
    factory: TFactory,
    _allocator: TAllocator,
    metric_names: BytecodeInterpreterMetricNames,
    get_worker_metric_labels: TMetricLabels,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TMetricLabels>
    BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TMetricLabels: BytecodeInterpreterMetricLabels,
{
    pub fn new(
        graph_root: (CompiledProgram, InstructionPointer),
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: TFactory,
        allocator: TAllocator,
        metric_names: BytecodeInterpreterMetricNames,
        get_worker_metric_labels: TMetricLabels,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            graph_root: Arc::new(graph_root),
            compiler_options,
            interpreter_options,
            factory,
            _allocator: allocator,
            metric_names: metric_names.init(),
            get_worker_metric_labels,
            main_pid,
            _expression: Default::default(),
        }
    }
}
pub struct BytecodeInterpreterState<T: Expression> {
    // TODO: Use newtypes for state hashmap keys
    workers: HashMap<StateToken, BytecodeInterpreterWorkerState<T>>,
    grouped_worker_metrics: HashMap<String, WorkerMetricsState>,
}
impl<T: Expression> Default for BytecodeInterpreterState<T> {
    fn default() -> Self {
        Self {
            workers: Default::default(),
            grouped_worker_metrics: Default::default(),
        }
    }
}

struct WorkerMetricsState {
    active_workers: IntSet<HashId>,
    quantile_metric_labels: [Vec<(SharedString, SharedString)>; NUM_QUANTILE_BUCKETS],
}

const NUM_QUANTILE_BUCKETS: usize = 4;
const QUANTILE_BUCKETS: [QuantileBucket; NUM_QUANTILE_BUCKETS] = [
    QuantileBucket(0.5),
    QuantileBucket(0.9),
    QuantileBucket(0.99),
    QuantileBucket(1.0),
];

struct BytecodeInterpreterWorkerState<T: Expression> {
    pid: ProcessId,
    label: String,
    state_index: Option<MessageOffset>,
    status: BytecodeInterpreterWorkerStatus<T>,
    invalidation_strategy: QueryInvalidationStrategy,
    updates_since_gc: usize,
    metrics: BytecodeWorkerStatistics,
}

enum BytecodeInterpreterWorkerStatus<T: Expression> {
    Working(BytecodeWorkerUpdateQueue<T>),
    Idle,
}

enum BytecodeWorkerUpdateQueue<T: Expression> {
    Single(SingleWorkerUpdateQueue<T>),
    Combined(CombinedWorkerUpdateQueue<T>),
    Exact(ExactWorkerUpdateQueue<T>),
}
impl<T: Expression> BytecodeWorkerUpdateQueue<T> {
    fn new(invalidation_strategy: QueryInvalidationStrategy) -> Self {
        match invalidation_strategy {
            QueryInvalidationStrategy::CombineUpdateBatches => Self::Single(Default::default()),
            QueryInvalidationStrategy::Exact => Self::Exact(Default::default()),
        }
    }
    fn push_update_batch(&mut self, updates: &Vec<(StateToken, T)>) {
        match self {
            Self::Single(queue) => {
                let initial_batch = std::mem::take(&mut queue.updates);
                *self = Self::Combined(CombinedWorkerUpdateQueue::from_iter(
                    initial_batch.into_iter().chain(
                        updates
                            .iter()
                            .map(|(state_token, value)| (*state_token, value.clone())),
                    ),
                ))
            }
            Self::Combined(queue) => {
                queue.updates.extend(
                    updates
                        .iter()
                        .map(|(state_token, value)| (*state_token, value.clone())),
                );
            }
            Self::Exact(queue) => queue.batches.push_back(updates.clone()),
        }
    }
    fn pop_update_batch(&mut self) -> Vec<(StateToken, T)> {
        match self {
            Self::Single(queue) => std::mem::take(&mut queue.updates),
            Self::Combined(queue) => std::mem::take(&mut queue.updates).into_iter().collect(),
            Self::Exact(queue) => queue.batches.pop_front().unwrap_or_default(),
        }
    }
}

struct SingleWorkerUpdateQueue<T: Expression> {
    updates: Vec<(StateToken, T)>,
}
impl<T: Expression> Default for SingleWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            updates: Default::default(),
        }
    }
}

struct CombinedWorkerUpdateQueue<T: Expression> {
    updates: IntMap<StateToken, T>,
}
impl<T: Expression> Default for CombinedWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            updates: Default::default(),
        }
    }
}
impl<T: Expression> FromIterator<(StateToken, T)> for CombinedWorkerUpdateQueue<T> {
    fn from_iter<TIter: IntoIterator<Item = (StateToken, T)>>(iter: TIter) -> Self {
        Self {
            updates: iter.into_iter().collect(),
        }
    }
}
impl<T: Expression> IntoIterator for CombinedWorkerUpdateQueue<T> {
    type Item = (StateToken, T);
    type IntoIter = std::collections::hash_map::IntoIter<StateToken, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.updates.into_iter()
    }
}

struct ExactWorkerUpdateQueue<T: Expression> {
    batches: VecDeque<Vec<(StateToken, T)>>,
}
impl<T: Expression> Default for ExactWorkerUpdateQueue<T> {
    fn default() -> Self {
        Self {
            batches: Default::default(),
        }
    }
}

dispatcher!({
    pub enum BytecodeInterpreterAction<T: Expression> {
        Inbox(EvaluateStartAction<T>),
        Inbox(EvaluateUpdateAction<T>),
        Inbox(EvaluateStopAction),
        Inbox(BytecodeInterpreterResultAction<T>),
        Inbox(BytecodeInterpreterGcCompleteAction),

        Outbox(EvaluateResultAction<T>),
        Outbox(BytecodeInterpreterInitAction),
        Outbox(BytecodeInterpreterEvaluateAction<T>),
        Outbox(BytecodeInterpreterGcAction),
    }

    impl<T, TFactory, TAllocator, TMetricLabels, TAction, TTask> Dispatcher<TAction, TTask>
        for BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TMetricLabels: BytecodeInterpreterMetricLabels,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask> + BytecodeWorkerTask<T, TFactory, TAllocator>,
    {
        type State = BytecodeInterpreterState<T>;
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

        fn accept(&self, _action: &EvaluateStopAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateStopAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateStopAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_stop(state, action, metadata, context)
        }

        fn accept(&self, _action: &BytecodeInterpreterResultAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &BytecodeInterpreterResultAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &BytecodeInterpreterResultAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_bytecode_interpreter_result(state, action, metadata, context)
        }

        fn accept(&self, _action: &BytecodeInterpreterGcCompleteAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &BytecodeInterpreterGcCompleteAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &BytecodeInterpreterGcCompleteAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_gc_complete_action(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator, TMetricLabels>
    BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TMetricLabels: BytecodeInterpreterMetricLabels,
{
    fn handle_gc_complete_action<TAction, TTask>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &BytecodeInterpreterGcCompleteAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let BytecodeInterpreterGcCompleteAction {
            cache_id,
            statistics,
        } = action;
        self.update_worker_cache_metrics(state, *cache_id, *statistics);
        None
    }
    fn handle_evaluate_start<TAction, TTask>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TFactory: Default,
        TAllocator: Default,
        TAction: Action
            + From<BytecodeInterpreterInitAction>
            + From<BytecodeInterpreterEvaluateAction<T>>,
        TTask:
            TaskFactory<TAction, TTask> + From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>,
    {
        let EvaluateStartAction {
            cache_id,
            label,
            query,
            evaluation_mode,
            invalidation_strategy,
        } = action;
        let actions = match state.workers.entry(*cache_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                let task_pid = context.generate_pid();
                let current_pid = context.pid();
                entry.insert(BytecodeInterpreterWorkerState {
                    pid: task_pid,
                    label: label.clone(),
                    state_index: None,
                    status: BytecodeInterpreterWorkerStatus::Working(
                        BytecodeWorkerUpdateQueue::new(*invalidation_strategy),
                    ),
                    invalidation_strategy: *invalidation_strategy,
                    updates_since_gc: 0,
                    metrics: Default::default(),
                });
                Some(SchedulerTransition::new([
                    SchedulerCommand::Task(
                        task_pid,
                        BytecodeWorkerTaskFactory {
                            cache_id: *cache_id,
                            query: query.clone(),
                            evaluation_mode: *evaluation_mode,
                            compiler_options: self.compiler_options,
                            interpreter_options: self.interpreter_options,
                            graph_root: self.graph_root.clone(),
                            metric_names: BytecodeWorkerMetricNames {
                                query_worker_compile_duration: self
                                    .metric_names
                                    .query_worker_compile_duration
                                    .into(),
                                query_worker_evaluate_duration: self
                                    .metric_names
                                    .query_worker_evaluate_duration
                                    .into(),
                                query_worker_gc_duration: self
                                    .metric_names
                                    .query_worker_gc_duration
                                    .into(),
                            },
                            caller_pid: current_pid,
                            _expression: PhantomData,
                            _factory: PhantomData,
                            _allocator: PhantomData,
                        }
                        .into(),
                    ),
                    SchedulerCommand::Send(
                        task_pid,
                        BytecodeInterpreterInitAction {
                            cache_id: *cache_id,
                        }
                        .into(),
                    ),
                    SchedulerCommand::Send(
                        task_pid,
                        BytecodeInterpreterEvaluateAction {
                            cache_id: *cache_id,
                            state_index: None,
                            state_updates: Default::default(),
                        }
                        .into(),
                    ),
                ]))
            }
        }?;
        match state.grouped_worker_metrics.entry(label.clone()) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().active_workers.insert(*cache_id);
            }
            Entry::Vacant(entry) => {
                let worker_labels = self.get_worker_metric_labels.labels(label.as_str());
                entry.insert(WorkerMetricsState {
                    active_workers: IntSet::from_iter(once(*cache_id)),
                    quantile_metric_labels: generate_quantile_metric_labels(
                        &QUANTILE_BUCKETS,
                        &worker_labels,
                    ),
                });
            }
        }
        Some(actions)
    }
    fn handle_evaluate_update<TAction, TTask>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &EvaluateUpdateAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<BytecodeInterpreterInitAction>
            + From<BytecodeInterpreterEvaluateAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateUpdateAction {
            cache_id,
            state_index,
            state_updates,
        } = action;
        let worker_state = state.workers.get_mut(cache_id)?;
        worker_state.state_index = *state_index;
        let evaluate_action = match &mut worker_state.status {
            BytecodeInterpreterWorkerStatus::Working(update_queue) => {
                update_queue.push_update_batch(&state_updates);
                None
            }
            BytecodeInterpreterWorkerStatus::Idle => Some(BytecodeInterpreterEvaluateAction {
                cache_id: *cache_id,
                state_index: *state_index,
                state_updates: state_updates.clone(),
            }),
        }?;
        let worker_pid = worker_state.pid;
        worker_state.status = BytecodeInterpreterWorkerStatus::Working(
            BytecodeWorkerUpdateQueue::new(worker_state.invalidation_strategy),
        );
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            worker_pid,
            evaluate_action.into(),
        ))))
    }
    fn handle_evaluate_stop<TAction, TTask>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &EvaluateStopAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateStopAction { cache_id } = action;
        // Reset the metrics for this worker
        self.update_worker_cache_metrics(state, *cache_id, Default::default());
        // Remove the worker
        let worker_state = state.workers.remove(cache_id)?;
        let worker_pid = worker_state.pid;
        // Clean up the worker metrics
        if let Entry::Occupied(mut entry) = state.grouped_worker_metrics.entry(worker_state.label) {
            entry.get_mut().active_workers.remove(cache_id);
            let is_final_worker_in_group = entry.get().active_workers.is_empty();
            if is_final_worker_in_group {
                entry.remove();
            }
        }
        Some(SchedulerTransition::new(once(SchedulerCommand::Kill(
            worker_pid,
        ))))
    }
    fn handle_bytecode_interpreter_result<TAction, TTask>(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        action: &BytecodeInterpreterResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<EvaluateResultAction<T>>
            + From<BytecodeInterpreterEvaluateAction<T>>
            + From<BytecodeInterpreterGcAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let BytecodeInterpreterResultAction {
            cache_id,
            state_index,
            result,
            statistics,
        } = action;
        self.update_worker_cache_metrics(state, *cache_id, *statistics);
        let worker_state = state.workers.get_mut(cache_id)?;
        let queued_evaluation = match std::mem::replace(
            &mut worker_state.status,
            BytecodeInterpreterWorkerStatus::Idle,
        ) {
            BytecodeInterpreterWorkerStatus::Working(mut existing_queue) => {
                let pending_updates = existing_queue.pop_update_batch();
                if pending_updates.is_empty() {
                    None
                } else {
                    Some((
                        BytecodeInterpreterEvaluateAction {
                            cache_id: *cache_id,
                            state_index: worker_state.state_index,
                            state_updates: pending_updates,
                        },
                        existing_queue,
                    ))
                }
            }
            BytecodeInterpreterWorkerStatus::Idle => None,
        };
        let gc_action = if is_unresolved_result(&result, &self.factory) {
            None
        } else {
            let should_gc = queued_evaluation.is_none()
                || worker_state.updates_since_gc >= MAX_UPDATES_WITHOUT_GC;
            if should_gc {
                worker_state.updates_since_gc = 0;
                Some(BytecodeInterpreterGcAction {
                    cache_id: *cache_id,
                    state_index: *state_index,
                })
            } else {
                worker_state.updates_since_gc += 1;
                None
            }
        };
        Some(SchedulerTransition::new(
            once(SchedulerCommand::Send(
                self.main_pid,
                EvaluateResultAction {
                    cache_id: *cache_id,
                    state_index: *state_index,
                    result: result.clone(),
                }
                .into(),
            ))
            .chain(gc_action.map(|action| {
                let worker_pid = worker_state.pid;
                SchedulerCommand::Send(worker_pid, action.into())
            }))
            .chain(queued_evaluation.map(|(action, remaining_queue)| {
                let worker_pid = worker_state.pid;
                worker_state.status = BytecodeInterpreterWorkerStatus::Working(remaining_queue);
                SchedulerCommand::Send(worker_pid, action.into())
            })),
        ))
    }
    fn update_worker_cache_metrics(
        &self,
        state: &mut BytecodeInterpreterState<T>,
        cache_id: StateToken,
        statistics: BytecodeWorkerStatistics,
    ) -> Option<()> {
        // Update the worker statistics
        let worker_state = state.workers.get_mut(&cache_id)?;
        worker_state.metrics = statistics;
        let worker_state = state.workers.get(&cache_id)?;
        // Determine the metric labels to be applied to this worker
        let worker_metrics = state.grouped_worker_metrics.get(&worker_state.label)?;
        let metric_labels = &worker_metrics.quantile_metric_labels;
        // Recompute the quantile bucket values for this worker group
        publish_quantile_bucketed_metric(
            worker_metrics
                .active_workers
                .iter()
                .filter_map(|cache_id| state.workers.get(cache_id))
                .map(|worker_state| worker_state.metrics.state_dependency_count as f64),
            self.metric_names.query_worker_state_dependency_count,
            &QUANTILE_BUCKETS,
            metric_labels,
        );
        publish_quantile_bucketed_metric(
            worker_metrics
                .active_workers
                .iter()
                .filter_map(|cache_id| state.workers.get(cache_id))
                .map(|worker_state| worker_state.metrics.evaluation_cache_entry_count as f64),
            self.metric_names.query_worker_evaluation_cache_entry_count,
            &QUANTILE_BUCKETS,
            metric_labels,
        );
        publish_quantile_bucketed_metric(
            worker_metrics
                .active_workers
                .iter()
                .filter_map(|cache_id| state.workers.get(cache_id))
                .map(|worker_state| worker_state.metrics.evaluation_cache_deep_size as f64),
            self.metric_names.query_worker_evaluation_cache_deep_size,
            &QUANTILE_BUCKETS,
            metric_labels,
        );
        Some(())
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
                .map(|item| item.as_deref())
                .any(is_unresolved_effect)
        })
        .unwrap_or(false)
}

fn is_unresolved_effect<T: Expression<Signal<T> = V>, V: ConditionType<T>>(effect: &V) -> bool {
    match effect.signal_type() {
        SignalType::Error => false,
        SignalType::Pending | SignalType::Custom(_) => true,
    }
}
