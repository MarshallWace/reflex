// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, sync::Arc, time::Duration};

use futures::Future;
use metrics::{
    decrement_gauge, describe_gauge, describe_histogram, gauge, histogram, increment_gauge, Unit,
};
use reflex_dispatcher::{Action, Named, ProcessId, SchedulerMode, TaskFactory};
use reflex_scheduler::tokio::{
    metrics::TokioTaskMetricNames, TokioSchedulerInstrumentation, TokioSchedulerState,
    TokioWorkerState,
};
use tokio::task::JoinHandle;
use tokio_metrics::{Instrumented, TaskMonitor};

use crate::GraphQlWebServerInstrumentation;

pub use metrics::SharedString;

#[derive(Clone, Copy, Debug)]
pub struct ServerSchedulerMetricNames {
    pub scheduler_queue_capacity: &'static str,
    pub scheduler_queue_current_size: &'static str,
    pub scheduler_command_waiting_duration_micros: &'static str,
    pub scheduler_command_working_duration_micros: &'static str,
    pub worker_count: &'static str,
    pub task_count: &'static str,
    pub worker_inbox_capacity: &'static str,
    pub worker_inbox_current_size: &'static str,
    pub worker_action_waiting_duration_micros: &'static str,
    pub worker_action_working_duration_micros: &'static str,

    pub tokio_task_metric_names: TokioTaskMetricNames,
}
impl Default for ServerSchedulerMetricNames {
    fn default() -> Self {
        Self {
            scheduler_queue_capacity: "scheduler_queue_capacity",
            scheduler_queue_current_size: "scheduler_queue_current_size",
            scheduler_command_waiting_duration_micros: "scheduler_command_waiting_duration_micros",
            scheduler_command_working_duration_micros: "scheduler_command_working_duration_micros",
            worker_count: "worker_count",
            task_count: "task_count",
            worker_inbox_capacity: "worker_inbox_capacity",
            worker_inbox_current_size: "worker_inbox_current_size",
            worker_action_waiting_duration_micros: "worker_action_waiting_duration_micros",
            worker_action_working_duration_micros: "worker_action_working_duration_micros",
            tokio_task_metric_names: TokioTaskMetricNames::default(),
        }
    }
}
impl ServerSchedulerMetricNames {
    pub fn init(self) -> Self {
        describe_gauge!(
            self.scheduler_queue_capacity,
            Unit::Count,
            "Event bus message queue buffer capacity"
        );
        describe_gauge!(
            self.scheduler_queue_current_size,
            Unit::Count,
            "Number of event bus messages currently queued awaiting processing"
        );
        describe_histogram!(
            self.scheduler_command_waiting_duration_micros,
            Unit::Microseconds,
            "Time individual messages spend in the event bus waiting to be handled"
        );
        describe_histogram!(
            self.scheduler_command_working_duration_micros,
            Unit::Microseconds,
            "Time spent handling an individual message that has been taken from the event bus"
        );
        describe_gauge!(
            self.worker_count,
            Unit::Count,
            "Overall number of active workers currently registered"
        );
        describe_gauge!(
            self.worker_inbox_capacity,
            Unit::Count,
            "Overall worker queue buffer capacity"
        );
        describe_gauge!(
            self.worker_inbox_current_size,
            Unit::Count,
            "Overall number of worker messages currently queued awaiting processing"
        );
        describe_histogram!(
            self.worker_action_waiting_duration_micros,
            Unit::Microseconds,
            "Time individual actions spend in the worker queue waiting to be handled"
        );
        describe_histogram!(
            self.worker_action_working_duration_micros,
            Unit::Microseconds,
            "Time spent handling an individual action that has been taken from the worker queue"
        );
        self
    }
}

pub trait ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn record_scheduler_state(&self, state: TokioSchedulerState);
    fn record_worker_state(&self, pid: ProcessId, actor: &TTask::Actor, state: TokioWorkerState);
}

impl<T, TAction, TTask> ServerMetricsSchedulerQueueInstrumentation<TAction, TTask> for Option<T>
where
    T: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn record_scheduler_state(&self, state: TokioSchedulerState) {
        match self {
            Some(inner) => inner.record_scheduler_state(state),
            None => {}
        }
    }
    fn record_worker_state(&self, pid: ProcessId, actor: &TTask::Actor, state: TokioWorkerState) {
        match self {
            Some(inner) => inner.record_worker_state(pid, actor, state),
            None => {}
        }
    }
}

pub struct NoopServerMetricsSchedulerQueueInstrumentation<TAction, TTask> {
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> Default for NoopServerMetricsSchedulerQueueInstrumentation<TAction, TTask> {
    fn default() -> Self {
        Self {
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> Clone for NoopServerMetricsSchedulerQueueInstrumentation<TAction, TTask> {
    fn clone(&self) -> Self {
        Self {
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> Copy for NoopServerMetricsSchedulerQueueInstrumentation<TAction, TTask> {}
impl<TAction, TTask> ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>
    for NoopServerMetricsSchedulerQueueInstrumentation<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn record_scheduler_state(&self, _state: TokioSchedulerState) {}
    fn record_worker_state(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _state: TokioWorkerState,
    ) {
    }
}

pub struct ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
{
    metric_names: ServerSchedulerMetricNames,
    queue_instrumentation: TQueue,
    scheduler_task_monitor: TaskMonitor,
    async_task_monitor: TaskMonitor,
    blocking_task_monitor: TaskMonitor,
    worker_task_monitor: TaskMonitor,
    task_inbox_task_monitor: TaskMonitor,
    dispose_task_monitor: TaskMonitor,
    subscribe_task_monitor: TaskMonitor,
    unsubscribe_task_monitor: TaskMonitor,
    graphql_connection_task_monitor: TaskMonitor,
    task_monitor_poll_handles: Arc<[JoinHandle<()>; 9]>,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask, TQueue> Clone for ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask> + Clone,
{
    fn clone(&self) -> Self {
        Self {
            metric_names: self.metric_names.clone(),
            queue_instrumentation: self.queue_instrumentation.clone(),
            scheduler_task_monitor: self.scheduler_task_monitor.clone(),
            async_task_monitor: self.async_task_monitor.clone(),
            blocking_task_monitor: self.blocking_task_monitor.clone(),
            worker_task_monitor: self.worker_task_monitor.clone(),
            task_inbox_task_monitor: self.task_inbox_task_monitor.clone(),
            dispose_task_monitor: self.dispose_task_monitor.clone(),
            subscribe_task_monitor: self.subscribe_task_monitor.clone(),
            unsubscribe_task_monitor: self.unsubscribe_task_monitor.clone(),
            graphql_connection_task_monitor: self.graphql_connection_task_monitor.clone(),
            task_monitor_poll_handles: self.task_monitor_poll_handles.clone(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask, TQueue> Drop for ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
{
    fn drop(&mut self) {
        for handle in self.task_monitor_poll_handles.iter() {
            handle.abort()
        }
    }
}
impl<TAction, TTask, TQueue> ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
{
    pub fn new(queue_instrumentation: TQueue, metric_names: ServerSchedulerMetricNames) -> Self {
        let (scheduler_task_monitor, scheduler_task_monitor_poll_task) =
            create_named_task_monitor("scheduler", &metric_names.tokio_task_metric_names);
        let (async_task_monitor, async_task_monitor_poll_task) =
            create_named_task_monitor("async", &metric_names.tokio_task_metric_names);
        let (blocking_task_monitor, blocking_task_monitor_poll_task) =
            create_named_task_monitor("blocking", &metric_names.tokio_task_metric_names);
        let (worker_task_monitor, worker_task_monitor_poll_task) =
            create_named_task_monitor("worker", &metric_names.tokio_task_metric_names);
        let (task_inbox_task_monitor, task_inbox_task_monitor_poll_task) =
            create_named_task_monitor("task_inbox", &metric_names.tokio_task_metric_names);
        let (dispose_task_monitor, dispose_task_monitor_poll_task) =
            create_named_task_monitor("dispose", &metric_names.tokio_task_metric_names);
        let (subscribe_task_monitor, subscribe_task_monitor_poll_task) =
            create_named_task_monitor("subscribe", &metric_names.tokio_task_metric_names);
        let (unsubscribe_task_monitor, unsubscribe_task_monitor_poll_task) =
            create_named_task_monitor("unsubscribe", &metric_names.tokio_task_metric_names);
        let (graphql_connection_task_monitor, graphql_connection_task_monitor_poll_task) =
            create_named_task_monitor("graphql_connection", &metric_names.tokio_task_metric_names);
        Self {
            metric_names: metric_names.init(),
            queue_instrumentation,
            scheduler_task_monitor,
            async_task_monitor,
            blocking_task_monitor,
            worker_task_monitor,
            task_inbox_task_monitor,
            dispose_task_monitor,
            subscribe_task_monitor,
            unsubscribe_task_monitor,
            graphql_connection_task_monitor,
            task_monitor_poll_handles: Arc::new([
                tokio::spawn(scheduler_task_monitor_poll_task),
                tokio::spawn(async_task_monitor_poll_task),
                tokio::spawn(blocking_task_monitor_poll_task),
                tokio::spawn(worker_task_monitor_poll_task),
                tokio::spawn(task_inbox_task_monitor_poll_task),
                tokio::spawn(dispose_task_monitor_poll_task),
                tokio::spawn(subscribe_task_monitor_poll_task),
                tokio::spawn(unsubscribe_task_monitor_poll_task),
                tokio::spawn(graphql_connection_task_monitor_poll_task),
            ]),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}

impl<TTask, TAction, TQueue> TokioSchedulerInstrumentation
    for ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    type InstrumentedTask<T: Future + Send + 'static> = Instrumented<T>;

    fn instrument_main_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.scheduler_task_monitor.instrument(task)
    }
    fn instrument_async_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.async_task_monitor.instrument(task)
    }
    fn instrument_blocking_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.blocking_task_monitor.instrument(task)
    }
    fn instrument_worker_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.worker_task_monitor.instrument(task)
    }
    fn instrument_task_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.task_inbox_task_monitor.instrument(task)
    }
    fn instrument_dispose_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.dispose_task_monitor.instrument(task)
    }
    fn instrument_subscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.subscribe_task_monitor.instrument(task)
    }
    fn instrument_unsubscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.unsubscribe_task_monitor.instrument(task)
    }

    fn record_scheduler_queue_capacity(&self, value: usize) {
        gauge!(self.metric_names.scheduler_queue_capacity, value as f64,);
    }
    fn record_scheduler_enqueue(&self, num_commands: usize) {
        increment_gauge!(
            self.metric_names.scheduler_queue_current_size,
            num_commands as f64,
        );
    }
    fn record_scheduler_dequeue(&self) {
        decrement_gauge!(self.metric_names.scheduler_queue_current_size, 1.0);
    }
    fn record_scheduler_command_waiting_duration(&self, value: Duration) {
        histogram!(
            self.metric_names.scheduler_command_waiting_duration_micros,
            value.as_micros() as f64,
        );
    }
    fn record_scheduler_command_working_duration(&self, value: Duration) {
        histogram!(
            self.metric_names.scheduler_command_working_duration_micros,
            value.as_micros() as f64,
        );
    }
    fn record_actor_spawn(&self, pid: ProcessId, actor: &TTask::Actor) {
        self.record_worker_spawn(pid, actor, 0);
    }
    fn record_actor_kill(&self, pid: ProcessId, actor: &TTask::Actor) {
        self.record_worker_kill(pid, actor, 0, 0);
    }
    fn record_worker_spawn(&self, _pid: ProcessId, actor: &TTask::Actor, inbox_capacity: usize) {
        increment_gauge!(
            self.metric_names.worker_count,
            1.0,
            "actor" => actor.name(),
        );
        increment_gauge!(
            self.metric_names.worker_inbox_capacity,
            inbox_capacity as f64,
            "actor" => actor.name(),
        );
    }
    fn record_worker_kill(
        &self,
        _pid: ProcessId,
        actor: &TTask::Actor,
        inbox_capacity: usize,
        inbox_size: usize,
    ) {
        decrement_gauge!(
            self.metric_names.worker_inbox_current_size,
            inbox_size as f64,
            "actor" => actor.name(),
        );
        decrement_gauge!(
            self.metric_names.worker_inbox_capacity,
            inbox_capacity as f64,
            "actor" => actor.name(),
        );
        decrement_gauge!(
            self.metric_names.worker_count,
            1.0,
            "actor" => actor.name(),
        );
    }
    fn record_task_spawn(&self, _pid: ProcessId, actor: &TTask::Actor) {
        increment_gauge!(
            self.metric_names.task_count,
            1.0,
            "actor" => actor.name(),
        );
    }
    fn record_task_kill(&self, _pid: ProcessId, actor: &TTask::Actor) {
        decrement_gauge!(
            self.metric_names.task_count,
            1.0,
            "actor" => actor.name(),
        );
    }
    fn record_worker_action_enqueue(
        &self,
        _pid: ProcessId,
        actor: &TTask::Actor,
        _action: &TAction,
    ) {
        increment_gauge!(
            self.metric_names.worker_inbox_current_size,
            1.0 as f64,
            "actor" => actor.name(),
        );
    }
    fn record_worker_action_dequeue(
        &self,
        _pid: ProcessId,
        actor: &TTask::Actor,
        _action: &TAction,
    ) {
        decrement_gauge!(
            self.metric_names.worker_inbox_current_size,
            1.0 as f64,
            "actor" => actor.name(),
        );
    }
    fn record_worker_action_waiting_duration(
        &self,
        _pid: ProcessId,
        actor: &TTask::Actor,
        action: &TAction,
        value: Duration,
    ) {
        histogram!(
            self.metric_names.worker_action_waiting_duration_micros,
            value.as_micros() as f64,
            "actor" => actor.name(),
            "action" => action.name(),
        );
    }
    fn record_worker_action_working_duration(
        &self,
        _pid: ProcessId,
        actor: &TTask::Actor,
        action: &TAction,
        scheduler_mode: SchedulerMode,
        value: Duration,
    ) {
        histogram!(
            self.metric_names.worker_action_working_duration_micros,
            value.as_micros() as f64,
            "actor" => actor.name(),
            "action" => action.name(),
            "mode" => match scheduler_mode {
                SchedulerMode::Sync => "sync",
                SchedulerMode::Async => "async",
                SchedulerMode::Blocking => "blocking",
            }
        );
    }
    fn record_scheduler_state(&self, state: TokioSchedulerState) {
        self.queue_instrumentation.record_scheduler_state(state)
    }
    fn record_worker_state(&self, pid: ProcessId, actor: &TTask::Actor, state: TokioWorkerState) {
        self.queue_instrumentation
            .record_worker_state(pid, actor, state)
    }
}

impl<TAction, TTask, TQueue> GraphQlWebServerInstrumentation
    for ServerMetricsInstrumentation<TAction, TTask, TQueue>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
    TQueue: ServerMetricsSchedulerQueueInstrumentation<TAction, TTask>,
{
    fn instrument_websocket_connection<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        self.graphql_connection_task_monitor.instrument(task)
    }
}

pub fn create_named_task_monitor(
    name: impl Into<SharedString>,
    metric_names: &TokioTaskMetricNames,
) -> (TaskMonitor, impl Future<Output = ()>) {
    let task_monitor = TaskMonitor::new();
    let labels = [("task_type".into(), name.into())];
    let task = poll_task_monitor(
        task_monitor.clone(),
        labels,
        Duration::from_secs(1),
        metric_names,
    );
    (task_monitor, task)
}

fn poll_task_monitor<const NUM_LABELS: usize>(
    task_monitor: TaskMonitor,
    labels: [(SharedString, SharedString); NUM_LABELS],
    poll_interval: Duration,
    metric_names: &TokioTaskMetricNames,
) -> impl Future<Output = ()> {
    let metric_names = *metric_names;
    async move {
        for interval in task_monitor.intervals() {
            gauge!(
                metric_names.tokio_task_dropped_count,
                interval.dropped_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_first_poll_count,
                interval.first_poll_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_instrumented_count,
                interval.instrumented_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_fast_poll_count,
                interval.total_fast_poll_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_fast_poll_duration,
                interval.total_fast_poll_duration.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_first_poll_delay,
                interval.total_first_poll_delay.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_idle_duration,
                interval.total_idle_duration.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_idled_count,
                interval.total_idled_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_poll_count,
                interval.total_poll_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_poll_duration,
                interval.total_poll_duration.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_scheduled_count,
                interval.total_scheduled_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_scheduled_duration,
                interval.total_scheduled_duration.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_slow_poll_count,
                interval.total_slow_poll_count as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_total_slow_poll_duration,
                interval.total_slow_poll_duration.as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_fast_poll_duration,
                interval.mean_fast_poll_duration().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_first_poll_delay,
                interval.mean_first_poll_delay().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_idle_duration,
                interval.mean_idle_duration().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_poll_duration,
                interval.mean_poll_duration().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_scheduled_duration,
                interval.mean_scheduled_duration().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_mean_slow_poll_duration,
                interval.mean_slow_poll_duration().as_micros() as f64,
                &labels
            );
            gauge!(
                metric_names.tokio_task_slow_poll_ratio,
                interval.slow_poll_ratio() as f64,
                &labels
            );
            tokio::time::sleep(poll_interval).await;
        }
    }
}
