// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    marker::PhantomData,
    sync::Arc,
    time::{Duration, Instant},
};

use futures::Future;
use metrics::{describe_counter, describe_gauge, describe_histogram, gauge, histogram, Unit};
use reflex_dispatcher::{Action, MessageData, Named, ProcessId, TaskFactory};
use reflex_scheduler::tokio::{
    metrics::TokioTaskMetricNames, TokioSchedulerHandlerTimer, TokioSchedulerInstrumentation,
};
use tokio::task::JoinHandle;
use tokio_metrics::{Instrumented, TaskMonitor};

use crate::GraphQlWebServerInstrumentation;

pub use metrics::SharedString;

#[derive(Clone, Copy, Debug)]
pub struct ServerSchedulerMetricNames {
    pub event_bus_queued_messages: &'static str,
    pub event_bus_message_queue_capacity: &'static str,
    pub event_processing_duration_micros: &'static str,
    pub event_bus_queued_duration_micros: &'static str,
    pub handler_processing_time_micros: &'static str,
    pub tokio_task_metric_names: TokioTaskMetricNames,
}
impl Default for ServerSchedulerMetricNames {
    fn default() -> Self {
        Self {
            event_bus_queued_messages: "event_bus_queued_messages",
            event_bus_message_queue_capacity: "event_bus_message_capacity",
            event_processing_duration_micros: "event_processing_duration_micros",
            event_bus_queued_duration_micros: "event_bus_queued_duration_micros",
            handler_processing_time_micros: "handler_processing_time_micros",
            tokio_task_metric_names: TokioTaskMetricNames::default(),
        }
    }
}
impl ServerSchedulerMetricNames {
    pub fn init(self) -> Self {
        describe_gauge!(
            self.event_bus_queued_messages,
            Unit::Count,
            "Number of event bus messages currently queued awaiting processing"
        );
        describe_gauge!(
            self.event_bus_message_queue_capacity,
            Unit::Count,
            "Event bus message queue buffer capacity"
        );
        describe_histogram!(
            self.event_bus_queued_duration_micros,
            Unit::Microseconds,
            "Time individual messages spend in the event bus waiting to be handled"
        );
        describe_histogram!(
            self.event_processing_duration_micros,
            Unit::Microseconds,
            "Time spent handling an individual message that has been taken from the event bus"
        );
        describe_counter!(
            self.handler_processing_time_micros,
            Unit::Count,
            "Time spent within a handler method for a specific handler / message combination"
        );
        self
    }
}

#[derive(Clone)]
pub struct ServerMetricsHandlerTimer<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    metric_names: ServerSchedulerMetricNames,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> ServerMetricsHandlerTimer<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(metric_names: ServerSchedulerMetricNames) -> Self {
        Self {
            metric_names,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> TokioSchedulerHandlerTimer for ServerMetricsHandlerTimer<TAction, TTask>
where
    TAction: Action + Named,
    TTask: TaskFactory<TAction, TTask>,
    TTask::Actor: Named,
{
    type Span = Instant;
    type Action = TAction;
    type Task = TTask;
    fn start_span(
        &self,
        _actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        _message: &Self::Action,
        _pid: ProcessId,
        _metadata: &MessageData,
    ) -> Self::Span {
        Instant::now()
    }
    fn end_span(
        &self,
        span: Self::Span,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        action: &Self::Action,
        pid: ProcessId,
        _metadata: &MessageData,
    ) {
        let elapsed_time = span.elapsed();
        let metric_labels = [
            ("pid", get_pid_metric_label(pid)),
            ("actor", actor.name().into()),
            ("action", action.name().into()),
        ];
        histogram!(
            self.metric_names.handler_processing_time_micros,
            elapsed_time.as_micros() as f64,
            &metric_labels,
        );
    }
}

#[derive(Clone)]
pub struct ServerMetricsInstrumentation {
    metric_names: ServerSchedulerMetricNames,
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
}
impl Drop for ServerMetricsInstrumentation {
    fn drop(&mut self) {
        for handle in self.task_monitor_poll_handles.iter() {
            handle.abort()
        }
    }
}
impl ServerMetricsInstrumentation {
    pub fn new(metric_names: ServerSchedulerMetricNames) -> Self {
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
        }
    }
}

impl TokioSchedulerInstrumentation for ServerMetricsInstrumentation {
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

    fn set_queue_capacity(&self, pid: ProcessId, value: usize) {
        gauge!(
            self.metric_names.event_bus_message_queue_capacity,
            value as f64,
            "pid" => get_pid_metric_label(pid),
        );
    }
    fn set_current_queue_size(&self, pid: ProcessId, value: usize) {
        gauge!(
            self.metric_names.event_bus_queued_messages,
            value as f64,
            "pid" => get_pid_metric_label(pid),
        );
    }
    fn record_queue_item_waiting_duration(&self, pid: ProcessId, value: Duration) {
        histogram!(
            self.metric_names.event_bus_queued_duration_micros,
            value.as_micros() as f64,
            "pid" => get_pid_metric_label(pid),
        );
    }
    fn record_queue_item_working_duration(&self, pid: ProcessId, value: Duration) {
        histogram!(
            self.metric_names.event_processing_duration_micros,
            value.as_micros() as f64,
            "pid" => get_pid_metric_label(pid),
        );
    }
}

impl GraphQlWebServerInstrumentation for ServerMetricsInstrumentation {
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

fn get_pid_metric_label(pid: ProcessId) -> SharedString {
    get_usize_metric_label(pid.into())
}

fn get_usize_metric_label(value: usize) -> SharedString {
    match value {
        0 => SharedString::borrowed("0"),
        1 => SharedString::borrowed("1"),
        2 => SharedString::borrowed("2"),
        3 => SharedString::borrowed("3"),
        4 => SharedString::borrowed("4"),
        5 => SharedString::borrowed("5"),
        6 => SharedString::borrowed("6"),
        7 => SharedString::borrowed("7"),
        8 => SharedString::borrowed("8"),
        9 => SharedString::borrowed("9"),
        10 => SharedString::borrowed("10"),
        11 => SharedString::borrowed("11"),
        12 => SharedString::borrowed("12"),
        13 => SharedString::borrowed("13"),
        14 => SharedString::borrowed("14"),
        15 => SharedString::borrowed("15"),
        value => SharedString::owned(format!("{}", value)),
    }
}
