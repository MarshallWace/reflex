// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    ops::Deref,
    pin::Pin,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use futures::{future, Future, Sink, SinkExt, Stream, StreamExt};
use metrics::{describe_gauge, describe_histogram, gauge, histogram, Unit};
use tokio::{
    sync::{mpsc, oneshot},
    task::JoinHandle,
};
use tokio_metrics::TaskMonitor;
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::sync::{PollSendError, PollSender};

use reflex_dispatcher::{
    utils::with_unsubscribe_callback::WithUnsubscribeCallback, Action, Actor, ActorInitContext,
    Handler, HandlerContext, MessageData, MessageOffset, ProcessId, SchedulerCommand,
    SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox, TaskMessage, Worker,
};

#[derive(Clone, Copy, Debug)]
pub struct TokioTaskMetricNames {
    pub tokio_task_dropped_count: &'static str,
    pub tokio_task_first_poll_count: &'static str,
    pub tokio_task_instrumented_count: &'static str,
    pub tokio_task_total_fast_poll_count: &'static str,
    pub tokio_task_total_fast_poll_duration: &'static str,
    pub tokio_task_total_first_poll_delay: &'static str,
    pub tokio_task_total_idle_duration: &'static str,
    pub tokio_task_total_idled_count: &'static str,
    pub tokio_task_total_poll_count: &'static str,
    pub tokio_task_total_poll_duration: &'static str,
    pub tokio_task_total_scheduled_count: &'static str,
    pub tokio_task_total_scheduled_duration: &'static str,
    pub tokio_task_total_slow_poll_count: &'static str,
    pub tokio_task_total_slow_poll_duration: &'static str,
    pub tokio_task_mean_fast_poll_duration: &'static str,
    pub tokio_task_mean_first_poll_delay: &'static str,
    pub tokio_task_mean_idle_duration: &'static str,
    pub tokio_task_mean_poll_duration: &'static str,
    pub tokio_task_mean_scheduled_duration: &'static str,
    pub tokio_task_mean_slow_poll_duration: &'static str,
    pub tokio_task_slow_poll_ratio: &'static str,
}

impl Default for TokioTaskMetricNames {
    fn default() -> Self {
        Self {
            tokio_task_dropped_count: "tokio_task_dropped_count",
            tokio_task_first_poll_count: "tokio_task_first_poll_count",
            tokio_task_instrumented_count: "tokio_task_instrumented_count",
            tokio_task_total_fast_poll_count: "tokio_task_total_fast_poll_count",
            tokio_task_total_fast_poll_duration: "tokio_task_total_fast_poll_duration",
            tokio_task_total_first_poll_delay: "tokio_task_total_first_poll_delay",
            tokio_task_total_idle_duration: "tokio_task_total_idle_duration",
            tokio_task_total_idled_count: "tokio_task_total_idled_count",
            tokio_task_total_poll_count: "tokio_task_total_poll_count",
            tokio_task_total_poll_duration: "tokio_task_total_poll_duration",
            tokio_task_total_scheduled_count: "tokio_task_total_scheduled_count",
            tokio_task_total_scheduled_duration: "tokio_task_total_scheduled_duration",
            tokio_task_total_slow_poll_count: "tokio_task_total_slow_poll_count",
            tokio_task_total_slow_poll_duration: "tokio_task_total_slow_poll_duration",
            tokio_task_mean_fast_poll_duration: "tokio_task_mean_fast_poll_duration",
            tokio_task_mean_first_poll_delay: "tokio_task_mean_first_poll_delay",
            tokio_task_mean_idle_duration: "tokio_task_mean_idle_duration",
            tokio_task_mean_poll_duration: "tokio_task_mean_poll_duration",
            tokio_task_mean_scheduled_duration: "tokio_task_mean_scheduled_duration",
            tokio_task_mean_slow_poll_duration: "tokio_task_mean_slow_poll_duration",
            tokio_task_slow_poll_ratio: "tokio_task_slow_poll_ratio",
        }
    }
}

pub fn get_task_monitor(
    tokio_task_metric_names: &TokioTaskMetricNames,
    task_type: &str,
) -> TaskMonitor {
    let task_monitor = tokio_metrics::TaskMonitor::new();
    describe_gauge!(
        tokio_task_metric_names.tokio_task_dropped_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.dropped_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_first_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.first_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_instrumented_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.instrumented_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_fast_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_fast_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_fast_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_first_poll_delay,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_first_poll_delay"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_idle_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idle_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_idled_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_idled_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_scheduled_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_scheduled_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_scheduled_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_slow_poll_count,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_count"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_total_slow_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#structfield.total_slow_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_fast_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_fast_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_first_poll_delay,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_first_poll_delay"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_idle_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_idle_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_poll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_scheduled_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_scheduled_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_mean_slow_poll_duration,
        Unit::Microseconds,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.mean_slowehttps://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.htmlpoll_duration"
    );
    describe_gauge!(
        tokio_task_metric_names.tokio_task_slow_poll_ratio,
        Unit::Count,
        "See https://docs.rs/tokio-metrics/latest/tokio_metrics/struct.TaskMetrics.html#method.slow_poll_ratio"
    );

    {
        let tokio_task_metric_names = tokio_task_metric_names.clone();
        let task_monitor = task_monitor.clone();
        let task_type = task_type.to_string();
        tokio::spawn(async move {
            let labels = [("task_type".to_string(), task_type)];
            for interval in task_monitor.intervals() {
                gauge!(
                    tokio_task_metric_names.tokio_task_dropped_count,
                    interval.dropped_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_first_poll_count,
                    interval.first_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_instrumented_count,
                    interval.instrumented_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_fast_poll_count,
                    interval.total_fast_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_fast_poll_duration,
                    interval.total_fast_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_first_poll_delay,
                    interval.total_first_poll_delay.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_idle_duration,
                    interval.total_idle_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_idled_count,
                    interval.total_idled_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_poll_count,
                    interval.total_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_poll_duration,
                    interval.total_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_scheduled_count,
                    interval.total_scheduled_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_scheduled_duration,
                    interval.total_scheduled_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_slow_poll_count,
                    interval.total_slow_poll_count as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_total_slow_poll_duration,
                    interval.total_slow_poll_duration.as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_fast_poll_duration,
                    interval.mean_fast_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_first_poll_delay,
                    interval.mean_first_poll_delay().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_idle_duration,
                    interval.mean_idle_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_poll_duration,
                    interval.mean_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_scheduled_duration,
                    interval.mean_scheduled_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_mean_slow_poll_duration,
                    interval.mean_slow_poll_duration().as_micros() as f64,
                    &labels
                );
                gauge!(
                    tokio_task_metric_names.tokio_task_slow_poll_ratio,
                    interval.slow_poll_ratio() as f64,
                    &labels
                );

                tokio::time::sleep(Duration::from_secs(1)).await;
            }
        });
    }

    task_monitor
}

#[derive(Clone, Copy, Debug)]
pub struct TokioSchedulerMetricNames {
    pub event_bus_queued_microtasks: &'static str,
    pub event_bus_microtask_queue_capacity: &'static str,
    pub event_processing_duration_micros: &'static str,
    pub event_batch_processing_duration_micros: &'static str,
    pub event_bus_queued_duration_micros: &'static str,
    pub tokio_task_metric_names: TokioTaskMetricNames,
}
impl TokioSchedulerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.event_bus_queued_microtasks,
            Unit::Count,
            "Number of event bus async microtasks currently queued awaiting processing"
        );
        describe_gauge!(
            self.event_bus_microtask_queue_capacity,
            Unit::Count,
            "Event bus async microtask queue capacity"
        );
        describe_histogram!(
            self.event_processing_duration_micros,
            Unit::Microseconds,
            "Time spent in the synchronous handling of an event from the event bus"
        );
        describe_histogram!(
            self.event_batch_processing_duration_micros,
            Unit::Microseconds,
            "Time spent in the synchronous handling loop of events from the event bus (i.e. including subsequent commands spawned during handling an initial event)"
        );
        describe_histogram!(
            self.event_bus_queued_duration_micros,
            Unit::Microseconds,
            "Time individual actions spend in the event bus"
        );
        self
    }
}
impl Default for TokioSchedulerMetricNames {
    fn default() -> Self {
        Self {
            event_bus_queued_microtasks: "event_bus_queued_microtasks",
            event_bus_microtask_queue_capacity: "event_bus_microtask_capacity",
            event_processing_duration_micros: "event_processing_duration_micros",
            event_batch_processing_duration_micros: "event_batch_processing_duration_micros",
            event_bus_queued_duration_micros: "event_bus_queued_duration_micros",
            tokio_task_metric_names: TokioTaskMetricNames::default(),
        }
    }
}

enum TokioCommand<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    Send {
        pid: ProcessId,
        message: AsyncMessage<TAction>,
        caller: Option<(MessageOffset, ProcessId)>,
    },
    Spawn {
        pid: ProcessId,
        factory: TTask,
        caller: Option<(MessageOffset, ProcessId)>,
    },
    Kill {
        pid: ProcessId,
    },
    Subscribe {
        subscription_id: TokioSubscriberId,
        pid: ProcessId,
        subscriber: TokioSubscriber<TAction>,
    },
    Unsubscribe {
        subscription_id: TokioSubscriberId,
        pid: ProcessId,
    },
}
impl<TAction, TTask> TokioCommand<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn metric_label(&self) -> &'static str {
        match self {
            TokioCommand::Send { .. } => "send",
            TokioCommand::Spawn { .. } => "spawn",
            TokioCommand::Kill { .. } => "kill",
            TokioCommand::Subscribe { .. } => "subscribe",
            TokioCommand::Unsubscribe { .. } => "unsubscribe",
        }
    }
}

pub enum AsyncMessage<TAction> {
    Owned(TAction),
    Shared(Arc<TAction>),
}
impl<TAction> TaskMessage<TAction> for AsyncMessage<TAction> where TAction: Action {}
impl<TAction> From<TAction> for AsyncMessage<TAction> {
    fn from(value: TAction) -> Self {
        Self::Owned(value)
    }
}
impl<TAction> From<Arc<TAction>> for AsyncMessage<TAction> {
    fn from(value: Arc<TAction>) -> Self {
        Self::Shared(value)
    }
}
impl<TAction> Deref for AsyncMessage<TAction> {
    type Target = TAction;
    fn deref(&self) -> &Self::Target {
        match self {
            AsyncMessage::Owned(target) => target,
            AsyncMessage::Shared(target) => target.as_ref(),
        }
    }
}

pub struct TokioSinkError<T>(Option<T>);
impl<T> TokioSinkError<T> {
    pub fn into_inner(self) -> Option<T> {
        let Self(inner) = self;
        inner
    }
}
impl<TAction, TTask> From<PollSendError<TokioCommand<TAction, TTask>>>
    for TokioSinkError<AsyncMessage<TAction>>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn from(err: PollSendError<TokioCommand<TAction, TTask>>) -> Self {
        match err.into_inner() {
            Some(TokioCommand::Send { message, .. }) => TokioSinkError(Some(message)),
            _ => TokioSinkError(None),
        }
    }
}

pub struct TokioScheduler<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    commands: mpsc::Sender<TokioCommand<TAction, TTask>>,
    task: JoinHandle<()>,
    blocking_task_pool: JoinHandle<()>,
    next_subscriber_id: AtomicUsize,
    unsubscribe_monitor: tokio_metrics::TaskMonitor,
}
impl<TAction, TTask> Drop for TokioScheduler<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn drop(&mut self) {
        self.task.abort();
        self.blocking_task_pool.abort();
    }
}

struct TokioProcess<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    inbox: mpsc::Sender<AsyncMessage<TAction>>,
    actor: Arc<TTask::Actor>,
    handle: JoinHandle<()>,
    dispose: Option<<TTask::Actor as Actor<TAction, TTask>>::Dispose>,
    subscribers: HashMap<TokioSubscriberId, TokioSubscriber<TAction>>,
}
impl<TAction, TTask> Drop for TokioProcess<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn drop(&mut self) {
        self.handle.abort();
    }
}
impl<TAction, TTask> TokioProcess<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    async fn abort(&mut self) {
        if let Some(dispose) = self.dispose.take() {
            dispose.await;
        }
    }
}

pub struct TokioInitContext {
    next_pid: ProcessId,
}
impl TokioInitContext {
    pub fn generate_pid(&mut self) -> ProcessId {
        let next_pid = self.next_pid.next();
        std::mem::replace(&mut self.next_pid, next_pid)
    }
}

pub struct TokioInbox<TAction: Action>(ReceiverStream<AsyncMessage<TAction>>);
impl<TAction> TaskInbox<TAction> for TokioInbox<TAction>
where
    TAction: Action + Send + Sync + 'static,
{
    type Message = AsyncMessage<TAction>;
}
impl<TAction> Stream for TokioInbox<TAction>
where
    TAction: Action + 'static,
{
    type Item = AsyncMessage<TAction>;
    fn poll_next(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        self.0.poll_next_unpin(cx)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct TokioSubscriberId(usize);
impl From<usize> for TokioSubscriberId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

struct TokioSubscriber<TAction> {
    emit:
        Box<dyn for<'a> Fn(&'a TAction) -> Option<Pin<Box<dyn Future<Output = ()> + Send>>> + Send>,
}
impl<TAction> TokioSubscriber<TAction> {
    fn emit(&self, action: &TAction) -> Option<impl Future<Output = ()> + Send + Unpin> {
        (self.emit)(action)
    }
}
impl<TAction> TokioSubscriber<TAction> {
    fn new(
        emit: impl for<'a> Fn(&'a TAction) -> Option<Pin<Box<dyn Future<Output = ()> + Send>>>
            + Send
            + 'static,
    ) -> Self {
        Self {
            emit: Box::new(emit),
        }
    }
}

impl<TAction, TTask> TokioScheduler<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new<V>(
        metric_names: TokioSchedulerMetricNames,
        init_commands: impl FnOnce(
            &mut TokioInitContext,
        ) -> (
            Vec<(ProcessId, TTask::Actor)>,
            SchedulerTransition<TAction, TTask>,
            V,
        ),
    ) -> (Self, V)
    where
        TAction: Send + Sync + 'static,
        TTask: Send + 'static,
        TTask::Actor: Send + Sync + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State:
            Send + 'static,
    {
        let metric_names = metric_names.init();
        let scheduler_pid = ProcessId::default();
        let mut context = TokioInitContext {
            next_pid: scheduler_pid.next(),
        };
        let (init_processes, commands, result) = (init_commands)(&mut context);
        let TokioInitContext { next_pid } = context;
        let (task, blocking_task_pool, commands) = create_scheduler_thread(
            scheduler_pid,
            init_processes,
            commands,
            next_pid,
            1024,
            metric_names,
        );
        let root_task_monitor =
            get_task_monitor(&metric_names.tokio_task_metric_names, "scheduler");
        let blocking_task_monitor =
            get_task_monitor(&metric_names.tokio_task_metric_names, "blocking");
        (
            Self {
                commands,
                // TODO: Panic main thread when scheduler task threads panic
                task: tokio::spawn(root_task_monitor.instrument(task)),
                blocking_task_pool: tokio::spawn(
                    blocking_task_monitor.instrument(blocking_task_pool),
                ),
                unsubscribe_monitor: get_task_monitor(
                    &metric_names.tokio_task_metric_names,
                    "unsubscribe",
                ),
                next_subscriber_id: Default::default(),
            },
            result,
        )
    }
    pub fn actions(&self) -> impl Sink<(ProcessId, TAction)>
    where
        TAction: Send + Sync + 'static,
        TTask: Send + 'static,
    {
        PollSender::new(self.commands.clone()).with(move |(pid, action)| {
            future::ready(Result::<_, TokioSinkError<AsyncMessage<TAction>>>::Ok(
                TokioCommand::Send {
                    pid,
                    message: AsyncMessage::Owned(action),
                    caller: None,
                },
            ))
        })
    }
    pub fn subscribe<V: Send + 'static>(
        &self,
        pid: ProcessId,
        transform: impl Fn(&TAction) -> Option<V> + Send + 'static,
    ) -> Pin<
        Box<dyn Future<Output = Pin<Box<dyn Stream<Item = V> + Send + 'static>>> + Send + 'static>,
    >
    where
        TAction: Send + Sync + 'static,
        TTask: Send + 'static,
    {
        let (results_tx, results_rx) = mpsc::channel(32);
        let subscription_id =
            TokioSubscriberId::from(increment_atomic_counter(&self.next_subscriber_id));
        let subscriber = TokioSubscriber::new(move |action| {
            let value = transform(action)?;
            let results = results_tx.clone();
            Some(Box::pin(async move {
                let _ = results.send(value).await;
            }))
        });
        let subscribe_action = {
            let commands = self.commands.clone();
            async move {
                let _ = commands
                    .send(TokioCommand::Subscribe {
                        subscription_id,
                        pid,
                        subscriber,
                    })
                    .await;
            }
        };
        let unsubscribe_action = {
            let commands = self.commands.clone();
            async move {
                let _ = commands
                    .send(TokioCommand::Unsubscribe {
                        subscription_id,
                        pid,
                    })
                    .await;
            }
        };
        let results_stream = ReceiverStream::new(results_rx);
        let unsubscribe_monitor = self.unsubscribe_monitor.clone();
        Box::pin(async move {
            let _ = subscribe_action.await;
            let results: Pin<Box<dyn Stream<Item = V> + Send + 'static>> =
                Box::pin(WithUnsubscribeCallback::new(results_stream, {
                    let mut unsubscribe_action = Some(unsubscribe_action);
                    move || {
                        if let Some(unsubscribe) = unsubscribe_action.take() {
                            let _ = tokio::spawn(unsubscribe_monitor.instrument(unsubscribe));
                        }
                    }
                }));
            results
        })
    }
}

fn create_scheduler_thread<TAction, TTask>(
    scheduler_pid: ProcessId,
    init_processes: impl IntoIterator<Item = (ProcessId, TTask::Actor)>,
    init_commands: SchedulerTransition<TAction, TTask>,
    next_pid: ProcessId,
    async_buffer_capacity: usize,
    metric_names: TokioSchedulerMetricNames,
) -> (
    impl Future<Output = ()> + Send,
    impl Future<Output = ()> + Send,
    mpsc::Sender<TokioCommand<TAction, TTask>>,
)
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    let (commands_tx, mut commands_rx) =
        mpsc::channel::<TokioCommand<TAction, TTask>>(async_buffer_capacity);
    let metric_labels = [("pid", format!("{}", usize::from(scheduler_pid)))];
    gauge!(
        metric_names.event_bus_microtask_queue_capacity,
        async_buffer_capacity as f64,
        &metric_labels
    );
    gauge!(
        metric_names.event_bus_queued_microtasks,
        0.0,
        &metric_labels
    );
    let task_monitor = get_task_monitor(&metric_names.tokio_task_metric_names, "task");
    let abort_monitor = get_task_monitor(&metric_names.tokio_task_metric_names, "abort");
    let subscription_monitor =
        get_task_monitor(&metric_names.tokio_task_metric_names, "subscription");
    let (blocking_tasks_tx, mut blocking_tasks_rx) =
        mpsc::channel::<BlockingTaskRequest<TAction, TTask>>(1024);
    let blocking_task_pool = async move {
        while let Some(request) = blocking_tasks_rx.recv().await {
            let BlockingTaskRequest {
                actor,
                mut state,
                message,
                metadata,
                mut context,
                response,
            } = request;
            let result = actor.handle(&mut state, &message, &metadata, &mut context);
            let _ = response.send((message, state, result));
        }
    };
    let task = {
        let next_pid = Arc::new(AtomicUsize::new(next_pid.into()));
        let next_offset = Arc::new(AtomicUsize::new(Default::default()));
        let async_commands = commands_tx.clone();
        let init_processes: HashMap<ProcessId, TokioProcess<TAction, TTask>> = init_processes
            .into_iter()
            .map(|(pid, actor)| {
                (
                    pid,
                    spawn_worker_process(
                        actor,
                        pid,
                        &next_pid,
                        &next_offset,
                        None,
                        &async_commands,
                        &blocking_tasks_tx,
                        &task_monitor,
                    ),
                )
            })
            .collect();
        let init_commands: VecDeque<(TokioCommand<TAction, TTask>, Instant)> = init_commands
            .into_iter()
            .filter_map(|command| match command {
                SchedulerCommand::Send(pid, action) => Some(TokioCommand::Send {
                    pid,
                    message: AsyncMessage::from(action),
                    caller: None,
                }),
                SchedulerCommand::Task(pid, factory) => Some(TokioCommand::Spawn {
                    pid,
                    factory,
                    caller: None,
                }),
                SchedulerCommand::Kill(pid) => Some(TokioCommand::Kill { pid }),
                // TODO: Forbid forward commands in initial scheduler command batch
                SchedulerCommand::Forward(_) => None,
            })
            .map(|command| (command, Instant::now()))
            .collect();
        async move {
            let mut processes = init_processes;
            let mut command_queue = init_commands;
            // Main runtime event loop
            loop {
                // Process any queued synchronous commands
                let command_batch_processing_start_time = Instant::now();
                while let Some((command, enqueue_time)) = command_queue.pop_front() {
                    record_time_spent_in_queue(enqueue_time, &metric_names);
                    let command_processing_start_time = Instant::now();
                    let command_metric_label = command.metric_label();
                    match command {
                        TokioCommand::Spawn {
                            pid,
                            factory,
                            caller,
                        } => {
                            if let Entry::Vacant(entry) = processes.entry(pid) {
                                let actor = factory.create();
                                let process = spawn_worker_process(
                                    actor,
                                    pid,
                                    &next_pid,
                                    &next_offset,
                                    caller,
                                    &async_commands,
                                    &blocking_tasks_tx,
                                    &task_monitor,
                                );
                                entry.insert(process);
                            }
                        }
                        TokioCommand::Send {
                            pid,
                            message,
                            // TODO: Expose tokio scheduler message parent offset
                            caller,
                        } => {
                            let (send_task, subscriptions) =
                                if let Some(worker) = processes.get(&pid) {
                                    let is_accepted_message = worker.actor.accept(&message);
                                    let subscriptions = if worker.subscribers.is_empty() {
                                        None
                                    } else {
                                        let subscriptions = worker
                                            .subscribers
                                            .values()
                                            .filter_map(|subscriber| subscriber.emit(&message))
                                            .collect::<Vec<_>>();
                                        if subscriptions.is_empty() {
                                            None
                                        } else {
                                            Some(subscriptions)
                                        }
                                    };
                                    let send_task = if is_accepted_message {
                                        Some(worker.inbox.send(message))
                                    } else {
                                        None
                                    };
                                    (send_task, subscriptions)
                                } else {
                                    (None, None)
                                };
                            if let Some(send_task) = send_task {
                                let _ = send_task.await;
                            }
                            if let Some(subscriptions) = subscriptions {
                                for task in subscriptions {
                                    let _ = tokio::spawn(subscription_monitor.instrument(task));
                                }
                            }
                        }
                        TokioCommand::Kill { pid } => {
                            if let Some(mut worker) = processes.remove(&pid) {
                                tokio::spawn(
                                    abort_monitor.instrument(async move { worker.abort().await }),
                                );
                            }
                        }
                        TokioCommand::Subscribe {
                            subscription_id,
                            pid,
                            subscriber,
                        } => {
                            if let Some(entry) = processes.get_mut(&pid).and_then(|worker| {
                                match worker.subscribers.entry(subscription_id) {
                                    Entry::Occupied(_) => None,
                                    Entry::Vacant(entry) => Some(entry),
                                }
                            }) {
                                entry.insert(subscriber);
                            }
                        }
                        TokioCommand::Unsubscribe {
                            subscription_id,
                            pid,
                        } => {
                            if let Some(worker) = processes.get_mut(&pid) {
                                worker.subscribers.remove(&subscription_id);
                            }
                        }
                    };

                    histogram!(
                        metric_names.event_processing_duration_micros,
                        command_processing_start_time.elapsed().as_micros() as f64,
                        "command_type" => command_metric_label
                    )
                }

                histogram!(
                    metric_names.event_batch_processing_duration_micros,
                    command_batch_processing_start_time.elapsed().as_micros() as f64,
                );
                // All synchronous tasks have been processed, so wait until the next asynchronous microtask arrives
                if let Some(operation) = commands_rx.recv().await {
                    gauge!(
                        metric_names.event_bus_queued_microtasks,
                        (async_buffer_capacity - async_commands.capacity()) as f64,
                        &metric_labels
                    );
                    // Push the microtask onto the event queue and continue the event loop
                    command_queue.push_back((operation, Instant::now()));
                } else {
                    // The async microtask command stream has ended; break out of the event loop
                    break;
                }
            }
        }
    };
    (task, blocking_task_pool, commands_tx)
}

fn spawn_worker_process<TAction, TTask>(
    actor: <TTask as TaskFactory<TAction, TTask>>::Actor,
    pid: ProcessId,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &Arc<AtomicUsize>,
    caller: Option<(MessageOffset, ProcessId)>,
    results: &mpsc::Sender<TokioCommand<TAction, TTask>>,
    blocking_tasks: &mpsc::Sender<BlockingTaskRequest<TAction, TTask>>,
    task_monitor: &TaskMonitor,
) -> TokioProcess<TAction, TTask>
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    let (inbox_tx, inbox_rx) = mpsc::channel(1024);
    let inbox = TokioInbox(ReceiverStream::new(inbox_rx));
    let actor = Arc::new(actor);
    let parent_offset = caller.map(|(parent_offset, _)| parent_offset);
    let context = get_handler_context(pid, &next_pid);
    let (task, dispose) = create_worker_task(
        actor.clone(),
        inbox,
        next_offset.clone(),
        pid,
        parent_offset,
        context,
        results.clone(),
        blocking_tasks.clone(),
    );
    TokioProcess {
        inbox: inbox_tx,
        actor,
        // TODO: Panic main scheduler thread when worker task panics
        handle: tokio::spawn(task_monitor.instrument(task)),
        dispose: Some(dispose),
        subscribers: Default::default(),
    }
}

struct BlockingTaskRequest<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    actor: Arc<TTask::Actor>,
    state: <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    message: AsyncMessage<TAction>,
    metadata: MessageData,
    context: TokioContext,
    response: oneshot::Sender<(
        AsyncMessage<TAction>,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
        Option<SchedulerTransition<TAction, TTask>>,
    )>,
}

fn create_worker_task<TAction, TTask>(
    actor: Arc<TTask::Actor>,
    inbox: TokioInbox<TAction>,
    next_offset: Arc<AtomicUsize>,
    pid: ProcessId,
    parent_offset: Option<MessageOffset>,
    context: TokioContext,
    results: mpsc::Sender<TokioCommand<TAction, TTask>>,
    blocking_tasks: mpsc::Sender<BlockingTaskRequest<TAction, TTask>>,
) -> (
    impl Future<Output = ()>,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose,
)
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    let (state, inbox, dispose) = actor.init(inbox, &context);
    let task = {
        let mut actor_state = Some(state);
        let mut inbox = with_incrementing_message_offset(inbox, next_offset);
        async move {
            while let Some((message, offset)) = inbox.next().await {
                if let Some(mut state) = actor_state.take() {
                    let scheduler_mode = actor.schedule(&message, &state);
                    let (message, state, actions) = match scheduler_mode {
                        None => (message, state, None),
                        Some(SchedulerMode::Sync) => {
                            let metadata = get_message_metadata(offset, parent_offset);
                            let mut context = context.clone();
                            let actions =
                                actor.handle(&mut state, &message, &metadata, &mut context);
                            (message, state, actions)
                        }
                        Some(SchedulerMode::Async) => {
                            match tokio::spawn({
                                let actor = actor.clone();
                                let metadata = get_message_metadata(offset, parent_offset);
                                let mut context = context.clone();
                                async move {
                                    let actions =
                                        actor.handle(&mut state, &message, &metadata, &mut context);
                                    (message, state, actions)
                                }
                            })
                            .await
                            {
                                Err(err) => {
                                    if err.is_panic() {
                                        std::panic::resume_unwind(err.into_panic())
                                    } else {
                                        break;
                                    }
                                }
                                Ok((message, state, actions)) => (message, state, actions),
                            }
                        }
                        Some(SchedulerMode::Blocking) => {
                            let (response_tx, response_rx) = oneshot::channel();
                            let actor = actor.clone();
                            let metadata = get_message_metadata(offset, parent_offset);
                            let context = context.clone();
                            match blocking_tasks
                                .send(BlockingTaskRequest {
                                    actor,
                                    state,
                                    message,
                                    metadata,
                                    context,
                                    response: response_tx,
                                })
                                .await
                            {
                                Err(_err) => {
                                    break;
                                }
                                Ok(_) => match response_rx.await {
                                    Err(_err) => break,
                                    Ok((message, state, actions)) => (message, state, actions),
                                },
                            }
                        }
                    };
                    actor_state.replace(state);
                    if let Some(commands) = actions {
                        let commands = collect_worker_results(commands, message, offset, pid);
                        for command in commands {
                            let _ = results.send(command).await;
                        }
                    }
                }
            }
        }
    };
    (task, dispose)
}

fn with_incrementing_message_offset<T>(
    input: impl Stream<Item = T> + Unpin,
    next_offset: Arc<AtomicUsize>,
) -> impl Stream<Item = (T, MessageOffset)> {
    input.map(move |item| {
        let offset = MessageOffset::from(increment_atomic_counter(&next_offset));
        (item, offset)
    })
}

fn collect_worker_results<TAction: Action, TTask: TaskFactory<TAction, TTask>>(
    commands: SchedulerTransition<TAction, TTask>,
    message: AsyncMessage<TAction>,
    offset: MessageOffset,
    pid: ProcessId,
) -> VecDeque<TokioCommand<TAction, TTask>> {
    let caller = Some((offset, pid));
    enum RedispatchedMessage<T> {
        Owned(T, Option<ProcessId>),
        Shared(Arc<T>, usize),
    }
    let num_commands = commands.len();
    let (mut results, redispatch_state) = commands.into_iter().fold(
        (
            VecDeque::with_capacity(num_commands),
            match message {
                AsyncMessage::Owned(message) => RedispatchedMessage::Owned(message, None),
                AsyncMessage::Shared(message) => RedispatchedMessage::Shared(message, 0),
            },
        ),
        |(mut queue, redispatch_state), command| match command {
            SchedulerCommand::Send(target_pid, message) => {
                queue.push_back(TokioCommand::Send {
                    pid: target_pid,
                    message: AsyncMessage::Owned(message),
                    caller,
                });
                (queue, redispatch_state)
            }
            SchedulerCommand::Task(worker_pid, factory) => {
                queue.push_back(TokioCommand::Spawn {
                    pid: worker_pid,
                    factory,
                    caller,
                });
                (queue, redispatch_state)
            }
            SchedulerCommand::Kill(worker_pid) => {
                queue.push_back(TokioCommand::Kill { pid: worker_pid });
                (queue, redispatch_state)
            }
            SchedulerCommand::Forward(target_pid) => match redispatch_state {
                RedispatchedMessage::Owned(message, None) => {
                    // This message might also be forwarded to another target, so don't enqueue it yet
                    // (seeing as it might need to be wrapped in a shared pointer before adding to the queue)
                    let redispatch_state = RedispatchedMessage::Owned(message, Some(target_pid));
                    (queue, redispatch_state)
                }
                RedispatchedMessage::Owned(message, Some(existing_target_pid)) => {
                    // This is the second forwarding target for the message, so we need to wrap it in a shared pointer
                    // before enqueueing both commands at the FRONT of the queue, for immediate redispatch.
                    // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                    // once all commands have been processed).
                    let shared_message = Arc::new(message);
                    queue.push_front(TokioCommand::Send {
                        pid: existing_target_pid,
                        message: AsyncMessage::Shared(Arc::clone(&shared_message)),
                        caller,
                    });
                    queue.push_front(TokioCommand::Send {
                        pid: target_pid,
                        message: AsyncMessage::Shared(Arc::clone(&shared_message)),
                        caller,
                    });
                    let num_forwarded_messages = 2;
                    let redispatch_state =
                        RedispatchedMessage::Shared(shared_message, num_forwarded_messages);
                    (queue, redispatch_state)
                }
                RedispatchedMessage::Shared(shared_message, num_forwarded_messages) => {
                    // This is an additional forwarding target for an already-shared message, so we can just clone the
                    // existing shared pointer and enqueue the command at the FRONT of the queue, for immediate redispatch.
                    // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                    // once all commands have been processed).
                    queue.push_front(TokioCommand::Send {
                        pid: target_pid,
                        message: AsyncMessage::Shared(Arc::clone(&shared_message)),
                        caller,
                    });
                    let redispatch_state =
                        RedispatchedMessage::Shared(shared_message, num_forwarded_messages + 1);
                    (queue, redispatch_state)
                }
            },
        },
    );
    match redispatch_state {
        RedispatchedMessage::Owned(message, target_pid) => {
            // If after all commands have been processed we're left with a single forwarding target for the message,
            // enqueue the forwarded command at the FRONT of the queue, for immediate redispatch
            if let Some(target_pid) = target_pid {
                results.push_front(TokioCommand::Send {
                    pid: target_pid,
                    message: AsyncMessage::Owned(message),
                    caller,
                });
            }
            results
        }
        RedispatchedMessage::Shared(_shared_message, num_forwarded_messages) => {
            // If multiple forwarded messages were encountered, the iteration will have added them to the front of the
            // queue in the opposite order from the order in which they were produced, so we need to reverse them
            reverse_first_n_queue_entries(&mut results, num_forwarded_messages);
            results
        }
    }
}

fn record_time_spent_in_queue(enqueue_time: Instant, metric_names: &TokioSchedulerMetricNames) {
    histogram!(
        metric_names.event_bus_queued_duration_micros,
        enqueue_time.elapsed().as_micros() as f64
    );
}

#[derive(Debug, Clone)]
struct TokioContext {
    pid: ProcessId,
    next_pid: Arc<AtomicUsize>,
}
impl ActorInitContext for TokioContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
}
impl HandlerContext for TokioContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
    fn generate_pid(&mut self) -> ProcessId {
        increment_atomic_counter(&self.next_pid).into()
    }
}

fn get_message_metadata(
    offset: MessageOffset,
    parent_offset: Option<MessageOffset>,
) -> MessageData {
    MessageData {
        offset,
        parent: parent_offset,
        timestamp: std::time::Instant::now(),
    }
}

fn get_handler_context(pid: ProcessId, next_pid: &Arc<AtomicUsize>) -> TokioContext {
    TokioContext {
        pid,
        next_pid: Arc::clone(&next_pid),
    }
}

fn increment_atomic_counter(counter: &AtomicUsize) -> usize {
    counter.fetch_add(1, Ordering::Relaxed)
}

fn reverse_first_n_queue_entries<T>(queue: &mut VecDeque<T>, num_entries: usize) {
    // Queue entries are reversed by pointer-swapping, which avoids unnecessary allocations
    let midpoint = num_entries / 2;
    for i in 0..midpoint {
        let left_index = i;
        let right_index = num_entries - 1 - i;
        queue.swap(left_index, right_index)
    }
}
