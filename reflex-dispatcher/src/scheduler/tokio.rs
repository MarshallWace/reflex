// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    pin::Pin,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use futures::{future, Future, Sink, SinkExt, Stream, StreamExt};
use metrics::{describe_gauge, describe_histogram, gauge, histogram, Unit};
use tokio::{sync::mpsc, task::JoinHandle};
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::sync::{PollSendError, PollSender};

use crate::tokio_task_metrics_export::{get_task_monitor, TokioTaskMetricNames};
use crate::{
    utils::with_unsubscribe_callback::WithUnsubscribeCallback, Action, Actor, AsyncActionFilter,
    AsyncActionStream, AsyncDispatchResult, AsyncScheduler, AsyncSubscriptionStream,
    BoxedDisposeCallback, DisposeCallback, HandlerContext, MessageData, MessageOffset,
    MiddlewareContext, PostMiddleware, PreMiddleware, ProcessId, Scheduler, SchedulerMiddleware,
    StateOperation, StateTransition, Worker, WorkerContext, WorkerFactory,
};

#[derive(Clone, Copy, Debug)]
pub struct TokioSchedulerMetricNames {
    pub event_bus_queued_microtasks: &'static str,
    pub event_bus_microtask_queue_capacity: &'static str,
    pub event_processing_duration_micros: &'static str,
    pub event_batch_processing_duration_micros: &'static str,
    pub top_level_actor_handle_duration_micros: &'static str,
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
            self.top_level_actor_handle_duration_micros,
            Unit::Microseconds,
            "Time spent in the call to the top level actor by the tokio scheduler"
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
            top_level_actor_handle_duration_micros: "top_level_actor_handle_duration_micros",
            tokio_task_metric_names: TokioTaskMetricNames::default(),
        }
    }
}

enum TokioCommand<TAction>
where
    TAction: Action + Send + 'static,
{
    Dispatch(TAction), // Only used when invoked directly via scheduler instance method
    Event(StateOperation<TAction>, Option<(MessageOffset, ProcessId)>),
    WorkerResult(
        ProcessId,
        StateTransition<TAction>,
        MessageData,
        Option<ProcessId>,
    ),
    Subscribe(TokioSubscriberId, TokioSubscriber<TAction>),
    Unsubscribe(TokioSubscriberId),
}

pub struct TokioSinkError<T>(Option<T>);
impl<T> TokioSinkError<T> {
    pub fn into_inner(self) -> Option<T> {
        let Self(inner) = self;
        inner
    }
}
impl<TAction> From<PollSendError<TokioCommand<TAction>>> for TokioSinkError<TAction>
where
    TAction: Action + Send + 'static,
{
    fn from(err: PollSendError<TokioCommand<TAction>>) -> Self {
        match err.into_inner() {
            Some(TokioCommand::Event(StateOperation::Send(_, action), _)) => {
                TokioSinkError(Some(action))
            }
            _ => TokioSinkError(None),
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct TokioSubscriberId(usize);
impl From<usize> for TokioSubscriberId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

struct TokioSubscriber<TAction: Send + 'static> {
    emit:
        Box<dyn for<'a> Fn(&'a TAction) -> Option<Pin<Box<dyn Future<Output = ()> + Send>>> + Send>,
}
impl<TAction: Send + 'static> TokioSubscriber<TAction> {
    fn emit(&self, action: &TAction) -> Option<impl Future<Output = ()> + Send + Unpin> {
        (self.emit)(action)
    }
}
impl<TAction: Send + 'static> TokioSubscriber<TAction> {
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

pub struct TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    root_pid: ProcessId,
    commands: mpsc::Sender<TokioCommand<TAction>>,
    task: JoinHandle<()>,
    next_subscriber_id: AtomicUsize,
    unsubscribe_monitor: tokio_metrics::TaskMonitor,
}
impl<TAction> Drop for TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    fn drop(&mut self) {
        self.task.abort();
    }
}
impl<TAction> Scheduler for TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    type Action = TAction;
}
impl<TAction> AsyncScheduler for TokioScheduler<TAction>
where
    TAction: Action + Send + Clone + 'static,
{
    fn dispatch<TActions: AsyncActionStream<Self::Action>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult {
        TokioScheduler::dispatch(self, actions)
    }
    fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<TAction, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V> {
        TokioScheduler::subscribe(self, transform)
    }
}
enum TokioProcess<TAction>
where
    TAction: Action + Send + 'static,
{
    Task(TokioTask),
    Worker(TokioWorker<TAction>),
}
impl<TAction> TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    pub fn new<TActor, TPreMiddleware, TPostMiddleware>(
        actor: TActor,
        middleware: SchedulerMiddleware<TPreMiddleware, TPostMiddleware, TAction>,
        metric_names: TokioSchedulerMetricNames,
    ) -> Self
    where
        TActor: Actor<TAction> + Send + 'static,
        TActor::State: Send + 'static,
        TPreMiddleware: PreMiddleware<TAction> + Send + 'static,
        TPreMiddleware::State: Send + 'static,
        TPostMiddleware: PostMiddleware<TAction> + Send + 'static,
        TPostMiddleware::State: Send + 'static,
    {
        let metric_names = metric_names.init();
        let root_id = ProcessId::default();
        let (task, commands) = Self::init(actor, middleware, root_id, 1024, metric_names);
        let root_task_monitor =
            get_task_monitor(&metric_names.tokio_task_metric_names, "scheduler");
        Self {
            root_pid: root_id,
            commands,
            task: tokio::spawn(root_task_monitor.instrument(task)),
            next_subscriber_id: Default::default(),
            unsubscribe_monitor: get_task_monitor(
                &metric_names.tokio_task_metric_names,
                "unsubscribe",
            ),
        }
    }
    pub fn dispatch<TActions: AsyncActionStream<TAction>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult {
        Box::pin({
            let commands = self.commands.clone();
            async move {
                let mut actions = actions;
                while let Some(action) = actions.next().await {
                    let _ = commands.send(TokioCommand::Dispatch(action)).await;
                }
            }
        })
    }
    pub fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<TAction, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V> {
        let (results_tx, results_rx) = mpsc::channel(32);
        let subscriber_id = increment_atomic_counter(&self.next_subscriber_id).into();
        let subscriber = TokioSubscriber::new(move |action| {
            let value = transform.transform(action)?;
            let results = results_tx.clone();
            Some(Box::pin(async move {
                let _ = results.send(value).await;
            }))
        });
        let subscribe_action = {
            let commands = self.commands.clone();
            async move {
                let _ = commands
                    .send(TokioCommand::Subscribe(subscriber_id, subscriber))
                    .await;
            }
        };
        let unsubscribe_action = {
            let commands = self.commands.clone();
            async move {
                let _ = commands
                    .send(TokioCommand::Unsubscribe(subscriber_id))
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
    pub fn commands(&self) -> impl Sink<TAction> {
        let root_pid = self.root_pid;
        PollSender::new(self.commands.clone()).with(move |action| {
            future::ready(Result::<_, TokioSinkError<TAction>>::Ok(
                TokioCommand::Event(StateOperation::Send(root_pid, action), None),
            ))
        })
    }
    fn init<TActor, TPreMiddleware, TPostMiddleware>(
        actor: TActor,
        middleware: SchedulerMiddleware<TPreMiddleware, TPostMiddleware, TAction>,
        root_pid: ProcessId,
        async_buffer_capacity: usize,
        metric_names: TokioSchedulerMetricNames,
    ) -> (
        impl Future<Output = ()> + Send,
        mpsc::Sender<TokioCommand<TAction>>,
    )
    where
        TActor: Actor<TAction> + Send + 'static,
        TActor::State: Send + 'static,
        TPreMiddleware: PreMiddleware<TAction> + Send + 'static,
        TPreMiddleware::State: Send + 'static,
        TPostMiddleware: PostMiddleware<TAction> + Send + 'static,
        TPostMiddleware::State: Send + 'static,
    {
        let (commands_tx, mut commands_rx) =
            mpsc::channel::<TokioCommand<TAction>>(async_buffer_capacity);
        let metric_labels = [("pid", format!("{}", usize::from(root_pid)))];
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
        let subscription_monitor =
            get_task_monitor(&metric_names.tokio_task_metric_names, "subscription");
        let task_monitor = get_task_monitor(&metric_names.tokio_task_metric_names, "task");
        let abort_monitor = get_task_monitor(&metric_names.tokio_task_metric_names, "abort");
        let worker_monitor = get_task_monitor(&metric_names.tokio_task_metric_names, "worker");
        let task = {
            let mut actor_state = Some(actor.init());
            let mut pre_middleware_state = Some(middleware.pre.init());
            let mut post_middleware_state = Some(middleware.post.init());
            let mut subscribers = HashMap::<TokioSubscriberId, TokioSubscriber<TAction>>::new();
            let async_commands = commands_tx.clone();
            async move {
                let mut next_offset = MessageOffset::default();
                let next_pid = Arc::new(AtomicUsize::new(root_pid.next().into()));
                let mut processes = HashMap::<ProcessId, TokioProcess<TAction>>::default();
                let mut worker_command_queue = Vec::new();
                let mut command_queue = VecDeque::new();
                // Main runtime event loop
                loop {
                    // Process any queued synchronous commands
                    let command_batch_processing_start_time = std::time::Instant::now();
                    while let Some(command) = command_queue.pop_front() {
                        let command_processing_start_time = std::time::Instant::now();
                        let command_type = match &command {
                            TokioCommand::Dispatch(_) => "dispatch",
                            TokioCommand::Event(_, _) => "event",
                            TokioCommand::WorkerResult(_, _, _, _) => "worker_result",
                            TokioCommand::Subscribe(_, _) => "subscribe",
                            TokioCommand::Unsubscribe(_) => "unsubscribe",
                        };
                        let (action, child_commands, worker_commands) = process_command(
                            command,
                            &actor,
                            &middleware,
                            &mut subscribers,
                            &mut processes,
                            root_pid,
                            &next_pid,
                            &mut next_offset,
                            &mut actor_state,
                            &mut pre_middleware_state,
                            &mut post_middleware_state,
                            &async_commands,
                            &task_monitor,
                            &abort_monitor,
                            &worker_monitor,
                            metric_names,
                        );
                        // Add any spawned child commands to the event queue
                        command_queue.extend(child_commands.into_iter().map(
                            |(operation, parent_offset)| {
                                TokioCommand::Event(
                                    operation,
                                    parent_offset.map(|parent_offset| (parent_offset, root_pid)),
                                )
                            },
                        ));
                        // Collect any spawned worker commands for dispatching once all synchronous commands have been processed
                        worker_command_queue.extend(worker_commands);
                        // If an action was dispatched on the main process, notify any subscribers before processing the next command
                        if let Some(action) = action {
                            for subscriber in subscribers.values() {
                                if let Some(task) = subscriber.emit(&action) {
                                    let _ = tokio::spawn(subscription_monitor.instrument(task));
                                }
                            }
                        }

                        histogram!(
                            metric_names.event_processing_duration_micros,
                            command_processing_start_time.elapsed().as_micros() as f64,
                            "command_type" => command_type
                        )
                    }

                    histogram!(
                        metric_names.event_batch_processing_duration_micros,
                        command_batch_processing_start_time.elapsed().as_micros() as f64,
                    );
                    // Dispatch worker messages to the corresponding async workers
                    let worker_tasks = worker_command_queue
                        .drain(..)
                        .filter_map(|(pid, (action, metadata, context))| {
                            processes.get(&pid).and_then(|process| match process {
                                TokioProcess::Worker(worker) => {
                                    Some(worker.inbox.send((action, metadata, context)))
                                }
                                _ => None,
                            })
                        })
                        .collect::<Vec<_>>();
                    for task in worker_tasks {
                        let _ = task.await;
                    }
                    // All synchronous tasks have been processed, so wait until the next asynchronous microtask arrives
                    if let Some(operation) = commands_rx.recv().await {
                        gauge!(
                            metric_names.event_bus_queued_microtasks,
                            (async_buffer_capacity - async_commands.capacity()) as f64,
                            &metric_labels
                        );
                        // Push the microtask onto the event queue and continue the event loop
                        command_queue.push_back(operation);
                    } else {
                        // The async microtask command stream has ended; break out of the event loop
                        break;
                    }
                }
            }
        };
        (task, commands_tx)
    }
}

fn process_command<
    TActor: Actor<TAction>,
    TPreMiddleware: PreMiddleware<TAction>,
    TPostMiddleware: PostMiddleware<TAction>,
    TAction: Action + Send,
>(
    command: TokioCommand<TAction>,
    actor: &TActor,
    middleware: &SchedulerMiddleware<TPreMiddleware, TPostMiddleware, TAction>,
    subscribers: &mut HashMap<TokioSubscriberId, TokioSubscriber<TAction>>,
    processes: &mut HashMap<ProcessId, TokioProcess<TAction>>,
    root_pid: ProcessId,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &mut MessageOffset,
    actor_state: &mut Option<TActor::State>,
    pre_middleware_state: &mut Option<TPreMiddleware::State>,
    post_middleware_state: &mut Option<TPostMiddleware::State>,
    async_commands: &mpsc::Sender<TokioCommand<TAction>>,
    task_monitor: &tokio_metrics::TaskMonitor,
    abort_monitor: &tokio_metrics::TaskMonitor,
    worker_monitor: &tokio_metrics::TaskMonitor,
    metric_names: TokioSchedulerMetricNames,
) -> (
    Option<TAction>,
    impl IntoIterator<
            Item = (StateOperation<TAction>, Option<MessageOffset>),
            IntoIter = impl Iterator<Item = (StateOperation<TAction>, Option<MessageOffset>)> + Send,
        > + Send,
    impl IntoIterator<
            Item = (ProcessId, (TAction, MessageData, TokioContext)),
            IntoIter = impl Iterator<Item = (ProcessId, (TAction, MessageData, TokioContext))> + Send,
        > + Send,
) {
    let (emitted_action, child_commands, worker_commands) = match command {
        TokioCommand::Dispatch(action) => {
            let offset = {
                let next_value = next_offset.next();
                std::mem::replace(next_offset, next_value)
            };
            let metadata = MessageData {
                offset,
                parent: None,
                timestamp: std::time::Instant::now(),
            };
            let emitted_action = None;
            let child_commands = {
                let operations = vec![StateOperation::Send(root_pid, action)];
                let parent_offset = None;
                let caller_pid = root_pid;
                Some((operations, metadata, parent_offset, caller_pid))
            };
            let worker_commands = None;
            (emitted_action, child_commands, worker_commands)
        }
        TokioCommand::Event(operation, caller) => {
            let offset = {
                let next_value = next_offset.next();
                std::mem::replace(next_offset, next_value)
            };
            let metadata = MessageData {
                offset,
                parent: caller.map(|(parent_offset, _)| parent_offset),
                timestamp: std::time::Instant::now(),
            };
            let caller_pid = caller.map(|(_, caller_pid)| caller_pid);
            let operation = {
                let (updated_state, operation) = middleware
                    .pre
                    .handle(
                        pre_middleware_state.take().unwrap(),
                        operation,
                        &metadata,
                        &MiddlewareContext { caller_pid },
                    )
                    .into_parts();
                pre_middleware_state.replace(updated_state);
                operation
            };
            match operation {
                StateOperation::Send(pid, action) => {
                    let mut context = TokioContext {
                        pid,
                        caller_pid,
                        next_pid: Arc::clone(&next_pid),
                    };
                    if pid == root_pid {
                        let child_commands = {
                            let actor_processing_start_time = std::time::Instant::now();
                            let (updated_state, child_commands) = actor
                                .handle(
                                    actor_state.take().unwrap(),
                                    &action,
                                    &metadata,
                                    &mut context,
                                )
                                .into_parts();
                            actor_state.replace(updated_state);
                            histogram!(
                                metric_names.top_level_actor_handle_duration_micros,
                                actor_processing_start_time.elapsed().as_micros() as f64
                            );
                            child_commands
                        };
                        let emitted_action = Some(action);
                        let child_commands = {
                            let operations = child_commands.into_operations();
                            let parent_offset = Some(metadata.offset);
                            let caller_pid = pid;
                            Some((operations, metadata, parent_offset, caller_pid))
                        };
                        let worker_commands = None;
                        (emitted_action, child_commands, worker_commands)
                    } else {
                        let emitted_action = None;
                        let child_commands = None;
                        let worker_commands = {
                            let worker_pid = pid;
                            Some((worker_pid, (action, metadata, context)))
                        };
                        (emitted_action, child_commands, worker_commands)
                    }
                }
                StateOperation::Task(pid, task) => {
                    if let Entry::Vacant(entry) = processes.entry(pid) {
                        let (mut stream, dispose) = task.into_parts();
                        entry.insert(TokioProcess::Task(TokioTask {
                            handle: tokio::spawn(task_monitor.instrument({
                                let results = async_commands.clone();
                                async move {
                                    while let Some(operation) = stream.next().await {
                                        let _ = results
                                            .send(TokioCommand::Event(
                                                operation,
                                                Some((metadata.offset, pid)),
                                            ))
                                            .await;
                                    }
                                }
                            })),
                            dispose: Some(dispose),
                        }));
                    }
                    let emitted_action = None;
                    let child_commands = None;
                    let worker_commands = None;
                    (emitted_action, child_commands, worker_commands)
                }
                StateOperation::Spawn(pid, factory) => {
                    if let Entry::Vacant(entry) = processes.entry(pid) {
                        entry.insert(TokioProcess::Worker(TokioWorker::new(
                            factory,
                            async_commands.clone(),
                            &worker_monitor,
                        )));
                    }
                    let emitted_action = None;
                    let child_commands = None;
                    let worker_commands = None;
                    (emitted_action, child_commands, worker_commands)
                }
                StateOperation::Kill(pid) => {
                    if let Entry::Occupied(entry) = processes.entry(pid) {
                        match entry.remove() {
                            TokioProcess::Task(mut task) => {
                                tokio::spawn(
                                    abort_monitor.instrument(async move { task.abort().await }),
                                );
                            }
                            TokioProcess::Worker(_) => {}
                        }
                    }
                    let emitted_action = None;
                    let child_commands = None;
                    let worker_commands = None;
                    (emitted_action, child_commands, worker_commands)
                }
            }
        }
        TokioCommand::WorkerResult(worker_pid, transition, metadata, _caller_pid) => {
            let emitted_action = None;
            let child_commands = {
                let operations = transition.into_operations();
                let parent_offset = Some(metadata.offset);
                let caller_pid = worker_pid;
                Some((operations, metadata, parent_offset, caller_pid))
            };
            let worker_commands = None;
            (emitted_action, child_commands, worker_commands)
        }
        TokioCommand::Subscribe(subscriber_id, subscriber) => {
            if let Entry::Vacant(entry) = subscribers.entry(subscriber_id) {
                entry.insert(subscriber);
            }
            (None, None, None)
        }
        TokioCommand::Unsubscribe(subscriber_id) => {
            subscribers.remove(&subscriber_id);
            (None, None, None)
        }
    };
    // Apply post-middleware to transform outgoing commands
    let child_commands = child_commands
        .map(|(operations, metadata, parent_offset, caller_pid)| {
            let (updated_state, child_commands) = middleware
                .post
                .handle(
                    post_middleware_state.take().unwrap(),
                    operations,
                    &metadata,
                    &MiddlewareContext {
                        caller_pid: Some(caller_pid),
                    },
                )
                .into_parts();
            post_middleware_state.replace(updated_state);
            child_commands
                .into_iter()
                .map(move |operation| (operation, parent_offset))
        })
        .into_iter()
        .flatten();
    (emitted_action, child_commands, worker_commands)
}

#[derive(Debug, Clone)]
struct TokioContext {
    pid: ProcessId,
    caller_pid: Option<ProcessId>,
    next_pid: Arc<AtomicUsize>,
}
impl HandlerContext for TokioContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
    fn caller_pid(&self) -> Option<ProcessId> {
        self.caller_pid
    }
    fn generate_pid(&mut self) -> ProcessId {
        increment_atomic_counter(&self.next_pid).into()
    }
}

pub struct TokioTask {
    handle: JoinHandle<()>,
    dispose: Option<BoxedDisposeCallback>,
}
impl TokioTask {
    async fn abort(&mut self) {
        if let Some(callback) = self.dispose.take() {
            callback.dispose().await;
            self.handle.abort();
        } else {
            self.handle.abort();
        }
    }
}

struct TokioWorker<TAction>
where
    TAction: Action + Send + 'static,
{
    inbox: mpsc::Sender<(TAction, MessageData, TokioContext)>,
    task: JoinHandle<()>,
}
impl<TAction> Drop for TokioWorker<TAction>
where
    TAction: Action + Send + 'static,
{
    fn drop(&mut self) {
        self.task.abort();
    }
}
impl<TAction> TokioWorker<TAction>
where
    TAction: Action + Send + 'static,
{
    fn new(
        factory: impl WorkerFactory<TAction, Worker = impl Worker<TAction> + Send + 'static>
            + Send
            + 'static,
        outbox: mpsc::Sender<TokioCommand<TAction>>,
        worker_monitor: &tokio_metrics::TaskMonitor,
    ) -> Self {
        let (inbox_tx, mut inbox_rx) = mpsc::channel(1024);
        Self {
            inbox: inbox_tx,
            task: tokio::spawn(worker_monitor.instrument({
                async move {
                    let mut worker = Some(factory.create());
                    while let Some((action, metadata, context)) = inbox_rx.recv().await {
                        if let Some(mut worker_instance) = worker.take() {
                            let worker_pid = context.pid();
                            let caller_pid = context.caller_pid();
                            let worker_task = tokio::task::spawn_blocking(move || {
                                let child_commands = worker_instance
                                    .handle(action, &metadata, &mut WorkerContext::new(context))
                                    .into_inner();
                                (worker_instance, child_commands)
                            });
                            match worker_task.await {
                                Ok((worker_instance, transition)) => {
                                    worker.replace(worker_instance);
                                    let _ = outbox
                                        .send(TokioCommand::WorkerResult(
                                            worker_pid, transition, metadata, caller_pid,
                                        ))
                                        .await;
                                }
                                Err(err) => {
                                    if err.is_cancelled() {
                                        break;
                                    } else if err.is_panic() {
                                        std::panic::resume_unwind(err.into_panic())
                                    }
                                }
                            }
                        }
                    }
                }
            })),
        }
    }
}

fn increment_atomic_counter(counter: &AtomicUsize) -> usize {
    counter.fetch_add(1, Ordering::Relaxed)
}
