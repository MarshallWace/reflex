// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use futures::{Future, StreamExt};
use metrics::{describe_gauge, gauge, Unit};
use tokio::{sync::mpsc, task::JoinHandle};

use crate::{
    Action, Actor, BoxedDisposeCallback, DisposeCallback, HandlerContext, MessageData,
    MessageOffset, ProcessId, Scheduler, StateOperation, WorkerContext, WorkerFactory,
    WorkerMessageQueue,
};

#[derive(Clone, Copy, Debug)]
pub struct TokioSchedulerMetricNames {
    pub event_bus_capacity: &'static str,
    pub event_bus_queued_messages: &'static str,
}
impl TokioSchedulerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.event_bus_capacity,
            Unit::Count,
            "Event bus queue capacity"
        );
        describe_gauge!(
            self.event_bus_queued_messages,
            Unit::Count,
            "Number of event bus messages currently queued awaiting processing"
        );
        self
    }
}
impl Default for TokioSchedulerMetricNames {
    fn default() -> Self {
        Self {
            event_bus_capacity: "event_bus_capacity",
            event_bus_queued_messages: "event_bus_queued_messages",
        }
    }
}

pub struct TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    root_pid: ProcessId,
    commands: mpsc::Sender<(StateOperation<TAction>, Option<(MessageOffset, ProcessId)>)>,
    task: JoinHandle<()>,
}
impl<TAction> Drop for TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    fn drop(&mut self) {
        self.task.abort();
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
    pub fn new(
        main: impl Actor<TAction> + Send + 'static,
        metric_names: TokioSchedulerMetricNames,
    ) -> Self {
        let metric_names = metric_names.init();
        let root_id = ProcessId::default();
        let (task, commands) = Self::listen(main, root_id, 1024, metric_names);
        Self {
            root_pid: root_id,
            commands,
            task: tokio::spawn(task),
        }
    }
    fn listen(
        actor: impl Actor<TAction>,
        root_pid: ProcessId,
        buffer_capacity: usize,
        metric_names: TokioSchedulerMetricNames,
    ) -> (
        impl Future<Output = ()>,
        mpsc::Sender<(StateOperation<TAction>, Option<(MessageOffset, ProcessId)>)>,
    ) {
        let (commands_tx, mut commands_rx) = mpsc::channel::<(
            StateOperation<TAction>,
            Option<(MessageOffset, ProcessId)>,
        )>(buffer_capacity);
        let metric_labels = [("pid", format!("{}", usize::from(root_pid)))];
        gauge!(
            metric_names.event_bus_capacity,
            buffer_capacity as f64,
            &metric_labels
        );
        gauge!(metric_names.event_bus_queued_messages, 0.0, &metric_labels);
        let task = {
            let mut actor = actor;
            let child_commands = commands_tx.clone();
            async move {
                let mut next_offset = MessageOffset::default();
                let next_pid = Arc::new(AtomicUsize::new(root_pid.next().into()));
                let mut processes = HashMap::<ProcessId, TokioProcess<TAction>>::default();
                while let Some((operation, caller)) = commands_rx.recv().await {
                    gauge!(
                        metric_names.event_bus_queued_messages,
                        (buffer_capacity - child_commands.capacity()) as f64,
                        &metric_labels
                    );
                    match operation {
                        StateOperation::Send(pid, action) => {
                            let metadata = MessageData {
                                offset: {
                                    let next_value = next_offset.next();
                                    std::mem::replace(&mut next_offset, next_value)
                                },
                                parent: caller.map(|(parent_offset, _)| parent_offset),
                                timestamp: std::time::Instant::now(),
                            };
                            let mut context = TokioContext {
                                pid,
                                caller_pid: caller.map(|(_, caller_pid)| caller_pid),
                                next_pid: Arc::clone(&next_pid),
                            };
                            if pid == root_pid {
                                let transition = actor.handle(&action, &metadata, &mut context);
                                for operation in transition {
                                    let _ = child_commands
                                        .send((operation, Some((metadata.offset, pid))))
                                        .await;
                                }
                            } else if let Some(TokioProcess::Worker(worker)) =
                                processes.get_mut(&pid)
                            {
                                let _ = worker.inbox.send((action, metadata, context)).await;
                            };
                        }
                        StateOperation::Task(pid, task) => {
                            if let Entry::Vacant(entry) = processes.entry(pid) {
                                let (mut stream, dispose) = task.into_parts();
                                entry.insert(TokioProcess::Task(TokioTask {
                                    handle: tokio::spawn({
                                        let results = child_commands.clone();
                                        async move {
                                            while let Some(operation) = stream.next().await {
                                                let _ = results.send((operation, caller)).await;
                                            }
                                        }
                                    }),
                                    dispose: Some(dispose),
                                }));
                            }
                        }
                        StateOperation::Spawn(pid, factory) => {
                            if let Entry::Vacant(entry) = processes.entry(pid) {
                                entry.insert(TokioProcess::Worker(TokioWorker::new(
                                    factory,
                                    child_commands.clone(),
                                )));
                            }
                        }
                        StateOperation::Kill(pid) => {
                            if let Entry::Occupied(entry) = processes.entry(pid) {
                                match entry.remove() {
                                    TokioProcess::Task(mut task) => {
                                        tokio::spawn(async move { task.abort().await });
                                    }
                                    TokioProcess::Worker(_) => {}
                                }
                            }
                        }
                    }
                }
            }
        };
        (task, commands_tx)
    }
}
impl<TAction> Scheduler for TokioScheduler<TAction>
where
    TAction: Action + Send + 'static,
{
    type Action = TAction;
    fn dispatch(&mut self, action: Self::Action) {
        let root_pid = self.root_pid;
        let commands = self.commands.clone();
        let _ = tokio::spawn(async move {
            commands
                .send((StateOperation::Send(root_pid, action), None))
                .await
        });
    }
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
        factory: WorkerFactory<TAction>,
        outbox: mpsc::Sender<(StateOperation<TAction>, Option<(MessageOffset, ProcessId)>)>,
    ) -> Self {
        let (inbox_tx, mut inbox_rx) = mpsc::channel(1024);
        Self {
            inbox: inbox_tx,
            task: tokio::spawn({
                async move {
                    let mut worker = Some(factory.create());
                    while let Some(message) = inbox_rx.recv().await {
                        let mut queue = WorkerMessageQueue::default();
                        let mut latest_context = None;
                        let mut latest_message = Some(message);
                        while let Some((action, metadata, context)) = latest_message {
                            latest_context.replace(context);
                            queue.push_back((action, metadata));
                            latest_message = inbox_rx.try_recv().ok();
                        }
                        if let Some(context) = latest_context {
                            let pid = context.pid();
                            if let Some(mut worker_instance) = worker.take() {
                                let worker_task = tokio::task::spawn_blocking(move || {
                                    let actions = worker_instance
                                        .handle(queue, &mut WorkerContext::new(context));
                                    (worker_instance, actions)
                                });
                                match worker_task.await {
                                    Ok((worker_instance, actions)) => {
                                        worker.replace(worker_instance);
                                        for (offset, operation) in actions {
                                            let _ =
                                                outbox.send((operation, Some((offset, pid)))).await;
                                        }
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
                }
            }),
        }
    }
}

fn increment_atomic_counter(counter: &AtomicUsize) -> usize {
    counter.fetch_add(1, Ordering::Relaxed)
}
