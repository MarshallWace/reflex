// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    iter::once,
    marker::PhantomData,
    ops::Deref,
    pin::Pin,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use futures::{
    future::{self, Ready},
    sink::With,
    Future, SinkExt, Stream, StreamExt,
};
use serde::{Deserialize, Serialize};
use tokio::{
    sync::{
        mpsc::{self, error::SendError},
        oneshot,
    },
    task::JoinHandle,
};
use tokio_stream::wrappers::ReceiverStream;
use tokio_util::sync::{PollSendError, PollSender};

use reflex_dispatcher::{
    utils::with_unsubscribe_callback::WithUnsubscribeCallback, Action, Actor, ActorEvents,
    AsyncScheduler, Handler, HandlerContext, MessageData, MessageOffset, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox, TaskMessage,
    Worker,
};

pub mod metrics;

pub trait TokioSchedulerInstrumentation {
    type Action: Action;
    type Task: TaskFactory<Self::Action, Self::Task>;
    type InstrumentedTask<T: Future + Send + 'static>: Future<Output = T::Output> + Send + 'static;
    fn instrument_main_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_async_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_blocking_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_worker_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_task_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_dispose_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_subscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn instrument_unsubscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
    fn record_scheduler_queue_capacity(&self, value: usize);
    fn record_scheduler_enqueue(&self, num_commands: usize);
    fn record_scheduler_dequeue(&self);
    fn record_scheduler_state(&self, state: TokioSchedulerState);
    fn record_worker_state(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        state: TokioWorkerState,
    );
    fn record_scheduler_command_waiting_duration(&self, value: Duration);
    fn record_scheduler_command_working_duration(&self, value: Duration);
    fn record_worker_spawn(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        inbox_capacity: usize,
    );
    fn record_worker_kill(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        inbox_capacity: usize,
        inbox_size: usize,
    );
    fn record_worker_action_enqueue(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        action: &Self::Action,
    );
    fn record_worker_action_dequeue(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        action: &Self::Action,
    );
    fn record_worker_action_waiting_duration(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        action: &Self::Action,
        value: Duration,
    );
    fn record_worker_action_working_duration(
        &self,
        pid: ProcessId,
        actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        action: &Self::Action,
        value: Duration,
    );
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum TokioSchedulerState {
    Waiting,
    Working {
        queue_size: usize,
        capacity: usize,
    },
    Dispatching {
        target_queue_size: usize,
        target_capacity: usize,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum TokioWorkerState {
    Waiting,
    Working,
    Sending {
        target_queue_size: usize,
        target_capacity: usize,
    },
    Awaiting,
    Dispatching {
        num_commands: usize,
        target_queue_size: usize,
        target_capacity: usize,
    },
}

pub enum TokioCommand<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    Send {
        pid: ProcessId,
        message: AsyncMessage<TAction>,
    },
    Spawn {
        pid: ProcessId,
        factory: TTask,
        caller: Option<(MessageOffset, ProcessId)>,
    },
    Kill {
        pid: ProcessId,
        caller: Option<(MessageOffset, ProcessId)>,
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

pub enum AsyncMessage<TAction> {
    // Message returned by a handler that has not yet been enqueued by the scheduler
    Pending {
        message: TAction,
    },
    // Non-shared message that has been enqueued by the scheduler
    Owned {
        message: TAction,
        offset: MessageOffset,
        redispatched_from: Option<MessageOffset>,
        caller: Option<(MessageOffset, ProcessId)>,
        enqueue_time: Instant,
    },
    // Shared message that has been enqueued by the scheduler
    Shared {
        message: Arc<TAction>,
        offset: MessageOffset,
        redispatched_from: Option<MessageOffset>,
        caller: Option<(MessageOffset, ProcessId)>,
        enqueue_time: Instant,
    },
}
impl<TAction> AsyncMessage<TAction>
where
    TAction: Action,
{
    pub fn offset(&self) -> Option<MessageOffset> {
        match self {
            AsyncMessage::Pending { .. } => None,
            AsyncMessage::Owned { offset, .. } => Some(*offset),
            AsyncMessage::Shared { offset, .. } => Some(*offset),
        }
    }
    pub fn parent_offset(&self) -> Option<MessageOffset> {
        match self {
            AsyncMessage::Pending { .. } => None,
            AsyncMessage::Owned { caller, .. } => caller.map(|(parent_offset, _)| parent_offset),
            AsyncMessage::Shared { caller, .. } => caller.map(|(parent_offset, _)| parent_offset),
        }
    }
    pub fn redispatched_from(&self) -> Option<MessageOffset> {
        match self {
            AsyncMessage::Pending { .. } => None,
            AsyncMessage::Owned {
                redispatched_from, ..
            } => *redispatched_from,
            AsyncMessage::Shared {
                redispatched_from, ..
            } => *redispatched_from,
        }
    }
    pub fn caller(&self) -> Option<Option<(MessageOffset, ProcessId)>> {
        match self {
            AsyncMessage::Pending { .. } => None,
            AsyncMessage::Owned { caller, .. } => Some(*caller),
            AsyncMessage::Shared { caller, .. } => Some(*caller),
        }
    }
    pub fn enqueue_time(&self) -> Option<Instant> {
        match self {
            AsyncMessage::Pending { .. } => None,
            AsyncMessage::Owned { enqueue_time, .. } => Some(*enqueue_time),
            AsyncMessage::Shared { enqueue_time, .. } => Some(*enqueue_time),
        }
    }
}
impl<TAction> TaskMessage<TAction> for AsyncMessage<TAction> where TAction: Action {}
impl<TAction> From<TAction> for AsyncMessage<TAction> {
    fn from(value: TAction) -> Self {
        Self::Pending { message: value }
    }
}
impl<TAction> Deref for AsyncMessage<TAction> {
    type Target = TAction;
    fn deref(&self) -> &Self::Target {
        match self {
            AsyncMessage::Pending { message } => message,
            AsyncMessage::Owned { message, .. } => message,
            AsyncMessage::Shared { message, .. } => message.as_ref(),
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
impl<TAction, TTask> From<PollSendError<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>>
    for TokioSinkError<Vec<(AsyncMessage<TAction>, Instant)>>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn from(err: PollSendError<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>) -> Self {
        match err.into_inner() {
            Some(commands) => {
                let messages = commands
                    .into_iter()
                    .filter_map(|(command, enqueue_time)| match command {
                        TokioCommand::Send { pid: _, message } => Some((message, enqueue_time)),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                TokioSinkError(if messages.is_empty() {
                    None
                } else {
                    Some(messages)
                })
            }
            _ => TokioSinkError(None),
        }
    }
}

struct TokioEventChannel<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Send + 'static,
{
    sender: mpsc::Sender<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>,
    instrumentation: TInstrumentation,
}
impl<TAction, TTask, TInstrumentation> Clone for TokioEventChannel<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Clone + Send + 'static,
{
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            instrumentation: self.instrumentation.clone(),
        }
    }
}
impl<TAction, TTask, TInstrumentation> TokioEventChannel<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Send + 'static,
{
    async fn send(
        &self,
        commands: VecDeque<(TokioCommand<TAction, TTask>, Instant)>,
    ) -> Result<(), SendError<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>> {
        self.instrumentation
            .record_scheduler_enqueue(commands.len());
        self.sender.send(commands).await
    }
    fn capacity(&self) -> usize {
        self.sender.max_capacity()
    }
    fn queue_size(&self) -> usize {
        self.sender.max_capacity() - self.sender.capacity()
    }
}

pub struct TokioScheduler<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Send + 'static,
{
    commands: TokioEventChannel<TAction, TTask, TInstrumentation>,
    task: JoinHandle<()>,
    async_task_pool: JoinHandle<()>,
    blocking_task_pool: JoinHandle<()>,
    next_subscriber_id: AtomicUsize,
    next_offset: Arc<AtomicUsize>,
}
impl<TAction, TTask, TInstrumentation> Drop for TokioScheduler<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Send + 'static,
{
    fn drop(&mut self) {
        self.task.abort();
        self.async_task_pool.abort();
        self.blocking_task_pool.abort();
    }
}

enum TokioProcess<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    // Synchronous actor
    Actor(TokioActorInstance<TAction, TTask>),
    // Asynchronous event stream (used as input to an actor process)
    Task(TokioTask<TAction, TTask>),
}

struct TokioActorInstance<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    inbox: mpsc::Sender<AsyncMessage<TAction>>,
    inbox_capacity: usize,
    actor: Arc<TTask::Actor>,
    handle: JoinHandle<()>,
    subscribers: HashMap<TokioSubscriberId, TokioSubscriber<TAction>>,
}
impl<TAction, TTask> Drop for TokioActorInstance<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn drop(&mut self) {
        self.handle.abort();
    }
}

struct TokioTask<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    inbox: mpsc::Sender<AsyncMessage<TAction>>,
    inbox_capacity: usize,
    actor_pid: ProcessId,
    handle: JoinHandle<()>,
    dispose: Option<<TTask::Actor as Actor<TAction, TTask>>::Dispose>,
}
impl<TAction, TTask> Drop for TokioTask<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn drop(&mut self) {
        self.handle.abort();
    }
}
impl<TAction, TTask> TokioTask<TAction, TTask>
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
pub struct TokioSubscriberId(usize);
impl From<usize> for TokioSubscriberId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

pub struct TokioSubscriber<TAction> {
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

pub trait TokioThreadPoolFactory<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    type Task: Future<Output = ()> + Unpin + Send + 'static;
    fn create(
        self,
        requests: impl Stream<Item = TokioThreadPoolRequest<TAction, TTask>> + Unpin + Send + 'static,
    ) -> Self::Task;
}

pub struct TokioThreadPoolRequest<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    pub actor: Arc<TTask::Actor>,
    pub state: <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    pub message: AsyncMessage<TAction>,
    pub metadata: MessageData,
    pub context: TokioHandlerContext,
    pub response: oneshot::Sender<(
        AsyncMessage<TAction>,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
        Option<SchedulerTransition<TAction, TTask>>,
    )>,
}

impl<TAction, TTask, TInstrumentation> TokioScheduler<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    pub fn new<V>(
        init_commands: impl FnOnce(
            &mut TokioInitContext,
        ) -> (
            Vec<(ProcessId, TTask::Actor)>,
            SchedulerTransition<TAction, TTask>,
            V,
        ),
        logger: impl TokioSchedulerLogger<Action = TAction, Task = TTask> + Send + 'static,
        instrumentation: TInstrumentation,
        async_tasks: impl TokioThreadPoolFactory<TAction, TTask> + 'static,
        blocking_tasks: impl TokioThreadPoolFactory<TAction, TTask> + 'static,
    ) -> (Self, V)
    where
        TAction: Sync,
        TTask::Actor: Send + Sync + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State:
            Send + 'static,
    {
        let mut context = TokioInitContext {
            next_pid: ProcessId::default().next(),
        };
        let (init_processes, commands, result) = (init_commands)(&mut context);
        let TokioInitContext { next_pid } = context;
        let next_offset = Arc::new(AtomicUsize::new(Default::default()));
        let (main_event_loop, async_task_pool, blocking_task_pool, commands) =
            create_scheduler_thread(
                async_tasks,
                blocking_tasks,
                init_processes,
                commands,
                next_pid,
                next_offset.clone(),
                1024,
                logger,
                instrumentation.clone(),
            );
        (
            Self {
                commands,
                // TODO: Panic main thread when scheduler task threads panic
                task: tokio::spawn(instrumentation.instrument_main_thread(main_event_loop)),
                async_task_pool: tokio::spawn(
                    instrumentation.instrument_async_task_pool(async_task_pool),
                ),
                blocking_task_pool: tokio::spawn(
                    instrumentation.instrument_blocking_task_pool(blocking_task_pool),
                ),
                next_subscriber_id: Default::default(),
                next_offset,
            },
            result,
        )
    }
}

impl<TAction, TTask, TInstrumentation> AsyncScheduler
    for TokioScheduler<TAction, TTask, TInstrumentation>
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Clone + Send + Sync,
{
    type Action = TAction;
    type Sink = With<
        PollSender<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>,
        VecDeque<(TokioCommand<TAction, TTask>, Instant)>,
        TAction,
        Ready<
            Result<
                VecDeque<(TokioCommand<TAction, TTask>, Instant)>,
                TokioSinkError<Vec<(AsyncMessage<TAction>, Instant)>>,
            >,
        >,
        Box<
            dyn FnMut(
                    TAction,
                ) -> Ready<
                    Result<
                        VecDeque<(TokioCommand<TAction, TTask>, Instant)>,
                        TokioSinkError<Vec<(AsyncMessage<TAction>, Instant)>>,
                    >,
                > + Send,
        >,
    >;
    type Subscription<F, V> = Pin<Box<dyn Future<Output = Self::SubscriptionResults<F, V>> + Send + 'static>>
    where
        F: Fn(&Self::Action) -> Option<V>,
        V: Send + 'static;
    type SubscriptionResults<F, V> = Pin<Box<dyn Stream<Item = V> + Send + 'static>>
    where
        F: Fn(&Self::Action) -> Option<V>,
        V: Send + 'static;

    fn actions(&self, pid: ProcessId) -> Self::Sink {
        PollSender::new(self.commands.sender.clone()).with(Box::new({
            let next_offset = self.next_offset.clone();
            let instrumentation = self.commands.instrumentation.clone();
            move |message| {
                future::ready(Ok({
                    instrumentation.record_scheduler_enqueue(1);
                    let enqueue_time = Instant::now();
                    let offset = MessageOffset::from(increment_atomic_counter(&next_offset));
                    [(
                        TokioCommand::Send {
                            pid,
                            message: AsyncMessage::Owned {
                                message,
                                offset,
                                redispatched_from: None,
                                caller: None,
                                enqueue_time,
                            },
                        },
                        enqueue_time,
                    )]
                    .into_iter()
                    .collect()
                }))
            }
        }))
    }
    fn subscribe<F, V>(&self, pid: ProcessId, selector: F) -> Self::Subscription<F, V>
    where
        F: Fn(&Self::Action) -> Option<V> + Send + 'static,
        V: Send + 'static,
    {
        let (results_tx, results_rx) = mpsc::channel(32);
        let subscription_id =
            TokioSubscriberId::from(increment_atomic_counter(&self.next_subscriber_id));
        let subscriber = TokioSubscriber::new(move |action| {
            let value = selector(action)?;
            let results = results_tx.clone();
            Some(Box::pin(async move {
                let _ = results.send(value).await;
            }))
        });
        let subscribe_task = {
            let commands = self.commands.clone();
            async move {
                let send_task = commands.send(
                    [(
                        TokioCommand::Subscribe {
                            subscription_id,
                            pid,
                            subscriber,
                        },
                        Instant::now(),
                    )]
                    .into_iter()
                    .collect(),
                );
                let _ = send_task.await;
            }
        };
        let unsubscribe_task = {
            let commands = self.commands.clone();
            async move {
                let _ = commands
                    .send(
                        [(
                            TokioCommand::Unsubscribe {
                                subscription_id,
                                pid,
                            },
                            Instant::now(),
                        )]
                        .into_iter()
                        .collect(),
                    )
                    .await;
            }
        };
        let results_stream = ReceiverStream::new(results_rx);
        Box::pin(async move {
            let _ = subscribe_task.await;
            // At this point the subscription has been initialized, which is a good opportunity for the listener to
            // perform any actions before subscribing to the results stream
            let results: Self::SubscriptionResults<F, V> =
                Box::pin(WithUnsubscribeCallback::new(results_stream, {
                    move || {
                        let _ = tokio::spawn(unsubscribe_task);
                    }
                }));
            results
        })
    }
}

fn create_scheduler_thread<TAction, TTask, TInstrumentation>(
    async_tasks: impl TokioThreadPoolFactory<TAction, TTask>,
    blocking_tasks: impl TokioThreadPoolFactory<TAction, TTask>,
    init_processes: impl IntoIterator<Item = (ProcessId, TTask::Actor)>,
    init_commands: SchedulerTransition<TAction, TTask>,
    next_pid: ProcessId,
    next_offset: Arc<AtomicUsize>,
    command_queue_capacity: usize,
    logger: impl TokioSchedulerLogger<Action = TAction, Task = TTask> + Send,
    instrumentation: TInstrumentation,
) -> (
    impl Future<Output = ()> + Send,
    impl Future<Output = ()> + Send,
    impl Future<Output = ()> + Send,
    TokioEventChannel<TAction, TTask, TInstrumentation>,
)
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    let (commands_tx, mut commands_rx) =
        mpsc::channel::<VecDeque<(TokioCommand<TAction, TTask>, Instant)>>(command_queue_capacity);
    let event_channel = TokioEventChannel {
        sender: commands_tx,
        instrumentation: instrumentation.clone(),
    };
    let (async_task_pool, async_tasks_tx) = {
        let (requests_tx, requests_rx) =
            mpsc::channel::<TokioThreadPoolRequest<TAction, TTask>>(command_queue_capacity);
        let task_pool = async_tasks.create(ReceiverStream::new(requests_rx));
        (task_pool, requests_tx)
    };
    let (blocking_task_pool, blocking_tasks_tx) = {
        let (requests_tx, requests_rx) =
            mpsc::channel::<TokioThreadPoolRequest<TAction, TTask>>(command_queue_capacity);
        let task_pool = blocking_tasks.create(ReceiverStream::new(requests_rx));
        (task_pool, requests_tx)
    };
    let event_loop_task = {
        let next_pid = Arc::new(AtomicUsize::new(next_pid.into()));
        let init_processes: HashMap<ProcessId, TokioProcess<TAction, TTask>> = init_processes
            .into_iter()
            .flat_map(|(pid, actor)| {
                let (process, extra) = spawn_worker_process(
                    actor,
                    pid,
                    1024,
                    &next_pid,
                    &next_offset,
                    &event_channel,
                    &async_tasks_tx,
                    &blocking_tasks_tx,
                    instrumentation.clone(),
                );
                once((pid, process)).chain(extra)
            })
            .collect();
        let enqueue_time = Instant::now();
        let init_commands = init_commands
            .into_iter()
            .filter_map(|command| match command {
                SchedulerCommand::Send(pid, message) => Some({
                    let offset = MessageOffset::from(increment_atomic_counter(&next_offset));
                    TokioCommand::Send {
                        pid,
                        message: AsyncMessage::Owned {
                            message,
                            offset,
                            redispatched_from: None,
                            caller: None,
                            enqueue_time,
                        },
                    }
                }),
                SchedulerCommand::Task(pid, factory) => Some(TokioCommand::Spawn {
                    pid,
                    factory,
                    caller: None,
                }),
                SchedulerCommand::Kill(pid) => Some(TokioCommand::Kill { pid, caller: None }),
                // TODO: Forbid forward commands in initial scheduler command batch
                SchedulerCommand::Forward(_) => None,
            });
        let async_commands = event_channel.clone();
        let mut logger = logger;
        let mut queue = init_commands
            .map({
                let enqueue_time = Instant::now();
                move |command| (command, enqueue_time)
            })
            .collect::<VecDeque<_>>();
        async move {
            let mut processes = init_processes;
            instrumentation.record_scheduler_queue_capacity(command_queue_capacity);
            instrumentation.record_scheduler_enqueue(queue.len());
            // Main runtime event loop
            loop {
                // Process queued commands individually
                while let Some((command, enqueue_time)) = queue.pop_front() {
                    instrumentation.record_scheduler_dequeue();
                    instrumentation.record_scheduler_state(TokioSchedulerState::Working {
                        queue_size: command_queue_capacity - async_commands.capacity(),
                        capacity: command_queue_capacity,
                    });
                    instrumentation
                        .record_scheduler_command_waiting_duration(enqueue_time.elapsed());
                    logger.log(&command);
                    let command_processing_start_time = Instant::now();
                    match command {
                        TokioCommand::Spawn {
                            pid,
                            factory,
                            caller: _,
                        } => {
                            if let Entry::Vacant(entry) = processes.entry(pid) {
                                let actor = factory.create();
                                let (process, sidecar) = spawn_worker_process(
                                    actor,
                                    pid,
                                    64,
                                    &next_pid,
                                    &next_offset,
                                    &async_commands,
                                    &async_tasks_tx,
                                    &blocking_tasks_tx,
                                    instrumentation.clone(),
                                );
                                entry.insert(process);
                                if let Some((pid, process)) = sidecar {
                                    processes.insert(pid, process);
                                }
                            }
                        }
                        TokioCommand::Send { pid, message } => {
                            let (send_task, subscriptions) = if let Some(process) =
                                processes.get(&pid)
                            {
                                match process {
                                    TokioProcess::Task(instance) => {
                                        let send_task = instance.inbox.send(message);
                                        let inbox_capacity = instance.inbox_capacity;
                                        let inbox_queue_size = instance.inbox.capacity();
                                        let subscriptions = None;
                                        (
                                            Some((send_task, inbox_capacity, inbox_queue_size)),
                                            subscriptions,
                                        )
                                    }
                                    TokioProcess::Actor(instance) => {
                                        let is_accepted_message = instance.actor.accept(&message);
                                        let subscriptions = if instance.subscribers.is_empty() {
                                            None
                                        } else {
                                            let subscriptions = instance
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
                                            let inbox_capacity = instance.inbox_capacity;
                                            let inbox_queue_size = instance.inbox.capacity();
                                            instrumentation.record_worker_action_enqueue(
                                                pid,
                                                &instance.actor,
                                                &message,
                                            );
                                            let send_task = instance.inbox.send(message);
                                            Some((send_task, inbox_capacity, inbox_queue_size))
                                        } else {
                                            None
                                        };
                                        (send_task, subscriptions)
                                    }
                                }
                            } else {
                                (None, None)
                            };
                            if let Some((send_task, target_capacity, target_queue_size)) = send_task
                            {
                                instrumentation.record_scheduler_state(
                                    TokioSchedulerState::Dispatching {
                                        target_queue_size,
                                        target_capacity,
                                    },
                                );
                                let _ = send_task.await;
                            }
                            if let Some(subscriptions) = subscriptions {
                                for task in subscriptions {
                                    let _ = tokio::spawn(
                                        instrumentation.instrument_subscribe_task(task),
                                    );
                                }
                            }
                        }
                        TokioCommand::Kill { pid, caller: _ } => {
                            if let Some(process) = processes.remove(&pid) {
                                let actor_instance = match process {
                                    TokioProcess::Actor(instance) => instance,
                                    TokioProcess::Task(mut task) => {
                                        let actor_pid = task.actor_pid;
                                        tokio::spawn(instrumentation.instrument_dispose_task(
                                            async move { task.abort().await },
                                        ));
                                        let actor_instance = processes
                                            .remove(&actor_pid)
                                            .and_then(|process| match process {
                                                TokioProcess::Actor(instance) => Some(instance),
                                                TokioProcess::Task(_) => None,
                                            })
                                            .expect("Invalid task actor process");
                                        actor_instance
                                    }
                                };
                                let inbox_capacity = actor_instance.inbox_capacity;
                                let inbox_size = inbox_capacity - actor_instance.inbox.capacity();
                                instrumentation.record_worker_kill(
                                    pid,
                                    &actor_instance.actor,
                                    inbox_capacity,
                                    inbox_size,
                                );
                            }
                        }
                        TokioCommand::Subscribe {
                            subscription_id,
                            pid,
                            subscriber,
                        } => {
                            if let Some(entry) = processes
                                .get_mut(&pid)
                                .and_then(|process| match process {
                                    TokioProcess::Actor(instance) => Some(instance),
                                    TokioProcess::Task(_) => None,
                                })
                                .and_then(|instance| {
                                    match instance.subscribers.entry(subscription_id) {
                                        Entry::Occupied(_) => None,
                                        Entry::Vacant(entry) => Some(entry),
                                    }
                                })
                            {
                                entry.insert(subscriber);
                            }
                        }
                        TokioCommand::Unsubscribe {
                            subscription_id,
                            pid,
                        } => {
                            if let Some(TokioProcess::Actor(instance)) = processes.get_mut(&pid) {
                                instance.subscribers.remove(&subscription_id);
                            }
                        }
                    };
                    instrumentation.record_scheduler_command_working_duration(
                        command_processing_start_time.elapsed(),
                    );
                }
                // Wait for the next batch of commands
                match commands_rx.recv().await {
                    None => break,
                    Some(commands) => {
                        // Collect any remaining buffered commands into the current batch
                        let mut combined_commands = commands;
                        while let Ok(commands) = commands_rx.try_recv() {
                            combined_commands.extend(commands);
                        }
                        // Assign this batch of commands to the queue
                        queue = combined_commands
                    }
                }
            }
        }
    };
    (
        event_loop_task,
        async_task_pool,
        blocking_task_pool,
        event_channel,
    )
}

fn spawn_worker_process<TAction, TTask, TInstrumentation>(
    actor: <TTask as TaskFactory<TAction, TTask>>::Actor,
    pid: ProcessId,
    inbox_capacity: usize,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &Arc<AtomicUsize>,
    results: &TokioEventChannel<TAction, TTask, TInstrumentation>,
    async_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    blocking_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    instrumentation: TInstrumentation,
) -> (
    TokioProcess<TAction, TTask>,
    Option<(ProcessId, TokioProcess<TAction, TTask>)>,
)
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    let state = actor.init();
    let (inbox_tx, inbox) = create_worker_inbox(inbox_capacity);
    instrumentation.record_worker_spawn(pid, &actor, inbox_capacity);
    match actor.events(inbox) {
        ActorEvents::Sync(inbox) => {
            let actor = Arc::new(actor);
            let instance = spawn_worker_actor_process(
                actor,
                state,
                inbox,
                inbox_tx,
                inbox_capacity,
                pid,
                next_pid,
                next_offset,
                results,
                async_tasks,
                blocking_tasks,
                instrumentation,
            );
            (TokioProcess::Actor(instance), None)
        }
        ActorEvents::Async(events, dispose) => {
            let actor = Arc::new(actor);
            let task_pid = pid;
            let actor_pid = ProcessId::from(increment_atomic_counter(next_pid));
            let (events, instance) = spawn_async_worker(
                actor,
                state,
                events,
                dispose,
                inbox_tx,
                task_pid,
                actor_pid,
                next_pid,
                next_offset,
                results,
                async_tasks,
                blocking_tasks,
                instrumentation,
            );
            (
                TokioProcess::Task(events),
                Some((actor_pid, TokioProcess::Actor(instance))),
            )
        }
    }
}

fn spawn_async_worker<TAction, TTask, TInstrumentation>(
    actor: Arc<<TTask as TaskFactory<TAction, TTask>>::Actor>,
    state: <<TTask as TaskFactory<TAction, TTask>>::Actor as Handler<
        TAction,
        SchedulerTransition<TAction, TTask>,
    >>::State,
    events: impl Stream<Item = AsyncMessage<TAction>> + Unpin + Send + 'static,
    dispose: Option<
        <<TTask as TaskFactory<TAction, TTask>>::Actor as Actor<TAction, TTask>>::Dispose,
    >,
    inbox_tx: mpsc::Sender<AsyncMessage<TAction>>,
    task_pid: ProcessId,
    actor_pid: ProcessId,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &Arc<AtomicUsize>,
    results: &TokioEventChannel<TAction, TTask, TInstrumentation>,
    async_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    blocking_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    instrumentation: TInstrumentation,
) -> (
    TokioTask<TAction, TTask>,
    TokioActorInstance<TAction, TTask>,
)
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    let inbox_capacity = 64;
    let (actor_inbox_tx, actor_inbox) = create_worker_inbox(inbox_capacity);
    let inbox_task =
        create_async_events_task(events, actor_pid, next_offset.clone(), results.clone());
    let actor_process = spawn_worker_actor_process(
        actor,
        state,
        actor_inbox,
        actor_inbox_tx,
        inbox_capacity,
        task_pid,
        next_pid,
        next_offset,
        results,
        async_tasks,
        blocking_tasks,
        instrumentation.clone(),
    );
    let task_process = TokioTask {
        inbox: inbox_tx,
        inbox_capacity,
        actor_pid,
        // TODO: Panic main scheduler thread when worker task panics
        handle: tokio::spawn(instrumentation.instrument_task_thread(inbox_task)),
        dispose,
    };
    (task_process, actor_process)
}

fn create_async_events_task<TAction, TTask, TInstrumentation>(
    mut events: impl Stream<Item = AsyncMessage<TAction>> + Unpin + Send + 'static,
    actor_pid: ProcessId,
    next_offset: Arc<AtomicUsize>,
    results: TokioEventChannel<TAction, TTask, TInstrumentation>,
) -> impl Future<Output = ()> + Send + 'static
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation:
        TokioSchedulerInstrumentation<Action = TAction, Task = TTask> + Send + Sync + 'static,
{
    async move {
        while let Some(message) = events.next().await {
            // TODO: instrument async event task messages
            let offset = MessageOffset::from(increment_atomic_counter(&next_offset));
            let (message, enqueue_time) = match message {
                AsyncMessage::Pending { message } => {
                    let enqueue_time = Instant::now();
                    (
                        AsyncMessage::Owned {
                            message,
                            offset,
                            redispatched_from: None,
                            caller: None,
                            enqueue_time,
                        },
                        enqueue_time,
                    )
                }
                AsyncMessage::Owned {
                    message,
                    offset: inbox_offset,
                    redispatched_from: _,
                    caller,
                    enqueue_time,
                } => (
                    AsyncMessage::Owned {
                        message,
                        offset,
                        redispatched_from: Some(inbox_offset),
                        caller,
                        enqueue_time,
                    },
                    enqueue_time,
                ),
                AsyncMessage::Shared {
                    message,
                    offset: inbox_offset,
                    redispatched_from: _,
                    caller,
                    enqueue_time,
                } => (
                    AsyncMessage::Shared {
                        message,
                        offset,
                        redispatched_from: Some(inbox_offset),
                        caller,
                        enqueue_time,
                    },
                    enqueue_time,
                ),
            };
            let _ = results
                .send(
                    [(
                        TokioCommand::Send {
                            pid: actor_pid,
                            message,
                        },
                        enqueue_time,
                    )]
                    .into_iter()
                    .collect(),
                )
                .await;
        }
    }
}

fn create_worker_inbox<TAction: Action>(
    buffer_size: usize,
) -> (mpsc::Sender<AsyncMessage<TAction>>, TokioInbox<TAction>) {
    let (inbox_tx, inbox_rx) = mpsc::channel(buffer_size);
    (inbox_tx, TokioInbox(ReceiverStream::new(inbox_rx)))
}

fn spawn_worker_actor_process<TAction, TTask, TInstrumentation>(
    actor: Arc<TTask::Actor>,
    state: <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    inbox: TokioInbox<TAction>,
    inbox_tx: mpsc::Sender<AsyncMessage<TAction>>,
    inbox_capacity: usize,
    pid: ProcessId,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &Arc<AtomicUsize>,
    results: &TokioEventChannel<TAction, TTask, TInstrumentation>,
    async_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    blocking_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    instrumentation: TInstrumentation,
) -> TokioActorInstance<TAction, TTask>
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    let task = create_worker_actor_task(
        actor.clone(),
        state,
        inbox,
        pid,
        next_pid,
        next_offset,
        results,
        async_tasks,
        blocking_tasks,
        instrumentation.clone(),
    );
    TokioActorInstance {
        inbox: inbox_tx,
        inbox_capacity,
        actor,
        // TODO: Panic main scheduler thread when worker task panics
        handle: tokio::spawn(instrumentation.instrument_worker_thread(task)),
        subscribers: Default::default(),
    }
}

fn create_worker_actor_task<TAction, TTask, TInstrumentation>(
    actor: Arc<TTask::Actor>,
    state: <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    inbox: TokioInbox<TAction>,
    pid: ProcessId,
    next_pid: &Arc<AtomicUsize>,
    next_offset: &Arc<AtomicUsize>,
    results: &TokioEventChannel<TAction, TTask, TInstrumentation>,
    async_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    blocking_tasks: &mpsc::Sender<TokioThreadPoolRequest<TAction, TTask>>,
    instrumentation: TInstrumentation,
) -> impl Future<Output = ()> + Send + 'static
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
{
    let mut actor_state = Some(state);
    let mut inbox = inbox;
    let context = get_handler_context(pid, next_pid);
    let next_offset = next_offset.clone();
    let results = results.clone();
    let async_tasks = async_tasks.clone();
    let blocking_tasks = blocking_tasks.clone();
    async move {
        while let Some(message) = {
            instrumentation.record_worker_state(pid, &actor, TokioWorkerState::Waiting);
            inbox.next()
        }
        .await
        {
            instrumentation.record_worker_action_dequeue(pid, &actor, &message);
            instrumentation.record_worker_state(pid, &actor, TokioWorkerState::Working);
            if let Some(enqueue_time) = message.enqueue_time() {
                instrumentation.record_worker_action_waiting_duration(
                    pid,
                    &actor,
                    &message,
                    enqueue_time.elapsed(),
                );
            }
            if let Some(offset) = message.offset() {
                let parent_offset = message.parent_offset();
                if let Some(mut state) = actor_state.take() {
                    let scheduler_mode = actor.schedule(&message, &state);
                    let handler_start_time = Instant::now();
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
                            let (response_tx, response_rx) = oneshot::channel();
                            let request = TokioThreadPoolRequest {
                                actor: actor.clone(),
                                state,
                                message,
                                metadata: get_message_metadata(offset, parent_offset),
                                context: context.clone(),
                                response: response_tx,
                            };
                            instrumentation.record_worker_state(pid, &actor, {
                                let target_capacity = async_tasks.max_capacity();
                                let target_queue_size = target_capacity - async_tasks.capacity();
                                TokioWorkerState::Sending {
                                    target_queue_size,
                                    target_capacity,
                                }
                            });
                            match async_tasks.send(request).await {
                                Err(_err) => break,
                                Ok(_) => {
                                    instrumentation.record_worker_state(
                                        pid,
                                        &actor,
                                        TokioWorkerState::Awaiting,
                                    );
                                    match response_rx.await {
                                        Err(_err) => break,
                                        Ok((message, state, actions)) => (message, state, actions),
                                    }
                                }
                            }
                        }
                        Some(SchedulerMode::Blocking) => {
                            let (response_tx, response_rx) = oneshot::channel();
                            let request = TokioThreadPoolRequest {
                                actor: actor.clone(),
                                state,
                                message,
                                metadata: get_message_metadata(offset, parent_offset),
                                context: context.clone(),
                                response: response_tx,
                            };
                            instrumentation.record_worker_state(pid, &actor, {
                                let target_capacity = blocking_tasks.max_capacity();
                                let target_queue_size = target_capacity - blocking_tasks.capacity();
                                TokioWorkerState::Sending {
                                    target_queue_size,
                                    target_capacity,
                                }
                            });
                            match blocking_tasks.send(request).await {
                                Err(_err) => break,
                                Ok(_) => {
                                    instrumentation.record_worker_state(
                                        pid,
                                        &actor,
                                        TokioWorkerState::Awaiting,
                                    );
                                    match response_rx.await {
                                        Err(_err) => break,
                                        Ok((message, state, actions)) => (message, state, actions),
                                    }
                                }
                            }
                        }
                    };
                    actor_state.replace(state);
                    instrumentation.record_worker_action_working_duration(
                        pid,
                        &actor,
                        &message,
                        handler_start_time.elapsed(),
                    );
                    if let Some(commands) = actions {
                        let enqueue_time = Instant::now();
                        let mut queue = VecDeque::with_capacity(commands.len());
                        collect_handler_results(
                            &mut queue,
                            commands,
                            message,
                            offset,
                            pid,
                            enqueue_time,
                            &next_offset,
                        );
                        instrumentation.record_worker_state(pid, &actor, {
                            TokioWorkerState::Dispatching {
                                num_commands: queue.len(),
                                target_queue_size: results.queue_size(),
                                target_capacity: results.capacity(),
                            }
                        });
                        let _ = results.send(queue).await;
                    }
                }
            }
        }
    }
}

fn collect_handler_results<TAction: Action, TTask: TaskFactory<TAction, TTask>>(
    queue: &mut VecDeque<(TokioCommand<TAction, TTask>, Instant)>,
    commands: SchedulerTransition<TAction, TTask>,
    message: AsyncMessage<TAction>,
    offset: MessageOffset,
    pid: ProcessId,
    enqueue_time: Instant,
    next_offset: &Arc<AtomicUsize>,
) {
    let redispatched_from = message.redispatched_from();
    let caller = Some((offset, pid));
    // Redispatches are handled specially in order to minimise unnecessary sharing/allocations for situations where the
    // current action is redispatched to a single target
    enum RedispatchedMessageState<T> {
        Owned {
            message: T,
            target_pid: Option<ProcessId>,
        },
        Shared {
            message: Arc<T>,
            num_redispatched_messages: usize,
        },
    }
    // Extend the queue with the handler commands
    let redispatch_state = commands.into_iter().fold(
        match message {
            AsyncMessage::Pending { message } => RedispatchedMessageState::Owned {
                message,
                target_pid: None,
            },
            AsyncMessage::Owned { message, .. } => RedispatchedMessageState::Owned {
                message,
                target_pid: None,
            },
            AsyncMessage::Shared { message, .. } => RedispatchedMessageState::Shared {
                message,
                num_redispatched_messages: 0,
            },
        },
        |redispatch_state, command| match command {
            SchedulerCommand::Send(target_pid, message) => {
                queue.push_back({
                    let offset = MessageOffset::from(increment_atomic_counter(next_offset));
                    (
                        TokioCommand::Send {
                            pid: target_pid,
                            message: AsyncMessage::Owned {
                                message,
                                offset,
                                redispatched_from: None,
                                caller,
                                enqueue_time,
                            },
                        },
                        enqueue_time,
                    )
                });
                redispatch_state
            }
            SchedulerCommand::Task(worker_pid, factory) => {
                queue.push_back((
                    TokioCommand::Spawn {
                        pid: worker_pid,
                        factory,
                        caller,
                    },
                    enqueue_time,
                ));
                redispatch_state
            }
            SchedulerCommand::Kill(worker_pid) => {
                queue.push_back((
                    TokioCommand::Kill {
                        pid: worker_pid,
                        caller,
                    },
                    enqueue_time,
                ));
                redispatch_state
            }
            SchedulerCommand::Forward(target_pid) => {
                let redispatched_from = Some(redispatched_from.unwrap_or(offset));
                match redispatch_state {
                    RedispatchedMessageState::Owned {
                        message,
                        target_pid: None,
                    } => {
                        // This message might still be forwarded to multiple targets, so don't enqueue it yet
                        // (seeing as it might need to be wrapped in a shared pointer before adding to the queue)
                        let redispatch_state = RedispatchedMessageState::Owned {
                            message,
                            target_pid: Some(target_pid),
                        };
                        redispatch_state
                    }
                    RedispatchedMessageState::Owned {
                        message,
                        target_pid: Some(existing_target_pid),
                    } => {
                        // This is the second forwarding target for the message, so we need to wrap it in a shared pointer
                        // before enqueueing both commands at the FRONT of the queue, for immediate redispatch.
                        // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                        // once all commands have been processed).
                        let shared_message = Arc::new(message);
                        queue.push_front({
                            let offset = MessageOffset::from(increment_atomic_counter(next_offset));
                            (
                                TokioCommand::Send {
                                    pid: existing_target_pid,
                                    message: AsyncMessage::Shared {
                                        message: Arc::clone(&shared_message),
                                        offset,
                                        redispatched_from,
                                        caller,
                                        enqueue_time,
                                    },
                                },
                                enqueue_time,
                            )
                        });
                        queue.push_front({
                            let offset = MessageOffset::from(increment_atomic_counter(next_offset));
                            (
                                TokioCommand::Send {
                                    pid: target_pid,
                                    message: AsyncMessage::Shared {
                                        message: Arc::clone(&shared_message),
                                        offset,
                                        redispatched_from,
                                        caller,
                                        enqueue_time,
                                    },
                                },
                                enqueue_time,
                            )
                        });
                        let num_redispatched_messages = 2;
                        let redispatch_state = RedispatchedMessageState::Shared {
                            message: shared_message,
                            num_redispatched_messages,
                        };
                        redispatch_state
                    }
                    RedispatchedMessageState::Shared {
                        message: shared_message,
                        num_redispatched_messages,
                    } => {
                        // This is an additional forwarding target for an already-shared message, so we can just clone the
                        // existing shared pointer and enqueue the command at the FRONT of the queue, for immediate redispatch.
                        // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                        // once all commands have been processed).
                        queue.push_front({
                            let offset = MessageOffset::from(increment_atomic_counter(next_offset));
                            (
                                TokioCommand::Send {
                                    pid: target_pid,
                                    message: AsyncMessage::Shared {
                                        message: Arc::clone(&shared_message),
                                        offset,
                                        redispatched_from,
                                        caller,
                                        enqueue_time,
                                    },
                                },
                                enqueue_time,
                            )
                        });
                        let redispatch_state = RedispatchedMessageState::Shared {
                            message: shared_message,
                            num_redispatched_messages: num_redispatched_messages + 1,
                        };
                        redispatch_state
                    }
                }
            }
        },
    );
    // Prepend any potential residual redispatched action that has not yet been added to the queue
    match redispatch_state {
        RedispatchedMessageState::Owned {
            message: _,
            target_pid: None,
        } => {
            // If no redispatch commands were encountered, no more manipulation of the queue needed
        }
        RedispatchedMessageState::Owned {
            message,
            target_pid: Some(target_pid),
        } => {
            // If after all commands have been processed we're left with a single forwarding target for the message,
            // enqueue the forwarded command at the FRONT of the queue, for immediate redispatch
            let redispatched_from = Some(redispatched_from.unwrap_or(offset));
            queue.push_front({
                let offset = MessageOffset::from(increment_atomic_counter(next_offset));
                (
                    TokioCommand::Send {
                        pid: target_pid,
                        message: AsyncMessage::Owned {
                            message,
                            offset,
                            redispatched_from,
                            caller,
                            enqueue_time,
                        },
                    },
                    enqueue_time,
                )
            });
        }
        RedispatchedMessageState::Shared {
            message: _,
            num_redispatched_messages,
        } => {
            // If multiple forwarded messages were encountered, the iteration will have added them to the front of the
            // queue in the opposite order from the order in which they were produced, so we need to reverse them
            reverse_first_n_queue_entries(queue, num_redispatched_messages);
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokioHandlerContext {
    pid: ProcessId,
    next_pid: Arc<AtomicUsize>,
}
impl HandlerContext for TokioHandlerContext {
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

fn get_handler_context(pid: ProcessId, next_pid: &Arc<AtomicUsize>) -> TokioHandlerContext {
    TokioHandlerContext {
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

pub trait TokioSchedulerLogger {
    type Action: Action;
    type Task: TaskFactory<Self::Action, Self::Task>;
    fn log(&mut self, command: &TokioCommand<Self::Action, Self::Task>);
}
impl<T> TokioSchedulerLogger for Option<T>
where
    T: TokioSchedulerLogger,
{
    type Action = T::Action;
    type Task = T::Task;
    fn log(&mut self, command: &TokioCommand<T::Action, T::Task>) {
        match self {
            Some(inner) => inner.log(command),
            None => {}
        }
    }
}

#[derive(Debug)]
pub struct NoopTokioSchedulerInstrumentation<TAction, TTask> {
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TAction, TTask> Clone for NoopTokioSchedulerInstrumentation<TAction, TTask> {
    fn clone(&self) -> Self {
        Self {
            _action: self._action.clone(),
            _task: self._task.clone(),
        }
    }
}
impl<TAction, TTask> Copy for NoopTokioSchedulerInstrumentation<TAction, TTask> {}
impl<TAction, TTask> Default for NoopTokioSchedulerInstrumentation<TAction, TTask> {
    fn default() -> Self {
        Self {
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> TokioSchedulerInstrumentation
    for NoopTokioSchedulerInstrumentation<TAction, TTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    type InstrumentedTask<T: Future + Send + 'static> = T;
    fn instrument_main_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_async_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_blocking_task_pool<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_worker_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_task_thread<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_dispose_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_subscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn instrument_unsubscribe_task<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T> {
        task
    }
    fn record_scheduler_queue_capacity(&self, _value: usize) {}
    fn record_scheduler_enqueue(&self, _num_commands: usize) {}
    fn record_scheduler_dequeue(&self) {}
    fn record_scheduler_state(&self, _state: TokioSchedulerState) {}
    fn record_scheduler_command_waiting_duration(&self, _value: Duration) {}
    fn record_scheduler_command_working_duration(&self, _value: Duration) {}
    fn record_worker_spawn(&self, _pid: ProcessId, _actor: &TTask::Actor, _inbox_capacity: usize) {}
    fn record_worker_kill(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _inbox_capacity: usize,
        _inbox_size: usize,
    ) {
    }
    fn record_worker_action_enqueue(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _action: &TAction,
    ) {
    }
    fn record_worker_action_dequeue(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _action: &TAction,
    ) {
    }
    fn record_worker_state(
        &self,
        _pid: ProcessId,
        _actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        _state: TokioWorkerState,
    ) {
    }
    fn record_worker_action_waiting_duration(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _action: &TAction,
        _value: Duration,
    ) {
    }
    fn record_worker_action_working_duration(
        &self,
        _pid: ProcessId,
        _actor: &TTask::Actor,
        _action: &TAction,
        _value: Duration,
    ) {
    }
}
