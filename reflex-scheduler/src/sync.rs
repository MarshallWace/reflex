// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    marker::PhantomData,
    ops::Deref,
    rc::Rc,
};

use futures::{
    stream::{self, Empty},
    Stream,
};

use pin_project::pin_project;
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, HandlerTransition, MessageData,
    MessageOffset, ProcessId, SchedulerCommand, SchedulerTransition, TaskFactory, TaskInbox,
    TaskMessage,
};

pub struct NoopTaskRunner;
impl<TMessage, TAction, TTask> TaskRunner<TMessage, TAction, TTask> for NoopTaskRunner
where
    TMessage: TaskMessage<TAction> + Send + 'static,
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Inbox = NoopTaskEvents<TMessage, TAction>;
    fn inbox(&mut self, _pid: ProcessId) -> Self::Inbox {
        NoopTaskEvents {
            inner: stream::empty(),
            _action: PhantomData,
        }
    }
    fn run(
        &mut self,
        _events: <TTask::Actor as Actor<TAction, TTask>>::Events<Self::Inbox>,
        _dispose: <TTask::Actor as Actor<TAction, TTask>>::Dispose,
    ) -> TaskHandle {
        TaskHandle::new(|| {})
    }
}

#[pin_project]
pub struct NoopTaskEvents<TMessage, TAction>
where
    TMessage: TaskMessage<TAction>,
    TAction: Action,
{
    #[pin]
    inner: Empty<TMessage>,
    _action: PhantomData<TAction>,
}
impl<TMessage, TAction> TaskInbox<TAction> for NoopTaskEvents<TMessage, TAction>
where
    TMessage: TaskMessage<TAction> + Send + 'static,
    TAction: Action + Send + 'static,
{
    type Message = TMessage;
}
impl<TMessage, TAction> Stream for NoopTaskEvents<TMessage, TAction>
where
    TMessage: TaskMessage<TAction>,
    TAction: Action,
{
    type Item = TMessage;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        self.project().inner.poll_next(cx)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

pub trait TaskRunner<TMessage, TAction, TTask>
where
    TMessage: TaskMessage<TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Inbox: TaskInbox<TAction>;
    fn inbox(&mut self, pid: ProcessId) -> Self::Inbox;
    fn run(
        &mut self,
        events: <TTask::Actor as Actor<TAction, TTask>>::Events<Self::Inbox>,
        dispose: <TTask::Actor as Actor<TAction, TTask>>::Dispose,
    ) -> TaskHandle;
}

pub struct TaskHandle {
    abort: Box<dyn FnOnce() + 'static>,
}
impl TaskHandle {
    pub fn new(abort: impl FnOnce() + 'static) -> Self {
        Self {
            abort: Box::new(abort),
        }
    }
    pub fn abort(self) {
        (self.abort)()
    }
}

struct SyncWorkerInstance<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    handle: TaskHandle,
    actor: TTask::Actor,
    state: <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
}

pub struct SyncScheduler<TAction, TTask, TRunner>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
    TRunner: TaskRunner<SyncMessage<TAction>, TAction, TTask>,
{
    processes: HashMap<ProcessId, SyncWorkerInstance<TAction, TTask>>,
    task_runner: TRunner,
    next_pid: ProcessId,
    next_offset: MessageOffset,
}

impl<TAction, TTask, TRunner> SyncScheduler<TAction, TTask, TRunner>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
    TRunner: TaskRunner<SyncMessage<TAction>, TAction, TTask>,
{
    pub fn new(runner: TRunner) -> Self {
        Self {
            processes: Default::default(),
            task_runner: runner,
            next_pid: ProcessId::default(),
            next_offset: Default::default(),
        }
    }
    pub fn generate_pid(&mut self) -> ProcessId {
        let next_pid = self.next_pid.next();
        std::mem::replace(&mut self.next_pid, next_pid)
    }
    pub fn spawn(&mut self, pid: ProcessId, factory: TTask) {
        if let Entry::Vacant(entry) = self.processes.entry(pid) {
            // Create an inbox for the worker
            let inbox = self.task_runner.inbox(pid);
            // Create the worker instance
            let actor = factory.create();
            let (state, events, dispose) = actor.init(inbox, &SyncActorInitContext { pid });
            // Initiate the worker event stream
            let handle = self.task_runner.run(events, dispose);
            // Keep hold of the worker instance
            entry.insert(SyncWorkerInstance {
                state,
                actor,
                handle,
            });
        }
    }
    pub fn kill(&mut self, pid: ProcessId) {
        if let Some(worker) = self.processes.remove(&pid) {
            worker.handle.abort();
        }
    }
    pub fn dispatch(&mut self, pid: ProcessId, action: TAction) {
        let mut queue = VecDeque::default();
        queue.push_back(SyncSchedulerQueueEntry {
            command: SyncSchedulerMessage::Send(pid, SyncMessage::from(action)),
            caller: None,
        });
        while let Some(SyncSchedulerQueueEntry { command, caller }) = queue.pop_front() {
            match command {
                SyncSchedulerMessage::Send(pid, message) => {
                    let metadata = MessageData {
                        offset: {
                            let next_offset = self.next_offset.next();
                            std::mem::replace(&mut self.next_offset, next_offset)
                        },
                        parent: caller.map(|(parent_offset, _)| parent_offset),
                        timestamp: std::time::Instant::now(),
                    };
                    let mut context = SyncContext {
                        pid,
                        next_pid: self.next_pid,
                    };
                    // Handle incoming action command
                    let child_commands = self
                        .processes
                        .get_mut(&pid)
                        .and_then(|worker| {
                            worker.actor.handle(
                                &mut worker.state,
                                &message,
                                &metadata,
                                &mut context,
                            )
                        })
                        .unwrap_or_default();
                    // Update the shared task ID counter
                    self.next_pid = context.next_pid;
                    // Enqueue outgoing commands
                    enqueue_handler_results(
                        &mut queue,
                        child_commands,
                        message,
                        metadata.offset,
                        pid,
                    );
                }
                SyncSchedulerMessage::Spawn(worker_pid, factory) => self.spawn(worker_pid, factory),
                SyncSchedulerMessage::Kill(worker_pid) => self.kill(worker_pid),
            };
        }
    }
}

struct SyncSchedulerQueueEntry<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    command: SyncSchedulerMessage<TAction, TTask>,
    caller: Option<(MessageOffset, ProcessId)>,
}

enum SyncSchedulerMessage<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    Send(ProcessId, SyncMessage<TAction>),
    Spawn(ProcessId, TTask),
    Kill(ProcessId),
}

pub enum SyncMessage<TAction> {
    Owned(TAction),
    Shared(Rc<TAction>),
}
impl<TAction> TaskMessage<TAction> for SyncMessage<TAction> where TAction: Action {}
impl<TAction> From<TAction> for SyncMessage<TAction> {
    fn from(value: TAction) -> Self {
        Self::Owned(value)
    }
}
impl<TAction> Deref for SyncMessage<TAction> {
    type Target = TAction;
    fn deref(&self) -> &Self::Target {
        match self {
            SyncMessage::Owned(target) => target,
            SyncMessage::Shared(target) => target.as_ref(),
        }
    }
}

fn enqueue_handler_results<TAction: Action, TTask: TaskFactory<TAction, TTask>>(
    queue: &mut VecDeque<SyncSchedulerQueueEntry<TAction, TTask>>,
    commands: HandlerTransition<SchedulerCommand<TAction, TTask>>,
    message: SyncMessage<TAction>,
    offset: MessageOffset,
    pid: ProcessId,
) {
    // Enqueues child commands with an emphasis on minimizing unnecessary allocations.
    // There are broadly two types of child commands: redispatch commands, which forward the existing message to another
    // target and need to be processed immediately (effectively 'depth first'), versus all the other scheduler commands,
    // which are added to the back of the queue (effectively being processed in 'breadth first' order).
    // The implementation allows an owned message to be forwarded to a single redispatch target without needing to be
    // wrapped in an unnecessary reference-counting pointer.
    let caller = Some((offset, pid));
    enum RedispatchedMessage<T> {
        Owned(T, Option<ProcessId>),
        Shared(Rc<T>, usize),
    }
    let redispatch_state = commands.into_iter().fold(
        match message {
            SyncMessage::Owned(message) => RedispatchedMessage::Owned(message, None),
            SyncMessage::Shared(message) => RedispatchedMessage::Shared(message, 0),
        },
        |redispatch_state, command| match command {
            SchedulerCommand::Send(target_pid, message) => {
                // Add the command to the back of the queue
                queue.push_back(SyncSchedulerQueueEntry {
                    command: SyncSchedulerMessage::Send(target_pid, SyncMessage::Owned(message)),
                    caller,
                });
                redispatch_state
            }
            SchedulerCommand::Task(worker_pid, factory) => {
                // Add the command to the back of the queue
                queue.push_back(SyncSchedulerQueueEntry {
                    command: SyncSchedulerMessage::Spawn(worker_pid, factory),
                    caller,
                });
                redispatch_state
            }
            SchedulerCommand::Kill(worker_pid) => {
                // Add the command to the back of the queue
                queue.push_back(SyncSchedulerQueueEntry {
                    command: SyncSchedulerMessage::Kill(worker_pid),
                    caller,
                });
                redispatch_state
            }
            SchedulerCommand::Forward(target_pid) => match redispatch_state {
                RedispatchedMessage::Owned(message, None) => {
                    // This message might also be forwarded to another target, so don't enqueue it yet
                    // (seeing as it might need to be wrapped in a shared pointer before adding to the queue)
                    RedispatchedMessage::Owned(message, Some(target_pid))
                }
                RedispatchedMessage::Owned(message, Some(existing_target_pid)) => {
                    // This is the second forwarding target for the message, so we need to wrap it in a shared pointer
                    // before enqueueing both commands at the FRONT of the queue, for immediate redispatch.
                    // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                    // once all commands have been processed).
                    let shared_message = Rc::new(message);
                    queue.push_front(SyncSchedulerQueueEntry {
                        command: SyncSchedulerMessage::Send(
                            existing_target_pid,
                            SyncMessage::Shared(Rc::clone(&shared_message)),
                        ),
                        caller,
                    });
                    queue.push_front(SyncSchedulerQueueEntry {
                        command: SyncSchedulerMessage::Send(
                            target_pid,
                            SyncMessage::Shared(Rc::clone(&shared_message)),
                        ),
                        caller,
                    });
                    let num_forwarded_messages = 2;
                    RedispatchedMessage::Shared(shared_message, num_forwarded_messages)
                }
                RedispatchedMessage::Shared(shared_message, num_forwarded_messages) => {
                    // This is an additional forwarding target for an already-shared message, so we can just clone the
                    // existing shared pointer and enqueue the command at the FRONT of the queue, for immediate redispatch.
                    // Forwarded actions are enqueued in reverse order (these will be reversed back to the initial order
                    // once all commands have been processed).
                    queue.push_front(SyncSchedulerQueueEntry {
                        command: SyncSchedulerMessage::Send(
                            target_pid,
                            SyncMessage::Shared(Rc::clone(&shared_message)),
                        ),
                        caller,
                    });
                    RedispatchedMessage::Shared(shared_message, num_forwarded_messages + 1)
                }
            },
        },
    );
    match redispatch_state {
        RedispatchedMessage::Owned(message, target_pid) => {
            // If after all commands have been processed we're left with a single forwarding target for the message,
            // enqueue the forwarded command at the FRONT of the queue, for immediate redispatch
            if let Some(target_pid) = target_pid {
                queue.push_front(SyncSchedulerQueueEntry {
                    command: SyncSchedulerMessage::Send(target_pid, SyncMessage::Owned(message)),
                    caller,
                });
            }
        }
        RedispatchedMessage::Shared(_shared_message, num_forwarded_messages) => {
            // If multiple forwarded messages were encountered, the iteration will have added them to the front of the
            // queue in the opposite order from the order in which they were produced, so we need to reverse them
            reverse_first_n_queue_entries(queue, num_forwarded_messages)
        }
    }
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

#[derive(Debug, Clone, Copy)]
pub struct SyncContext {
    pid: ProcessId,
    next_pid: ProcessId,
}
impl HandlerContext for SyncContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
    fn generate_pid(&mut self) -> ProcessId {
        let next_pid = self.next_pid.next();
        std::mem::replace(&mut self.next_pid, next_pid)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SyncActorInitContext {
    pid: ProcessId,
}
impl ActorInitContext for SyncActorInitContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
}
