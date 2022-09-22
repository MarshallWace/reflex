// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap, VecDeque};

use crate::{
    Action, Actor, BoxedWorkerInstance, HandlerContext, MessageData, MessageOffset,
    MiddlewareContext, OperationStream, PostMiddleware, PreMiddleware, ProcessId, Scheduler,
    SchedulerMiddleware, StateOperation, StateTransition, WorkerContext, WorkerFactory,
};

pub struct NoopTaskRunner;
impl<T: Action> TaskRunner<T> for NoopTaskRunner {
    fn run(&mut self, _task: OperationStream<T>) -> TaskHandle {
        TaskHandle::new(|| {})
    }
}

pub trait TaskRunner<T: Action> {
    fn run(&mut self, task: OperationStream<T>) -> TaskHandle;
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

enum SyncProcess<TAction>
where
    TAction: Action + Send + 'static,
{
    Task(TaskHandle),
    Worker(BoxedWorkerInstance<TAction>),
}

pub struct SyncScheduler<TActor, TPreMiddleware, TPostMiddleware, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TPreMiddleware: PreMiddleware<TAction>,
    TPostMiddleware: PostMiddleware<TAction>,
    TAction: Action + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    actor: TActor,
    actor_state: Option<TActor::State>,
    middleware: SchedulerMiddleware<TPreMiddleware, TPostMiddleware, TAction>,
    pre_middleware_state: Option<TPreMiddleware::State>,
    post_middleware_state: Option<TPostMiddleware::State>,
    root_pid: ProcessId,
    next_pid: ProcessId,
    next_offset: MessageOffset,
    processes: HashMap<ProcessId, SyncProcess<TAction>>,
    task_runner: TRunner,
}
impl<TActor, TPreMiddleware, TPostMiddleware, TAction, TRunner> Scheduler
    for SyncScheduler<TActor, TPreMiddleware, TPostMiddleware, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TPreMiddleware: PreMiddleware<TAction>,
    TPostMiddleware: PostMiddleware<TAction>,
    TAction: Action + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    type Action = TAction;
}
impl<TActor, TPreMiddleware, TPostMiddleware, TAction, TRunner>
    SyncScheduler<TActor, TPreMiddleware, TPostMiddleware, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TPreMiddleware: PreMiddleware<TAction>,
    TPostMiddleware: PostMiddleware<TAction>,
    TAction: Action + Clone + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    pub fn new(
        actor: TActor,
        middleware: SchedulerMiddleware<TPreMiddleware, TPostMiddleware, TAction>,
        runner: TRunner,
    ) -> Self {
        let root_pid = ProcessId::default();
        let next_pid = root_pid.next();
        let actor_state = actor.init();
        let pre_middleware_state = middleware.pre.init();
        let post_middleware_state = middleware.post.init();
        Self {
            actor_state: Some(actor_state),
            pre_middleware_state: Some(pre_middleware_state),
            post_middleware_state: Some(post_middleware_state),
            actor,
            middleware,
            root_pid,
            next_pid,
            task_runner: runner,
            processes: Default::default(),
            next_offset: Default::default(),
        }
    }
    pub fn dispatch(&mut self, action: TAction) {
        let mut queue = VecDeque::default();
        queue.push_back((StateOperation::Send(self.root_pid, action), None));
        while let Some((operation, caller)) = queue.pop_front() {
            let metadata = MessageData {
                offset: {
                    let next_offset = self.next_offset.next();
                    std::mem::replace(&mut self.next_offset, next_offset)
                },
                parent: caller.map(|(parent, _)| parent),
                timestamp: std::time::Instant::now(),
            };
            let caller_pid = caller.map(|(_, caller_pid)| caller_pid);
            let operation = {
                let (updated_state, operation) = self
                    .middleware
                    .pre
                    .handle(
                        self.pre_middleware_state.take().unwrap(),
                        operation,
                        &metadata,
                        &MiddlewareContext { caller_pid },
                    )
                    .into_parts();
                self.pre_middleware_state.replace(updated_state);
                operation
            };
            match operation {
                StateOperation::Send(pid, action) => {
                    let mut context = SyncContext {
                        pid,
                        caller_pid,
                        next_pid: self.next_pid,
                    };
                    // Handle incoming action command
                    let child_commands = if pid == self.root_pid {
                        let (updated_state, child_commands) = self
                            .actor
                            .handle(
                                self.actor_state.take().unwrap(),
                                &action,
                                &metadata,
                                &mut context,
                            )
                            .into_parts();
                        self.actor_state.replace(updated_state);
                        child_commands
                    } else if let Some(SyncProcess::Worker(worker)) = self.processes.get_mut(&pid) {
                        worker
                            .handle(action, &metadata, &mut WorkerContext::new(context))
                            .into_inner()
                    } else {
                        StateTransition::default()
                    };
                    // Update the shared task ID counter
                    self.next_pid = context.next_pid;
                    // Apply post-middleware to transform outgoing commands
                    let child_commands = {
                        let (updated_state, child_commands) = self
                            .middleware
                            .post
                            .handle(
                                self.post_middleware_state.take().unwrap(),
                                child_commands.into_operations(),
                                &metadata,
                                &MiddlewareContext { caller_pid },
                            )
                            .into_parts();
                        self.post_middleware_state.replace(updated_state);
                        child_commands
                    };
                    // Enqueue outgoing commands
                    queue.extend(
                        child_commands
                            .into_iter()
                            .map(|operation| (operation, Some((metadata.offset, pid)))),
                    );
                }
                StateOperation::Task(pid, task) => {
                    if let Entry::Vacant(entry) = self.processes.entry(pid) {
                        entry.insert(SyncProcess::Task(self.task_runner.run(task)));
                    }
                }
                StateOperation::Spawn(pid, factory) => {
                    if let Entry::Vacant(entry) = self.processes.entry(pid) {
                        entry.insert(SyncProcess::Worker(factory.create()));
                    }
                }
                StateOperation::Kill(pid) => {
                    if let Entry::Occupied(entry) = self.processes.entry(pid) {
                        match entry.remove() {
                            SyncProcess::Task(task) => task.abort(),
                            SyncProcess::Worker(_) => {}
                        }
                    }
                }
            };
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SyncContext {
    pid: ProcessId,
    caller_pid: Option<ProcessId>,
    next_pid: ProcessId,
}
impl HandlerContext for SyncContext {
    fn pid(&self) -> ProcessId {
        self.pid
    }
    fn caller_pid(&self) -> Option<ProcessId> {
        self.caller_pid
    }
    fn generate_pid(&mut self) -> ProcessId {
        let next_pid = self.next_pid.next();
        std::mem::replace(&mut self.next_pid, next_pid)
    }
}
