// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::{hash_map::Entry, HashMap, VecDeque};

use crate::{
    Action, Actor, HandlerContext, MessageData, MessageOffset, OperationStream, ProcessId,
    Scheduler, StateOperation, WorkerContext, WorkerInstance, WorkerMessageQueue,
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
    Worker(WorkerInstance<TAction>),
}

pub struct SyncScheduler<TActor, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TAction: Action + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    main: TActor,
    root_pid: ProcessId,
    next_pid: ProcessId,
    next_offset: MessageOffset,
    processes: HashMap<ProcessId, SyncProcess<TAction>>,
    task_runner: TRunner,
}
impl<TActor, TAction, TRunner> SyncScheduler<TActor, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TAction: Action + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    pub fn new(main: TActor, runner: TRunner) -> Self {
        let root_pid = ProcessId::default();
        let next_pid = root_pid.next();
        Self {
            main,
            root_pid,
            next_pid,
            task_runner: runner,
            processes: Default::default(),
            next_offset: Default::default(),
        }
    }
}
impl<TActor, TAction, TRunner> Scheduler for SyncScheduler<TActor, TAction, TRunner>
where
    TActor: Actor<TAction>,
    TAction: Action + Send + 'static,
    TRunner: TaskRunner<TAction>,
{
    type Action = TAction;
    fn dispatch(&mut self, action: Self::Action) {
        let mut queue = VecDeque::default();
        queue.push_back((StateOperation::Send(self.root_pid, action), None));
        while let Some((operation, caller)) = queue.pop_front() {
            match operation {
                StateOperation::Send(pid, action) => {
                    let metadata = MessageData {
                        offset: {
                            let next_offset = self.next_offset.next();
                            std::mem::replace(&mut self.next_offset, next_offset)
                        },
                        parent: caller.map(|(parent, _)| parent),
                        timestamp: std::time::Instant::now(),
                    };
                    let mut context = SyncContext {
                        pid,
                        caller_pid: caller.map(|(_, pid)| pid),
                        next_pid: self.next_pid,
                    };
                    if pid == self.root_pid {
                        let transition = self.main.handle(&action, &metadata, &mut context);
                        queue.extend(
                            transition
                                .into_iter()
                                .map(|operation| (operation, Some((metadata.offset, pid)))),
                        );
                    } else if let Some(SyncProcess::Worker(worker)) = self.processes.get_mut(&pid) {
                        let mut inbox = WorkerMessageQueue::new();
                        inbox.push_back((action, metadata));
                        let transition = worker.handle(inbox, &mut WorkerContext::new(context));
                        queue.extend(
                            transition
                                .into_iter()
                                .map(|(offset, operation)| (operation, Some((offset, pid)))),
                        );
                    };
                    self.next_pid = context.next_pid;
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
            }
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
