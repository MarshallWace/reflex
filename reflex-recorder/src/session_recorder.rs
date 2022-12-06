// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, ops::Deref};

use reflex_dispatcher::{Action, ProcessId, TaskFactory};
use reflex_scheduler::tokio::{AsyncMessage, TokioCommand, TokioSchedulerLogger};

pub trait ActionRecorder {
    type Action: Action;
    fn record(&mut self, action: &Self::Action);
}

pub struct SessionRecorder<TRecorder, TAction, TTask>
where
    TRecorder: ActionRecorder,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    recorder: TRecorder,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TRecorder, TAction, TTask> SessionRecorder<TRecorder, TAction, TTask>
where
    TRecorder: ActionRecorder,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(recorder: TRecorder) -> Self {
        Self {
            recorder,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}

impl<TRecorder, TAction, TTask> TokioSchedulerLogger for SessionRecorder<TRecorder, TAction, TTask>
where
    TRecorder: ActionRecorder<Action = TAction>,
    TAction: Action + Clone + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log_scheduler_command(
        &mut self,
        _command: &TokioCommand<Self::Action, Self::Task>,
        _enqueue_time: std::time::Instant,
    ) {
    }
    fn log_worker_message(
        &mut self,
        message: &AsyncMessage<Self::Action>,
        _actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        _pid: ProcessId,
    ) {
        let action = message.deref();
        self.recorder.record(action)
    }
    fn log_task_message(&mut self, _message: &AsyncMessage<Self::Action>, _pid: ProcessId) {}
}
