// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use reflex_dispatcher::{
    Action, MessageData, MiddlewareContext, PostMiddleware, PreMiddleware, SchedulerCommand,
    SchedulerTransition, TaskFactory,
};

pub trait OperationRecorderFactory<TRecorder: OperationRecorder> {
    fn create(&self) -> TRecorder;
}

pub trait OperationRecorder {
    type Action: Action;
    type Task: TaskFactory<Self::Action, Self::Task>;
    fn record(&mut self, operation: &SchedulerCommand<Self::Action, Self::Task>);
    fn record_batch(&mut self, operations: &[SchedulerCommand<Self::Action, Self::Task>]) {
        for operation in operations {
            self.record(operation)
        }
    }
}

pub struct SessionRecorder<
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction>,
    TAction: Action,
> {
    recorder_factory: TRecorderFactory,
    _recorder: PhantomData<TRecorder>,
    _action: PhantomData<TAction>,
}
impl<TRecorderFactory, TRecorder, TAction> SessionRecorder<TRecorderFactory, TRecorder, TAction>
where
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction>,
    TAction: Action,
{
    pub fn new(recorder: TRecorderFactory) -> Self {
        Self {
            recorder_factory: recorder,
            _recorder: Default::default(),
            _action: Default::default(),
        }
    }
}

pub struct SessionRecorderState<TRecorder: OperationRecorder> {
    recorder: TRecorder,
}
impl<TRecorder> SessionRecorderState<TRecorder>
where
    TRecorder: OperationRecorder,
{
    pub fn new(recorder: TRecorder) -> Self {
        Self { recorder }
    }
}

impl<TRecorderFactory, TRecorder, TAction, TTask> PreMiddleware<TAction, TTask>
    for SessionRecorder<TRecorderFactory, TRecorder, TAction>
where
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = SessionRecorderState<TRecorder>;
    fn init(&self) -> Self::State {
        SessionRecorderState::new(self.recorder_factory.create())
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operation: SchedulerCommand<TAction, TTask>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> SchedulerCommand<TAction, TTask> {
        state.recorder.record(&operation);
        operation
    }
}

impl<TRecorderFactory, TRecorder, TAction, TTask> PostMiddleware<TAction, TTask>
    for SessionRecorder<TRecorderFactory, TRecorder, TAction>
where
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction, Task = TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = SessionRecorderState<TRecorder>;
    fn init(&self) -> Self::State {
        SessionRecorderState::new(self.recorder_factory.create())
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operations: Vec<SchedulerCommand<TAction, TTask>>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        state.recorder.record_batch(&operations);
        None
    }
}
