// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use crate::{
    Action, BoxedWorkerFactory, MessageData, MiddlewareContext, OperationStream, PostMiddleware,
    PostMiddlewareTransition, PreMiddleware, PreMiddlewareTransition, StateOperation,
};

pub trait OperationRecorderFactory<TRecorder: OperationRecorder> {
    fn create(&self) -> TRecorder;
}

pub trait OperationRecorder {
    type Action: Action;
    fn record(&mut self, operation: StateOperation<Self::Action>);
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

impl<TRecorderFactory, TRecorder, TAction> PreMiddleware<TAction>
    for SessionRecorder<TRecorderFactory, TRecorder, TAction>
where
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction>,
    TAction: Action + Clone + Send + 'static,
{
    type State = SessionRecorderState<TRecorder>;
    fn init(&self) -> Self::State {
        SessionRecorderState::new(self.recorder_factory.create())
    }
    fn handle(
        &self,
        state: Self::State,
        operation: StateOperation<TAction>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction> {
        let cloned_operation = match &operation {
            StateOperation::Send(pid, action) => StateOperation::Send(*pid, action.clone()),
            StateOperation::Task(pid, _) => StateOperation::Task(*pid, OperationStream::noop()),
            StateOperation::Spawn(pid, _) => {
                StateOperation::Spawn(*pid, BoxedWorkerFactory::noop())
            }
            StateOperation::Kill(pid) => StateOperation::Kill(*pid),
        };
        let mut state = state;
        state.recorder.record(cloned_operation);
        PreMiddlewareTransition::new(state, operation)
    }
}

impl<TRecorderFactory, TRecorder, TAction> PostMiddleware<TAction>
    for SessionRecorder<TRecorderFactory, TRecorder, TAction>
where
    TRecorderFactory: OperationRecorderFactory<TRecorder>,
    TRecorder: OperationRecorder<Action = TAction>,
    TAction: Action,
{
    type State = SessionRecorderState<TRecorder>;
    fn init(&self) -> Self::State {
        SessionRecorderState::new(self.recorder_factory.create())
    }
    fn handle(
        &self,
        state: Self::State,
        operation: StateOperation<TAction>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State> {
        let mut state = state;
        state.recorder.record(operation);
        PostMiddlewareTransition::new(state)
    }
}
