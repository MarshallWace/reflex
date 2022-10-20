// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, MiddlewareContext,
    NoopDisposeCallback, PostMiddleware, PreMiddleware, SchedulerCommand, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};

#[derive(Default, Clone, Copy)]
pub struct NoopActor;

impl<TAction, TTask> Actor<TAction, TTask> for NoopActor
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> = TInbox;
    type Dispose = NoopDisposeCallback;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        _context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        (Default::default(), inbox, Default::default())
    }
}

impl<I, O> Worker<I, O> for NoopActor {
    fn accept(&self, _message: &I) -> bool {
        false
    }
    fn schedule(&self, _message: &I, _state: &Self::State) -> Option<SchedulerMode> {
        None
    }
}

impl<I, O> Handler<I, O> for NoopActor {
    type State = ();
    fn handle(
        &self,
        _state: &mut Self::State,
        _message: &I,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<O> {
        None
    }
}

impl<TAction, TTask> PreMiddleware<TAction, TTask> for NoopActor
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        _state: &mut Self::State,
        operation: SchedulerCommand<TAction, TTask>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> SchedulerCommand<TAction, TTask> {
        operation
    }
}

impl<TAction, TTask> PostMiddleware<TAction, TTask> for NoopActor
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = ();
    fn init(&self) -> Self::State {
        ()
    }
    fn handle(
        &self,
        _state: &mut Self::State,
        operations: Vec<SchedulerCommand<TAction, TTask>>,
        _metadata: &MessageData,
        _context: &MiddlewareContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        Some(SchedulerTransition::new(operations))
    }
}
