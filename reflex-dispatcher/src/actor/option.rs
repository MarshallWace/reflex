// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{Future, Stream};
use pin_project::pin_project;

use crate::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, MiddlewareContext,
    PostMiddleware, PreMiddleware, SchedulerCommand, SchedulerMode, SchedulerTransition,
    TaskFactory, TaskInbox, Worker,
};

impl<T, TAction, TTask> Actor<TAction, TTask> for Option<T>
where
    T: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> = OptionActorEvents<T, TInbox, TAction, TTask>;
    type Dispose = OptionActorDispose<T, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            None => (
                None,
                OptionActorEvents::None(inbox),
                OptionActorDispose::None,
            ),
            Some(actor) => {
                let (state, events, dispose) = actor.init(inbox, context);
                (
                    Some(state),
                    OptionActorEvents::Some(events),
                    OptionActorDispose::Some(dispose),
                )
            }
        }
    }
}

#[pin_project(project = OptionActorEventsVariant)]
pub enum OptionActorEvents<T, TInbox, TAction, TTask>
where
    T: Actor<TAction, TTask>,
    TInbox: TaskInbox<TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    None(#[pin] TInbox),
    Some(#[pin] <T as Actor<TAction, TTask>>::Events<TInbox>),
}
impl<T, TInbox, TAction, TTask> Stream for OptionActorEvents<T, TInbox, TAction, TTask>
where
    T: Actor<TAction, TTask>,
    TInbox: TaskInbox<TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            OptionActorEventsVariant::None(inner) => inner.poll_next(cx),
            OptionActorEventsVariant::Some(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::None(inner) => inner.size_hint(),
            Self::Some(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = OptionActorDisposeVariant)]
pub enum OptionActorDispose<T, TAction, TTask>
where
    T: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    None,
    Some(#[pin] <T as Actor<TAction, TTask>>::Dispose),
}
impl<T, TAction, TTask> Future for OptionActorDispose<T, TAction, TTask>
where
    T: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            OptionActorDisposeVariant::None => std::task::Poll::Ready(()),
            OptionActorDisposeVariant::Some(inner) => inner.poll(cx),
        }
    }
}

impl<T, I, O> Worker<I, O> for Option<T>
where
    T: Worker<I, O>,
{
    fn accept(&self, message: &I) -> bool {
        match self {
            Some(inner) => inner.accept(message),
            None => false,
        }
    }
    fn schedule(&self, message: &I, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.schedule(message, state),
            (_, _) => None,
        }
    }
}

impl<T, I, O> Handler<I, O> for Option<T>
where
    T: Handler<I, O>,
{
    type State = Option<T::State>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &I,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<O> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.handle(state, action, metadata, context),
            (_, _) => None,
        }
    }
}

impl<T, TAction, TTask> PreMiddleware<TAction, TTask> for Option<T>
where
    T: PreMiddleware<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = Option<T::State>;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operation: SchedulerCommand<TAction, TTask>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> SchedulerCommand<TAction, TTask> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.handle(state, operation, metadata, context),
            (_, _) => operation,
        }
    }
}

impl<T, TAction, TTask> PostMiddleware<TAction, TTask> for Option<T>
where
    T: PostMiddleware<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = Option<T::State>;
    fn init(&self) -> Self::State {
        self.as_ref().map(|inner| inner.init())
    }
    fn handle(
        &self,
        state: &mut Self::State,
        operations: Vec<SchedulerCommand<TAction, TTask>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Some(inner), Some(state)) => inner.handle(state, operations, metadata, context),
            (_, _) => None,
        }
    }
}
