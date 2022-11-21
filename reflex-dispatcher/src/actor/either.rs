// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{Future, Stream};
use pin_project::pin_project;

use crate::{
    Action, Actor, Handler, HandlerContext, MessageData, Named, SchedulerMode, TaskFactory,
    TaskInbox, Worker,
};

pub enum EitherActor<T1, T2> {
    Left(T1),
    Right(T2),
}
impl<T1, T2> Named for EitherActor<T1, T2>
where
    T1: Named,
    T2: Named,
{
    fn name(&self) -> &'static str {
        match self {
            EitherActor::Left(inner) => inner.name(),
            EitherActor::Right(inner) => inner.name(),
        }
    }
}
impl<T1, T2> Clone for EitherActor<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Left(inner) => Self::Left(inner.clone()),
            Self::Right(inner) => Self::Right(inner.clone()),
        }
    }
}

pub enum EitherActorState<T1, T2> {
    Left(T1),
    Right(T2),
}

impl<T1, T2, TAction, TTask> Actor<TAction, TTask> for EitherActor<T1, T2>
where
    T1: Actor<TAction, TTask>,
    T2: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> = EitherActorEvents<T1, T2, TInbox, TAction, TTask>;
    type Dispose = EitherActorDispose<T1, T2, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl crate::ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::Left(inner) => {
                let (state, events, dispose) = inner.init(inbox, context);
                (
                    Self::State::Left(state),
                    Self::Events::Left(events),
                    Self::Dispose::Left(dispose),
                )
            }
            Self::Right(inner) => {
                let (state, events, dispose) = inner.init(inbox, context);
                (
                    Self::State::Right(state),
                    Self::Events::Right(events),
                    Self::Dispose::Right(dispose),
                )
            }
        }
    }
}

#[pin_project(project = EitherActorEventsVariant)]
pub enum EitherActorEvents<T1, T2, TInbox, TAction, TTask>
where
    T1: Actor<TAction, TTask>,
    T2: Actor<TAction, TTask>,
    TInbox: TaskInbox<TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    Left(#[pin] <T1 as Actor<TAction, TTask>>::Events<TInbox>),
    Right(#[pin] <T2 as Actor<TAction, TTask>>::Events<TInbox>),
}
impl<T1, T2, TInbox, TAction, TTask> Stream for EitherActorEvents<T1, T2, TInbox, TAction, TTask>
where
    T1: Actor<TAction, TTask>,
    T2: Actor<TAction, TTask>,
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
            EitherActorEventsVariant::Left(inner) => inner.poll_next(cx),
            EitherActorEventsVariant::Right(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Left(inner) => inner.size_hint(),
            Self::Right(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = EitherActorDisposeVariant)]
pub enum EitherActorDispose<T1, T2, TAction, TTask>
where
    T1: Actor<TAction, TTask>,
    T2: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    Left(#[pin] <T1 as Actor<TAction, TTask>>::Dispose),
    Right(#[pin] <T2 as Actor<TAction, TTask>>::Dispose),
}
impl<T1, T2, TAction, TTask> Future for EitherActorDispose<T1, T2, TAction, TTask>
where
    T1: Actor<TAction, TTask>,
    T2: Actor<TAction, TTask>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            EitherActorDisposeVariant::Left(inner) => inner.poll(cx),
            EitherActorDisposeVariant::Right(inner) => inner.poll(cx),
        }
    }
}

impl<T1, T2, I, O> Worker<I, O> for EitherActor<T1, T2>
where
    T1: Worker<I, O>,
    T2: Worker<I, O>,
{
    fn accept(&self, message: &I) -> bool {
        match self {
            Self::Left(actor) => actor.accept(message),
            Self::Right(actor) => actor.accept(message),
        }
    }
    fn schedule(&self, message: &I, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => actor.schedule(message, state),
            (Self::Right(actor), Self::State::Right(state)) => actor.schedule(message, state),
            _ => None,
        }
    }
}

impl<T1, T2, I, O> Handler<I, O> for EitherActor<T1, T2>
where
    T1: Handler<I, O>,
    T2: Handler<I, O>,
{
    type State = EitherActorState<T1::State, T2::State>;
    fn handle(
        &self,
        state: &mut Self::State,
        message: &I,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<O> {
        match (self, state) {
            (Self::Left(actor), Self::State::Left(state)) => {
                actor.handle(state, message, metadata, context)
            }
            (Self::Right(actor), Self::State::Right(state)) => {
                actor.handle(state, message, metadata, context)
            }
            (_, _) => None,
        }
    }
}
