// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use futures::{Future, Stream};
use pin_project::pin_project;
use reflex::core::{Applicable, Expression};
use reflex_dispatcher::{
    Action, Actor, ActorEvents, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};
use reflex_macros::blanket_trait;
use reflex_runtime::{AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator};
use reflex_utils::reconnect::ReconnectTimeout;

use crate::task::{
    fetch::FetchHandlerTask, graphql::GraphQlHandlerTask, timeout::TimeoutHandlerTask,
    timestamp::TimestampHandlerTask,
};

use self::{
    fetch::{FetchHandler, FetchHandlerAction, FetchHandlerState},
    graphql::{GraphQlHandler, GraphQlHandlerAction, GraphQlHandlerState},
    loader::{LoaderHandler, LoaderHandlerAction, LoaderHandlerState},
    scan::{ScanHandler, ScanHandlerAction, ScanHandlerState},
    timeout::{TimeoutHandler, TimeoutHandlerAction, TimeoutHandlerState},
    timestamp::{TimestampHandler, TimestampHandlerAction, TimestampHandlerState},
    variable::{VariableHandler, VariableHandlerAction, VariableHandlerState},
};

pub mod fetch;
pub mod graphql;
pub mod loader;
pub mod scan;
pub mod timeout;
pub mod timestamp;
pub mod variable;

blanket_trait!(
    pub trait HandlerAction<T: Expression>:
        FetchHandlerAction<T>
        + GraphQlHandlerAction<T>
        + LoaderHandlerAction<T>
        + ScanHandlerAction<T>
        + TimeoutHandlerAction<T>
        + TimestampHandlerAction<T>
        + VariableHandlerAction<T>
    {
    }
);

blanket_trait!(
    pub trait HandlerTask<TConnect>:
        FetchHandlerTask<TConnect>
        + GraphQlHandlerTask<TConnect>
        + TimeoutHandlerTask
        + TimestampHandlerTask
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

#[derive(Clone)]
pub enum HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
{
    FetchHandler(FetchHandler<T, TFactory, TAllocator, TConnect>),
    GraphQlHandler(GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect>),
    LoaderHandler(LoaderHandler<T, TFactory, TAllocator>),
    ScanHandler(ScanHandler<T, TFactory, TAllocator>),
    TimeoutHandler(TimeoutHandler<T, TFactory, TAllocator>),
    TimestampHandler(TimestampHandler<T, TFactory, TAllocator>),
    VariableHandler(VariableHandler<T, TFactory, TAllocator>),
}
impl<T, TFactory, TAllocator, TConnect, TReconnect> Named
    for HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
{
    fn name(&self) -> &'static str {
        match self {
            Self::FetchHandler(inner) => inner.name(),
            Self::GraphQlHandler(inner) => inner.name(),
            Self::LoaderHandler(inner) => inner.name(),
            Self::ScanHandler(inner) => inner.name(),
            Self::TimeoutHandler(inner) => inner.name(),
            Self::TimestampHandler(inner) => inner.name(),
            Self::VariableHandler(inner) => inner.name(),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask> Actor<TAction, TTask>
    for HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        HandlerActorEvents<T, TFactory, TAllocator, TConnect, TReconnect, TInbox, TAction, TTask>;
    type Dispose =
        HandlerActorDispose<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask>;
    fn init(&self) -> Self::State {
        match self {
            Self::FetchHandler(actor) => HandlerActorState::FetchHandler(<FetchHandler<
                T,
                TFactory,
                TAllocator,
                TConnect,
            > as Actor<TAction, TTask>>::init(
                actor
            )),
            Self::GraphQlHandler(actor) => {
                HandlerActorState::GraphQlHandler(<GraphQlHandler<
                    T,
                    TFactory,
                    TAllocator,
                    TConnect,
                    TReconnect,
                > as Actor<TAction, TTask>>::init(
                    actor
                ))
            }
            Self::LoaderHandler(actor) => {
                HandlerActorState::LoaderHandler(
                    <LoaderHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(actor),
                )
            }
            Self::ScanHandler(actor) => {
                HandlerActorState::ScanHandler(<ScanHandler<T, TFactory, TAllocator> as Actor<
                    TAction,
                    TTask,
                >>::init(actor))
            }
            Self::TimeoutHandler(actor) => {
                HandlerActorState::TimeoutHandler(
                    <TimeoutHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(actor),
                )
            }
            Self::TimestampHandler(actor) => HandlerActorState::TimestampHandler(
                <TimestampHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(actor),
            ),
            Self::VariableHandler(actor) => {
                HandlerActorState::VariableHandler(
                    <VariableHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(
                        actor,
                    ),
                )
            }
        }
    }
    fn events<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
    ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
        match self {
            Self::FetchHandler(actor) => {
                <FetchHandler<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::events(
                    actor, inbox,
                )
                .map(|(events, dispose)| {
                    (
                        HandlerActorEvents::FetchHandler(events),
                        dispose.map(HandlerActorDispose::FetchHandler),
                    )
                })
            }
            Self::GraphQlHandler(actor) => {
                <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                    TAction,
                    TTask,
                >>::events(actor, inbox)
                .map(|(events, dispose)| {
                    (
                        HandlerActorEvents::GraphQlHandler(events),
                        dispose.map(HandlerActorDispose::GraphQlHandler),
                    )
                })
            }
            Self::LoaderHandler(actor) => <LoaderHandler<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (
                    HandlerActorEvents::LoaderHandler(events),
                    dispose.map(HandlerActorDispose::LoaderHandler),
                )
            }),
            Self::ScanHandler(actor) => <ScanHandler<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (
                    HandlerActorEvents::ScanHandler(events),
                    dispose.map(HandlerActorDispose::ScanHandler),
                )
            }),
            Self::TimeoutHandler(actor) => <TimeoutHandler<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (
                    HandlerActorEvents::TimeoutHandler(events),
                    dispose.map(HandlerActorDispose::TimeoutHandler),
                )
            }),
            Self::TimestampHandler(actor) => <TimestampHandler<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (
                    HandlerActorEvents::TimestampHandler(events),
                    dispose.map(HandlerActorDispose::TimestampHandler),
                )
            }),
            Self::VariableHandler(actor) => <VariableHandler<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (
                    HandlerActorEvents::VariableHandler(events),
                    dispose.map(HandlerActorDispose::VariableHandler),
                )
            }),
        }
    }
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask> TaskFactory<TAction, TTask>
    for HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    type Actor = Self;

    fn create(self) -> Self::Actor {
        self
    }
}

pub enum HandlerActorState<T: Expression> {
    FetchHandler(FetchHandlerState),
    GraphQlHandler(GraphQlHandlerState),
    LoaderHandler(LoaderHandlerState<T>),
    ScanHandler(ScanHandlerState<T>),
    TimeoutHandler(TimeoutHandlerState),
    TimestampHandler(TimestampHandlerState),
    VariableHandler(VariableHandlerState<T>),
}

#[pin_project(project = HandlerActorEventsVariant)]
pub enum HandlerActorEvents<T, TFactory, TAllocator, TConnect, TReconnect, TInbox, TAction, TTask>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    FetchHandler(
        #[pin]
        <FetchHandler<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    GraphQlHandler(
        #[pin]
        <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Events<TInbox>,
    ),
    LoaderHandler(
        #[pin] <LoaderHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    ScanHandler(
        #[pin] <ScanHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    TimeoutHandler(
        #[pin] <TimeoutHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    TimestampHandler(
        #[pin] <TimestampHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    VariableHandler(
        #[pin] <VariableHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TInbox, TAction, TTask> Stream
    for HandlerActorEvents<T, TFactory, TAllocator, TConnect, TReconnect, TInbox, TAction, TTask>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            HandlerActorEventsVariant::FetchHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::GraphQlHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::LoaderHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::ScanHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::TimeoutHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::TimestampHandler(inner) => inner.poll_next(cx),
            HandlerActorEventsVariant::VariableHandler(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::FetchHandler(inner) => inner.size_hint(),
            Self::GraphQlHandler(inner) => inner.size_hint(),
            Self::LoaderHandler(inner) => inner.size_hint(),
            Self::ScanHandler(inner) => inner.size_hint(),
            Self::TimeoutHandler(inner) => inner.size_hint(),
            Self::TimestampHandler(inner) => inner.size_hint(),
            Self::VariableHandler(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = HandlerActorDisposeVariant)]
pub enum HandlerActorDispose<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    FetchHandler(
        #[pin] <FetchHandler<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::Dispose,
    ),
    GraphQlHandler(
        #[pin]
        <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Dispose,
    ),
    LoaderHandler(
        #[pin] <LoaderHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
    ScanHandler(#[pin] <ScanHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose),
    TimeoutHandler(
        #[pin] <TimeoutHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
    TimestampHandler(
        #[pin] <TimestampHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
    VariableHandler(
        #[pin] <VariableHandler<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose,
    ),
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask> Future
    for HandlerActorDispose<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            HandlerActorDisposeVariant::FetchHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::GraphQlHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::LoaderHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::ScanHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::TimeoutHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::TimestampHandler(inner) => inner.poll(cx),
            HandlerActorDisposeVariant::VariableHandler(inner) => inner.poll(cx),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask>
    Worker<TAction, SchedulerTransition<TAction, TTask>>
    for HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::FetchHandler(inner) => {
                <FetchHandler<T, TFactory, TAllocator, TConnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(inner, message)
            }
            Self::GraphQlHandler(inner) => {
                <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(inner, message)
            }
            Self::LoaderHandler(inner) => <LoaderHandler<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::ScanHandler(inner) => <ScanHandler<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::TimeoutHandler(inner) => <TimeoutHandler<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::TimestampHandler(inner) => {
                <TimestampHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(inner, message)
            }
            Self::VariableHandler(inner) => <VariableHandler<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::FetchHandler(actor), HandlerActorState::FetchHandler(state)) => {
                <FetchHandler<T, TFactory, TAllocator, TConnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::GraphQlHandler(actor), HandlerActorState::GraphQlHandler(state)) => {
                <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::LoaderHandler(actor), HandlerActorState::LoaderHandler(state)) => {
                <LoaderHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::ScanHandler(actor), HandlerActorState::ScanHandler(state)) => {
                <ScanHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::TimeoutHandler(actor), HandlerActorState::TimeoutHandler(state)) => {
                <TimeoutHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::TimestampHandler(actor), HandlerActorState::TimestampHandler(state)) => {
                <TimestampHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::VariableHandler(actor), HandlerActorState::VariableHandler(state)) => {
                <VariableHandler<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            _ => unreachable!(),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TAction, TTask>
    Handler<TAction, SchedulerTransition<TAction, TTask>>
    for HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>
where
    T: AsyncExpression + Applicable<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TAction: Action + HandlerAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + HandlerTask<TConnect>,
{
    type State = HandlerActorState<T>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::FetchHandler(inner), HandlerActorState::FetchHandler(state)) => {
                <FetchHandler<T, TFactory, TAllocator, TConnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::GraphQlHandler(inner), HandlerActorState::GraphQlHandler(state)) => {
                <GraphQlHandler<T, TFactory, TAllocator, TConnect, TReconnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::LoaderHandler(inner), HandlerActorState::LoaderHandler(state)) => {
                <LoaderHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::ScanHandler(inner), HandlerActorState::ScanHandler(state)) => {
                <ScanHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::TimeoutHandler(inner), HandlerActorState::TimeoutHandler(state)) => {
                <TimeoutHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::TimestampHandler(inner), HandlerActorState::TimestampHandler(state)) => {
                <TimestampHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            (Self::VariableHandler(inner), HandlerActorState::VariableHandler(state)) => {
                <VariableHandler<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(inner, state, action, metadata, context)
            }
            _ => unreachable!(),
        }
    }
}
