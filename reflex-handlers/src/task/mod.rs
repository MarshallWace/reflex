// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::pin::Pin;

use futures::{Future, Stream};
use pin_project::pin_project;
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};

pub mod fetch;
pub mod graphql;
pub mod timeout;
pub mod timestamp;

use crate::task::{
    fetch::{
        FetchHandlerTask, FetchHandlerTaskAction, FetchHandlerTaskActor, FetchHandlerTaskFactory,
    },
    graphql::{
        GraphQlHandlerTask, GraphQlHandlerTaskAction, GraphQlHandlerTaskActor,
        GraphQlHandlerTaskFactory,
    },
    timeout::{
        TimeoutHandlerTask, TimeoutHandlerTaskAction, TimeoutHandlerTaskActor,
        TimeoutHandlerTaskFactory,
    },
    timestamp::{
        TimestampHandlerTask, TimestampHandlerTaskAction, TimestampHandlerTaskActor,
        TimestampHandlerTaskFactory,
    },
};

use self::graphql::{
    GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory,
};

pub trait DefaultHandlersTaskAction:
    Action
    + FetchHandlerTaskAction
    + GraphQlHandlerTaskAction
    + TimeoutHandlerTaskAction
    + TimestampHandlerTaskAction
{
}
impl<TAction> DefaultHandlersTaskAction for TAction where
    Self: Action
        + FetchHandlerTaskAction
        + GraphQlHandlerTaskAction
        + TimeoutHandlerTaskAction
        + TimestampHandlerTaskAction
{
}

pub trait DefaultHandlersTask<TConnect>:
    FetchHandlerTask<TConnect>
    + GraphQlHandlerTask<TConnect>
    + TimeoutHandlerTask
    + TimestampHandlerTask
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
}
impl<TSelf, TConnect> DefaultHandlersTask<TConnect> for TSelf
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    Self: FetchHandlerTask<TConnect>
        + GraphQlHandlerTask<TConnect>
        + TimeoutHandlerTask
        + TimestampHandlerTask,
{
}

// TODO: Implement Serialize/Deserialize traits for DefaultHandlersTaskFactory
#[derive(Clone)]
pub enum DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    Fetch(FetchHandlerTaskFactory<TConnect>),
    GraphQl(GraphQlHandlerTaskFactory<TConnect>),
    Timeout(TimeoutHandlerTaskFactory),
    Timestamp(TimestampHandlerTaskFactory),
}
impl<TConnect> Named for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        match self {
            Self::Fetch(inner) => inner.name(),
            Self::GraphQl(inner) => inner.name(),
            Self::Timeout(inner) => inner.name(),
            Self::Timestamp(inner) => inner.name(),
        }
    }
}

impl<TConnect, TAction, TTask> TaskFactory<TAction, TTask> for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = DefaultHandlersTaskActor<TConnect>;
    fn create(self) -> Self::Actor {
        match self {
            Self::Fetch(inner) => DefaultHandlersTaskActor::Fetch(<FetchHandlerTaskFactory<
                TConnect,
            > as TaskFactory<TAction, TTask>>::create(
                inner
            )),
            Self::GraphQl(inner) => DefaultHandlersTaskActor::GraphQl(<GraphQlHandlerTaskFactory<
                TConnect,
            > as TaskFactory<
                TAction,
                TTask,
            >>::create(inner)),
            Self::Timeout(inner) => {
                DefaultHandlersTaskActor::Timeout(<TimeoutHandlerTaskFactory as TaskFactory<
                    TAction,
                    TTask,
                >>::create(inner))
            }
            Self::Timestamp(inner) => {
                DefaultHandlersTaskActor::Timestamp(<TimestampHandlerTaskFactory as TaskFactory<
                    TAction,
                    TTask,
                >>::create(inner))
            }
        }
    }
}

#[derive(Clone)]
pub enum DefaultHandlersTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    Fetch(FetchHandlerTaskActor<TConnect>),
    GraphQl(GraphQlHandlerTaskActor<TConnect>),
    Timeout(TimeoutHandlerTaskActor),
    Timestamp(TimestampHandlerTaskActor),
}
impl<TConnect> Named for DefaultHandlersTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        match self {
            Self::Fetch(inner) => inner.name(),
            Self::GraphQl(inner) => inner.name(),
            Self::Timeout(inner) => inner.name(),
            Self::Timestamp(inner) => inner.name(),
        }
    }
}

impl<TConnect, TAction, TTask> Actor<TAction, TTask> for DefaultHandlersTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        DefaultHandlersTaskEvents<TConnect, TInbox, TAction, TTask>;
    type Dispose = DefaultHandlersTaskDispose<TConnect, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::Fetch(inner) => {
                let (state, events, dispose) = <FetchHandlerTaskActor<TConnect> as Actor<
                    TAction,
                    TTask,
                >>::init(inner, inbox, context);
                (
                    DefaultHandlersTaskActorState::Fetch(state),
                    DefaultHandlersTaskEvents::Fetch(events),
                    DefaultHandlersTaskDispose::Fetch(dispose),
                )
            }
            Self::GraphQl(inner) => {
                let (state, events, dispose) = <GraphQlHandlerTaskActor<TConnect> as Actor<
                    TAction,
                    TTask,
                >>::init(inner, inbox, context);
                (
                    DefaultHandlersTaskActorState::GraphQl(state),
                    DefaultHandlersTaskEvents::GraphQl(events),
                    DefaultHandlersTaskDispose::GraphQl(dispose),
                )
            }
            Self::Timeout(inner) => {
                let (state, events, dispose) =
                    <TimeoutHandlerTaskActor as Actor<TAction, TTask>>::init(inner, inbox, context);
                (
                    DefaultHandlersTaskActorState::Timeout(state),
                    DefaultHandlersTaskEvents::Timeout(events),
                    DefaultHandlersTaskDispose::Timeout(dispose),
                )
            }
            Self::Timestamp(inner) => {
                let (state, events, dispose) = <TimestampHandlerTaskActor as Actor<
                    TAction,
                    TTask,
                >>::init(inner, inbox, context);
                (
                    DefaultHandlersTaskActorState::Timestamp(state),
                    DefaultHandlersTaskEvents::Timestamp(events),
                    DefaultHandlersTaskDispose::Timestamp(dispose),
                )
            }
        }
    }
}

impl<TConnect, TAction, TTask> Worker<TAction, SchedulerTransition<TAction, TTask>>
    for DefaultHandlersTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::Fetch(inner) => <FetchHandlerTaskActor<TConnect> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::GraphQl(inner) => <GraphQlHandlerTaskActor<TConnect> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::Timeout(inner) => <TimeoutHandlerTaskActor as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
            Self::Timestamp(inner) => <TimestampHandlerTaskActor as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(inner, message),
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::Fetch(actor), DefaultHandlersTaskActorState::Fetch(state)) => {
                <FetchHandlerTaskActor<TConnect> as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::GraphQl(actor), DefaultHandlersTaskActorState::GraphQl(state)) => {
                <GraphQlHandlerTaskActor<TConnect> as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::Timeout(actor), DefaultHandlersTaskActorState::Timeout(state)) => {
                <TimeoutHandlerTaskActor as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::Timestamp(actor), DefaultHandlersTaskActorState::Timestamp(state)) => {
                <TimestampHandlerTaskActor as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            _ => unreachable!(),
        }
    }
}

impl<TConnect, TAction, TTask> Handler<TAction, SchedulerTransition<TAction, TTask>>
    for DefaultHandlersTaskActor<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = DefaultHandlersTaskActorState<TConnect, TAction, TTask>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::Fetch(actor), DefaultHandlersTaskActorState::Fetch(state)) => {
                <FetchHandlerTaskActor<TConnect> as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::GraphQl(actor), DefaultHandlersTaskActorState::GraphQl(state)) => {
                <GraphQlHandlerTaskActor<TConnect> as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::Timeout(actor), DefaultHandlersTaskActorState::Timeout(state)) => {
                <TimeoutHandlerTaskActor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::Timestamp(actor), DefaultHandlersTaskActorState::Timestamp(state)) => {
                <TimestampHandlerTaskActor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            _ => unreachable!(),
        }
    }
}

pub enum DefaultHandlersTaskActorState<TConnect, TAction, TTask>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    Fetch(
        <FetchHandlerTaskActor<TConnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    GraphQl(
        <GraphQlHandlerTaskActor<TConnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    Timeout(
        <TimeoutHandlerTaskActor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    ),
    Timestamp(
        <TimestampHandlerTaskActor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    ),
}

#[pin_project(project = DefaultHandlersTaskEventsVariant)]
pub enum DefaultHandlersTaskEvents<TConnect, TInbox, TAction, TTask>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    Fetch(#[pin] <FetchHandlerTaskActor<TConnect> as Actor<TAction, TTask>>::Events<TInbox>),
    GraphQl(#[pin] <GraphQlHandlerTaskActor<TConnect> as Actor<TAction, TTask>>::Events<TInbox>),
    Timeout(#[pin] <TimeoutHandlerTaskActor as Actor<TAction, TTask>>::Events<TInbox>),
    Timestamp(#[pin] <TimestampHandlerTaskActor as Actor<TAction, TTask>>::Events<TInbox>),
}
impl<TConnect, TInbox, TAction, TTask> Stream
    for DefaultHandlersTaskEvents<TConnect, TInbox, TAction, TTask>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            DefaultHandlersTaskEventsVariant::Fetch(inner) => inner.poll_next(cx),
            DefaultHandlersTaskEventsVariant::GraphQl(inner) => inner.poll_next(cx),
            DefaultHandlersTaskEventsVariant::Timeout(inner) => inner.poll_next(cx),
            DefaultHandlersTaskEventsVariant::Timestamp(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Fetch(inner) => inner.size_hint(),
            Self::GraphQl(inner) => inner.size_hint(),
            Self::Timeout(inner) => inner.size_hint(),
            Self::Timestamp(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = DefaultHandlersTaskDisposeVariant)]
pub enum DefaultHandlersTaskDispose<TConnect, TAction, TTask>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    Fetch(#[pin] <FetchHandlerTaskActor<TConnect> as Actor<TAction, TTask>>::Dispose),
    GraphQl(#[pin] <GraphQlHandlerTaskActor<TConnect> as Actor<TAction, TTask>>::Dispose),
    Timeout(#[pin] <TimeoutHandlerTaskActor as Actor<TAction, TTask>>::Dispose),
    Timestamp(#[pin] <TimestampHandlerTaskActor as Actor<TAction, TTask>>::Dispose),
}
impl<TConnect, TAction, TTask> Future for DefaultHandlersTaskDispose<TConnect, TAction, TTask>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + DefaultHandlersTaskAction + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            DefaultHandlersTaskDisposeVariant::Fetch(inner) => inner.poll(cx),
            DefaultHandlersTaskDisposeVariant::GraphQl(inner) => inner.poll(cx),
            DefaultHandlersTaskDisposeVariant::Timeout(inner) => inner.poll(cx),
            DefaultHandlersTaskDisposeVariant::Timestamp(inner) => inner.poll(cx),
        }
    }
}

impl<TConnect> From<FetchHandlerTaskFactory<TConnect>> for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: FetchHandlerTaskFactory<TConnect>) -> Self {
        Self::Fetch(value)
    }
}

impl<TConnect> From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerHttpFetchTaskFactory<TConnect>) -> Self {
        Self::GraphQl(GraphQlHandlerTaskFactory::HttpFetch(value))
    }
}

impl<TConnect> From<GraphQlHandlerWebSocketConnectionTaskFactory>
    for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerWebSocketConnectionTaskFactory) -> Self {
        Self::GraphQl(GraphQlHandlerTaskFactory::WebSocketConnection(value))
    }
}

impl<TConnect> From<TimeoutHandlerTaskFactory> for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: TimeoutHandlerTaskFactory) -> Self {
        Self::Timeout(value)
    }
}

impl<TConnect> From<TimestampHandlerTaskFactory> for DefaultHandlersTaskFactory<TConnect>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: TimestampHandlerTaskFactory) -> Self {
        Self::Timestamp(value)
    }
}
