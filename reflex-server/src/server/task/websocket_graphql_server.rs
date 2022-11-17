// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, pin::Pin, time::Duration};

use futures::{Future, FutureExt, Stream};
use pin_project::pin_project;
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, BoxedActionStream, Handler, HandlerContext, MessageData,
    Named, NoopDisposeCallback, ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition,
    TaskFactory, TaskInbox, Worker,
};
use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::server::action::websocket_server::WebSocketServerThrottleTimeoutAction;

pub trait WebSocketGraphQlServerTaskAction:
    Action + WebSocketGraphQlServerThrottleTimeoutTaskAction
{
}
impl<_Self> WebSocketGraphQlServerTaskAction for _Self where
    Self: Action + WebSocketGraphQlServerThrottleTimeoutTaskAction
{
}

pub trait WebSocketGraphQlServerTask:
    From<WebSocketGraphQlServerThrottleTimeoutTaskFactory>
{
}
impl<_Self> WebSocketGraphQlServerTask for _Self where
    Self: From<WebSocketGraphQlServerThrottleTimeoutTaskFactory>
{
}

#[derive(Clone, Serialize, Deserialize)]
pub enum WebSocketGraphQlServerTaskFactory {
    ThrottleTimeout(WebSocketGraphQlServerThrottleTimeoutTaskFactory),
}
impl Named for WebSocketGraphQlServerTaskFactory {
    fn name(&self) -> &'static str {
        match self {
            Self::ThrottleTimeout(inner) => inner.name(),
        }
    }
}
impl<TAction, TTask> TaskFactory<TAction, TTask> for WebSocketGraphQlServerTaskFactory
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = WebSocketGraphQlServerTaskActor;
    fn create(self) -> Self::Actor {
        match self {
            Self::ThrottleTimeout(actor) => {
                Self::Actor::ThrottleTimeout(
                    <WebSocketGraphQlServerThrottleTimeoutTaskFactory as TaskFactory<
                        TAction,
                        TTask,
                    >>::create(actor),
                )
            }
        }
    }
}

#[derive(Clone)]
pub enum WebSocketGraphQlServerTaskActor {
    ThrottleTimeout(WebSocketGraphQlServerThrottleTimeoutTaskActor),
}
impl Named for WebSocketGraphQlServerTaskActor {
    fn name(&self) -> &'static str {
        match self {
            Self::ThrottleTimeout(inner) => inner.name(),
        }
    }
}

impl<TAction, TTask> Actor<TAction, TTask> for WebSocketGraphQlServerTaskActor
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        WebSocketGraphQlServerTaskEvents<TInbox, TAction, TTask>;
    type Dispose = WebSocketGraphQlServerTaskDispose<TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::ThrottleTimeout(actor) => {
                let (state, events, dispose) =
                    <WebSocketGraphQlServerThrottleTimeoutTaskActor as Actor<TAction, TTask>>::init(
                        actor, inbox, context,
                    );
                (
                    WebSocketGraphQlServerTaskActorState::ThrottleTimeout(state),
                    WebSocketGraphQlServerTaskEvents::ThrottleTimeout(events),
                    WebSocketGraphQlServerTaskDispose::ThrottleTimeout(dispose),
                )
            }
        }
    }
}

impl<TAction, TTask> Worker<TAction, SchedulerTransition<TAction, TTask>>
    for WebSocketGraphQlServerTaskActor
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::ThrottleTimeout(actor) => {
                <WebSocketGraphQlServerThrottleTimeoutTaskActor as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (
                Self::ThrottleTimeout(actor),
                WebSocketGraphQlServerTaskActorState::ThrottleTimeout(state),
            ) => <WebSocketGraphQlServerThrottleTimeoutTaskActor as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::schedule(actor, message, state),
        }
    }
}

impl<TAction, TTask> Handler<TAction, SchedulerTransition<TAction, TTask>>
    for WebSocketGraphQlServerTaskActor
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = WebSocketGraphQlServerTaskActorState;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (
                Self::ThrottleTimeout(actor),
                WebSocketGraphQlServerTaskActorState::ThrottleTimeout(state),
            ) => <WebSocketGraphQlServerThrottleTimeoutTaskActor as Handler<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::handle(actor, state, action, metadata, context),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum WebSocketGraphQlServerTaskActorState {
    ThrottleTimeout(WebSocketGraphQlServerThrottleTimeoutTaskActorState),
}

#[pin_project(project = WebSocketGraphQlServerTaskEventsVariant)]
pub enum WebSocketGraphQlServerTaskEvents<TInbox, TAction, TTask>
where
    TInbox: TaskInbox<TAction>,
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    ThrottleTimeout(
        #[pin]
        <WebSocketGraphQlServerThrottleTimeoutTaskActor as Actor<TAction, TTask>>::Events<TInbox>,
    ),
}
impl<TInbox, TAction, TTask> Stream for WebSocketGraphQlServerTaskEvents<TInbox, TAction, TTask>
where
    TInbox: TaskInbox<TAction>,
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            WebSocketGraphQlServerTaskEventsVariant::ThrottleTimeout(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::ThrottleTimeout(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = WebSocketGraphQlServerTaskDisposeVariant)]
pub enum WebSocketGraphQlServerTaskDispose<TAction, TTask>
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    ThrottleTimeout(
        #[pin] <WebSocketGraphQlServerThrottleTimeoutTaskActor as Actor<TAction, TTask>>::Dispose,
    ),
}
impl<TAction, TTask> Future for WebSocketGraphQlServerTaskDispose<TAction, TTask>
where
    TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            WebSocketGraphQlServerTaskDisposeVariant::ThrottleTimeout(inner) => inner.poll(cx),
        }
    }
}

pub trait WebSocketGraphQlServerThrottleTimeoutTaskAction:
    Action
    + WebSocketGraphQlServerThrottleTimeoutTaskEventsAction
    + WebSocketGraphQlServerThrottleTimeoutTaskActorAction
{
}
impl<_Self> WebSocketGraphQlServerThrottleTimeoutTaskAction for _Self where
    Self: WebSocketGraphQlServerThrottleTimeoutTaskEventsAction
        + WebSocketGraphQlServerThrottleTimeoutTaskActorAction
{
}

pub trait WebSocketGraphQlServerThrottleTimeoutTaskEventsAction:
    Action + From<WebSocketServerThrottleTimeoutAction>
{
}
impl<_Self> WebSocketGraphQlServerThrottleTimeoutTaskEventsAction for _Self where
    Self: Action + From<WebSocketServerThrottleTimeoutAction>
{
}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct WebSocketGraphQlServerThrottleTimeoutTaskFactory {
    pub subscription_id: Uuid,
    pub delay: Duration,
    pub caller_pid: ProcessId,
}
impl<TAction, TTask> TaskFactory<TAction, TTask>
    for WebSocketGraphQlServerThrottleTimeoutTaskFactory
where
    TAction: Action + WebSocketGraphQlServerThrottleTimeoutTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = WebSocketGraphQlServerThrottleTimeoutTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            subscription_id,
            delay,
            caller_pid,
        } = self;
        WebSocketGraphQlServerThrottleTimeoutTaskActor {
            subscription_id,
            delay,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct WebSocketGraphQlServerThrottleTimeoutTaskActor {
    subscription_id: Uuid,
    delay: Duration,
    caller_pid: ProcessId,
}

#[derive(Default, Clone, Copy, Debug)]
pub struct WebSocketGraphQlServerThrottleTimeoutTaskActorState;

dispatcher!({
    pub enum WebSocketGraphQlServerThrottleTimeoutTaskActorAction {
        Inbox(WebSocketServerThrottleTimeoutAction),

        Outbox(WebSocketServerThrottleTimeoutAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for WebSocketGraphQlServerThrottleTimeoutTaskActor
    where
        TAction: Action + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = WebSocketGraphQlServerThrottleTimeoutTaskActorState;
        type Events<TInbox: TaskInbox<TAction>> = BoxedActionStream<TInbox::Message>;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (
                Default::default(),
                Box::pin(self.events(inbox, context)),
                Default::default(),
            )
        }

        fn accept(&self, _action: &WebSocketServerThrottleTimeoutAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &WebSocketServerThrottleTimeoutAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &WebSocketServerThrottleTimeoutAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_websocket_server_throttle_timeout(state, action, metadata, context)
        }
    }
});

impl WebSocketGraphQlServerThrottleTimeoutTaskActor {
    fn events<TInbox, TAction>(
        &self,
        _inbox: TInbox,
        _context: &impl ActorInitContext,
    ) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action + From<WebSocketServerThrottleTimeoutAction>,
    {
        tokio::time::sleep(self.delay)
            .map({
                let subscription_id = self.subscription_id;
                move |_| TAction::from(WebSocketServerThrottleTimeoutAction { subscription_id })
            })
            .map(|action| TInbox::Message::from(action))
            .into_stream()
    }
    fn handle_websocket_server_throttle_timeout<TAction, TTask>(
        &self,
        _state: &mut WebSocketGraphQlServerThrottleTimeoutTaskActorState,
        _action: &WebSocketServerThrottleTimeoutAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + Send + 'static + From<WebSocketServerThrottleTimeoutAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
}

impl From<WebSocketGraphQlServerThrottleTimeoutTaskFactory> for WebSocketGraphQlServerTaskFactory {
    fn from(value: WebSocketGraphQlServerThrottleTimeoutTaskFactory) -> Self {
        Self::ThrottleTimeout(value)
    }
}
