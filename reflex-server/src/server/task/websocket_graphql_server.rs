// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{iter::once, time::Duration};

use futures::{FutureExt, Stream};
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, task_factory_enum, Named};
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

task_factory_enum!({
    #[derive(Clone, Serialize, Deserialize)]
    pub enum WebSocketGraphQlServerTaskFactory {
        ThrottleTimeout(WebSocketGraphQlServerThrottleTimeoutTaskFactory),
    }
    impl<TAction, TTask> TaskFactory<TAction, TTask> for WebSocketGraphQlServerTaskFactory
    where
        TAction: Action + WebSocketGraphQlServerTaskAction + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
    }
});

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

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Async(Box::pin(self.events(inbox)), None)
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
    fn events<TInbox, TAction>(&self, _inbox: TInbox) -> impl Stream<Item = TInbox::Message>
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
