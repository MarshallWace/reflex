// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Duration;

use futures::{FutureExt, Stream};
use reflex::core::Uuid;
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};

use crate::action::timeout::TimeoutHandlerTimeoutAction;

pub trait TimeoutHandlerTask: From<TimeoutHandlerTaskFactory> {}
impl<_Self> TimeoutHandlerTask for _Self where Self: From<TimeoutHandlerTaskFactory> {}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct TimeoutHandlerTaskFactory {
    pub operation_id: Uuid,
    pub duration: Duration,
    pub caller_pid: ProcessId,
}
impl<TAction, TTask> TaskFactory<TAction, TTask> for TimeoutHandlerTaskFactory
where
    TAction: Action + TimeoutHandlerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = TimeoutHandlerTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            operation_id,
            duration,
            caller_pid,
        } = self;
        TimeoutHandlerTaskActor {
            operation_id,
            duration,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct TimeoutHandlerTaskActor {
    operation_id: Uuid,
    duration: Duration,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct TimeoutHandlerTaskActorState;

dispatcher!({
    pub enum TimeoutHandlerTaskAction {
        Inbox(TimeoutHandlerTimeoutAction),

        Outbox(TimeoutHandlerTimeoutAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for TimeoutHandlerTaskActor
    where
        TAction: Action + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = TimeoutHandlerTaskActorState;
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

        fn accept(&self, _action: &TimeoutHandlerTimeoutAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &TimeoutHandlerTimeoutAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &TimeoutHandlerTimeoutAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_timeout_handler_timeout(state, action, metadata, context)
        }
    }
});

impl TimeoutHandlerTaskActor {
    fn events<TInbox, TAction>(&self, _inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action + From<TimeoutHandlerTimeoutAction>,
    {
        let duration = self.duration;
        let operation_id = self.operation_id;
        tokio::time::sleep(duration)
            .map(move |_| TAction::from(TimeoutHandlerTimeoutAction { operation_id }))
            .map(|action| TInbox::Message::from(action))
            .into_stream()
    }
    fn handle_timeout_handler_timeout<TAction, TTask>(
        &self,
        _state: &mut TimeoutHandlerTaskActorState,
        _action: &TimeoutHandlerTimeoutAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TimeoutHandlerTimeoutAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(context.pid()),
            SchedulerCommand::Forward(self.caller_pid),
        ]))
    }
}
