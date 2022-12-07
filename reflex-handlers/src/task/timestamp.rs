// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    iter::once,
    time::{Duration, Instant, SystemTime},
};

use futures::{Stream, StreamExt};
use reflex::core::Uuid;
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};

use crate::action::timestamp::TimestampHandlerUpdateAction;

pub trait TimestampHandlerTask: From<TimestampHandlerTaskFactory> {}
impl<_Self> TimestampHandlerTask for _Self where Self: From<TimestampHandlerTaskFactory> {}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct TimestampHandlerTaskFactory {
    pub operation_id: Uuid,
    pub interval: Duration,
    pub caller_pid: ProcessId,
}
impl<TAction, TTask> TaskFactory<TAction, TTask> for TimestampHandlerTaskFactory
where
    TAction: Action + TimestampHandlerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = TimestampHandlerTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            operation_id,
            interval,
            caller_pid,
        } = self;
        TimestampHandlerTaskActor {
            operation_id,
            interval,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct TimestampHandlerTaskActor {
    operation_id: Uuid,
    interval: Duration,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct TimestampHandlerTaskActorState;

dispatcher!({
    pub enum TimestampHandlerTaskAction {
        Inbox(TimestampHandlerUpdateAction),

        Outbox(TimestampHandlerUpdateAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for TimestampHandlerTaskActor
    where
        TAction: Action + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = TimestampHandlerTaskActorState;
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

        fn accept(&self, _action: &TimestampHandlerUpdateAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &TimestampHandlerUpdateAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &TimestampHandlerUpdateAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_update_action(state, action, metadata, context)
        }
    }
});

impl TimestampHandlerTaskActor {
    fn events<TInbox, TAction>(&self, inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action + From<TimestampHandlerUpdateAction>,
    {
        let interval = self.interval;
        let operation_id = self.operation_id;
        let now = Instant::now();
        let first_update = now.checked_add(interval).unwrap_or(now);
        inbox
            .interval(first_update, interval)
            .map(move |_| {
                TAction::from(TimestampHandlerUpdateAction {
                    operation_id,
                    timestamp: SystemTime::now(),
                })
            })
            .map(|action| TInbox::Message::from(action))
    }
    fn handle_update_action<TAction, TTask>(
        &self,
        _state: &mut TimestampHandlerTaskActorState,
        _action: &TimestampHandlerUpdateAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TimestampHandlerUpdateAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new(once(SchedulerCommand::Forward(
            self.caller_pid,
        ))))
    }
}
