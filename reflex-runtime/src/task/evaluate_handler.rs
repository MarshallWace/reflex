// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Duration;

use futures::{FutureExt, Stream};
use reflex_dispatcher::{
    Action, ActorEvents, BoxedActionStream, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};

use crate::action::effect::EffectThrottleEmitAction;

pub trait EvaluateHandlerTask: From<EffectThrottleTaskFactory> {}
impl<_Self> EvaluateHandlerTask for _Self where Self: From<EffectThrottleTaskFactory> {}

#[derive(Named, Clone, Serialize, Deserialize)]
pub struct EffectThrottleTaskFactory {
    pub timeout: Duration,
    pub caller_pid: ProcessId,
}
impl<TAction, TTask> TaskFactory<TAction, TTask> for EffectThrottleTaskFactory
where
    TAction: Action + EvaluateHandlerTaskAction + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    type Actor = EffectThrottleEmitTaskActor;
    fn create(self) -> Self::Actor {
        let Self {
            timeout,
            caller_pid,
        } = self;
        EffectThrottleEmitTaskActor {
            timeout,
            caller_pid,
        }
    }
}

#[derive(Named, Clone)]
pub struct EffectThrottleEmitTaskActor {
    timeout: Duration,
    caller_pid: ProcessId,
}

#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct EffectThrottleEmitTaskActorState;

dispatcher!({
    pub enum EvaluateHandlerTaskAction {
        Inbox(EffectThrottleEmitAction),

        Outbox(EffectThrottleEmitAction),
    }

    impl<TAction, TTask> Dispatcher<TAction, TTask> for EffectThrottleEmitTaskActor
    where
        TAction: Action + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = EffectThrottleEmitTaskActorState;
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

        fn accept(&self, _action: &EffectThrottleEmitAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectThrottleEmitAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectThrottleEmitAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_throttle_emit(state, action, metadata, context)
        }
    }
});

impl EffectThrottleEmitTaskActor {
    fn events<TInbox, TAction>(&self, inbox: TInbox) -> impl Stream<Item = TInbox::Message>
    where
        TInbox: TaskInbox<TAction>,
        TAction: Action + From<EffectThrottleEmitAction>,
    {
        let duration = self.timeout;
        inbox
            .sleep(duration)
            .map(move |_| TAction::from(EffectThrottleEmitAction))
            .map(|action| TInbox::Message::from(action))
            .into_stream()
    }
    fn handle_effect_throttle_emit<TAction, TTask>(
        &self,
        _state: &mut EffectThrottleEmitTaskActorState,
        _action: &EffectThrottleEmitAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectThrottleEmitAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(context.pid()),
            SchedulerCommand::Forward(self.caller_pid),
        ]))
    }
}
