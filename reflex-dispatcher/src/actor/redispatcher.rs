// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
    Worker,
};

#[derive(Clone)]
pub struct Redispatcher {
    target_pids: Vec<ProcessId>,
}
impl Redispatcher {
    pub fn new(target_pids: impl IntoIterator<Item = ProcessId>) -> Self {
        Self {
            target_pids: target_pids.into_iter().collect(),
        }
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct RedispatcherState;

impl<TAction: Action, TTask: TaskFactory<TAction, TTask>> Actor<TAction, TTask> for Redispatcher {
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

impl<TAction: Action, TTask: TaskFactory<TAction, TTask>>
    Worker<TAction, SchedulerTransition<TAction, TTask>> for Redispatcher
{
    fn accept(&self, _message: &TAction) -> bool {
        true
    }
    fn schedule(&self, _message: &TAction, _state: &Self::State) -> Option<SchedulerMode> {
        Some(SchedulerMode::Sync)
    }
}

impl<TAction: Action, TTask: TaskFactory<TAction, TTask>>
    Handler<TAction, SchedulerTransition<TAction, TTask>> for Redispatcher
{
    type State = RedispatcherState;
    fn handle(
        &self,
        _state: &mut Self::State,
        _message: &TAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        Some(SchedulerTransition::new(
            self.target_pids
                .iter()
                .cloned()
                .map(SchedulerCommand::Forward),
        ))
    }
}
