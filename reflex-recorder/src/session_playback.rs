// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use reflex_macros::{dispatcher, Named};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};

use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, SerializableAction, SerializedAction,
    TaskFactory, TaskInbox,
};

#[derive(Named, PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct SessionPlaybackBeginAction {
    pub num_frames: usize,
}
impl Action for SessionPlaybackBeginAction {}
impl SerializableAction for SessionPlaybackBeginAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([("num_frames", JsonValue::Number(self.num_frames.into()))])
    }
}

#[derive(Named, PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct SessionPlaybackEndAction;
impl Action for SessionPlaybackEndAction {}
impl SerializableAction for SessionPlaybackEndAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([])
    }
}

#[derive(Named, Clone)]
pub struct SessionPlayback<TRecordedAction, TRecordedTask>
where
    TRecordedAction: Action,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    frames: Vec<SchedulerCommand<TRecordedAction, TRecordedTask>>,
    main_pid: ProcessId,
}
impl<TRecordedAction, TRecordedTask> SessionPlayback<TRecordedAction, TRecordedTask>
where
    TRecordedAction: Action,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    pub fn new(
        operations: impl IntoIterator<Item = SchedulerCommand<TRecordedAction, TRecordedTask>>,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            frames: operations.into_iter().collect(),
            main_pid,
        }
    }
    pub fn frames(&self) -> &[SchedulerCommand<TRecordedAction, TRecordedTask>] {
        &self.frames
    }
}
impl<TRecordedAction, TRecordedTask> SessionPlayback<TRecordedAction, TRecordedTask>
where
    TRecordedAction: Action + SerializableAction,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    pub fn to_json(&self) -> JsonValue {
        JsonValue::Array(
            self.frames()
                .iter()
                .map(|action| match action {
                    SchedulerCommand::Forward(pid) => {
                        json!({
                            "type": "forward",
                            "pid": pid,
                        })
                    }
                    SchedulerCommand::Send(pid, action) => {
                        let action_type = action.name();
                        let args = action.to_json().into_iter().collect::<Vec<_>>();
                        json!({
                            "type": "send",
                            "pid": pid,
                            "action": {
                                "type": action_type,
                                "args": args,
                            },
                        })
                    }
                    SchedulerCommand::Task(pid, _) => json!({
                        "type": "task",
                        "pid": pid,
                    }),
                    SchedulerCommand::Kill(pid) => json!({
                        "type": "kill",
                        "pid": pid,
                    }),
                })
                .collect::<Vec<_>>(),
        )
    }
}

#[derive(Clone, Copy, Default)]
pub struct SessionPlaybackState {
    pub program_counter: usize,
}

dispatcher!({
    pub enum SessionPlaybackAction {
        Inbox(SessionPlaybackBeginAction),

        Outbox(SessionPlaybackEndAction),
    }

    impl<TRecordedAction, TRecordedTask, TAction, TTask> Dispatcher<TAction, TTask>
        for SessionPlayback<TRecordedAction, TRecordedTask>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
        TRecordedAction: Action + Clone,
        TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
        TAction: Action + From<TRecordedAction>,
        TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
    {
        type State = SessionPlaybackState;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Sync(inbox)
        }

        fn accept(&self, _action: &SessionPlaybackBeginAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &SessionPlaybackBeginAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &SessionPlaybackBeginAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_session_playback_begin(state, action, metadata, context)
        }
    }
});

impl<TRecordedAction, TRecordedTask> SessionPlayback<TRecordedAction, TRecordedTask>
where
    TRecordedAction: Action + Clone,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
{
    fn handle_session_playback_begin<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackState,
        action: &SessionPlaybackBeginAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TRecordedAction> + From<SessionPlaybackEndAction>,
        TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
    {
        let SessionPlaybackBeginAction { num_frames } = action;
        let start_index = state.program_counter;
        let until_index = (start_index + num_frames).min(self.frames.len());
        let replayed_operations = self
            .frames
            .get(start_index..until_index)
            .into_iter()
            .flatten()
            .map(|frame| match frame {
                SchedulerCommand::Forward(pid) => SchedulerCommand::Forward(*pid),
                SchedulerCommand::Send(pid, action) => {
                    SchedulerCommand::Send(*pid, TAction::from(action.clone()))
                }
                SchedulerCommand::Task(pid, factory) => {
                    SchedulerCommand::Task(*pid, TTask::from(factory.clone()))
                }
                SchedulerCommand::Kill(pid) => SchedulerCommand::Kill(*pid),
            })
            .collect::<Vec<_>>();
        state.program_counter = until_index;
        Some(SchedulerTransition::new(
            replayed_operations
                .into_iter()
                .chain(once(SchedulerCommand::Send(
                    self.main_pid,
                    SessionPlaybackEndAction.into(),
                ))),
        ))
    }
}
impl<TRecordedAction, TRecordedTask, TAction, TTask> TaskFactory<TAction, TTask>
    for SessionPlayback<TRecordedAction, TRecordedTask>
where
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
    TRecordedAction: Action + Clone,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
    TAction: Action + SessionPlaybackAction + From<TRecordedAction>,
    TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
{
    type Actor = Self;
    fn create(self) -> Self::Actor {
        self
    }
}
