// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};

use crate::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, NamedAction,
    OutboundAction, SerializableAction, SerializedAction, StateOperation, StateTransition,
};

pub trait SessionPlaybackAction:
    Action + InboundAction<SessionPlaybackBeginAction> + OutboundAction<SessionPlaybackEndAction>
{
}
impl<TAction> SessionPlaybackAction for TAction where
    Self: Action
        + InboundAction<SessionPlaybackBeginAction>
        + OutboundAction<SessionPlaybackEndAction>
{
}

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum SessionPlaybackActions {
    Begin(SessionPlaybackBeginAction),
    End(SessionPlaybackEndAction),
}
impl Action for SessionPlaybackActions {}
impl NamedAction for SessionPlaybackActions {
    fn name(&self) -> &'static str {
        match self {
            Self::Begin(action) => action.name(),
            Self::End(action) => action.name(),
        }
    }
}
impl SerializableAction for SessionPlaybackActions {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Begin(action) => action.to_json(),
            Self::End(action) => action.to_json(),
        }
    }
}

impl From<SessionPlaybackBeginAction> for SessionPlaybackActions {
    fn from(value: SessionPlaybackBeginAction) -> Self {
        Self::Begin(value)
    }
}
impl From<SessionPlaybackActions> for Option<SessionPlaybackBeginAction> {
    fn from(value: SessionPlaybackActions) -> Self {
        match value {
            SessionPlaybackActions::Begin(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a SessionPlaybackActions> for Option<&'a SessionPlaybackBeginAction> {
    fn from(value: &'a SessionPlaybackActions) -> Self {
        match value {
            SessionPlaybackActions::Begin(value) => Some(value),
            _ => None,
        }
    }
}

impl From<SessionPlaybackEndAction> for SessionPlaybackActions {
    fn from(value: SessionPlaybackEndAction) -> Self {
        Self::End(value)
    }
}
impl From<SessionPlaybackActions> for Option<SessionPlaybackEndAction> {
    fn from(value: SessionPlaybackActions) -> Self {
        match value {
            SessionPlaybackActions::End(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a> From<&'a SessionPlaybackActions> for Option<&'a SessionPlaybackEndAction> {
    fn from(value: &'a SessionPlaybackActions) -> Self {
        match value {
            SessionPlaybackActions::End(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct SessionPlaybackBeginAction {
    pub num_frames: usize,
}
impl Action for SessionPlaybackBeginAction {}
impl NamedAction for SessionPlaybackBeginAction {
    fn name(&self) -> &'static str {
        "SessionPlaybackBeginAction"
    }
}
impl SerializableAction for SessionPlaybackBeginAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([("num_frames", JsonValue::Number(self.num_frames.into()))])
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct SessionPlaybackEndAction;
impl Action for SessionPlaybackEndAction {}
impl NamedAction for SessionPlaybackEndAction {
    fn name(&self) -> &'static str {
        "SessionPlaybackEndAction"
    }
}
impl SerializableAction for SessionPlaybackEndAction {
    fn to_json(&self) -> SerializedAction {
        SerializedAction::from_iter([])
    }
}

pub struct SessionPlayback<TAction: Action> {
    frames: Vec<StateOperation<TAction>>,
}
impl<TAction> SessionPlayback<TAction>
where
    TAction: Action,
{
    pub fn new(operations: impl IntoIterator<Item = StateOperation<TAction>>) -> Self {
        Self::from_iter(operations)
    }
    pub fn frames(&self) -> &[StateOperation<TAction>] {
        &self.frames
    }
}
impl<TAction> SessionPlayback<TAction>
where
    TAction: Action + SerializableAction,
{
    pub fn to_json(&self) -> JsonValue {
        JsonValue::Array(
            self.frames()
                .iter()
                .map(|action| match action {
                    StateOperation::Send(pid, action) => {
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
                    StateOperation::Task(pid, _) => json!({
                        "type": "task",
                        "pid": pid,
                    }),
                    StateOperation::Spawn(pid, _) => json!({
                        "type": "spawn",
                        "pid": pid,
                    }),
                    StateOperation::Kill(pid) => json!({
                        "type": "kill",
                        "pid": pid,
                    }),
                })
                .collect::<Vec<_>>(),
        )
    }
}
impl<TAction> FromIterator<StateOperation<TAction>> for SessionPlayback<TAction>
where
    TAction: Action,
{
    fn from_iter<T: IntoIterator<Item = StateOperation<TAction>>>(iter: T) -> Self {
        Self {
            frames: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct SessionPlaybackState {
    pub program_counter: usize,
}

impl<TRecordedAction, TAction> Actor<TAction> for SessionPlayback<TRecordedAction>
where
    TRecordedAction: Action,
    TAction: SessionPlaybackAction,
    for<'a> &'a StateOperation<TRecordedAction>: Into<StateOperation<TAction>>,
{
    type State = SessionPlaybackState;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_session_playback_begin(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<TRecordedAction> SessionPlayback<TRecordedAction>
where
    TRecordedAction: Action,
{
    fn handle_session_playback_begin<TAction>(
        &self,
        state: &mut SessionPlaybackState,
        action: &SessionPlaybackBeginAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<SessionPlaybackEndAction>,
        for<'a> &'a StateOperation<TRecordedAction>: Into<StateOperation<TAction>>,
    {
        let SessionPlaybackBeginAction { num_frames } = action;
        let start_index = state.program_counter;
        let until_index = (start_index + num_frames).min(self.frames.len());
        let replayed_operations = self
            .frames
            .get(start_index..until_index)
            .into_iter()
            .flatten()
            .map(|frame| Into::<StateOperation<TAction>>::into(frame))
            .collect::<Vec<_>>();
        state.program_counter = until_index;
        Some(StateTransition::new(replayed_operations.into_iter().chain(
            once(StateOperation::Send(
                context.pid(),
                SessionPlaybackEndAction.into(),
            )),
        )))
    }
}
