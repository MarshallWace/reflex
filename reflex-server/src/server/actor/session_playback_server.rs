// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use bytes::Bytes;
use http::{Method, Request, StatusCode};
use reflex::core::{
    ConditionListType, ConditionType, Expression, ExpressionFactory, RefType, SignalTermType,
    SignalType,
};
use reflex_dispatcher::{
    session_playback::{SessionPlayback, SessionPlaybackState},
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    ProcessId, SerializableAction, SessionPlaybackAction, SessionPlaybackBeginAction,
    SessionPlaybackEndAction, StateOperation, StateTransition,
};
use reflex_json::{json, JsonValue};
use reflex_runtime::action::evaluate::EvaluateResultAction;
use uuid::Uuid;

use crate::server::{
    action::session_playback_server::{
        SessionPlaybackServerHttpRequestAction, SessionPlaybackServerHttpResponseAction,
    },
    actor::query_inspector_server::{
        QueryInspectorServer, QueryInspectorServerAction, QueryInspectorServerState,
    },
    utils::{create_accepted_http_response, create_json_http_response},
};

#[derive(Clone, Copy, Debug)]
enum SessionPlaybackCommand {
    Step,
    Continue,
    End,
    Reset,
}
impl std::fmt::Display for SessionPlaybackCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Step => write!(f, "step"),
            Self::Continue => write!(f, "continue"),
            Self::End => write!(f, "end"),
            Self::Reset => write!(f, "reset"),
        }
    }
}

pub trait SessionPlaybackServerAction<T: Expression>:
    Action
    + SerializableAction
    + SessionPlaybackAction
    + QueryInspectorServerAction<T>
    + InboundAction<SessionPlaybackBeginAction>
    + InboundAction<SessionPlaybackEndAction>
    + InboundAction<SessionPlaybackServerHttpRequestAction>
    + InboundAction<SessionPlaybackServerHttpResponseAction>
    + OutboundAction<SessionPlaybackBeginAction>
    + OutboundAction<SessionPlaybackServerHttpRequestAction>
    + OutboundAction<SessionPlaybackServerHttpResponseAction>
{
}
impl<T: Expression, TAction> SessionPlaybackServerAction<T> for TAction where
    Self: Action
        + SerializableAction
        + SessionPlaybackAction
        + QueryInspectorServerAction<T>
        + InboundAction<SessionPlaybackBeginAction>
        + InboundAction<SessionPlaybackEndAction>
        + InboundAction<SessionPlaybackServerHttpRequestAction>
        + InboundAction<SessionPlaybackServerHttpResponseAction>
        + OutboundAction<SessionPlaybackBeginAction>
        + OutboundAction<SessionPlaybackServerHttpRequestAction>
        + OutboundAction<SessionPlaybackServerHttpResponseAction>
{
}

pub struct SessionPlaybackServer<
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + InboundAction<EvaluateResultAction<T>>,
> {
    session_playback: SessionPlayback<TRecordedAction>,
    query_inspector_server: QueryInspectorServer<T, TFactory>,
    factory: TFactory,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TRecordedAction> SessionPlaybackServer<T, TFactory, TRecordedAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TRecordedAction: Action + SerializableAction + InboundAction<EvaluateResultAction<T>>,
{
    pub fn new(
        captured_session: impl IntoIterator<Item = StateOperation<TRecordedAction>>,
        factory: TFactory,
    ) -> Self {
        Self {
            factory: factory.clone(),
            session_playback: SessionPlayback::new(captured_session),
            query_inspector_server: QueryInspectorServer::new(factory),
            _expression: Default::default(),
        }
    }
}

pub struct SessionPlaybackServerState<T: Expression> {
    session_playback: Option<SessionPlaybackState>,
    query_inspector: Option<QueryInspectorServerState<T>>,
    active_request: Option<Uuid>,
}
impl<T: Expression> Default for SessionPlaybackServerState<T> {
    fn default() -> Self {
        Self {
            session_playback: Some(Default::default()),
            query_inspector: Some(Default::default()),
            active_request: Default::default(),
        }
    }
}
impl<T: Expression> SessionPlaybackServerState<T> {
    fn as_inner(&self) -> Option<&SessionPlaybackState> {
        self.session_playback.as_ref()
    }
    fn take_inner(&mut self) -> Option<SessionPlaybackState> {
        self.session_playback.take()
    }
    fn replace_inner(&mut self, value: SessionPlaybackState) -> Option<SessionPlaybackState> {
        self.session_playback.replace(value)
    }
    fn take_query_inspector(&mut self) -> Option<QueryInspectorServerState<T>> {
        self.query_inspector.take()
    }
    fn replace_query_inspector(
        &mut self,
        value: QueryInspectorServerState<T>,
    ) -> Option<QueryInspectorServerState<T>> {
        self.query_inspector.replace(value)
    }
}

impl<T, TFactory, TRecordedAction, TAction> Actor<TAction>
    for SessionPlaybackServer<T, TFactory, TRecordedAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction:
        Action + SerializableAction + InboundAction<EvaluateResultAction<T>> + SerializableAction,
    TAction: SessionPlaybackServerAction<T>,
    for<'a> &'a StateOperation<TRecordedAction>: Into<StateOperation<TAction>>,
{
    type State = SessionPlaybackServerState<T>;
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
            self.handle_session_playback_server_http_request(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_session_playback_server_http_response(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_session_playback_end(&mut state, action, metadata, context)
        } else {
            let session_playback_actions = if let Some(inner_state) = state.take_inner() {
                let (inner_state, actions) = self
                    .session_playback
                    .handle(inner_state, action, metadata, context)
                    .into_parts();
                state.replace_inner(inner_state);
                Some(actions)
            } else {
                None
            };
            let query_inspector_actions = if let Some(inner_state) = state.take_query_inspector() {
                let (inner_state, actions) = self
                    .query_inspector_server
                    .handle(inner_state, action, metadata, context)
                    .into_parts();
                state.replace_query_inspector(inner_state);
                Some(actions)
            } else {
                None
            };
            match (session_playback_actions, query_inspector_actions) {
                (Some(session_playback_actions), Some(query_inspector_actions)) => {
                    Some(session_playback_actions.append(query_inspector_actions))
                }
                (Some(session_playback_actions), None) => Some(session_playback_actions),
                (None, Some(query_inspector_actions)) => Some(query_inspector_actions),
                (None, None) => None,
            }
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TRecordedAction> SessionPlaybackServer<T, TFactory, TRecordedAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + InboundAction<EvaluateResultAction<T>>,
{
    fn handle_session_playback_end<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        _action: &SessionPlaybackEndAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let request_id = state.active_request.take()?;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            SessionPlaybackServerHttpResponseAction {
                request_id,
                response: create_json_http_response(
                    StatusCode::OK,
                    empty(),
                    &json!({
                        "currentFrame": state.as_inner().map(|state| state.program_counter).unwrap_or(0),
                    }),
                ),
            }
            .into(),
        ))))
    }
    fn handle_session_playback_server_http_request<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<SessionPlaybackBeginAction>
            + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id: _,
            request,
        } = action;
        match request.uri().path() {
            "/" => {
                self.handle_session_playback_server_http_root_path(state, action, metadata, context)
            }
            _ => {
                self.handle_session_playback_server_path_not_found(state, action, metadata, context)
            }
        }
    }
    fn handle_session_playback_server_http_response<TAction>(
        &self,
        _state: &mut SessionPlaybackServerState<T>,
        _action: &SessionPlaybackServerHttpResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        None
    }
    fn handle_session_playback_server_http_root_path<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<SessionPlaybackBeginAction>
            + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request,
        } = action;
        match request.method() {
            &Method::GET => {
                self.handle_session_playback_server_data(state, action, metadata, context)
            }
            &Method::POST => match parse_debugger_command(request) {
                Some(SessionPlaybackCommand::Step) => self
                    .handle_session_playback_server_step_command(state, action, metadata, context),
                Some(SessionPlaybackCommand::Continue) => self
                    .handle_session_playback_server_continue_command(
                        state, action, metadata, context,
                    ),
                Some(SessionPlaybackCommand::End) => self
                    .handle_session_playback_server_end_command(state, action, metadata, context),
                Some(SessionPlaybackCommand::Reset) => self
                    .handle_session_playback_server_reset_command(state, action, metadata, context),
                None => Some(StateTransition::new(once(StateOperation::Send(
                    context.pid(),
                    SessionPlaybackServerHttpResponseAction {
                        request_id: *request_id,
                        response: create_accepted_http_response(
                            StatusCode::BAD_REQUEST,
                            empty(),
                            None,
                            request.headers(),
                        ),
                    }
                    .into(),
                )))),
            },
            _ => {
                self.handle_session_playback_server_path_not_found(state, action, metadata, context)
            }
        }
    }
    fn handle_session_playback_server_data<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            SessionPlaybackServerHttpResponseAction {
                request_id: *request_id,
                response: create_json_http_response(
                    StatusCode::OK,
                    empty(),
                    &json!({
                        "currentFrame": state.session_playback.as_ref().map(|session_playback| session_playback.program_counter),
                        "events": self.session_playback.to_json()
                    }),
                ),
            }
            .into(),
        ))))
    }
    fn handle_session_playback_server_step_command<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<SessionPlaybackBeginAction>
            + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state
            .as_inner()
            .map(|state| state.program_counter)
            .unwrap_or(0);
        let is_already_final_frame = previous_index == frames.len();
        if is_already_final_frame {
            *state = SessionPlaybackServerState::default();
            Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                SessionPlaybackServerHttpResponseAction {
                    request_id: *request_id,
                    response: create_json_http_response(
                        StatusCode::OK,
                        empty(),
                        &json!({
                            "currentFrame": 0,
                        }),
                    ),
                }
                .into(),
            ))))
        } else {
            let num_frames = 1;
            Some(StateTransition::new(create_debugger_advance_action(
                num_frames,
                *request_id,
                state,
                context,
            )))
        }
    }
    fn handle_session_playback_server_continue_command<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<SessionPlaybackBeginAction>
            + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state
            .as_inner()
            .map(|state| state.program_counter)
            .unwrap_or(0);
        let next_index = frames
            .get(previous_index + 1..)
            .into_iter()
            .flatten()
            .enumerate()
            .find_map(|(index, action)| match action {
                StateOperation::Send(pid, action)
                    if *pid == ProcessId::default() && is_error_action(action, &self.factory) =>
                {
                    Some(index + 1)
                }
                _ => None,
            })
            .map(|index| previous_index + index)
            .unwrap_or(frames.len());
        let num_frames = next_index - previous_index;
        if num_frames == 0 {
            *state = SessionPlaybackServerState::default();
            return Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                SessionPlaybackServerHttpResponseAction {
                    request_id: *request_id,
                    response: create_json_http_response(
                        StatusCode::OK,
                        empty(),
                        &json!({
                            "currentFrame": 0,
                        }),
                    ),
                }
                .into(),
            ))));
        }
        Some(StateTransition::new(create_debugger_advance_action(
            num_frames,
            *request_id,
            state,
            context,
        )))
    }
    fn handle_session_playback_server_end_command<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action
            + OutboundAction<SessionPlaybackBeginAction>
            + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state
            .as_inner()
            .map(|state| state.program_counter)
            .unwrap_or(0);
        let num_frames = frames.len() - previous_index;
        if num_frames == 0 {
            return Some(StateTransition::new(once(StateOperation::Send(
                context.pid(),
                SessionPlaybackServerHttpResponseAction {
                    request_id: *request_id,
                    response: create_json_http_response(
                        StatusCode::OK,
                        empty(),
                        &json!({
                            "currentFrame": previous_index,
                        }),
                    ),
                }
                .into(),
            ))));
        }
        Some(StateTransition::new(create_debugger_advance_action(
            num_frames,
            *request_id,
            state,
            context,
        )))
    }
    fn handle_session_playback_server_reset_command<TAction>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        *state = SessionPlaybackServerState::default();
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            SessionPlaybackServerHttpResponseAction {
                request_id: *request_id,
                response: create_json_http_response(
                    StatusCode::OK,
                    empty(),
                    &json!({
                        "currentFrame": 0,
                    }),
                ),
            }
            .into(),
        ))))
    }
    fn handle_session_playback_server_path_not_found<TAction>(
        &self,
        _state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<SessionPlaybackServerHttpResponseAction>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request,
        } = action;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            SessionPlaybackServerHttpResponseAction {
                request_id: *request_id,
                response: create_accepted_http_response(
                    StatusCode::NOT_FOUND,
                    empty(),
                    None,
                    request.headers(),
                ),
            }
            .into(),
        ))))
    }
}

fn create_debugger_advance_action<T: Expression, TAction>(
    num_frames: usize,
    request_id: Uuid,
    state: &mut SessionPlaybackServerState<T>,
    context: &impl HandlerContext,
) -> impl IntoIterator<Item = StateOperation<TAction>>
where
    TAction: Action + OutboundAction<SessionPlaybackBeginAction>,
{
    state.active_request.replace(request_id);
    let pid = context.pid();
    once(StateOperation::Send(
        pid,
        SessionPlaybackBeginAction { num_frames }.into(),
    ))
}

fn parse_debugger_command(request: &Request<Bytes>) -> Option<SessionPlaybackCommand> {
    serde_json::from_slice::<JsonValue>(request.body())
        .ok()
        .and_then(|body| match body {
            JsonValue::Object(fields) => match fields.get("action").and_then(|value| match value {
                JsonValue::String(value) => Some(value.as_str()),
                _ => None,
            }) {
                Some("continue") => Some(SessionPlaybackCommand::Continue),
                Some("step") => Some(SessionPlaybackCommand::Step),
                Some("end") => Some(SessionPlaybackCommand::End),
                Some("reset") => Some(SessionPlaybackCommand::Reset),
                _ => None,
            },
            _ => None,
        })
}

fn is_error_action<T, TFactory, TAction>(action: &TAction, factory: &TFactory) -> bool
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + InboundAction<EvaluateResultAction<T>>,
{
    if let Some(EvaluateResultAction { result, .. }) = action.match_type() {
        is_error_expression(result.result(), factory)
    } else {
        false
    }
}

fn is_error_expression<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> bool {
    factory
        .match_signal_term(value)
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .any(|effect| matches!(effect.signal_type(), SignalType::Error))
        })
        .unwrap_or(false)
}
