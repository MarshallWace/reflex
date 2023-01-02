// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
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
    Action, Actor, ActorEvents, Handler, HandlerContext, Matcher, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, SerializableAction,
    TaskFactory, TaskInbox, Worker,
};
use reflex_json::{json, JsonValue};
use reflex_macros::{blanket_trait, Named};
use reflex_recorder::session_playback::{
    SessionPlayback, SessionPlaybackAction, SessionPlaybackBeginAction, SessionPlaybackEndAction,
    SessionPlaybackState,
};
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

blanket_trait!(
    pub trait SessionPlaybackServerAction<T: Expression>:
        SerializableAction
        + SessionPlaybackAction
        + QueryInspectorServerAction<T>
        + Matcher<SessionPlaybackBeginAction>
        + Matcher<SessionPlaybackEndAction>
        + Matcher<SessionPlaybackServerHttpRequestAction>
        + Matcher<SessionPlaybackServerHttpResponseAction>
        + From<SessionPlaybackBeginAction>
        + From<SessionPlaybackServerHttpRequestAction>
        + From<SessionPlaybackServerHttpResponseAction>
    {
    }
);

#[derive(Named, Clone)]
pub struct SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    session_playback: SessionPlayback<TRecordedAction, TRecordedTask>,
    query_inspector_server: QueryInspectorServer<T, TFactory>,
    factory: TFactory,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TRecordedAction, TRecordedTask>
    SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TRecordedAction: Action + SerializableAction + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    pub fn new(
        captured_session: impl IntoIterator<Item = SchedulerCommand<TRecordedAction, TRecordedTask>>,
        factory: TFactory,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory: factory.clone(),
            session_playback: SessionPlayback::new(captured_session, main_pid),
            query_inspector_server: QueryInspectorServer::new(factory, main_pid),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct SessionPlaybackServerState<T: Expression> {
    session_playback: SessionPlaybackState,
    query_inspector: QueryInspectorServerState<T>,
    active_request: Option<Uuid>,
}
impl<T: Expression> Default for SessionPlaybackServerState<T> {
    fn default() -> Self {
        Self {
            session_playback: Default::default(),
            query_inspector: Default::default(),
            active_request: Default::default(),
        }
    }
}

impl<T, TFactory, TRecordedAction, TRecordedTask, TAction, TTask> Actor<TAction, TTask>
    for SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Clone + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
    TAction: Action + SessionPlaybackServerAction<T> + From<TRecordedAction>,
    TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
{
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
}
impl<T, TFactory, TRecordedAction, TRecordedTask, TAction, TTask> TaskFactory<TAction, TTask>
    for SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Clone + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
    TAction: Action + SessionPlaybackServerAction<T> + From<TRecordedAction>,
    TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
{
    type Actor = Self;
    fn create(self) -> Self::Actor {
        self
    }
}

impl<T, TFactory, TRecordedAction, TRecordedTask, TAction, TTask>
    Worker<TAction, SchedulerTransition<TAction, TTask>>
    for SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Clone + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
    TAction: Action + SessionPlaybackServerAction<T> + From<TRecordedAction>,
    TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
{
    fn accept(&self, _message: &TAction) -> bool {
        true
    }
    fn schedule(&self, _message: &TAction, _state: &Self::State) -> Option<SchedulerMode> {
        Some(SchedulerMode::Async)
    }
}

impl<T, TFactory, TRecordedAction, TRecordedTask, TAction, TTask>
    Handler<TAction, SchedulerTransition<TAction, TTask>>
    for SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Clone + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask> + Clone,
    TAction: Action + SessionPlaybackServerAction<T> + From<TRecordedAction>,
    TTask: TaskFactory<TAction, TTask> + From<TRecordedTask>,
{
    type State = SessionPlaybackServerState<T>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        let mut state = state;
        if let Some(action) = action.match_type() {
            self.handle_session_playback_server_http_request(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_session_playback_server_http_response(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_session_playback_end(&mut state, action, metadata, context)
        } else {
            let session_playback_actions = self.session_playback.handle(
                &mut state.session_playback,
                action,
                metadata,
                context,
            );
            let query_inspector_actions = self.query_inspector_server.handle(
                &mut state.query_inspector,
                action,
                metadata,
                context,
            );
            match (session_playback_actions, query_inspector_actions) {
                (Some(session_playback_actions), Some(query_inspector_actions)) => {
                    Some(session_playback_actions.append(query_inspector_actions))
                }
                (Some(session_playback_actions), None) => Some(session_playback_actions),
                (None, Some(query_inspector_actions)) => Some(query_inspector_actions),
                (None, None) => None,
            }
        }
    }
}
impl<T, TFactory, TRecordedAction, TRecordedTask>
    SessionPlaybackServer<T, TFactory, TRecordedAction, TRecordedTask>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TRecordedAction: Action + SerializableAction + Matcher<EvaluateResultAction<T>>,
    TRecordedTask: TaskFactory<TRecordedAction, TRecordedTask>,
{
    fn handle_session_playback_end<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        _action: &SessionPlaybackEndAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let request_id = state.active_request.take()?;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            SessionPlaybackServerHttpResponseAction {
                request_id,
                response: create_json_http_response(
                    StatusCode::OK,
                    empty(),
                    &json!({
                       "currentFrame": state.session_playback.program_counter,
                    }),
                ),
            }
            .into(),
        ))))
    }
    fn handle_session_playback_server_http_request<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<SessionPlaybackBeginAction>
            + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
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
    fn handle_session_playback_server_http_response<TAction, TTask>(
        &self,
        _state: &mut SessionPlaybackServerState<T>,
        _action: &SessionPlaybackServerHttpResponseAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        None
    }
    fn handle_session_playback_server_http_root_path<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<SessionPlaybackBeginAction>
            + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
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
                None => Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                    self.main_pid,
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
    fn handle_session_playback_server_data<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            SessionPlaybackServerHttpResponseAction {
                request_id: *request_id,
                response: create_json_http_response(
                    StatusCode::OK,
                    empty(),
                    &json!({
                        "currentFrame": state.session_playback.program_counter,
                        "events": self.session_playback.to_json()
                    }),
                ),
            }
            .into(),
        ))))
    }
    fn handle_session_playback_server_step_command<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<SessionPlaybackBeginAction>
            + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state.session_playback.program_counter;
        let is_already_final_frame = previous_index == frames.len();
        if is_already_final_frame {
            *state = SessionPlaybackServerState::default();
            Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
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
            Some(SchedulerTransition::new(create_debugger_advance_action(
                num_frames,
                *request_id,
                state,
                self.main_pid,
            )))
        }
    }
    fn handle_session_playback_server_continue_command<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<SessionPlaybackBeginAction>
            + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state.session_playback.program_counter;
        let next_index = frames
            .get(previous_index + 1..)
            .into_iter()
            .flatten()
            .enumerate()
            .find_map(|(index, action)| match action {
                SchedulerCommand::Send(pid, action)
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
            return Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
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
        Some(SchedulerTransition::new(create_debugger_advance_action(
            num_frames,
            *request_id,
            state,
            self.main_pid,
        )))
    }
    fn handle_session_playback_server_end_command<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<SessionPlaybackBeginAction>
            + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        let frames = self.session_playback.frames();
        let previous_index = state.session_playback.program_counter;
        let num_frames = frames.len() - previous_index;
        if num_frames == 0 {
            return Some(SchedulerTransition::new(once(SchedulerCommand::Send(
                self.main_pid,
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
        Some(SchedulerTransition::new(create_debugger_advance_action(
            num_frames,
            *request_id,
            state,
            self.main_pid,
        )))
    }
    fn handle_session_playback_server_reset_command<TAction, TTask>(
        &self,
        state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        *state = SessionPlaybackServerState::default();
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
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
    fn handle_session_playback_server_path_not_found<TAction, TTask>(
        &self,
        _state: &mut SessionPlaybackServerState<T>,
        action: &SessionPlaybackServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<SessionPlaybackServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let SessionPlaybackServerHttpRequestAction {
            request_id,
            request,
        } = action;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
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

fn create_debugger_advance_action<T: Expression, TAction, TTask>(
    num_frames: usize,
    request_id: Uuid,
    state: &mut SessionPlaybackServerState<T>,
    main_pid: ProcessId,
) -> impl IntoIterator<Item = SchedulerCommand<TAction, TTask>>
where
    TAction: Action + From<SessionPlaybackBeginAction>,
    TTask: TaskFactory<TAction, TTask>,
{
    state.active_request.replace(request_id);
    once(SchedulerCommand::Send(
        main_pid,
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
    TAction: Action + Matcher<EvaluateResultAction<T>>,
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
                .any(|effect| matches!(effect.as_deref().signal_type(), SignalType::Error))
        })
        .unwrap_or(false)
}
