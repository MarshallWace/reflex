// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use http::StatusCode;
use reflex::core::{Expression, ExpressionFactory};
use reflex_dispatcher::{
    Action, Actor, ActorEvents, Handler, HandlerContext, Matcher, MessageData, NoopDisposeCallback,
    ProcessId, SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
    Worker,
};
use reflex_macros::{blanket_trait, Named};
use reflex_runtime::actor::query_inspector::{
    QueryInspector, QueryInspectorAction, QueryInspectorState,
};
use serde_json::json;

use crate::server::{
    action::query_inspector_server::{
        QueryInspectorServerHttpRequestAction, QueryInspectorServerHttpResponseAction,
    },
    utils::{
        create_accepted_http_response, create_content_type_header, create_html_http_response,
        create_http_response, create_json_http_response, is_json_request,
    },
};

const INDEX_FILE: &'static str = include_str!("../template/debugger/index.html");

blanket_trait!(
    pub trait QueryInspectorServerAction<T: Expression>:
        Matcher<QueryInspectorServerHttpRequestAction>
        + Matcher<QueryInspectorServerHttpResponseAction>
        + From<QueryInspectorServerHttpRequestAction>
        + From<QueryInspectorServerHttpResponseAction>
        + QueryInspectorAction<T>
    {
    }
);

#[derive(Named, Clone)]
pub struct QueryInspectorServer<T: Expression, TFactory: ExpressionFactory<T>> {
    query_inspector: QueryInspector<T>,
    factory: TFactory,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory> QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
{
    pub fn new(factory: TFactory, main_pid: ProcessId) -> Self {
        Self {
            factory: factory.clone(),
            query_inspector: QueryInspector::default(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub type QueryInspectorServerState<T> = QueryInspectorState<T>;

impl<T, TFactory, TAction, TTask> Actor<TAction, TTask> for QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + QueryInspectorServerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
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

impl<T, TFactory, TAction, TTask> Worker<TAction, SchedulerTransition<TAction, TTask>>
    for QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + QueryInspectorServerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    fn accept(&self, action: &TAction) -> bool {
        if let Some(QueryInspectorServerHttpRequestAction { .. }) = action.match_type() {
            true
        } else if let Some(QueryInspectorServerHttpResponseAction { .. }) = action.match_type() {
            false
        } else {
            <QueryInspector<T> as Worker<TAction, SchedulerTransition<TAction, TTask>>>::accept(
                &self.query_inspector,
                action,
            )
        }
    }
    fn schedule(&self, _message: &TAction, _state: &Self::State) -> Option<SchedulerMode> {
        Some(SchedulerMode::Async)
    }
}

impl<T, TFactory, TAction, TTask> Handler<TAction, SchedulerTransition<TAction, TTask>>
    for QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + QueryInspectorServerAction<T>,
    TTask: TaskFactory<TAction, TTask>,
{
    type State = QueryInspectorServerState<T>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        if let Some(action) = action.match_type() {
            self.handle_query_inspector_server_http_request(state, action, metadata, context)
        } else if let Some(QueryInspectorServerHttpResponseAction { .. }) = action.match_type() {
            None
        } else {
            self.query_inspector
                .handle(state, action, metadata, context)
        }
    }
}

impl<T, TFactory> QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
{
    fn handle_query_inspector_server_http_request<TAction, TTask>(
        &self,
        state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryInspectorServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id: _,
            request,
        } = action;
        match request.uri().path() {
            "/" => self.handle_query_inspector_server_root_path(state, action, metadata, context),
            "/env.js" => {
                self.handle_query_inspector_server_env_path(state, action, metadata, context)
            }
            _ => {
                self.handle_query_inspector_server_path_not_found(state, action, metadata, context)
            }
        }
    }
    fn handle_query_inspector_server_root_path<TAction, TTask>(
        &self,
        state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryInspectorServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request,
        } = action;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            QueryInspectorServerHttpResponseAction {
                request_id: *request_id,
                response: if is_json_request(request.headers()) {
                    create_json_http_response(
                        StatusCode::OK,
                        empty(),
                        &state.to_json(&self.factory),
                    )
                } else {
                    create_html_http_response(StatusCode::OK, empty(), INDEX_FILE)
                },
            }
            .into(),
        ))))
    }
    fn handle_query_inspector_server_env_path<TAction, TTask>(
        &self,
        _state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryInspectorServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            QueryInspectorServerHttpResponseAction {
                request_id: *request_id,
                response: create_http_response(
                    StatusCode::OK,
                    once(create_content_type_header("application/javascript")),
                    Some(format!(
                        "window.ENV = {};\n",
                        json!({
                          "DEBUGGER_URL": Option::<String>::None,
                          "INSPECTOR_URL": "/inspect",
                        })
                        .to_string()
                    )),
                ),
            }
            .into(),
        ))))
    }
    fn handle_query_inspector_server_path_not_found<TAction, TTask>(
        &self,
        _state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryInspectorServerHttpResponseAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request,
        } = action;
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            QueryInspectorServerHttpResponseAction {
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
