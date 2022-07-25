// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    iter::{empty, once},
    marker::PhantomData,
};

use http::StatusCode;
use reflex::core::{Expression, ExpressionFactory};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_runtime::actor::query_inspector::{
    QueryInspector, QueryInspectorAction, QueryInspectorState,
};
use serde_json::json;

use crate::server::{
    action::query_inspector_server::{
        QueryInspectorServerHttpRequestAction, QueryInspectorServerHttpResponseAction,
    },
    create_content_type_header, create_html_http_response, create_http_response, is_json_request,
    utils::{create_accepted_http_response, create_json_http_response},
};

const INDEX_FILE: &'static str = include_str!("../template/debugger/index.html");

pub trait QueryInspectorServerAction<T: Expression>:
    Action
    + QueryInspectorAction<T>
    + InboundAction<QueryInspectorServerHttpRequestAction>
    + InboundAction<QueryInspectorServerHttpResponseAction>
    + OutboundAction<QueryInspectorServerHttpRequestAction>
    + OutboundAction<QueryInspectorServerHttpResponseAction>
{
}
impl<T: Expression, TAction> QueryInspectorServerAction<T> for TAction where
    Self: Action
        + QueryInspectorAction<T>
        + InboundAction<QueryInspectorServerHttpRequestAction>
        + InboundAction<QueryInspectorServerHttpResponseAction>
        + OutboundAction<QueryInspectorServerHttpRequestAction>
        + OutboundAction<QueryInspectorServerHttpResponseAction>
{
}

pub struct QueryInspectorServer<T: Expression, TFactory: ExpressionFactory<T>> {
    query_inspector: QueryInspector<T>,
    factory: TFactory,
    _expression: PhantomData<T>,
}
impl<T, TFactory> QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
{
    pub fn new(factory: TFactory) -> Self {
        Self {
            factory: factory.clone(),
            query_inspector: QueryInspector::default(),
            _expression: Default::default(),
        }
    }
}

pub struct QueryInspectorServerState<T: Expression>(Option<QueryInspectorState<T>>);
impl<T: Expression> Default for QueryInspectorServerState<T> {
    fn default() -> Self {
        Self(Some(QueryInspectorState::default()))
    }
}
impl<T: Expression> QueryInspectorServerState<T> {
    fn as_inner(&self) -> Option<&QueryInspectorState<T>> {
        self.0.as_ref()
    }
    fn take_inner(&mut self) -> Option<QueryInspectorState<T>> {
        self.0.take()
    }
    fn replace_inner(&mut self, value: QueryInspectorState<T>) -> Option<QueryInspectorState<T>> {
        self.0.replace(value)
    }
}

impl<T, TFactory, TAction> Actor<TAction> for QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: QueryInspectorServerAction<T>,
{
    type State = QueryInspectorServerState<T>;
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
        let (state, inner_transition) = {
            if let Some(inner_state) = state.take_inner() {
                let (inner_state, actions) = self
                    .query_inspector
                    .handle(inner_state, action, metadata, context)
                    .into_parts();
                state.replace_inner(inner_state);
                (state, actions)
            } else {
                (state, Default::default())
            }
        };
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_query_inspector_server_http_request(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, inner_transition.append(actions))
    }
}
impl<T, TFactory> QueryInspectorServer<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
{
    fn handle_query_inspector_server_http_request<TAction>(
        &self,
        state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryInspectorServerHttpResponseAction>,
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
    fn handle_query_inspector_server_root_path<TAction>(
        &self,
        state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryInspectorServerHttpResponseAction>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request,
        } = action;
        let inner_state = state.as_inner()?;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            QueryInspectorServerHttpResponseAction {
                request_id: *request_id,
                response: if is_json_request(request.headers()) {
                    create_json_http_response(
                        StatusCode::OK,
                        empty(),
                        &inner_state.to_json(&self.factory),
                    )
                } else {
                    create_html_http_response(StatusCode::OK, empty(), INDEX_FILE)
                },
            }
            .into(),
        ))))
    }
    fn handle_query_inspector_server_env_path<TAction>(
        &self,
        _state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryInspectorServerHttpResponseAction>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request: _,
        } = action;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
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
    fn handle_query_inspector_server_path_not_found<TAction>(
        &self,
        _state: &mut QueryInspectorServerState<T>,
        action: &QueryInspectorServerHttpRequestAction,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryInspectorServerHttpResponseAction>,
    {
        let QueryInspectorServerHttpRequestAction {
            request_id,
            request,
        } = action;
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
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
