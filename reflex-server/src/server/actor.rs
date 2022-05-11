// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use http::HeaderMap;
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator},
    stdlib::Stdlib,
};
use reflex_dispatcher::{
    compose_actors, Action, Actor, ChainedActor, HandlerContext, MessageData, StateTransition,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::{RuntimeAction, RuntimeActor, RuntimeMetricNames},
    AsyncExpression,
};

use crate::server::{
    actor::{
        graphql_server::{GraphQlServer, GraphQlServerAction, GraphQlServerMetricNames},
        http_graphql_server::{
            HttpGraphQlServer, HttpGraphQlServerAction, HttpGraphQlServerMetricNames,
        },
        websocket_graphql_server::{
            WebSocketGraphQlServer, WebSocketGraphQlServerAction, WebSocketGraphQlServerMetricNames,
        },
    },
    HttpGraphQlServerQueryTransform, WebSocketGraphQlServerQueryTransform,
};

pub mod graphql_server;
pub mod http_graphql_server;
pub mod websocket_graphql_server;

#[derive(Default, Clone, Copy, Debug)]
pub struct ServerMetricNames {
    pub graphql_server: GraphQlServerMetricNames,
    pub http_graphql_server: HttpGraphQlServerMetricNames,
    pub websocket_graphql_server: WebSocketGraphQlServerMetricNames,
    pub runtime: RuntimeMetricNames,
}

pub trait ServerAction<T: Expression>:
    Action
    + RuntimeAction<T>
    + HttpGraphQlServerAction<T>
    + WebSocketGraphQlServerAction<T>
    + GraphQlServerAction<T>
    + RuntimeAction<T>
{
}
impl<T: Expression, TAction> ServerAction<T> for TAction where
    Self: Action
        + RuntimeAction<T>
        + HttpGraphQlServerAction<T>
        + WebSocketGraphQlServerAction<T>
        + GraphQlServerAction<T>
        + RuntimeAction<T>
{
}

pub(crate) struct ServerActor<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TAction,
> where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    THttpMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
    TAction: ServerAction<T> + Send + 'static,
{
    inner: ChainedActor<
        TAction,
        ChainedActor<
            TAction,
            HttpGraphQlServer<T, TFactory, TTransformHttp, THttpMetricLabels>,
            WebSocketGraphQlServer<T, TFactory, TTransformWs, TConnectionMetricLabels>,
        >,
        ChainedActor<
            TAction,
            GraphQlServer<T, TFactory, TAllocator, TOperationMetricLabels>,
            RuntimeActor<T, TFactory, TAllocator, TAction>,
        >,
    >,
}
impl<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TAction,
    >
    ServerActor<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TAction,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    THttpMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
    TAction: ServerAction<T> + Send + 'static,
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        transform_http: TTransformHttp,
        transform_ws: TTransformWs,
        metric_names: ServerMetricNames,
        get_http_query_metric_labels: THttpMetricLabels,
        get_websocket_connection_metric_labels: TConnectionMetricLabels,
        get_operation_metric_labels: TOperationMetricLabels,
    ) -> Self {
        Self {
            inner: compose_actors(
                compose_actors(
                    HttpGraphQlServer::new(
                        factory.clone(),
                        transform_http,
                        metric_names.http_graphql_server,
                        get_http_query_metric_labels,
                    ),
                    WebSocketGraphQlServer::new(
                        factory.clone(),
                        transform_ws,
                        metric_names.websocket_graphql_server,
                        get_websocket_connection_metric_labels,
                    ),
                ),
                compose_actors(
                    GraphQlServer::new(
                        factory.clone(),
                        allocator.clone(),
                        metric_names.graphql_server,
                        get_operation_metric_labels,
                    ),
                    RuntimeActor::new(factory, allocator, metric_names.runtime),
                ),
            ),
        }
    }
}
impl<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TAction,
    > Actor<TAction>
    for ServerActor<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TAction,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    THttpMetricLabels: Fn(&GraphQlOperationPayload, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(Option<&str>, &GraphQlOperationPayload) -> Vec<(String, String)>,
    TAction: ServerAction<T> + Send + 'static,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        self.inner.handle(action, metadata, context)
    }
}
