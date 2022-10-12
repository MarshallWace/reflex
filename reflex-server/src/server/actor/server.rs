// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use http::HeaderMap;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, ChainedActor, ChainedActorState, HandlerContext,
    InstrumentedActor, InstrumentedActorMetricNames, MessageData,
};
use reflex_graphql::{
    stdlib::Stdlib as GraphQlStdlib,
    validate::{parse_graphql_schema_types, ValidateQueryGraphQlTransform},
    GraphQlOperation, GraphQlSchema,
};
use reflex_json::JsonValue;
use reflex_runtime::{
    actor::{RuntimeAction, RuntimeActor, RuntimeMetricNames},
    AsyncExpression,
};
use reflex_stdlib::Stdlib;

use crate::server::{
    actor::{
        graphql_server::{GraphQlServer, GraphQlServerAction, GraphQlServerMetricNames},
        http_graphql_server::{
            ChainedHttpGraphQlServerQueryTransform, HttpGraphQlServer, HttpGraphQlServerAction,
            HttpGraphQlServerMetricNames,
        },
        websocket_graphql_server::{
            ChainedWebSocketGraphQlServerQueryTransform, WebSocketGraphQlServer,
            WebSocketGraphQlServerAction, WebSocketGraphQlServerMetricNames,
        },
    },
    HttpGraphQlServerQueryTransform, WebSocketGraphQlServerQueryTransform,
};

#[derive(Default, Clone, Copy, Debug)]
pub struct ServerMetricNames {
    pub graphql_server: GraphQlServerMetricNames,
    pub http_graphql_server: HttpGraphQlServerMetricNames,
    pub websocket_graphql_server: WebSocketGraphQlServerMetricNames,
    pub runtime: RuntimeMetricNames,
    pub actor: InstrumentedActorMetricNames,
}

pub trait ServerAction<T: Expression>:
    Action
    + RuntimeAction<T>
    + HttpGraphQlServerAction<T>
    + WebSocketGraphQlServerAction<T>
    + GraphQlServerAction<T>
{
}
impl<T: Expression, TAction> ServerAction<T> for TAction where
    Self: Action
        + RuntimeAction<T>
        + HttpGraphQlServerAction<T>
        + WebSocketGraphQlServerAction<T>
        + GraphQlServerAction<T>
{
}

pub(crate) struct ServerActor<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
> where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: Fn(&GraphQlOperation) -> String,
    THttpMetricLabels: Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
{
    inner: ChainedActor<
        ChainedActor<
            InstrumentedActor<
                HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                >,
            >,
            InstrumentedActor<
                WebSocketGraphQlServer<
                    T,
                    TFactory,
                    ChainedWebSocketGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformWs,
                    >,
                    TConnectionMetricLabels,
                >,
            >,
        >,
        ChainedActor<
            InstrumentedActor<
                GraphQlServer<T, TFactory, TAllocator, TGraphQlQueryLabel, TOperationMetricLabels>,
            >,
            InstrumentedActor<RuntimeActor<T, TFactory, TAllocator>>,
        >,
    >,
}
impl<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
    >
    ServerActor<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: Fn(&GraphQlOperation) -> String,
    THttpMetricLabels: Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
{
    pub fn new(
        schema: Option<GraphQlSchema>,
        factory: TFactory,
        allocator: TAllocator,
        transform_http: TTransformHttp,
        transform_ws: TTransformWs,
        metric_names: ServerMetricNames,
        get_graphql_query_label: TGraphQlQueryLabel,
        get_http_query_metric_labels: THttpMetricLabels,
        get_websocket_connection_metric_labels: TConnectionMetricLabels,
        get_operation_metric_labels: TOperationMetricLabels,
    ) -> Result<Self, String> {
        let schema_types = schema.map(parse_graphql_schema_types).transpose()?;
        let validate_query_transform = schema_types.clone().map(ValidateQueryGraphQlTransform::new);
        Ok(Self {
            inner: ChainedActor::new(
                ChainedActor::new(
                    InstrumentedActor::new(
                        HttpGraphQlServer::new(
                            schema_types.clone(),
                            factory.clone(),
                            ChainedHttpGraphQlServerQueryTransform {
                                left: validate_query_transform.clone(),
                                right: transform_http,
                            },
                            metric_names.http_graphql_server,
                            get_http_query_metric_labels,
                        ),
                        "http_graphql_server",
                        metric_names.actor,
                    ),
                    InstrumentedActor::new(
                        WebSocketGraphQlServer::new(
                            schema_types,
                            factory.clone(),
                            ChainedWebSocketGraphQlServerQueryTransform {
                                left: validate_query_transform,
                                right: transform_ws,
                            },
                            metric_names.websocket_graphql_server,
                            get_websocket_connection_metric_labels,
                        ),
                        "websocket_graphql_server",
                        metric_names.actor,
                    ),
                ),
                ChainedActor::new(
                    InstrumentedActor::new(
                        GraphQlServer::new(
                            factory.clone(),
                            allocator.clone(),
                            metric_names.graphql_server,
                            get_graphql_query_label,
                            get_operation_metric_labels,
                        ),
                        "graphql_server",
                        metric_names.actor,
                    ),
                    InstrumentedActor::new(
                        RuntimeActor::new(factory, allocator, metric_names.runtime),
                        "runtime",
                        metric_names.actor,
                    ),
                ),
            ),
        })
    }
}
impl<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
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
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: Fn(&GraphQlOperation) -> String,
    THttpMetricLabels: Fn(&GraphQlOperation, &HeaderMap) -> Vec<(String, String)>,
    TConnectionMetricLabels: Fn(Option<&JsonValue>, &HeaderMap) -> Vec<(String, String)>,
    TOperationMetricLabels: Fn(&GraphQlOperation) -> Vec<(String, String)>,
    TAction: ServerAction<T> + Send + 'static,
{
    type State = ChainedActorState<
        TAction,
        ChainedActor<
            InstrumentedActor<
                HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                >,
            >,
            InstrumentedActor<
                WebSocketGraphQlServer<
                    T,
                    TFactory,
                    ChainedWebSocketGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformWs,
                    >,
                    TConnectionMetricLabels,
                >,
            >,
        >,
        ChainedActor<
            InstrumentedActor<
                GraphQlServer<T, TFactory, TAllocator, TGraphQlQueryLabel, TOperationMetricLabels>,
            >,
            InstrumentedActor<RuntimeActor<T, TFactory, TAllocator>>,
        >,
    >;
    fn init(&self) -> Self::State {
        self.inner.init()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        self.inner.handle(state, action, metadata, context)
    }
}
