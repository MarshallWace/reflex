// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use futures::{Future, Stream};
use opentelemetry::trace::{Span, Tracer};
use pin_project::pin_project;
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_dispatcher::{
    Action, Actor, ActorEvents, Handler, HandlerContext, MessageData, Named, SchedulerMode,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};
use reflex_graphql::{stdlib::Stdlib as GraphQlStdlib, validate::ValidateQueryGraphQlTransform};
use reflex_macros::blanket_trait;
use reflex_runtime::{
    actor::{RuntimeAction, RuntimeActor, RuntimeActorState},
    task::evaluate_handler::EvaluateHandlerTask,
    AsyncExpression,
};
use reflex_stdlib::Stdlib;

use crate::server::{
    task::websocket_graphql_server::{
        WebSocketGraphQlServerTask, WebSocketGraphQlServerTaskAction,
    },
    ChainedHttpGraphQlServerQueryTransform, ChainedWebSocketGraphQlServerQueryTransform,
    GraphQlServer, GraphQlServerAction, GraphQlServerOperationMetricLabels,
    GraphQlServerQueryLabel, GraphQlServerState, HttpGraphQlServer, HttpGraphQlServerAction,
    HttpGraphQlServerQueryMetricLabels, HttpGraphQlServerQueryTransform, HttpGraphQlServerState,
    WebSocketGraphQlServer, WebSocketGraphQlServerAction,
    WebSocketGraphQlServerConnectionMetricLabels, WebSocketGraphQlServerQueryTransform,
    WebSocketGraphQlServerState,
};

blanket_trait!(
    pub trait ServerAction<T: Expression>:
        RuntimeAction<T>
        + HttpGraphQlServerAction<T>
        + WebSocketGraphQlServerAction<T>
        + GraphQlServerAction<T>
        + WebSocketGraphQlServerTaskAction
    {
    }
);

#[derive(Clone)]
pub enum ServerActor<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TTracer,
> where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    Runtime(RuntimeActor<T, TFactory, TAllocator>),
    GraphQlServer(
        GraphQlServer<T, TFactory, TAllocator, TGraphQlQueryLabel, TOperationMetricLabels, TTracer>,
    ),
    HttpGraphQlServer(
        HttpGraphQlServer<
            T,
            TFactory,
            ChainedHttpGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformHttp,
            >,
            THttpMetricLabels,
        >,
    ),
    WebSocketGraphQlServer(
        WebSocketGraphQlServer<
            T,
            TFactory,
            ChainedWebSocketGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformWs,
            >,
            TConnectionMetricLabels,
        >,
    ),
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
        TTracer,
    > Named
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
        TTracer,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(inner) => inner.name(),
            Self::GraphQlServer(inner) => inner.name(),
            Self::HttpGraphQlServer(inner) => inner.name(),
            Self::WebSocketGraphQlServer(inner) => inner.name(),
        }
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
        TTracer,
        TAction,
        TTask,
    > TaskFactory<TAction, TTask>
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
        TTracer,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    type Actor = Self;

    fn create(self) -> Self::Actor {
        self
    }
}

pub enum ServerActorState<T, TSpan>
where
    T: Expression,
    TSpan: Span,
{
    Runtime(RuntimeActorState<T>),
    GraphQlServer(GraphQlServerState<T, TSpan>),
    HttpGraphQlServer(HttpGraphQlServerState),
    WebSocketGraphQlServer(WebSocketGraphQlServerState<T>),
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
        TTracer,
        TAction,
        TTask,
    > Actor<TAction, TTask>
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
        TTracer,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    type Events<TInbox: TaskInbox<TAction>> = ServerActorEvents<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TInbox,
        TAction,
        TTask,
    >;
    type Dispose = ServerActorDispose<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
        TTask,
    >;
    fn init(&self) -> Self::State {
        match self {
            Self::Runtime(inner) => {
                ServerActorState::Runtime(<RuntimeActor<T, TFactory, TAllocator> as Actor<
                    TAction,
                    TTask,
                >>::init(inner))
            }
            Self::GraphQlServer(inner) => ServerActorState::GraphQlServer(<GraphQlServer<
                T,
                TFactory,
                TAllocator,
                TGraphQlQueryLabel,
                TOperationMetricLabels,
                TTracer,
            > as Actor<TAction, TTask>>::init(
                inner
            )),
            Self::HttpGraphQlServer(inner) => {
                ServerActorState::HttpGraphQlServer(<HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                > as Actor<TAction, TTask>>::init(
                    inner
                ))
            }
            Self::WebSocketGraphQlServer(inner) => {
                ServerActorState::WebSocketGraphQlServer(<WebSocketGraphQlServer<
                    T,
                    TFactory,
                    ChainedWebSocketGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformWs,
                    >,
                    TConnectionMetricLabels,
                > as Actor<TAction, TTask>>::init(
                    inner
                ))
            }
        }
    }
    fn events<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
    ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
        match self {
            Self::Runtime(inner) => <RuntimeActor<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(inner, inbox)
            .map(|(events, dispose)| {
                (
                    ServerActorEvents::Runtime(events),
                    dispose.map(ServerActorDispose::Runtime),
                )
            }),
            Self::GraphQlServer(inner) => {
                <GraphQlServer<
                    T,
                    TFactory,
                    TAllocator,
                    TGraphQlQueryLabel,
                    TOperationMetricLabels,
                    TTracer,
                > as Actor<TAction, TTask>>::events(inner, inbox)
                .map(|(events, dispose)| {
                    (
                        ServerActorEvents::GraphQlServer(events),
                        dispose.map(ServerActorDispose::GraphQlServer),
                    )
                })
            }
            Self::HttpGraphQlServer(inner) => {
                <HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                > as Actor<TAction, TTask>>::events(inner, inbox)
                .map(|(events, dispose)| {
                    (
                        ServerActorEvents::HttpGraphQlServer(events),
                        dispose.map(ServerActorDispose::HttpGraphQlServer),
                    )
                })
            }
            Self::WebSocketGraphQlServer(inner) => {
                <WebSocketGraphQlServer<
                    T,
                    TFactory,
                    ChainedWebSocketGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformWs,
                    >,
                    TConnectionMetricLabels,
                > as Actor<TAction, TTask>>::events(inner, inbox)
                .map(|(events, dispose)| {
                    (
                        ServerActorEvents::WebSocketGraphQlServer(events),
                        dispose.map(ServerActorDispose::WebSocketGraphQlServer),
                    )
                })
            }
        }
    }
}

#[pin_project(project = ServerActorEventsVariant)]
pub enum ServerActorEvents<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TTracer,
    TInbox,
    TAction,
    TTask,
> where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    Runtime(
        #[pin] <RuntimeActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    GraphQlServer(
        #[pin]
        <GraphQlServer<
            T,
            TFactory,
            TAllocator,
            TGraphQlQueryLabel,
            TOperationMetricLabels,
            TTracer,
        > as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    HttpGraphQlServer(
        #[pin]
        <HttpGraphQlServer<
            T,
            TFactory,
            ChainedHttpGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformHttp,
            >,
            THttpMetricLabels,
        > as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    WebSocketGraphQlServer(
        #[pin]
        <WebSocketGraphQlServer<
            T,
            TFactory,
            ChainedWebSocketGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformWs,
            >,
            TConnectionMetricLabels,
        > as Actor<TAction, TTask>>::Events<TInbox>,
    ),
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
        TTracer,
        TInbox,
        TAction,
        TTask,
    > Stream
    for ServerActorEvents<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TInbox,
        TAction,
        TTask,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            ServerActorEventsVariant::Runtime(inner) => inner.poll_next(cx),
            ServerActorEventsVariant::GraphQlServer(inner) => inner.poll_next(cx),
            ServerActorEventsVariant::HttpGraphQlServer(inner) => inner.poll_next(cx),
            ServerActorEventsVariant::WebSocketGraphQlServer(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Runtime(inner) => inner.size_hint(),
            Self::GraphQlServer(inner) => inner.size_hint(),
            Self::HttpGraphQlServer(inner) => inner.size_hint(),
            Self::WebSocketGraphQlServer(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = ServerActorDisposeVariant)]
pub enum ServerActorDispose<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TTracer,
    TAction,
    TTask,
> where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    Runtime(#[pin] <RuntimeActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose),
    GraphQlServer(
        #[pin]
        <GraphQlServer<
            T,
            TFactory,
            TAllocator,
            TGraphQlQueryLabel,
            TOperationMetricLabels,
            TTracer,
        > as Actor<TAction, TTask>>::Dispose,
    ),
    HttpGraphQlServer(
        #[pin]
        <HttpGraphQlServer<
            T,
            TFactory,
            ChainedHttpGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformHttp,
            >,
            THttpMetricLabels,
        > as Actor<TAction, TTask>>::Dispose,
    ),
    WebSocketGraphQlServer(
        #[pin]
        <WebSocketGraphQlServer<
            T,
            TFactory,
            ChainedWebSocketGraphQlServerQueryTransform<
                Option<ValidateQueryGraphQlTransform<'static, String>>,
                TTransformWs,
            >,
            TConnectionMetricLabels,
        > as Actor<TAction, TTask>>::Dispose,
    ),
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
        TTracer,
        TAction,
        TTask,
    > Future
    for ServerActorDispose<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
        TTask,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            ServerActorDisposeVariant::Runtime(inner) => inner.poll(cx),
            ServerActorDisposeVariant::GraphQlServer(inner) => inner.poll(cx),
            ServerActorDisposeVariant::HttpGraphQlServer(inner) => inner.poll(cx),
            ServerActorDisposeVariant::WebSocketGraphQlServer(inner) => inner.poll(cx),
        }
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
        TTracer,
        TAction,
        TTask,
    > Worker<TAction, SchedulerTransition<TAction, TTask>>
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
        TTracer,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::Runtime(actor) => <RuntimeActor<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::GraphQlServer(actor) => <GraphQlServer<
                T,
                TFactory,
                TAllocator,
                TGraphQlQueryLabel,
                TOperationMetricLabels,
                TTracer,
            > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::accept(
                actor, message
            ),
            Self::HttpGraphQlServer(actor) => <HttpGraphQlServer<
                T,
                TFactory,
                ChainedHttpGraphQlServerQueryTransform<
                    Option<ValidateQueryGraphQlTransform<'static, String>>,
                    TTransformHttp,
                >,
                THttpMetricLabels,
            > as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::WebSocketGraphQlServer(actor) => <WebSocketGraphQlServer<
                T,
                TFactory,
                ChainedWebSocketGraphQlServerQueryTransform<
                    Option<ValidateQueryGraphQlTransform<'static, String>>,
                    TTransformWs,
                >,
                TConnectionMetricLabels,
            > as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::Runtime(actor), ServerActorState::Runtime(state)) => {
                <RuntimeActor<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::GraphQlServer(actor), ServerActorState::GraphQlServer(state)) => {
                <GraphQlServer<
                    T,
                    TFactory,
                    TAllocator,
                    TGraphQlQueryLabel,
                    TOperationMetricLabels,
                    TTracer,
                > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::HttpGraphQlServer(actor), ServerActorState::HttpGraphQlServer(state)) => {
                <HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (
                Self::WebSocketGraphQlServer(actor),
                ServerActorState::WebSocketGraphQlServer(state),
            ) => <WebSocketGraphQlServer<
                T,
                TFactory,
                ChainedWebSocketGraphQlServerQueryTransform<
                    Option<ValidateQueryGraphQlTransform<'static, String>>,
                    TTransformWs,
                >,
                TConnectionMetricLabels,
            > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                actor, message, state,
            ),
            _ => unreachable!(),
        }
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
        TTracer,
        TAction,
        TTask,
    > Handler<TAction, SchedulerTransition<TAction, TTask>>
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
        TTracer,
    >
where
    T: AsyncExpression,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerAction<T>,
    TTask: TaskFactory<TAction, TTask> + WebSocketGraphQlServerTask + EvaluateHandlerTask,
{
    type State = ServerActorState<T, TTracer::Span>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::Runtime(actor), ServerActorState::Runtime(state)) => {
                <RuntimeActor<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::GraphQlServer(actor), ServerActorState::GraphQlServer(state)) => {
                <GraphQlServer<
                    T,
                    TFactory,
                    TAllocator,
                    TGraphQlQueryLabel,
                    TOperationMetricLabels,
                    TTracer,
                > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::HttpGraphQlServer(actor), ServerActorState::HttpGraphQlServer(state)) => {
                <HttpGraphQlServer<
                    T,
                    TFactory,
                    ChainedHttpGraphQlServerQueryTransform<
                        Option<ValidateQueryGraphQlTransform<'static, String>>,
                        TTransformHttp,
                    >,
                    THttpMetricLabels,
                > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (
                Self::WebSocketGraphQlServer(actor),
                ServerActorState::WebSocketGraphQlServer(state),
            ) => <WebSocketGraphQlServer<
                T,
                TFactory,
                ChainedWebSocketGraphQlServerQueryTransform<
                    Option<ValidateQueryGraphQlTransform<'static, String>>,
                    TTransformWs,
                >,
                TConnectionMetricLabels,
            > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                actor, state, action, metadata, context,
            ),
            _ => unreachable!(),
        }
    }
}
