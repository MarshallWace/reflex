// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use opentelemetry::trace::Tracer;
use reflex::core::{Applicable, Expression, Reducible, Rewritable};
use reflex_dispatcher::{Action, Redispatcher};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_handlers::{
    actor::{HandlerAction, HandlerActor},
    task::{
        fetch::FetchHandlerTaskFactory,
        graphql::{
            GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory,
        },
        timeout::TimeoutHandlerTaskFactory,
        timestamp::TimestampHandlerTaskFactory,
        DefaultHandlersTaskFactory,
    },
};
use reflex_interpreter::compiler::Compile;
use reflex_macros::{blanket_trait, task_factory_enum, Matcher};
use reflex_runtime::{
    actor::bytecode_interpreter::{
        BytecodeInterpreter, BytecodeInterpreterAction, BytecodeInterpreterMetricLabels,
    },
    task::{
        bytecode_worker::BytecodeWorkerTaskFactory, evaluate_handler::EffectThrottleTaskFactory,
        RuntimeTask, RuntimeTaskFactory,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_stdlib::Stdlib;
use reflex_utils::reconnect::ReconnectTimeout;

use crate::{
    actor::{ServerAction, ServerActor},
    server::{
        task::websocket_graphql_server::{
            WebSocketGraphQlServerTaskFactory, WebSocketGraphQlServerThrottleTimeoutTaskFactory,
        },
        GraphQlServerOperationMetricLabels, GraphQlServerQueryLabel,
        HttpGraphQlServerQueryMetricLabels, HttpGraphQlServerQueryTransform,
        WebSocketGraphQlServerConnectionMetricLabels, WebSocketGraphQlServerQueryTransform,
    },
    task::{ServerTask, ServerTaskAction, ServerTaskFactory},
    GraphQlWebServerTask,
};

blanket_trait!(
    pub trait ServerCliTaskAction<T: Expression>:
        ServerAction<T> + BytecodeInterpreterAction<T> + ServerTaskAction<T> + HandlerAction<T>
    {
    }
);

blanket_trait!(
    pub trait ServerCliTask<T, TFactory, TAllocator, TConnect>:
        RuntimeTask<T, TFactory, TAllocator>
        + ServerTask<T, TFactory, TAllocator, TConnect>
        + GraphQlWebServerTask<T, TFactory, TAllocator>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::String: Send,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
        T::Signal<T>: Send,
        T::SignalList<T>: Send,
        T::StructPrototype<T>: Send,
        T::ExpressionList<T>: Send,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

task_factory_enum!({
    #[derive(Matcher, Clone)]
    pub enum ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::String: Send,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
        T::Signal<T>: Send,
        T::SignalList<T>: Send,
        T::StructPrototype<T>: Send,
        T::ExpressionList<T>: Send,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TReconnect: ReconnectTimeout + Send + Clone + 'static,
        TTransformHttp: HttpGraphQlServerQueryTransform,
        TTransformWs: WebSocketGraphQlServerQueryTransform,
        TGraphQlQueryLabel: GraphQlServerQueryLabel,
        THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
        TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
        TOperationMetricLabels: GraphQlServerOperationMetricLabels,
        TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
        TTracer: Tracer,
        TTracer::Span: Send + Sync + 'static,
    {
        Runtime(EffectThrottleTaskFactory),
        ServerTask(ServerTaskFactory<T, TFactory, TAllocator, TConnect>),
        Server(
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
                TTracer,
            >,
        ),
        BytecodeInterpreter(BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>),
        Handler(HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>),
        Main(Redispatcher),
    }

    impl<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            TTransformHttp,
            TTransformWs,
            TGraphQlQueryLabel,
            THttpMetricLabels,
            TConnectionMetricLabels,
            TWorkerMetricLabels,
            TOperationMetricLabels,
            TTracer,
            TAction,
        >
        TaskFactory<
            TAction,
            ServerCliTaskFactory<
                T,
                TFactory,
                TAllocator,
                TConnect,
                TReconnect,
                TTransformHttp,
                TTransformWs,
                TGraphQlQueryLabel,
                THttpMetricLabels,
                TConnectionMetricLabels,
                TWorkerMetricLabels,
                TOperationMetricLabels,
                TTracer,
            >,
        >
        for ServerCliTaskFactory<
            T,
            TFactory,
            TAllocator,
            TConnect,
            TReconnect,
            TTransformHttp,
            TTransformWs,
            TGraphQlQueryLabel,
            THttpMetricLabels,
            TConnectionMetricLabels,
            TWorkerMetricLabels,
            TOperationMetricLabels,
            TTracer,
        >
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::String: Send,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
        T::Signal<T>: Send,
        T::SignalList<T>: Send,
        T::StructPrototype<T>: Send,
        T::ExpressionList<T>: Send,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TReconnect: ReconnectTimeout + Send + Clone + 'static,
        TTransformHttp: HttpGraphQlServerQueryTransform,
        TTransformWs: WebSocketGraphQlServerQueryTransform,
        TGraphQlQueryLabel: GraphQlServerQueryLabel,
        THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
        TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
        TOperationMetricLabels: GraphQlServerOperationMetricLabels,
        TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
        TTracer: Tracer,
        TTracer::Span: Send + Sync + 'static,
        TAction: Action + ServerCliTaskAction<T> + Send + 'static,
    {
    }
});

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    >
    From<
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
            TTracer,
        >,
    >
    for ServerCliTaskActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
{
    fn from(
        value: ServerActor<
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
        >,
    ) -> Self {
        Self::Server(value)
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    > From<BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>>
    for ServerCliTaskActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
{
    fn from(value: BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>) -> Self {
        Self::BytecodeInterpreter(value)
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    > From<HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>>
    for ServerCliTaskActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
{
    fn from(value: HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>) -> Self {
        Self::Handler(value)
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    > From<Redispatcher>
    for ServerCliTaskActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
        TAction,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
{
    fn from(value: Redispatcher) -> Self {
        Self::Main(value)
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<RuntimeTaskFactory<T, TFactory, TAllocator>>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: RuntimeTaskFactory<T, TFactory, TAllocator>) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<DefaultHandlersTaskFactory<TConnect>>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: DefaultHandlersTaskFactory<TConnect>) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<WebSocketGraphQlServerTaskFactory>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: WebSocketGraphQlServerTaskFactory) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: BytecodeWorkerTaskFactory<T, TFactory, TAllocator>) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<FetchHandlerTaskFactory<TConnect>>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: FetchHandlerTaskFactory<TConnect>) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerHttpFetchTaskFactory<TConnect>) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<GraphQlHandlerWebSocketConnectionTaskFactory>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerWebSocketConnectionTaskFactory) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<TimeoutHandlerTaskFactory>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: TimeoutHandlerTaskFactory) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<TimestampHandlerTaskFactory>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: TimestampHandlerTaskFactory) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}

impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    > From<WebSocketGraphQlServerThrottleTimeoutTaskFactory>
    for ServerCliTaskFactory<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TWorkerMetricLabels,
        TOperationMetricLabels,
        TTracer,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib> + Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TTransformHttp: HttpGraphQlServerQueryTransform,
    TTransformWs: WebSocketGraphQlServerQueryTransform,
    TGraphQlQueryLabel: GraphQlServerQueryLabel,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels,
    TTracer: Tracer,
    TTracer::Span: Send + Sync + 'static,
{
    fn from(value: WebSocketGraphQlServerThrottleTimeoutTaskFactory) -> Self {
        Self::ServerTask(ServerTaskFactory::from(value))
    }
}
