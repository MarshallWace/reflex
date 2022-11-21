// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{marker::PhantomData, pin::Pin};

use futures::{Future, Stream};
use opentelemetry::trace::Tracer;
use pin_project::pin_project;
use reflex::core::{Applicable, Expression, Reducible, Rewritable};
use reflex_dispatcher::{
    Action, Actor, ActorEvents, Handler, HandlerContext, MessageData, Named, Redispatcher,
    SchedulerTransition, TaskFactory, TaskInbox, Worker,
};
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
use reflex_macros::blanket_trait;
use reflex_runtime::{
    actor::bytecode_interpreter::{
        BytecodeInterpreter, BytecodeInterpreterAction, BytecodeInterpreterMetricLabels,
    },
    task::{bytecode_worker::BytecodeWorkerTaskFactory, RuntimeTaskFactory},
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
    task::{ServerTask, ServerTaskAction, ServerTaskActor, ServerTaskFactory},
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
        ServerTask<T, TFactory, TAllocator, TConnect>
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

#[derive(Clone)]
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
> where
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
    ServerTask(ServerTaskFactory<T, TFactory, TAllocator, TConnect>),
    _Unreachable(
        std::convert::Infallible,
        PhantomData<TReconnect>,
        PhantomData<TTransformHttp>,
        PhantomData<TTransformWs>,
        PhantomData<TGraphQlQueryLabel>,
        PhantomData<THttpMetricLabels>,
        PhantomData<TConnectionMetricLabels>,
        PhantomData<TWorkerMetricLabels>,
        PhantomData<TOperationMetricLabels>,
        PhantomData<TTracer>,
    ),
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
    > Named
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
    fn name(&self) -> &'static str {
        match self {
            Self::ServerTask(inner) => inner.name(),
            Self::_Unreachable(inner, ..) => match *inner {},
        }
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
    > TaskFactory<TAction, Self>
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
    type Actor = ServerCliTaskActor<
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
    >;
    fn create(self) -> Self::Actor {
        match self {
            Self::ServerTask(inner) => {
                ServerCliTaskActor::ServerTask(
                    <ServerTaskFactory<T, TFactory, TAllocator, TConnect> as TaskFactory<
                        TAction,
                        Self,
                    >>::create(inner),
                )
            }
            Self::_Unreachable(inner, ..) => match inner {},
        }
    }
}

#[derive(Clone)]
pub enum ServerCliTaskActor<
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
> where
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
    ServerTask(ServerTaskActor<T, TFactory, TAllocator, TConnect>),
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
    > Named
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
    fn name(&self) -> &'static str {
        match self {
            Self::Server(inner) => inner.name(),
            Self::BytecodeInterpreter(inner) => inner.name(),
            Self::ServerTask(inner) => inner.name(),
            Self::Handler(inner) => inner.name(),
            Self::Main(inner) => inner.name(),
        }
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
        TTask,
    > Actor<TAction, TTask>
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    type Events<TInbox: TaskInbox<TAction>> = ServerCliTaskEvents<
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
        TInbox,
        TAction,
        TTask,
    >;
    type Dispose = ServerCliTaskDispose<
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
        TTask,
    >;
    fn init(&self) -> Self::State {
        match self {
            Self::Server(actor) => ServerCliTaskActorState::Server(<ServerActor<
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
            > as Actor<TAction, TTask>>::init(
                actor
            )),
            Self::BytecodeInterpreter(actor) => {
                ServerCliTaskActorState::BytecodeInterpreter(<BytecodeInterpreter<
                    T,
                    TFactory,
                    TAllocator,
                    TWorkerMetricLabels,
                > as Actor<TAction, TTask>>::init(
                    actor
                ))
            }
            Self::ServerTask(actor) => {
                ServerCliTaskActorState::ServerTask(<ServerTaskActor<
                    T,
                    TFactory,
                    TAllocator,
                    TConnect,
                > as Actor<TAction, TTask>>::init(
                    actor
                ))
            }
            Self::Handler(actor) => ServerCliTaskActorState::Handler(<HandlerActor<
                T,
                TFactory,
                TAllocator,
                TConnect,
                TReconnect,
            > as Actor<TAction, TTask>>::init(
                actor
            )),
            Self::Main(actor) => {
                ServerCliTaskActorState::Main(<Redispatcher as Actor<TAction, TTask>>::init(actor))
            }
        }
    }
    fn events<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
    ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
        match self {
            Self::Server(actor) => {
                <ServerActor<
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
                    > as Actor<TAction, TTask>>::events(actor, inbox).map(|(events, dispose)| {(

                    ServerCliTaskEvents::Server(events),
                    dispose.map(ServerCliTaskDispose::Server),
                )})

            }
            Self::BytecodeInterpreter(actor) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Actor<
                        TAction,
                        TTask,
                    >>::events(actor, inbox).map(|(events, dispose)| {(

                    ServerCliTaskEvents::BytecodeInterpreter(events),
                    dispose.map(ServerCliTaskDispose::BytecodeInterpreter),
                )})

            }
            Self::ServerTask(actor) => {
                <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::events(
                        actor, inbox,
                    ).map(|(events, dispose)| {(

                    ServerCliTaskEvents::ServerTask(events),
                    dispose.map(ServerCliTaskDispose::ServerTask),
                )})

            }
            Self::Handler(actor) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                        TAction,
                        TTask,
                    >>::events(actor, inbox).map(|(events, dispose)| {(

                    ServerCliTaskEvents::Handler(events),
                    dispose.map(ServerCliTaskDispose::Handler),
                )})

            }
            Self::Main(actor) => {
                <Redispatcher as Actor<TAction, TTask>>::events(actor, inbox).map(|(events, dispose)| (
                    ServerCliTaskEvents::Main(events),
                    dispose.map(ServerCliTaskDispose::Main),
                ))

            }
        }
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
        TTask,
    > Worker<TAction, SchedulerTransition<TAction, TTask>>
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::Server(actor) => <ServerActor<
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
            > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::accept(
                actor, message
            ),
            Self::BytecodeInterpreter(actor) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
            Self::ServerTask(actor) => {
                <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
            Self::Handler(actor) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
            Self::Main(actor) => <Redispatcher as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
        }
    }
    fn schedule(
        &self,
        message: &TAction,
        state: &Self::State,
    ) -> Option<reflex_dispatcher::SchedulerMode> {
        match (self, state) {
            (Self::Server(actor), ServerCliTaskActorState::Server(state)) => {
                <ServerActor<
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
                > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (
                Self::BytecodeInterpreter(actor),
                ServerCliTaskActorState::BytecodeInterpreter(state),
            ) => <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::schedule(actor, message, state),
            (Self::ServerTask(actor), ServerCliTaskActorState::ServerTask(state)) => {
                <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::Handler(actor), ServerCliTaskActorState::Handler(state)) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::Main(actor), ServerCliTaskActorState::Main(state)) => {
                <Redispatcher as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            _ => unreachable!(),
        }
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
        TTask,
    > Handler<TAction, SchedulerTransition<TAction, TTask>>
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    type State = ServerCliTaskActorState<
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
        TTask,
    >;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::Server(actor), ServerCliTaskActorState::Server(state)) => {
                <ServerActor<
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
                > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (
                Self::BytecodeInterpreter(actor),
                ServerCliTaskActorState::BytecodeInterpreter(state),
            ) => <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Handler<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::handle(actor, state, action, metadata, context),
            (Self::ServerTask(actor), ServerCliTaskActorState::ServerTask(state)) => {
                <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::Handler(actor), ServerCliTaskActorState::Handler(state)) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::Main(actor), ServerCliTaskActorState::Main(state)) => {
                <Redispatcher as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            _ => unreachable!(),
        }
    }
}

pub enum ServerCliTaskActorState<
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
    TTask,
> where
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    Server(
        <ServerActor<
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
        > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State,
    ),
    BytecodeInterpreter(
        <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    ServerTask(
        <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    Handler(
        <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    Main(<Redispatcher as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State),
}

#[pin_project(project = ServerCliTaskEventsVariant)]
pub enum ServerCliTaskEvents<
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
    TInbox,
    TAction,
    TTask,
> where
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
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    Server(
        #[pin]
        <ServerActor<
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
        > as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    BytecodeInterpreter(
        #[pin]
        <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Actor<
            TAction,
            TTask,
        >>::Events<TInbox>,
    ),
    ServerTask(
        #[pin]
        <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::Events<
            TInbox,
        >,
    ),
    Handler(
        #[pin]
        <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Events<TInbox>,
    ),
    Main(#[pin] <Redispatcher as Actor<TAction, TTask>>::Events<TInbox>),
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
        TInbox,
        TAction,
        TTask,
    > Stream
    for ServerCliTaskEvents<
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
        TInbox,
        TAction,
        TTask,
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
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerCliTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            ServerCliTaskEventsVariant::Server(inner) => inner.poll_next(cx),
            ServerCliTaskEventsVariant::BytecodeInterpreter(inner) => inner.poll_next(cx),
            ServerCliTaskEventsVariant::ServerTask(inner) => inner.poll_next(cx),
            ServerCliTaskEventsVariant::Handler(inner) => inner.poll_next(cx),
            ServerCliTaskEventsVariant::Main(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Server(inner) => inner.size_hint(),
            Self::BytecodeInterpreter(inner) => inner.size_hint(),
            Self::ServerTask(inner) => inner.size_hint(),
            Self::Handler(inner) => inner.size_hint(),
            Self::Main(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = ServerCliTaskDisposeVariant)]
pub enum ServerCliTaskDispose<
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
    TTask,
> where
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    Server(
        #[pin]
        <ServerActor<
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
        > as Actor<TAction, TTask>>::Dispose,
    ),
    BytecodeInterpreter(
        #[pin]
        <BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels> as Actor<
            TAction,
            TTask,
        >>::Dispose,
    ),
    ServerTask(
        #[pin]
        <ServerTaskActor<T, TFactory, TAllocator, TConnect> as Actor<TAction, TTask>>::Dispose,
    ),
    Handler(
        #[pin]
        <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Dispose,
    ),
    Main(#[pin] <Redispatcher as Actor<TAction, TTask>>::Dispose),
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
        TTask,
    > Future
    for ServerCliTaskDispose<
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
        TTask,
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
    TTask: TaskFactory<TAction, TTask> + ServerCliTask<T, TFactory, TAllocator, TConnect>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            ServerCliTaskDisposeVariant::Server(inner) => inner.poll(cx),
            ServerCliTaskDisposeVariant::BytecodeInterpreter(inner) => inner.poll(cx),
            ServerCliTaskDisposeVariant::ServerTask(inner) => inner.poll(cx),
            ServerCliTaskDisposeVariant::Handler(inner) => inner.poll(cx),
            ServerCliTaskDisposeVariant::Main(inner) => inner.poll(cx),
        }
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
