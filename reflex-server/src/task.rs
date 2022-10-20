// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::pin::Pin;

use futures::{Future, Stream};
use pin_project::pin_project;
use reflex::core::{Applicable, Expression, Reducible, Rewritable};
use reflex_dispatcher::{
    Action, Actor, ActorInitContext, Handler, HandlerContext, MessageData, SchedulerTransition,
    TaskFactory, TaskInbox, Worker,
};
use reflex_handlers::task::{
    fetch::FetchHandlerTaskFactory,
    graphql::{GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory},
    timeout::TimeoutHandlerTaskFactory,
    timestamp::TimestampHandlerTaskFactory,
    DefaultHandlersTask, DefaultHandlersTaskAction, DefaultHandlersTaskActor,
    DefaultHandlersTaskFactory,
};
use reflex_interpreter::compiler::Compile;
use reflex_runtime::{
    task::{
        bytecode_worker::BytecodeWorkerTaskFactory, RuntimeTask, RuntimeTaskAction,
        RuntimeTaskActor, RuntimeTaskFactory,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::server::task::websocket_graphql_server::{
    WebSocketGraphQlServerTask, WebSocketGraphQlServerTaskAction, WebSocketGraphQlServerTaskActor,
    WebSocketGraphQlServerTaskFactory, WebSocketGraphQlServerThrottleTimeoutTaskFactory,
};

pub trait ServerTaskAction<T: Expression>:
    RuntimeTaskAction<T> + DefaultHandlersTaskAction + WebSocketGraphQlServerTaskAction
{
}
impl<_Self, T: Expression> ServerTaskAction<T> for _Self where
    Self: RuntimeTaskAction<T> + DefaultHandlersTaskAction + WebSocketGraphQlServerTaskAction
{
}

pub trait ServerTask<T, TFactory, TAllocator, TConnect>:
    RuntimeTask<T, TFactory, TAllocator> + DefaultHandlersTask<TConnect> + WebSocketGraphQlServerTask
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
}
impl<_Self, T, TFactory, TAllocator, TConnect> ServerTask<T, TFactory, TAllocator, TConnect>
    for _Self
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    Self: RuntimeTask<T, TFactory, TAllocator>
        + DefaultHandlersTask<TConnect>
        + WebSocketGraphQlServerTask,
{
}

#[derive(Clone)]
pub enum ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    Runtime(RuntimeTaskFactory<T, TFactory, TAllocator>),
    DefaultHandlers(DefaultHandlersTaskFactory<TConnect>),
    WebSocketGraphQlServer(WebSocketGraphQlServerTaskFactory),
}
impl<T, TFactory, TAllocator, TConnect, TAction, TTask> TaskFactory<TAction, TTask>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    type Actor = ServerTaskActor<T, TFactory, TAllocator, TConnect>;
    fn create(self) -> Self::Actor {
        match self {
            Self::Runtime(inner) => ServerTaskActor::Runtime(<RuntimeTaskFactory<
                T,
                TFactory,
                TAllocator,
            > as TaskFactory<TAction, TTask>>::create(
                inner
            )),
            Self::DefaultHandlers(inner) => ServerTaskActor::DefaultHandlers(
                <DefaultHandlersTaskFactory<TConnect> as TaskFactory<TAction, TTask>>::create(
                    inner,
                ),
            ),
            Self::WebSocketGraphQlServer(inner) => ServerTaskActor::WebSocketGraphQlServer(
                <WebSocketGraphQlServerTaskFactory as TaskFactory<TAction, TTask>>::create(inner),
            ),
        }
    }
}

pub enum ServerTaskActor<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    Runtime(RuntimeTaskActor<T, TFactory, TAllocator>),
    DefaultHandlers(DefaultHandlersTaskActor<TConnect>),
    WebSocketGraphQlServer(WebSocketGraphQlServerTaskActor),
}
impl<T, TFactory, TAllocator, TConnect, TAction, TTask> Actor<TAction, TTask>
    for ServerTaskActor<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    type Events<TInbox: TaskInbox<TAction>> =
        ServerTaskEvents<T, TFactory, TAllocator, TConnect, TInbox, TAction, TTask>;
    type Dispose = ServerTaskDispose<T, TFactory, TAllocator, TConnect, TAction, TTask>;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
        match self {
            Self::Runtime(actor) => {
                let (state, events, dispose) =
                    <RuntimeTaskActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::init(
                        actor, inbox, context,
                    );
                (
                    ServerTaskActorState::Runtime(state),
                    ServerTaskEvents::Runtime(events),
                    ServerTaskDispose::Runtime(dispose),
                )
            }
            Self::DefaultHandlers(actor) => {
                let (state, events, dispose) = <DefaultHandlersTaskActor<TConnect> as Actor<
                    TAction,
                    TTask,
                >>::init(actor, inbox, context);
                (
                    ServerTaskActorState::DefaultHandlers(state),
                    ServerTaskEvents::DefaultHandlers(events),
                    ServerTaskDispose::DefaultHandlers(dispose),
                )
            }
            Self::WebSocketGraphQlServer(actor) => {
                let (state, events, dispose) = <WebSocketGraphQlServerTaskActor as Actor<
                    TAction,
                    TTask,
                >>::init(actor, inbox, context);
                (
                    ServerTaskActorState::WebSocketGraphQlServer(state),
                    ServerTaskEvents::WebSocketGraphQlServer(events),
                    ServerTaskDispose::WebSocketGraphQlServer(dispose),
                )
            }
        }
    }
}
impl<T, TFactory, TAllocator, TConnect, TAction, TTask>
    Worker<TAction, SchedulerTransition<TAction, TTask>>
    for ServerTaskActor<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::Runtime(actor) => <RuntimeTaskActor<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::DefaultHandlers(actor) => <DefaultHandlersTaskActor<TConnect> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::WebSocketGraphQlServer(actor) => <WebSocketGraphQlServerTaskActor as Worker<
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
            (Self::Runtime(actor), ServerTaskActorState::Runtime(state)) => {
                <RuntimeTaskActor<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::DefaultHandlers(actor), ServerTaskActorState::DefaultHandlers(state)) => {
                <DefaultHandlersTaskActor<TConnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (
                Self::WebSocketGraphQlServer(actor),
                ServerTaskActorState::WebSocketGraphQlServer(state),
            ) => <WebSocketGraphQlServerTaskActor as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::schedule(actor, message, state),
            _ => unreachable!(),
        }
    }
}
impl<T, TFactory, TAllocator, TConnect, TAction, TTask>
    Handler<TAction, SchedulerTransition<TAction, TTask>>
    for ServerTaskActor<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    type State = ServerTaskActorState<T, TFactory, TAllocator, TConnect, TAction, TTask>;
    fn handle(
        &self,
        state: &mut Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>> {
        match (self, state) {
            (Self::Runtime(actor), ServerTaskActorState::Runtime(state)) => {
                <RuntimeTaskActor<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::DefaultHandlers(actor), ServerTaskActorState::DefaultHandlers(state)) => {
                <DefaultHandlersTaskActor<TConnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (
                Self::WebSocketGraphQlServer(actor),
                ServerTaskActorState::WebSocketGraphQlServer(state),
            ) => <WebSocketGraphQlServerTaskActor as Handler<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::handle(actor, state, action, metadata, context),
            _ => unreachable!(),
        }
    }
}

pub enum ServerTaskActorState<T, TFactory, TAllocator, TConnect, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    Runtime(
        <RuntimeTaskActor<T, TFactory, TAllocator> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    DefaultHandlers(
        <DefaultHandlersTaskActor<TConnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    WebSocketGraphQlServer(
        <WebSocketGraphQlServerTaskActor as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
}

#[pin_project(project = ServerTaskEventsVariant)]
pub enum ServerTaskEvents<T, TFactory, TAllocator, TConnect, TInbox, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    Runtime(
        #[pin] <RuntimeTaskActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    DefaultHandlers(
        #[pin] <DefaultHandlersTaskActor<TConnect> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    WebSocketGraphQlServer(
        #[pin] <WebSocketGraphQlServerTaskActor as Actor<TAction, TTask>>::Events<TInbox>,
    ),
}
impl<T, TFactory, TAllocator, TConnect, TInbox, TAction, TTask> Stream
    for ServerTaskEvents<T, TFactory, TAllocator, TConnect, TInbox, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            ServerTaskEventsVariant::Runtime(inner) => inner.poll_next(cx),
            ServerTaskEventsVariant::DefaultHandlers(inner) => inner.poll_next(cx),
            ServerTaskEventsVariant::WebSocketGraphQlServer(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Runtime(inner) => inner.size_hint(),
            Self::DefaultHandlers(inner) => inner.size_hint(),
            Self::WebSocketGraphQlServer(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = ServerTaskDisposeVariant)]
pub enum ServerTaskDispose<T, TFactory, TAllocator, TConnect, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    Runtime(#[pin] <RuntimeTaskActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose),
    DefaultHandlers(#[pin] <DefaultHandlersTaskActor<TConnect> as Actor<TAction, TTask>>::Dispose),
    WebSocketGraphQlServer(
        #[pin] <WebSocketGraphQlServerTaskActor as Actor<TAction, TTask>>::Dispose,
    ),
}
impl<T, TFactory, TAllocator, TConnect, TAction, TTask> Future
    for ServerTaskDispose<T, TFactory, TAllocator, TConnect, TAction, TTask>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + ServerTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + ServerTask<T, TFactory, TAllocator, TConnect>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            ServerTaskDisposeVariant::Runtime(inner) => inner.poll(cx),
            ServerTaskDisposeVariant::DefaultHandlers(inner) => inner.poll(cx),
            ServerTaskDisposeVariant::WebSocketGraphQlServer(inner) => inner.poll(cx),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect> From<RuntimeTaskFactory<T, TFactory, TAllocator>>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: RuntimeTaskFactory<T, TFactory, TAllocator>) -> Self {
        Self::Runtime(value)
    }
}

impl<T, TFactory, TAllocator, TConnect> From<DefaultHandlersTaskFactory<TConnect>>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: DefaultHandlersTaskFactory<TConnect>) -> Self {
        Self::DefaultHandlers(value)
    }
}

impl<T, TFactory, TAllocator, TConnect> From<WebSocketGraphQlServerTaskFactory>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: WebSocketGraphQlServerTaskFactory) -> Self {
        Self::WebSocketGraphQlServer(value)
    }
}

impl<T, TFactory, TAllocator, TConnect> From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: BytecodeWorkerTaskFactory<T, TFactory, TAllocator>) -> Self {
        RuntimeTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<FetchHandlerTaskFactory<TConnect>>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: FetchHandlerTaskFactory<TConnect>) -> Self {
        DefaultHandlersTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerHttpFetchTaskFactory<TConnect>) -> Self {
        DefaultHandlersTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<GraphQlHandlerWebSocketConnectionTaskFactory>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: GraphQlHandlerWebSocketConnectionTaskFactory) -> Self {
        DefaultHandlersTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<TimeoutHandlerTaskFactory>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: TimeoutHandlerTaskFactory) -> Self {
        DefaultHandlersTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<TimestampHandlerTaskFactory>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: TimestampHandlerTaskFactory) -> Self {
        DefaultHandlersTaskFactory::from(value).into()
    }
}

impl<T, TFactory, TAllocator, TConnect> From<WebSocketGraphQlServerThrottleTimeoutTaskFactory>
    for ServerTaskFactory<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn from(value: WebSocketGraphQlServerThrottleTimeoutTaskFactory) -> Self {
        WebSocketGraphQlServerTaskFactory::from(value).into()
    }
}
