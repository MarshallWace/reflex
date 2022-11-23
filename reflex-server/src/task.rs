// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{Applicable, Expression, Reducible, Rewritable};
use reflex_dispatcher::{Action, TaskFactory};
use reflex_handlers::task::fetch::FetchHandlerTaskFactory;
use reflex_handlers::task::graphql::{
    GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory,
};
use reflex_handlers::task::timeout::TimeoutHandlerTaskFactory;
use reflex_handlers::task::timestamp::TimestampHandlerTaskFactory;
use reflex_handlers::task::{
    DefaultHandlersTask, DefaultHandlersTaskAction, DefaultHandlersTaskFactory,
};
use reflex_interpreter::compiler::Compile;
use reflex_macros::{blanket_trait, task_factory_enum};
use reflex_runtime::task::bytecode_worker::BytecodeWorkerTaskFactory;
use reflex_runtime::{
    task::{RuntimeTask, RuntimeTaskAction, RuntimeTaskFactory},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::server::task::websocket_graphql_server::{
    WebSocketGraphQlServerTask, WebSocketGraphQlServerTaskAction,
    WebSocketGraphQlServerTaskFactory, WebSocketGraphQlServerThrottleTimeoutTaskFactory,
};

blanket_trait!(
    pub trait ServerTaskAction<T: Expression>:
        RuntimeTaskAction<T> + DefaultHandlersTaskAction + WebSocketGraphQlServerTaskAction
    {
    }
);

blanket_trait!(
    pub trait ServerTask<T, TFactory, TAllocator, TConnect>:
        RuntimeTask<T, TFactory, TAllocator>
        + DefaultHandlersTask<TConnect>
        + WebSocketGraphQlServerTask
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

task_factory_enum!({
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
    }
});

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
