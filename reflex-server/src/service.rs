// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{convert::Infallible, iter::once, marker::PhantomData, sync::Arc, time::Duration};

use futures::{future, stream, Future, FutureExt, Sink, SinkExt, Stream, StreamExt};
use http::{header, HeaderValue, Method, Request, Response, StatusCode};
use hyper::{
    service::{service_fn, Service},
    upgrade::Upgraded,
    Body,
};
use hyper_tungstenite::{
    tungstenite::{Error as TungsteniteError, Message},
    HyperWebsocket, WebSocketStream,
};
use opentelemetry::trace::{Span, Tracer};
use reflex::core::{
    Applicable, Expression, ExpressionFactory, HeapAllocator, InstructionPointer, Reducible,
    Rewritable,
};
use reflex_dispatcher::{
    utils::take_until_final_item::TakeUntilFinalItem, Action, Actor, AsyncScheduler, Handler,
    HandlerContext, Matcher, ProcessId, Redispatcher, SchedulerTransition, TaskFactory,
};
use reflex_graphql::{
    create_json_error_object,
    stdlib::Stdlib as GraphQlStdlib,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage,
    },
    validate::parse_graphql_schema_types,
    GraphQlSchema,
};
use reflex_interpreter::{
    compiler::{Compile, CompiledProgram, CompilerOptions},
    InterpreterOptions,
};
use reflex_json::JsonValue;
use reflex_macros::blanket_trait;
use reflex_runtime::{
    actor::bytecode_interpreter::{
        BytecodeInterpreter, BytecodeInterpreterAction, BytecodeInterpreterMetricLabels,
        BytecodeInterpreterMetricNames,
    },
    task::RuntimeTask,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use reflex_scheduler::tokio::{
    TokioInbox, TokioScheduler, TokioSchedulerBuilder, TokioSchedulerInstrumentation,
    TokioSchedulerLogger, TokioThreadPoolFactory,
};
use reflex_stdlib::Stdlib;
use uuid::Uuid;

use crate::{
    actor::{ServerAction, ServerActor},
    server::{
        action::{
            http_server::{HttpServerRequestAction, HttpServerResponseAction},
            query_inspector_server::{
                QueryInspectorServerHttpRequestAction, QueryInspectorServerHttpResponseAction,
            },
            session_playback_server::{
                SessionPlaybackServerHttpRequestAction, SessionPlaybackServerHttpResponseAction,
            },
            websocket_server::{
                WebSocketServerConnectAction, WebSocketServerDisconnectAction,
                WebSocketServerReceiveAction, WebSocketServerSendAction,
            },
        },
        actor::{
            http_graphql_server::HttpGraphQlServerQueryTransform,
            websocket_graphql_server::WebSocketGraphQlServerQueryTransform,
        },
        playground::handle_playground_http_request,
        task::websocket_graphql_server::WebSocketGraphQlServerTask,
        utils::{
            clone_http_request_wrapper, clone_http_response, clone_http_response_wrapper,
            create_http_response, create_json_http_response, get_cors_headers,
        },
        GraphQlServerOperationMetricLabels, GraphQlServerQueryLabel,
        HttpGraphQlServerQueryMetricLabels, SessionPlaybackServerAction,
        WebSocketGraphQlServerConnectionMetricLabels,
    },
    server_actors,
    utils::server::handle_http_request,
    ServerMetricNames,
};

blanket_trait!(
    pub trait GraphQlWebServerAction<T: Expression>:
        Action
        + ServerAction<T>
        + BytecodeInterpreterAction<T>
        + Matcher<HttpServerResponseAction>
        + Matcher<WebSocketServerSendAction>
        + Matcher<WebSocketServerDisconnectAction>
        + Matcher<QueryInspectorServerHttpResponseAction>
        + From<HttpServerRequestAction>
        + From<HttpServerResponseAction>
        + From<WebSocketServerConnectAction>
        + From<WebSocketServerReceiveAction>
        + From<QueryInspectorServerHttpRequestAction>
    {
    }
);

#[derive(Default, Clone, Copy, Debug)]
pub struct GraphQlWebServerMetricNames {
    pub server: ServerMetricNames,
    pub interpreter: BytecodeInterpreterMetricNames,
}

blanket_trait!(
    pub trait GraphQlWebServerTask<T, TFactory, TAllocator>:
        RuntimeTask<T, TFactory, TAllocator> + WebSocketGraphQlServerTask
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
    {
    }
);

pub trait GraphQlWebServerActor<
    T,
    TFactory,
    TAllocator,
    TTransformHttp,
    TTransformWs,
    TGraphQlQueryLabel,
    THttpMetricLabels,
    TConnectionMetricLabels,
    TOperationMetricLabels,
    TWorkerMetricLabels,
    TTracer,
>:
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
    > + From<BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>>
    + From<Redispatcher> where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TTransformHttp: HttpGraphQlServerQueryTransform + Send + 'static,
    TTransformWs: WebSocketGraphQlServerQueryTransform + Send + 'static,
    TGraphQlQueryLabel: GraphQlServerQueryLabel + Send + 'static,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels + Send + 'static,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels + Send + 'static,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels + Send + 'static,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TTracer: Tracer + Send + 'static,
    TTracer::Span: Span + Send + Sync + 'static,
{
}
impl<
        _Self,
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TWorkerMetricLabels,
        TTracer,
    >
    GraphQlWebServerActor<
        T,
        TFactory,
        TAllocator,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TWorkerMetricLabels,
        TTracer,
    > for _Self
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal<T>: Send,
    T::SignalList<T>: Send,
    T::StructPrototype<T>: Send,
    T::ExpressionList<T>: Send,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TTransformHttp: HttpGraphQlServerQueryTransform + Send + 'static,
    TTransformWs: WebSocketGraphQlServerQueryTransform + Send + 'static,
    TGraphQlQueryLabel: GraphQlServerQueryLabel + Send + 'static,
    THttpMetricLabels: HttpGraphQlServerQueryMetricLabels + Send + 'static,
    TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels + Send + 'static,
    TOperationMetricLabels: GraphQlServerOperationMetricLabels + Send + 'static,
    TWorkerMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TTracer: Tracer + Send + 'static,
    TTracer::Span: Span + Send + Sync + 'static,
    Self: From<
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
        > + From<BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>>
        + From<Redispatcher>,
{
}

pub trait GraphQlWebServerInstrumentation: TokioSchedulerInstrumentation {
    fn instrument_websocket_connection<T: Future + Send + 'static>(
        &self,
        task: T,
    ) -> Self::InstrumentedTask<T>;
}

pub struct GraphQlWebServerActorFactory<
    TAction,
    TTask,
    TLogger,
    TInstrumentation,
    TAsyncTasks,
    TBlockingTasks,
    TFn,
    TActors,
> where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TLogger: TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
    TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
    TFn: for<'a> FnOnce(
        &'a mut GraphQlWebServerInitContext<
            'a,
            TAction,
            TTask,
            TLogger,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
        >,
    ) -> TActors,
    TActors: IntoIterator<Item = (ProcessId, TTask::Actor)> + 'static,
{
    factory: TFn,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
    _logger: PhantomData<TLogger>,
    _instrumentation: PhantomData<TInstrumentation>,
    _async_tasks: PhantomData<TAsyncTasks>,
    _blocking_tasks: PhantomData<TBlockingTasks>,
    _actors: PhantomData<TActors>,
}

impl<TAction, TTask, TLogger, TInstrumentation, TAsyncTasks, TBlockingTasks, TFn, TActors>
    GraphQlWebServerActorFactory<
        TAction,
        TTask,
        TLogger,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
        TFn,
        TActors,
    >
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TLogger: TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
    TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TFn: for<'a> FnOnce(
        &'a mut GraphQlWebServerInitContext<
            'a,
            TAction,
            TTask,
            TLogger,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
        >,
    ) -> TActors,
    TActors: IntoIterator<Item = (ProcessId, TTask::Actor)> + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    pub fn new(factory: TFn) -> Self {
        Self {
            factory,
            _action: PhantomData,
            _task: PhantomData,
            _logger: PhantomData,
            _instrumentation: PhantomData,
            _async_tasks: PhantomData,
            _blocking_tasks: PhantomData,
            _actors: PhantomData,
        }
    }
    fn create<'a>(
        self,
        context: &'a mut GraphQlWebServerInitContext<
            'a,
            TAction,
            TTask,
            TLogger,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
        >,
    ) -> TActors {
        let Self { factory, .. } = self;
        factory(context)
    }
}

pub struct GraphQlWebServerInitContext<
    'a,
    TAction,
    TTask,
    TLogger,
    TInstrumentation,
    TAsyncTasks,
    TBlockingTasks,
> where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TLogger: TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
    TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    builder: &'a mut TokioSchedulerBuilder<
        TAction,
        TTask,
        TLogger,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
    >,
    main_pid: ProcessId,
}
impl<'a, TAction, TTask, TLogger, TInstrumentation, TAsyncTasks, TBlockingTasks> HandlerContext
    for GraphQlWebServerInitContext<
        'a,
        TAction,
        TTask,
        TLogger,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
    >
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TLogger: TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
    TInstrumentation: TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
        + Clone
        + Send
        + Sync
        + 'static,
    TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    fn pid(&self) -> ProcessId {
        self.main_pid
    }
    fn generate_pid(&mut self) -> ProcessId {
        self.builder.generate_pid()
    }
}

pub struct GraphQlWebServer<TAction, TTask>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
{
    runtime: TokioScheduler<TAction, TTask>,
    main_pid: ProcessId,
}
impl<TAction, TTask> GraphQlWebServer<TAction, TTask>
where
    TAction: Action + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
{
    pub fn new<
        T,
        TFactory,
        TAllocator,
        TActorFactory,
        TActors,
        TTransformHttp,
        TTransformWs,
        TGraphQlQueryLabel,
        THttpMetricLabels,
        TConnectionMetricLabels,
        TOperationMetricLabels,
        TWorkerMetricLabels,
        TTracer,
        TLogger,
        TInstrumentation,
        TAsyncTasks,
        TBlockingTasks,
    >(
        graph_root: (CompiledProgram, InstructionPointer),
        schema: Option<GraphQlSchema>,
        custom_actors: GraphQlWebServerActorFactory<
            TAction,
            TTask,
            TLogger,
            TInstrumentation,
            TAsyncTasks,
            TBlockingTasks,
            TActorFactory,
            TActors,
        >,
        compiler_options: CompilerOptions,
        interpreter_options: InterpreterOptions,
        factory: TFactory,
        allocator: TAllocator,
        transform_http: TTransformHttp,
        transform_ws: TTransformWs,
        metric_names: GraphQlWebServerMetricNames,
        get_graphql_query_label: TGraphQlQueryLabel,
        get_http_query_metric_labels: THttpMetricLabels,
        get_websocket_connection_metric_labels: TConnectionMetricLabels,
        get_operation_metric_labels: TOperationMetricLabels,
        get_worker_metric_labels: TWorkerMetricLabels,
        tracer: TTracer,
        logger: TLogger,
        instrumentation: TInstrumentation,
        async_tasks: TAsyncTasks,
        blocking_tasks: TBlockingTasks,
        effect_throttle: Option<Duration>,
    ) -> Result<Self, String>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        T::String: Send,
        T::Builtin: Send,
        T::Signal<T>: Send,
        T::SignalList<T>: Send,
        T::StructPrototype<T>: Send,
        T::ExpressionList<T>: Send,
        T::Builtin: From<Stdlib> + From<GraphQlStdlib> + 'static,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TActorFactory: for<'a> FnOnce(
            &'a mut GraphQlWebServerInitContext<
                'a,
                TAction,
                TTask,
                TLogger,
                TInstrumentation,
                TAsyncTasks,
                TBlockingTasks,
            >,
        ) -> TActors,
        TActors: IntoIterator<Item = (ProcessId, TTask::Actor)> + 'static,
        TTransformHttp: HttpGraphQlServerQueryTransform + Send + 'static,
        TTransformWs: WebSocketGraphQlServerQueryTransform + Send + 'static,
        TGraphQlQueryLabel: GraphQlServerQueryLabel + Send + 'static,
        THttpMetricLabels: HttpGraphQlServerQueryMetricLabels + Send + 'static,
        TConnectionMetricLabels: WebSocketGraphQlServerConnectionMetricLabels + Send + 'static,
        TOperationMetricLabels: GraphQlServerOperationMetricLabels + Send + 'static,
        TWorkerMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
        TTracer: Tracer + Send + 'static,
        TTracer::Span: Span + Send + Sync + 'static,
        TLogger:
            TokioSchedulerLogger<Action = TAction, Task = TTask> + Clone + Send + Sync + 'static,
        TInstrumentation: GraphQlWebServerInstrumentation
            + TokioSchedulerInstrumentation<Action = TAction, Task = TTask>
            + Clone
            + Send
            + Sync
            + 'static,
        TAsyncTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
        TBlockingTasks: TokioThreadPoolFactory<TAction, TTask> + 'static,
        TAction: Action + GraphQlWebServerAction<T> + Send + Sync + 'static,
        TTask: RuntimeTask<T, TFactory, TAllocator> + WebSocketGraphQlServerTask + Send + 'static,
        TTask::Actor: From<
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
            > + From<BytecodeInterpreter<T, TFactory, TAllocator, TWorkerMetricLabels>>
            + From<Redispatcher>,
        TTask::Actor: Send + Sync + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + Sync + 'static,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State:
            Send + 'static,
    {
        let schema_types = schema.map(parse_graphql_schema_types).transpose()?;
        let (runtime, main_pid) = {
            let mut builder =
                TokioSchedulerBuilder::new(logger, instrumentation, async_tasks, blocking_tasks);
            let main_pid = builder.generate_pid();
            // Construct a set of core worker actors with corresponding autogenerated PIDs
            let core_workers = {
                server_actors(
                    schema_types,
                    factory.clone(),
                    allocator.clone(),
                    transform_http,
                    transform_ws,
                    effect_throttle,
                    metric_names.server,
                    get_graphql_query_label,
                    get_http_query_metric_labels,
                    get_websocket_connection_metric_labels,
                    get_operation_metric_labels,
                    tracer,
                    main_pid,
                )
                .into_iter()
                .map(TTask::Actor::from)
            }
            .chain(
                once(BytecodeInterpreter::new(
                    graph_root,
                    compiler_options,
                    interpreter_options,
                    factory,
                    allocator,
                    metric_names.interpreter,
                    get_worker_metric_labels,
                    main_pid,
                ))
                .map(TTask::Actor::from),
            )
            .map(|actor| (builder.generate_pid(), actor))
            .collect::<Vec<_>>();
            let workers = core_workers
                .into_iter()
                .chain(custom_actors.create(&mut GraphQlWebServerInitContext {
                    builder: &mut builder,
                    main_pid,
                }))
                .collect::<Vec<_>>();
            // Register a top-level redispatcher actor to allow broadcasting messages to all core workers
            let worker_pids = workers.iter().map(|(pid, _)| *pid);
            let main_actor = TTask::Actor::from(Redispatcher::new(worker_pids));
            builder.worker(main_pid, main_actor);
            // Register the core workers at their respective PIDs
            for (pid, worker) in workers {
                builder.worker(pid, worker);
            }
            // Fire up the runtime
            let runtime = builder.build();
            (runtime, main_pid)
        };
        Ok(Self { runtime, main_pid })
    }
    pub fn handle_graphql_http_request(
        &self,
        request: Request<Body>,
    ) -> impl Future<Output = Response<Body>>
    where
        TAction: Action
            + Matcher<HttpServerResponseAction>
            + From<HttpServerRequestAction>
            + From<HttpServerResponseAction>
            + Send
            + Sync
            + 'static,
        TTask: TaskFactory<TAction, TTask> + Send + 'static,
        TTask::Actor: Send + Sync + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
        <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + Sync + 'static,
        <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State:
            Send + 'static,
    {
        handle_graphql_http_request(request, &self.runtime, self.main_pid)
    }
    pub fn main_pid(&self) -> ProcessId {
        self.main_pid
    }
}

impl<TAction, TTask> AsyncScheduler for GraphQlWebServer<TAction, TTask>
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
    TTask::Actor: Send + Sync + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Events<TokioInbox<TAction>>: Send + 'static,
    <TTask::Actor as Actor<TAction, TTask>>::Dispose: Send + Sync + 'static,
    <TTask::Actor as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State: Send + 'static,
{
    type Action = <TokioScheduler<TAction, TTask> as AsyncScheduler>::Action;
    type Sink = <TokioScheduler<TAction, TTask> as AsyncScheduler>::Sink;
    type Subscription<F, V> = <TokioScheduler<TAction, TTask> as AsyncScheduler>::Subscription<F, V>
        where
            F: Fn(&Self::Action) -> Option<V>,
            V: Send + 'static;
    type SubscriptionResults<F, V> = <TokioScheduler<TAction, TTask> as AsyncScheduler>::SubscriptionResults<F, V>
        where
            F: Fn(&Self::Action) -> Option<V>,
            V: Send + 'static;
    fn actions(&self, pid: ProcessId) -> Self::Sink {
        self.runtime.actions(pid)
    }
    fn subscribe<F, V>(&self, pid: ProcessId, selector: F) -> Self::Subscription<F, V>
    where
        F: Fn(&Self::Action) -> Option<V> + Send + Sync + 'static,
        V: Send + Sync + 'static,
    {
        self.runtime.subscribe(pid, selector)
    }
}

pub fn graphql_service<TAction>(
    runtime: Arc<impl AsyncScheduler<Action = TAction> + Send + Sync + 'static>,
    server_pid: ProcessId,
    instrumentation: impl GraphQlWebServerInstrumentation + Clone + Send + 'static,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    TAction: Action
        + Matcher<HttpServerResponseAction>
        + Matcher<WebSocketServerSendAction>
        + Matcher<WebSocketServerDisconnectAction>
        + From<HttpServerRequestAction>
        + From<HttpServerResponseAction>
        + From<WebSocketServerConnectAction>
        + From<WebSocketServerSendAction>
        + From<WebSocketServerReceiveAction>
        + Send
        + Sync
        + 'static,
{
    service_fn({
        move |req: Request<Body>| {
            let runtime = runtime.clone();
            let instrumentation = instrumentation.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::POST => handle_graphql_http_request(req, &*runtime, server_pid).await,
                    &Method::GET => {
                        if hyper_tungstenite::is_upgrade_request(&req) {
                            match handle_graphql_websocket_request(req, &*runtime, server_pid).await
                            {
                                Err(response) => response,
                                Ok((response, listen_task)) => {
                                    let _ = tokio::spawn(
                                        instrumentation
                                            .instrument_websocket_connection(listen_task),
                                    );
                                    response
                                }
                            }
                        } else {
                            handle_playground_http_request(req).await
                        }
                    }
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => method_not_allowed(req),
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

pub fn query_inspector_service<TAction>(
    runtime: Arc<impl AsyncScheduler<Action = TAction> + Send + Sync>,
    server_pid: ProcessId,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    TAction: Action
        + Matcher<QueryInspectorServerHttpResponseAction>
        + From<QueryInspectorServerHttpRequestAction>
        + From<QueryInspectorServerHttpResponseAction>
        + Send
        + Sync
        + 'static,
{
    service_fn({
        move |req: Request<Body>| {
            let runtime = runtime.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => handle_query_inspector_http_request(req, &*runtime, server_pid).await,
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

pub fn session_playback_service<T, TAction>(
    runtime: Arc<impl AsyncScheduler<Action = TAction> + Send + Sync>,
    server_pid: ProcessId,
) -> impl Service<
    Request<Body>,
    Response = Response<Body>,
    Error = Infallible,
    Future = impl Future<Output = Result<Response<Body>, Infallible>> + Send,
>
where
    T: Expression,
    TAction: Action + SessionPlaybackServerAction<T> + Send + Sync + 'static,
{
    service_fn({
        move |req: Request<Body>| {
            let runtime = runtime.clone();
            async move {
                let cors_headers = get_cors_headers(&req).into_iter().collect::<Vec<_>>();
                let mut response = match req.method() {
                    &Method::OPTIONS => handle_cors_preflight_request(req),
                    _ => handle_session_playback_http_request(req, &*runtime, server_pid).await,
                };
                response.headers_mut().extend(cors_headers);
                Ok(response)
            }
        }
    })
}

fn handle_graphql_http_request<TAction>(
    request: Request<Body>,
    runtime: &impl AsyncScheduler<Action = TAction>,
    server_pid: ProcessId,
) -> impl Future<Output = Response<Body>>
where
    TAction: Action
        + Matcher<HttpServerResponseAction>
        + From<HttpServerRequestAction>
        + From<HttpServerResponseAction>
        + Send
        + Sync
        + 'static,
{
    handle_http_request(
        request,
        runtime,
        server_pid,
        |request_id, request| {
            HttpServerRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            HttpServerResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let HttpServerResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response_wrapper(response).map(|_| response.body().clone()))
        },
    )
}

fn handle_graphql_websocket_request<TAction>(
    request: Request<Body>,
    runtime: &impl AsyncScheduler<Action = TAction>,
    server_pid: ProcessId,
) -> impl Future<Output = Result<(Response<Body>, impl Future<Output = ()>), Response<Body>>>
where
    TAction: Action
        + Matcher<HttpServerResponseAction>
        + Matcher<WebSocketServerSendAction>
        + Matcher<WebSocketServerDisconnectAction>
        + From<HttpServerResponseAction>
        + From<WebSocketServerConnectAction>
        + From<WebSocketServerSendAction>
        + From<WebSocketServerReceiveAction>
        + Send
        + Sync
        + 'static,
{
    let connection_id = Uuid::new_v4();
    let request_headers = clone_http_request_wrapper(&request);
    match upgrade_websocket(request, "graphql-ws") {
        Err(response) => future::ready(Err(response)).left_future(),
        Ok((response, connection)) => future::ready(Ok((
            response,
            connection.then({
                let subscribe_websocket_responses =
                    create_websocket_response_stream(runtime, connection_id, server_pid);
                let mut commands = runtime.actions(server_pid);
                move |connection| match connection {
                    Err(err) => {
                        let action = TAction::from(WebSocketServerSendAction {
                            connection_id,
                            message: GraphQlSubscriptionServerMessage::ConnectionError(
                                create_json_error_object(
                                    format!("Failed to initiate WebSocket connection: {}", err),
                                    None,
                                ),
                            ),
                        });
                        async move {
                            let _ = commands.send(action);
                        }
                    }
                    .left_future(),
                    Ok(upgraded_socket) => {
                        let actions = create_websocket_action_stream(
                            WebSocketServerConnectAction {
                                connection_id,
                                request: request_headers,
                            },
                            upgraded_socket,
                            subscribe_websocket_responses,
                        )
                        .map(move |action| action);
                        pipe_stream(actions, commands).right_future()
                    }
                }
            }),
        )))
        .right_future(),
    }
}

fn create_websocket_response_stream<TAction>(
    runtime: &impl AsyncScheduler<Action = TAction>,
    connection_id: Uuid,
    server_pid: ProcessId,
) -> impl Future<Output = impl Stream<Item = Message>>
where
    TAction: Action
        + Matcher<WebSocketServerSendAction>
        + Matcher<WebSocketServerDisconnectAction>
        + Send
        + Sync
        + 'static,
{
    runtime
        .subscribe(server_pid, move |action: &TAction| {
            if let Some(action) = action.match_type() {
                let WebSocketServerSendAction {
                    connection_id: emitted_connection_id,
                    message,
                } = action;
                if *emitted_connection_id != connection_id {
                    return None;
                }
                let message = message.clone().into_json();
                Some(Message::Text(message.to_string()))
            } else if let Some(action) = action.match_type() {
                let WebSocketServerDisconnectAction {
                    connection_id: emitted_connection_id,
                } = action;
                if *emitted_connection_id != connection_id {
                    return None;
                }
                Some(Message::Close(None))
            } else {
                None
            }
        })
        .map(|stream| {
            TakeUntilFinalItem::new(stream, |message| matches!(message, &Message::Close(_)))
        })
}

fn create_websocket_action_stream<TAction>(
    connect_action: WebSocketServerConnectAction,
    upgraded_socket: WebSocketStream<Upgraded>,
    subscribe_websocket_responses: impl Future<Output = impl Stream<Item = Message>>,
) -> impl Stream<Item = TAction>
where
    TAction: Action
        + From<WebSocketServerConnectAction>
        + From<WebSocketServerSendAction>
        + From<WebSocketServerReceiveAction>,
{
    let connection_id = connect_action.connection_id;
    let connect_action = stream::iter(once(connect_action.into()));
    let message_actions = subscribe_websocket_responses
        .map(move |response_stream| {
            let (websocket_tx, websocket_rx) = upgraded_socket.split();
            let request_stream = websocket_rx
                .filter_map(|message| {
                    let parsed_message = message
                        .or_else(|err| match err {
                            TungsteniteError::ConnectionClosed => Ok(Message::Close(None)),
                            err => Err(format!("{}", err)),
                        })
                        .and_then(parse_websocket_message)
                        .transpose();
                    future::ready(parsed_message)
                })
                .chain(stream::iter(once(Ok(
                    GraphQlSubscriptionClientMessage::ConnectionTerminate,
                ))))
                .map(move |parsed_message| match parsed_message {
                    Err(err) => WebSocketServerSendAction {
                        connection_id,
                        message: GraphQlSubscriptionServerMessage::ConnectionError(
                            JsonValue::from(err),
                        ),
                    }
                    .into(),
                    Ok(message) => WebSocketServerReceiveAction {
                        connection_id,
                        message,
                    }
                    .into(),
                });
            let response_stream =
                ignore_stream_results(pipe_stream(response_stream, websocket_tx).into_stream());
            let duplex_stream = stream::select(request_stream, response_stream);
            duplex_stream
        })
        .into_stream()
        .flatten();
    connect_action.chain(message_actions)
}

fn handle_query_inspector_http_request<TAction>(
    request: Request<Body>,
    runtime: &impl AsyncScheduler<Action = TAction>,
    server_pid: ProcessId,
) -> impl Future<Output = Response<Body>>
where
    TAction: Action
        + Matcher<QueryInspectorServerHttpResponseAction>
        + From<QueryInspectorServerHttpRequestAction>
        + From<QueryInspectorServerHttpResponseAction>
        + Send
        + Sync
        + 'static,
{
    handle_http_request(
        request,
        runtime,
        server_pid,
        |request_id, request| {
            QueryInspectorServerHttpRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            QueryInspectorServerHttpResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let QueryInspectorServerHttpResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response(response))
        },
    )
}

fn handle_session_playback_http_request<TAction>(
    request: Request<Body>,
    runtime: &impl AsyncScheduler<Action = TAction>,
    server_pid: ProcessId,
) -> impl Future<Output = Response<Body>>
where
    TAction: Action
        + Matcher<SessionPlaybackServerHttpResponseAction>
        + From<SessionPlaybackServerHttpRequestAction>
        + From<SessionPlaybackServerHttpResponseAction>
        + Send
        + Sync
        + 'static,
{
    handle_http_request(
        request,
        runtime,
        server_pid,
        |request_id, request| {
            SessionPlaybackServerHttpRequestAction {
                request_id,
                request,
            }
            .into()
        },
        |request_id, response| {
            SessionPlaybackServerHttpResponseAction {
                request_id,
                response,
            }
            .into()
        },
        |request_id, action| {
            let SessionPlaybackServerHttpResponseAction {
                request_id: response_id,
                response,
            } = action.match_type()?;
            if *response_id != request_id {
                return None;
            }
            Some(clone_http_response(response))
        },
    )
}

fn handle_cors_preflight_request<T: From<String> + Default>(_req: Request<T>) -> Response<T> {
    create_http_response(StatusCode::NO_CONTENT, None, None)
}

fn method_not_allowed<T: From<String> + Default>(_req: Request<T>) -> Response<T> {
    create_http_response(
        StatusCode::METHOD_NOT_ALLOWED,
        None,
        Some(format!("{}", StatusCode::METHOD_NOT_ALLOWED)),
    )
}

fn upgrade_websocket(
    request: Request<Body>,
    protocol: &'static str,
) -> Result<(Response<Body>, HyperWebsocket), Response<Body>> {
    match hyper_tungstenite::upgrade(request, None) {
        Err(err) => Err(create_json_error_message_response(
            StatusCode::BAD_REQUEST,
            format!("{}", err),
        )),
        Ok((mut response, websocket)) => {
            response.headers_mut().insert(
                header::SEC_WEBSOCKET_PROTOCOL,
                HeaderValue::from_static(protocol),
            );
            Ok((response, websocket))
        }
    }
}

fn parse_websocket_message(
    message: Message,
) -> Result<Option<GraphQlSubscriptionClientMessage>, String> {
    match message {
        Message::Text(data) => deserialize_graphql_client_message(&data).map(Some),
        Message::Binary(_) => Err(String::from("Unsupported message encoding")),
        Message::Close(_) => Ok(Some(GraphQlSubscriptionClientMessage::ConnectionTerminate)),
        Message::Ping(_) | Message::Pong(_) | Message::Frame(_) => Ok(None),
    }
}

fn create_json_error_message_response<T: From<String> + Default>(
    status_code: StatusCode,
    message: String,
) -> Response<T> {
    create_json_http_response(status_code, None, &JsonValue::String(message))
}

fn pipe_stream<T>(
    stream: impl Stream<Item = T>,
    sink: impl Sink<T> + Unpin,
) -> impl Future<Output = ()> {
    stream
        .fold(sink, |mut sink, value| async move {
            let _ = sink.send(value).await;
            sink
        })
        .map(|_| ())
}

fn ignore_stream_results<T, V>(stream: impl Stream<Item = T>) -> impl Stream<Item = V> {
    stream.flat_map(|_| stream::empty())
}
