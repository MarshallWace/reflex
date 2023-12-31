// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    fs,
    iter::once,
    marker::PhantomData,
    ops::Deref,
    path::{Path, PathBuf},
    pin::Pin,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use futures::{Future, Stream, StreamExt};
use metrics::SharedString;
use pin_project::pin_project;
use reflex::{
    cache::SubstitutionCache,
    core::{
        Applicable, ConditionType, Expression, ExpressionFactory, HeapAllocator,
        InstructionPointer, Reducible, Rewritable, StateCache,
    },
    env::inject_env_vars,
};
use reflex_cli::{
    builtins::CliBuiltins, create_parser, format_signal_result, repl, Syntax, SyntaxParser,
};
use reflex_dispatcher::{
    Action, Actor, ActorEvents, AsyncScheduler, Handler, HandlerContext, Matcher, MessageData,
    Named, ProcessId, Redispatcher, SchedulerMode, SchedulerTransition, SerializableAction,
    SerializedAction, TaskFactory, TaskInbox, Worker,
};
use reflex_grpc::{
    action::*,
    actor::{GrpcHandler, GrpcHandlerAction, GrpcHandlerMetricNames, GrpcHandlerTask},
    load_grpc_services,
    task::{GrpcHandlerConnectionTaskAction, GrpcHandlerConnectionTaskFactory},
    DefaultGrpcConfig, GrpcConfig,
};
use reflex_handlers::{
    action::{
        fetch::{
            FetchHandlerActions, FetchHandlerConnectionErrorAction, FetchHandlerFetchCompleteAction,
        },
        graphql::*,
        timeout::{TimeoutHandlerActions, TimeoutHandlerTimeoutAction},
        timestamp::{TimestampHandlerActions, TimestampHandlerUpdateAction},
    },
    actor::{HandlerAction, HandlerActor, HandlerTask},
    default_handler_actors, hyper,
    task::{
        fetch::FetchHandlerTaskFactory,
        graphql::{
            GraphQlHandlerHttpFetchTaskFactory, GraphQlHandlerWebSocketConnectionTaskFactory,
        },
        timeout::TimeoutHandlerTaskFactory,
        timestamp::TimestampHandlerTaskFactory,
        DefaultHandlersTaskAction, DefaultHandlersTaskFactory,
    },
    utils::tls::{create_https_client, hyper_rustls},
    DefaultHandlerMetricNames,
};
use reflex_interpreter::{
    compiler::{hash_compiled_program, Compile, Compiler, CompilerMode, CompilerOptions},
    execute, DefaultInterpreterCache, InterpreterOptions,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_lang::{allocator::DefaultAllocator, CachedSharedTerm, SharedTermFactory};
use reflex_macros::{blanket_trait, task_factory_enum, Matcher, Named};
use reflex_protobuf::types::WellKnownTypesTranscoder;
use reflex_runtime::{
    action::{bytecode_interpreter::*, effect::*, evaluate::*, query::*, RuntimeActions},
    actor::{
        bytecode_interpreter::{
            BytecodeInterpreter, BytecodeInterpreterAction, BytecodeInterpreterMetricLabels,
            BytecodeInterpreterMetricNames,
        },
        evaluate_handler::{
            create_evaluate_effect, create_evaluate_effect_type, is_evaluate_effect_type,
            parse_evaluate_effect_result,
        },
        RuntimeAction, RuntimeActor, RuntimeMetricNames,
    },
    runtime_actors,
    task::{
        bytecode_worker::BytecodeWorkerTaskFactory, evaluate_handler::EffectThrottleTaskFactory,
        RuntimeTask, RuntimeTaskAction, RuntimeTaskFactory,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy,
};
use reflex_scheduler::threadpool::TokioRuntimeThreadPoolFactory;
use reflex_scheduler::tokio::{
    AsyncMessage, NoopTokioSchedulerInstrumentation, TokioCommand, TokioSchedulerBuilder,
    TokioSchedulerLogger,
};
use reflex_utils::reconnect::{NoopReconnectTimeout, ReconnectTimeout};

/// Reflex runtime evaluator
#[derive(Parser)]
struct Args {
    /// Optional entry point module to evaluate (defaults to REPL)
    entry_point: Option<PathBuf>,
    /// Entry point syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Path to custom TLS certificate
    #[clap(long)]
    tls_cert: Option<PathBuf>,
    /// Paths of compiled gRPC service definition protobufs
    #[clap(long)]
    grpc_service: Vec<PathBuf>,
    /// Throttle stateful effect updates
    #[clap(long)]
    effect_throttle_ms: Option<u64>,
    /// Log runtime actions
    #[clap(long)]
    log: bool,
    /// Prevent static optimizations
    #[clap(long)]
    unoptimized: bool,
    /// Add debug printing of bytecode output from compiler
    #[clap(long)]
    debug_compiler: bool,
    /// Add debug printing of bytecode execution
    #[clap(long)]
    debug_interpreter: bool,
    /// Add debug printing of stack during execution
    #[clap(long)]
    debug_stack: bool,
}

#[tokio::main]
pub async fn main() -> Result<()> {
    type TBuiltin = CliBuiltins;
    type T = CachedSharedTerm<TBuiltin>;
    type TFactory = SharedTermFactory<TBuiltin>;
    type TAllocator = DefaultAllocator<T>;
    type TConnect = hyper_rustls::HttpsConnector<hyper::client::HttpConnector>;
    type TReconnect = NoopReconnectTimeout;
    type TGrpcConfig = DefaultGrpcConfig;
    type TAction = CliActions<T>;
    type TMetricLabels = CliMetricLabels;
    type TTask =
        CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>;
    type TInstrumentation = NoopTokioSchedulerInstrumentation<TAction, TTask>;
    let args = Args::parse();
    let unoptimized = args.unoptimized;
    let debug_actions = args.log;
    let debug_compiler = args.debug_compiler;
    let debug_interpreter = args.debug_interpreter;
    let debug_stack = args.debug_stack;
    let effect_throttle = args.effect_throttle_ms.map(Duration::from_millis);
    let factory: TFactory = SharedTermFactory::<TBuiltin>::default();
    let allocator: TAllocator = DefaultAllocator::default();
    let input_path = args.entry_point;
    let syntax = args.syntax;
    let https_client: hyper::Client<TConnect> = create_https_client(None)?;
    let grpc_services = load_grpc_services(args.grpc_service.iter())
        .with_context(|| "Failed to load gRPC service descriptor")?;
    let grpc_config = DefaultGrpcConfig::default();
    let grpc_max_operations_per_connection =
        match std::env::var("GRPC_MAX_OPERATIONS_PER_CONNECTION") {
            Ok(value) => str::parse::<usize>(&value)
                .with_context(|| "Invalid value for GRPC_MAX_OPERATIONS_PER_CONNECTION")
                .map(Some),
            _ => Ok(None),
        }?;
    match input_path {
        None => {
            let state = StateCache::default();
            let mut cache = SubstitutionCache::new();
            let parser = create_parser(syntax, None, &factory, &allocator);
            repl::run(parser, &state, &factory, &allocator, &mut cache)?;
        }
        Some(input_path) => {
            let compiler_options = CompilerOptions {
                debug: debug_compiler,
                ..if unoptimized {
                    CompilerOptions::unoptimized()
                } else {
                    CompilerOptions::default()
                }
            };
            let interpreter_options = InterpreterOptions {
                debug_instructions: debug_interpreter || debug_stack,
                debug_stack,
                ..InterpreterOptions::default()
            };
            let source = read_file(&input_path)?;
            let parser = create_parser(syntax, Some(&input_path), &factory, &allocator);
            let expression = parser
                .parse(&source)
                .map(|expression| {
                    inject_env_vars(expression, std::env::vars(), &factory, &allocator)
                })
                .map_err(|err| {
                    anyhow!(
                        "Failed to parse source at {}: {}",
                        input_path.display(),
                        err
                    )
                })?;
            let program = Compiler::new(compiler_options, None)
                .compile(&expression, CompilerMode::Function, &factory, &allocator)
                .map_err(|err| {
                    anyhow!(
                        "Failed to compile source at {}: {}",
                        input_path.display(),
                        err
                    )
                })?;
            let entry_point = InstructionPointer::default();
            let state = StateCache::default();
            let state_id = 0;
            let mut interpreter_cache = DefaultInterpreterCache::default();
            let cache_key = hash_compiled_program(&program, &entry_point);

            let (result, _cache_entries) = execute(
                cache_key,
                &program,
                entry_point,
                state_id,
                &state,
                &factory,
                &allocator,
                &interpreter_options,
                &mut interpreter_cache,
            )
            .map_err(|err| {
                anyhow!(
                    "Failed to execute program at {}: {}",
                    input_path.display(),
                    err
                )
            })?;
            let (output, dependencies) = result.into_parts();
            if dependencies.is_empty() {
                println!("{}", output);
            } else {
                let graph_root = (program, entry_point);
                let (evaluate_effect, subscribe_action) =
                    create_query(expression, &factory, &allocator);
                let logger = if debug_actions {
                    Some(CliActionLogger::stderr())
                } else {
                    None
                };
                let instrumentation = NoopTokioSchedulerInstrumentation::default();
                let async_tasks =
                    TokioRuntimeThreadPoolFactory::new(tokio::runtime::Handle::current());
                let blocking_tasks =
                    TokioRuntimeThreadPoolFactory::new(tokio::runtime::Handle::current());
                let (scheduler, main_pid) = {
                    let mut builder = TokioSchedulerBuilder::<TAction, TTask, _, _, _, _>::new(
                        logger,
                        instrumentation,
                        async_tasks,
                        blocking_tasks,
                    );
                    let main_pid = builder.generate_pid();
                    let actors = {
                        runtime_actors(
                            factory.clone(),
                            allocator.clone(),
                            effect_throttle,
                            RuntimeMetricNames::default(),
                            main_pid,
                        )
                        .into_iter()
                        .map(CliActor::Runtime)
                    }
                    .chain(once(CliActor::BytecodeInterpreter(
                        BytecodeInterpreter::new(
                            graph_root,
                            compiler_options,
                            interpreter_options,
                            factory.clone(),
                            allocator.clone(),
                            BytecodeInterpreterMetricNames::default(),
                            CliMetricLabels,
                            main_pid,
                        ),
                    )))
                    .chain(
                        default_handler_actors::<
                            TAction,
                            TTask,
                            T,
                            TFactory,
                            TAllocator,
                            TConnect,
                            TReconnect,
                        >(
                            https_client,
                            &factory,
                            &allocator,
                            NoopReconnectTimeout,
                            DefaultHandlerMetricNames::default(),
                            main_pid,
                        )
                        .into_iter()
                        .map(|actor| CliActor::Handler(actor)),
                    )
                    .chain(once(CliActor::Grpc(GrpcHandler::new(
                        grpc_services,
                        WellKnownTypesTranscoder,
                        factory.clone(),
                        allocator.clone(),
                        NoopReconnectTimeout,
                        grpc_max_operations_per_connection,
                        grpc_config,
                        GrpcHandlerMetricNames::default(),
                        main_pid,
                    ))))
                    .map(|actor| (builder.generate_pid(), actor))
                    .collect::<Vec<_>>();
                    let actor_pids = actors.iter().map(|(pid, _)| *pid);
                    builder.worker(main_pid, CliActor::Main(Redispatcher::new(actor_pids)));
                    for (pid, actor) in actors {
                        builder.worker(pid, actor);
                    }
                    builder.send(main_pid, TAction::from(subscribe_action));
                    let runtime = builder.build();
                    (runtime, main_pid)
                };
                let mut results_stream = tokio::spawn(scheduler.subscribe(main_pid, {
                    let factory = factory.clone();
                    move |action: &CliActions<CachedSharedTerm<CliBuiltins>>| {
                        let EffectEmitAction { effect_types } = action.match_type()?;
                        let update = effect_types
                            .iter()
                            .filter(|batch| is_evaluate_effect_type(&batch.effect_type, &factory))
                            .flat_map(|batch| batch.updates.iter())
                            .filter(|(key, _)| *key == evaluate_effect.id())
                            .filter_map({
                                let factory = factory.clone();
                                move |(_, value)| parse_evaluate_effect_result(value, &factory)
                            })
                            .next()?;
                        Some(update.result().clone())
                    }
                }))
                .await
                .unwrap();
                while let Some(value) = results_stream.next().await {
                    let output = match factory.match_signal_term(&value) {
                        None => format!("{}", value),
                        Some(signal) => format_signal_result(signal),
                    };
                    println!("{}{}", clear_escape_sequence(), output);
                }
            }
        }
    }
    Ok(())
}

fn create_query<
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
>(
    query: T,
    factory: &TFactory,
    allocator: &TAllocator,
) -> (T::Signal, EffectSubscribeAction<T>) {
    let evaluate_effect = create_evaluate_effect(
        String::from("<anonymous>"),
        query,
        QueryEvaluationMode::Standalone,
        QueryInvalidationStrategy::Exact,
        factory,
        allocator,
    );
    let subscribe_action = EffectSubscribeAction {
        effect_type: create_evaluate_effect_type(factory, allocator),
        effects: vec![evaluate_effect.clone()],
    };
    (evaluate_effect, subscribe_action)
}

struct CliMetricLabels;
impl BytecodeInterpreterMetricLabels for CliMetricLabels {
    fn labels(&self, query_name: &str) -> Vec<(SharedString, SharedString)> {
        vec![("worker".into(), String::from(query_name).into())]
    }
}

pub struct CliActionLogger<
    TOut: std::io::Write,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
> {
    output: TOut,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TOut: std::io::Write, TAction: Action, TTask: TaskFactory<TAction, TTask>>
    CliActionLogger<TOut, TAction, TTask>
{
    pub fn new(output: TOut) -> Self {
        Self {
            output,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
    fn log(&mut self, action: &impl SerializableAction) {
        let serialized_args = JsonValue::Object(JsonMap::from_iter(action.to_json()));
        let _ = writeln!(
            self.output,
            "[{}] {}",
            action.name(),
            serialized_args.to_string()
        );
    }
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>>
    CliActionLogger<std::io::Stderr, TAction, TTask>
{
    pub fn stderr() -> Self {
        Self::new(std::io::stderr())
    }
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>> Clone
    for CliActionLogger<std::io::Stderr, TAction, TTask>
{
    fn clone(&self) -> Self {
        Self::new(std::io::stderr())
    }
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>>
    CliActionLogger<std::io::Stdout, TAction, TTask>
{
    pub fn stdout() -> Self {
        Self::new(std::io::stdout())
    }
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>> Clone
    for CliActionLogger<std::io::Stdout, TAction, TTask>
{
    fn clone(&self) -> Self {
        Self::new(std::io::stdout())
    }
}
impl<TOut: std::io::Write, TAction: Action, TTask: TaskFactory<TAction, TTask>> TokioSchedulerLogger
    for CliActionLogger<TOut, TAction, TTask>
where
    TAction: SerializableAction,
{
    type Action = TAction;
    type Task = TTask;
    fn log_scheduler_command(
        &mut self,
        command: &TokioCommand<Self::Action, Self::Task>,
        _enqueue_time: Instant,
    ) {
        match command {
            TokioCommand::Send { pid: _, message } if message.redispatched_from().is_none() => {
                let action = message.deref();
                self.log(action);
            }
            _ => {}
        }
    }
    fn log_worker_message(
        &mut self,
        _message: &AsyncMessage<Self::Action>,
        _actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        _pid: ProcessId,
    ) {
    }
    fn log_task_message(&mut self, _message: &AsyncMessage<Self::Action>, _pid: ProcessId) {}
}

fn read_file(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("Failed to read path {}", path.display()))
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum CliActions<T: Expression> {
    Runtime(RuntimeActions<T>),
    BytecodeInterpreter(BytecodeInterpreterActions<T>),
    FetchHandler(FetchHandlerActions),
    GraphQlHandler(GraphQlHandlerActions),
    TimeoutHandler(TimeoutHandlerActions),
    TimestampHandler(TimestampHandlerActions),
    GrpcHandler(GrpcHandlerActions),
}
impl<T: Expression> Named for CliActions<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(action) => action.name(),
            Self::BytecodeInterpreter(action) => action.name(),
            Self::FetchHandler(action) => action.name(),
            Self::GraphQlHandler(action) => action.name(),
            Self::TimeoutHandler(action) => action.name(),
            Self::TimestampHandler(action) => action.name(),
            Self::GrpcHandler(action) => action.name(),
        }
    }
}
impl<T: Expression> Action for CliActions<T> {}
impl<T: Expression> SerializableAction for CliActions<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Runtime(action) => action.to_json(),
            Self::BytecodeInterpreter(action) => action.to_json(),
            Self::FetchHandler(action) => action.to_json(),
            Self::GraphQlHandler(action) => action.to_json(),
            Self::TimeoutHandler(action) => action.to_json(),
            Self::TimestampHandler(action) => action.to_json(),
            Self::GrpcHandler(action) => action.to_json(),
        }
    }
}

blanket_trait!(
    trait CliAction<T: Expression>:
        SerializableAction
        + RuntimeAction<T>
        + HandlerAction<T>
        + GrpcHandlerAction<T>
        + BytecodeInterpreterAction<T>
        + CliTaskAction<T>
    {
    }
);

blanket_trait!(
    trait CliTask<T, TFactory, TAllocator, TConnect>:
        RuntimeTask<T, TFactory, TAllocator>
        + HandlerTask<TConnect>
        + GrpcHandlerTask<T, TFactory, TAllocator, WellKnownTypesTranscoder>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
    }
);

enum CliActor<
    T,
    TFactory,
    TAllocator,
    TConnect,
    TReconnect,
    TGrpcConfig,
    TMetricLabels,
    TAction,
    TTask,
> where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    Runtime(RuntimeActor<T, TFactory, TAllocator>),
    Handler(HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect>),
    Grpc(GrpcHandler<T, TFactory, TAllocator, WellKnownTypesTranscoder, TGrpcConfig, TReconnect>),
    BytecodeInterpreter(BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels>),
    Main(Redispatcher),
    Task(CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask>),
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction, TTask>
    Named
    for CliActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliTaskAction<T> + Send + 'static,
    TTask: TaskFactory<TAction, TTask>,
{
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(inner) => inner.name(),
            Self::Handler(inner) => inner.name(),
            Self::Grpc(inner) => inner.name(),
            Self::BytecodeInterpreter(inner) => inner.name(),
            Self::Main(inner) => inner.name(),
            Self::Task(inner) => inner.name(),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction, TTask>
    Actor<TAction, TTask>
    for CliActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    type Events<TInbox: TaskInbox<TAction>> = CliEvents<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TInbox,
        TAction,
        TTask,
    >;
    type Dispose = CliDispose<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >;
    fn init(&self) -> Self::State {
        match self {
            Self::Runtime(actor) => {
                CliActorState::Runtime(<RuntimeActor<T, TFactory, TAllocator> as Actor<
                    TAction,
                    TTask,
                >>::init(actor))
            }
            Self::Handler(actor) => {
                CliActorState::Handler(
                    <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                        TAction,
                        TTask,
                    >>::init(actor),
                )
            }
            Self::Grpc(actor) => {
                CliActorState::Grpc(<GrpcHandler<
                    T,
                    TFactory,
                    TAllocator,
                    WellKnownTypesTranscoder,
                    TGrpcConfig,
                    TReconnect,
                > as Actor<TAction, TTask>>::init(actor))
            }
            Self::BytecodeInterpreter(actor) => {
                CliActorState::BytecodeInterpreter(<BytecodeInterpreter<
                    T,
                    TFactory,
                    TAllocator,
                    TMetricLabels,
                > as Actor<TAction, TTask>>::init(
                    actor
                ))
            }
            Self::Main(actor) => {
                CliActorState::Main(<Redispatcher as Actor<TAction, TTask>>::init(actor))
            }
            Self::Task(actor) => {
                CliActorState::Task(<CliTaskActor<
                    T,
                    TFactory,
                    TAllocator,
                    TConnect,
                    TAction,
                    TTask,
                > as Actor<TAction, TTask>>::init(actor))
            }
        }
    }
    fn events<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
    ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
        match self {
            Self::Runtime(actor) => <RuntimeActor<T, TFactory, TAllocator> as Actor<
                TAction,
                TTask,
            >>::events(actor, inbox)
            .map(|(events, dispose)| {
                (CliEvents::Runtime(events), dispose.map(CliDispose::Runtime))
            }),
            Self::Handler(actor) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                    TAction,
                    TTask,
                >>::events(actor, inbox)
                .map(|(events, dispose)| {
                    (CliEvents::Handler(events), dispose.map(CliDispose::Handler))
                })
            }
            Self::Grpc(actor) => <GrpcHandler<
                T,
                TFactory,
                TAllocator,
                WellKnownTypesTranscoder,
                TGrpcConfig,
                TReconnect,
            > as Actor<TAction, TTask>>::events(actor, inbox)
            .map(|(events, dispose)| (CliEvents::Grpc(events), dispose.map(CliDispose::Grpc))),
            Self::BytecodeInterpreter(actor) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Actor<
                    TAction,
                    TTask,
                >>::events(actor, inbox)
                .map(|(events, dispose)| {
                    (
                        CliEvents::BytecodeInterpreter(events),
                        dispose.map(CliDispose::BytecodeInterpreter),
                    )
                })
            }
            Self::Main(actor) => <Redispatcher as Actor<TAction, TTask>>::events(actor, inbox)
                .map(|(events, dispose)| (CliEvents::Main(events), dispose.map(CliDispose::Main))),
            Self::Task(actor) => {
                <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Actor<
                    TAction,
                    TTask,
                >>::events(actor, inbox)
                .map(|(events, dispose)| (CliEvents::Task(events), dispose.map(CliDispose::Task)))
            }
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction, TTask>
    Worker<TAction, SchedulerTransition<TAction, TTask>>
    for CliActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    fn accept(&self, message: &TAction) -> bool {
        match self {
            Self::Runtime(actor) => <RuntimeActor<T, TFactory, TAllocator> as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::Handler(actor) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
            Self::Grpc(actor) => <GrpcHandler<
                T,
                TFactory,
                TAllocator,
                WellKnownTypesTranscoder,
                TGrpcConfig,
                TReconnect,
            > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::accept(
                actor, message
            ),
            Self::BytecodeInterpreter(actor) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
            Self::Main(actor) => <Redispatcher as Worker<
                TAction,
                SchedulerTransition<TAction, TTask>,
            >>::accept(actor, message),
            Self::Task(actor) => {
                <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::accept(actor, message)
            }
        }
    }
    fn schedule(&self, message: &TAction, state: &Self::State) -> Option<SchedulerMode> {
        match (self, state) {
            (Self::Runtime(actor), CliActorState::Runtime(state)) => {
                <RuntimeActor<T, TFactory, TAllocator> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::Handler(actor), CliActorState::Handler(state)) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::Grpc(actor), CliActorState::Grpc(state)) => {
                <GrpcHandler<
                    T,
                    TFactory,
                    TAllocator,
                    WellKnownTypesTranscoder,
                    TGrpcConfig,
                    TReconnect,
                > as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::BytecodeInterpreter(actor), CliActorState::BytecodeInterpreter(state)) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            (Self::Main(actor), CliActorState::Main(state)) => {
                <Redispatcher as Worker<TAction, SchedulerTransition<TAction, TTask>>>::schedule(
                    actor, message, state,
                )
            }
            (Self::Task(actor), CliActorState::Task(state)) => {
                <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Worker<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::schedule(actor, message, state)
            }
            _ => unreachable!(),
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction, TTask>
    Handler<TAction, SchedulerTransition<TAction, TTask>>
    for CliActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    type State = CliActorState<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
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
            (Self::Runtime(actor), CliActorState::Runtime(state)) => {
                <RuntimeActor<T, TFactory, TAllocator> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::Handler(actor), CliActorState::Handler(state)) => {
                <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::Grpc(actor), CliActorState::Grpc(state)) => {
                <GrpcHandler<
                    T,
                    TFactory,
                    TAllocator,
                    WellKnownTypesTranscoder,
                    TGrpcConfig,
                    TReconnect,
                > as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::BytecodeInterpreter(actor), CliActorState::BytecodeInterpreter(state)) => {
                <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            (Self::Main(actor), CliActorState::Main(state)) => {
                <Redispatcher as Handler<TAction, SchedulerTransition<TAction, TTask>>>::handle(
                    actor, state, action, metadata, context,
                )
            }
            (Self::Task(actor), CliActorState::Task(state)) => {
                <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Handler<
                    TAction,
                    SchedulerTransition<TAction, TTask>,
                >>::handle(actor, state, action, metadata, context)
            }
            _ => unreachable!(),
        }
    }
}

enum CliActorState<
    T,
    TFactory,
    TAllocator,
    TConnect,
    TReconnect,
    TGrpcConfig,
    TMetricLabels,
    TAction,
    TTask,
> where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    Runtime(
        <RuntimeActor<T, TFactory, TAllocator> as Handler<
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
    Grpc(
        <GrpcHandler<T, TFactory, TAllocator, WellKnownTypesTranscoder, TGrpcConfig, TReconnect> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    BytecodeInterpreter(
        <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
    Main(<Redispatcher as Handler<TAction, SchedulerTransition<TAction, TTask>>>::State),
    Task(
        <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Handler<
            TAction,
            SchedulerTransition<TAction, TTask>,
        >>::State,
    ),
}

#[pin_project(project = CliEventsVariant)]
enum CliEvents<
    T,
    TFactory,
    TAllocator,
    TConnect,
    TReconnect,
    TGrpcConfig,
    TMetricLabels,
    TInbox,
    TAction,
    TTask,
> where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    Runtime(
        #[pin] <RuntimeActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Events<TInbox>,
    ),
    Handler(
        #[pin]
        <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Events<TInbox>,
    ),
    Grpc(
        #[pin]
        <GrpcHandler<T, TFactory, TAllocator, WellKnownTypesTranscoder, TGrpcConfig, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Events<TInbox>,
    ),
    BytecodeInterpreter(
        #[pin]
        <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Actor<
                TAction,
                TTask,
            >>::Events<TInbox>,
    ),
    Main(#[pin] <Redispatcher as Actor<TAction, TTask>>::Events<TInbox>),
    Task(
        #[pin]
        <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Actor<
            TAction,
            TTask,
        >>::Events<TInbox>,
    ),
}
impl<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TInbox,
        TAction,
        TTask,
    > Stream
    for CliEvents<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TInbox,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TInbox: TaskInbox<TAction>,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    type Item = TInbox::Message;
    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.project() {
            CliEventsVariant::Runtime(inner) => inner.poll_next(cx),
            CliEventsVariant::Handler(inner) => inner.poll_next(cx),
            CliEventsVariant::Grpc(inner) => inner.poll_next(cx),
            CliEventsVariant::BytecodeInterpreter(inner) => inner.poll_next(cx),
            CliEventsVariant::Main(inner) => inner.poll_next(cx),
            CliEventsVariant::Task(inner) => inner.poll_next(cx),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Runtime(inner) => inner.size_hint(),
            Self::Handler(inner) => inner.size_hint(),
            Self::Grpc(inner) => inner.size_hint(),
            Self::BytecodeInterpreter(inner) => inner.size_hint(),
            Self::Main(inner) => inner.size_hint(),
            Self::Task(inner) => inner.size_hint(),
        }
    }
}

#[pin_project(project = CliDisposeVariant)]
enum CliDispose<
    T,
    TFactory,
    TAllocator,
    TConnect,
    TReconnect,
    TGrpcConfig,
    TMetricLabels,
    TAction,
    TTask,
> where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    Runtime(#[pin] <RuntimeActor<T, TFactory, TAllocator> as Actor<TAction, TTask>>::Dispose),
    Handler(
        #[pin]
        <HandlerActor<T, TFactory, TAllocator, TConnect, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Dispose,
    ),
    Grpc(
        #[pin]
        <GrpcHandler<T, TFactory, TAllocator, WellKnownTypesTranscoder, TGrpcConfig, TReconnect> as Actor<
                TAction,
                TTask,
            >>::Dispose,
    ),
    BytecodeInterpreter(
        #[pin]
        <BytecodeInterpreter<T, TFactory, TAllocator, TMetricLabels> as Actor<
                TAction,
                TTask,
            >>::Dispose,
    ),
    Main(#[pin] <Redispatcher as Actor<TAction, TTask>>::Dispose),
    Task(
        #[pin]
        <CliTaskActor<T, TFactory, TAllocator, TConnect, TAction, TTask> as Actor<
            TAction,
            TTask,
        >>::Dispose,
    ),
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction, TTask>
    Future
    for CliDispose<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        TTask,
    >
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
    TTask:
        TaskFactory<TAction, TTask> + CliTask<T, TFactory, TAllocator, TConnect> + Send + 'static,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            CliDisposeVariant::Runtime(inner) => inner.poll(cx),
            CliDisposeVariant::Handler(inner) => inner.poll(cx),
            CliDisposeVariant::Grpc(inner) => inner.poll(cx),
            CliDisposeVariant::BytecodeInterpreter(inner) => inner.poll(cx),
            CliDisposeVariant::Main(inner) => inner.poll(cx),
            CliDisposeVariant::Task(inner) => inner.poll(cx),
        }
    }
}

blanket_trait!(
    trait CliTaskAction<T: Expression>:
        Action + RuntimeTaskAction<T> + DefaultHandlersTaskAction + GrpcHandlerConnectionTaskAction
    {
    }
);

task_factory_enum!({
    #[derive(Matcher, Clone)]
    enum CliTaskFactory<T, TFactory, TAllocator, TConnect>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
        Runtime(RuntimeTaskFactory<T, TFactory, TAllocator>),
        DefaultHandlers(DefaultHandlersTaskFactory<TConnect>),
        GrpcHandler(GrpcHandlerConnectionTaskFactory),
    }

    impl<T, TFactory, TAllocator, TConnect, TAction, TTask> TaskFactory<TAction, TTask>
        for CliTaskFactory<T, TFactory, TAllocator, TConnect>
    where
        T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
        TFactory: AsyncExpressionFactory<T> + Default,
        TAllocator: AsyncHeapAllocator<T> + Default,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action + CliTaskAction<T> + Send + 'static,
        TTask: TaskFactory<TAction, TTask>,
    {
    }
});

#[derive(Named, Clone)]
struct CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    inner: CliTaskFactory<T, TFactory, TAllocator, TConnect>,
    _reconnect: PhantomData<TReconnect>,
    _grpc_config: PhantomData<TGrpcConfig>,
    _metric_labels: PhantomData<TMetricLabels>,
}
impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<CliTaskFactory<T, TFactory, TAllocator, TConnect>>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: CliTaskFactory<T, TFactory, TAllocator, TConnect>) -> Self {
        Self {
            inner: value,
            _reconnect: PhantomData,
            _grpc_config: PhantomData,
            _metric_labels: PhantomData,
        }
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels, TAction>
    TaskFactory<TAction, Self>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
    TAction: Action + CliAction<T> + Send + 'static,
{
    type Actor = CliActor<
        T,
        TFactory,
        TAllocator,
        TConnect,
        TReconnect,
        TGrpcConfig,
        TMetricLabels,
        TAction,
        Self,
    >;
    fn create(self) -> Self::Actor {
        CliActor::Task(<CliTaskFactory<T, TFactory, TAllocator, TConnect> as TaskFactory<TAction, Self>>::create(self.inner))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<EffectThrottleTaskFactory>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: EffectThrottleTaskFactory) -> Self {
        Self::from(CliTaskFactory::Runtime(RuntimeTaskFactory::from(value)))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<BytecodeWorkerTaskFactory<T, TFactory, TAllocator>>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: BytecodeWorkerTaskFactory<T, TFactory, TAllocator>) -> Self {
        Self::from(CliTaskFactory::Runtime(RuntimeTaskFactory::from(value)))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<FetchHandlerTaskFactory<TConnect>>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: FetchHandlerTaskFactory<TConnect>) -> Self {
        Self::from(CliTaskFactory::DefaultHandlers(
            DefaultHandlersTaskFactory::from(value),
        ))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<GraphQlHandlerHttpFetchTaskFactory<TConnect>>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: GraphQlHandlerHttpFetchTaskFactory<TConnect>) -> Self {
        Self::from(CliTaskFactory::DefaultHandlers(
            DefaultHandlersTaskFactory::from(value),
        ))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<GraphQlHandlerWebSocketConnectionTaskFactory>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: GraphQlHandlerWebSocketConnectionTaskFactory) -> Self {
        Self::from(CliTaskFactory::DefaultHandlers(
            DefaultHandlersTaskFactory::from(value),
        ))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<TimeoutHandlerTaskFactory>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: TimeoutHandlerTaskFactory) -> Self {
        Self::from(CliTaskFactory::DefaultHandlers(
            DefaultHandlersTaskFactory::from(value),
        ))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<TimestampHandlerTaskFactory>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: TimestampHandlerTaskFactory) -> Self {
        Self::from(CliTaskFactory::DefaultHandlers(
            DefaultHandlersTaskFactory::from(value),
        ))
    }
}

impl<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
    From<GrpcHandlerConnectionTaskFactory>
    for CliActorFactory<T, TFactory, TAllocator, TConnect, TReconnect, TGrpcConfig, TMetricLabels>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send,
    T::Builtin: Send,
    T::Signal: Send,
    T::SignalList: Send,
    T::StructPrototype: Send,
    T::ExpressionList: Send,
    TFactory: AsyncExpressionFactory<T> + Default,
    TAllocator: AsyncHeapAllocator<T> + Default,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TReconnect: ReconnectTimeout + Send + Clone + 'static,
    TGrpcConfig: GrpcConfig + Send + Clone + 'static,
    TMetricLabels: BytecodeInterpreterMetricLabels + Send + 'static,
{
    fn from(value: GrpcHandlerConnectionTaskFactory) -> Self {
        Self::from(CliTaskFactory::GrpcHandler(value))
    }
}

impl<T: Expression> From<RuntimeActions<T>> for CliActions<T> {
    fn from(value: RuntimeActions<T>) -> Self {
        Self::Runtime(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<RuntimeActions<T>> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::Runtime(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a RuntimeActions<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::Runtime(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<BytecodeInterpreterActions<T>> for CliActions<T> {
    fn from(value: BytecodeInterpreterActions<T>) -> Self {
        Self::BytecodeInterpreter(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterActions<T>> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::BytecodeInterpreter(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a BytecodeInterpreterActions<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::BytecodeInterpreter(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<FetchHandlerActions> for CliActions<T> {
    fn from(value: FetchHandlerActions) -> Self {
        Self::FetchHandler(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<FetchHandlerActions> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::FetchHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a FetchHandlerActions> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::FetchHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlHandlerActions> for CliActions<T> {
    fn from(value: GraphQlHandlerActions) -> Self {
        Self::GraphQlHandler(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerActions> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GraphQlHandlerActions> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<TimeoutHandlerActions> for CliActions<T> {
    fn from(value: TimeoutHandlerActions) -> Self {
        Self::TimeoutHandler(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<TimeoutHandlerActions> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::TimeoutHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a TimeoutHandlerActions> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::TimeoutHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<TimestampHandlerActions> for CliActions<T> {
    fn from(value: TimestampHandlerActions) -> Self {
        Self::TimestampHandler(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<TimestampHandlerActions> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::TimestampHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a TimestampHandlerActions> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::TimestampHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GrpcHandlerActions> for CliActions<T> {
    fn from(value: GrpcHandlerActions) -> Self {
        Self::GrpcHandler(value)
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerActions> {
    fn from(value: CliActions<T>) -> Self {
        match value {
            CliActions::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerActions> {
    fn from(value: &'a CliActions<T>) -> Self {
        match value {
            CliActions::GrpcHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectActions<T>> for CliActions<T> {
    fn from(value: EffectActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EffectActions<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EffectActions<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for CliActions<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for CliActions<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for CliActions<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EffectEmitAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectThrottleEmitAction> for CliActions<T> {
    fn from(value: EffectThrottleEmitAction) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EffectThrottleEmitAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EffectThrottleEmitAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateActions<T>> for CliActions<T> {
    fn from(value: EvaluateActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EvaluateActions<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EvaluateActions<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for CliActions<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for CliActions<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction> for CliActions<T> {
    fn from(value: EvaluateStopAction) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EvaluateStopAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for CliActions<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryActions<T>> for CliActions<T> {
    fn from(value: QueryActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<QueryActions<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a QueryActions<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for CliActions<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for CliActions<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for CliActions<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<QueryEmitAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterInitAction> for CliActions<T> {
    fn from(value: BytecodeInterpreterInitAction) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterInitAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a BytecodeInterpreterInitAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterEvaluateAction<T>> for CliActions<T> {
    fn from(value: BytecodeInterpreterEvaluateAction<T>) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterEvaluateAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a BytecodeInterpreterEvaluateAction<T>>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterGcCompleteAction> for CliActions<T> {
    fn from(value: BytecodeInterpreterGcCompleteAction) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterGcCompleteAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a BytecodeInterpreterGcCompleteAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterResultAction<T>> for CliActions<T> {
    fn from(value: BytecodeInterpreterResultAction<T>) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterResultAction<T>> {
    fn from(value: CliActions<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a BytecodeInterpreterResultAction<T>> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<BytecodeInterpreterGcAction> for CliActions<T> {
    fn from(value: BytecodeInterpreterGcAction) -> Self {
        BytecodeInterpreterActions::<T>::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<BytecodeInterpreterGcAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a BytecodeInterpreterGcAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a BytecodeInterpreterActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<FetchHandlerFetchCompleteAction> for CliActions<T> {
    fn from(value: FetchHandlerFetchCompleteAction) -> Self {
        FetchHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<FetchHandlerFetchCompleteAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<FetchHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a FetchHandlerFetchCompleteAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a FetchHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<FetchHandlerConnectionErrorAction> for CliActions<T> {
    fn from(value: FetchHandlerConnectionErrorAction) -> Self {
        FetchHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<FetchHandlerConnectionErrorAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<FetchHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a FetchHandlerConnectionErrorAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a FetchHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerHttpFetchCompleteAction> for CliActions<T> {
    fn from(value: GraphQlHandlerHttpFetchCompleteAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerHttpFetchCompleteAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerHttpFetchCompleteAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerHttpConnectionErrorAction> for CliActions<T> {
    fn from(value: GraphQlHandlerHttpConnectionErrorAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerHttpConnectionErrorAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerHttpConnectionErrorAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectSuccessAction> for CliActions<T> {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerWebSocketConnectSuccessAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketClientMessageAction> for CliActions<T> {
    fn from(value: GraphQlHandlerWebSocketClientMessageAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerWebSocketClientMessageAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerWebSocketClientMessageAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketServerMessageAction> for CliActions<T> {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerWebSocketServerMessageAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectionErrorAction> for CliActions<T> {
    fn from(value: GraphQlHandlerWebSocketConnectionErrorAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GraphQlHandlerWebSocketConnectionErrorAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectionErrorAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectionTerminateAction> for CliActions<T> {
    fn from(value: GraphQlHandlerWebSocketConnectionTerminateAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>>
    for Option<GraphQlHandlerWebSocketConnectionTerminateAction>
{
    fn from(value: CliActions<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectionTerminateAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TimeoutHandlerTimeoutAction> for CliActions<T> {
    fn from(value: TimeoutHandlerTimeoutAction) -> Self {
        TimeoutHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<TimeoutHandlerTimeoutAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<TimeoutHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a TimeoutHandlerTimeoutAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a TimeoutHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<TimestampHandlerUpdateAction> for CliActions<T> {
    fn from(value: TimestampHandlerUpdateAction) -> Self {
        TimestampHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<TimestampHandlerUpdateAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<TimestampHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a TimestampHandlerUpdateAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a TimestampHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectSuccessAction> for CliActions<T> {
    fn from(value: GrpcHandlerConnectSuccessAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerConnectSuccessAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerConnectSuccessAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectErrorAction> for CliActions<T> {
    fn from(value: GrpcHandlerConnectErrorAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerConnectErrorAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerConnectErrorAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerRequestStartAction> for CliActions<T> {
    fn from(value: GrpcHandlerRequestStartAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerRequestStartAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerRequestStartAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerRequestStopAction> for CliActions<T> {
    fn from(value: GrpcHandlerRequestStopAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerRequestStopAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerRequestStopAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerSuccessResponseAction> for CliActions<T> {
    fn from(value: GrpcHandlerSuccessResponseAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerSuccessResponseAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerSuccessResponseAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerErrorResponseAction> for CliActions<T> {
    fn from(value: GrpcHandlerErrorResponseAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerErrorResponseAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerErrorResponseAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerTransportErrorAction> for CliActions<T> {
    fn from(value: GrpcHandlerTransportErrorAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerTransportErrorAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerTransportErrorAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerAbortRequestAction> for CliActions<T> {
    fn from(value: GrpcHandlerAbortRequestAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerAbortRequestAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>> for Option<&'a GrpcHandlerAbortRequestAction> {
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GrpcHandlerConnectionTerminateAction> for CliActions<T> {
    fn from(value: GrpcHandlerConnectionTerminateAction) -> Self {
        GrpcHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliActions<T>> for Option<GrpcHandlerConnectionTerminateAction> {
    fn from(value: CliActions<T>) -> Self {
        Option::<GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliActions<T>>
    for Option<&'a GrpcHandlerConnectionTerminateAction>
{
    fn from(value: &'a CliActions<T>) -> Self {
        Option::<&'a GrpcHandlerActions>::from(value).and_then(|value| value.into())
    }
}
