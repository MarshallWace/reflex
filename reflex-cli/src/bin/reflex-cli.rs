// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    fs,
    iter::once,
    marker::PhantomData,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use futures::{stream, StreamExt};
use reflex::{
    cache::SubstitutionCache,
    core::{
        ConditionListType, ConditionType, Expression, ExpressionFactory, ExpressionListType,
        InstructionPointer, LetTermType, RefType, SignalTermType, StateCache,
    },
    env::inject_env_vars,
};
use reflex_cli::{builtins::CliBuiltins, create_parser, repl, Syntax, SyntaxParser};
use reflex_dispatcher::{
    scheduler::tokio::{TokioScheduler, TokioSchedulerMetricNames},
    Action, Actor, ActorTransition, ChainedActor, EitherActor, HandlerContext, Matcher,
    MessageData, NamedAction, SchedulerMiddleware, SerializableAction, SerializedAction,
};
use reflex_handlers::{
    action::graphql::*,
    default_handlers,
    utils::tls::{create_https_client, tokio_native_tls::native_tls::Certificate},
    DefaultHandlersMetricNames,
};
use reflex_interpreter::{
    compiler::{hash_compiled_program, CompiledProgram, Compiler, CompilerMode, CompilerOptions},
    execute, DefaultInterpreterCache, InterpreterOptions,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_lang::{
    allocator::DefaultAllocator, term::SignalTerm, CachedSharedTerm, SharedTermFactory,
};
use reflex_runtime::{
    action::{effect::*, evaluate::*, query::*, RuntimeActions},
    actor::{
        bytecode_interpreter::BytecodeInterpreter,
        evaluate_handler::{
            create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
        },
        RuntimeActor, RuntimeMetricNames,
    },
    worker::bytecode_worker::BytecodeWorkerMetricNames,
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy,
};
use reflex_utils::reconnect::NoopReconnectTimeout;

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
    let args = Args::parse();
    let unoptimized = args.unoptimized;
    let debug_actions = args.log;
    let debug_compiler = args.debug_compiler;
    let debug_interpreter = args.debug_interpreter;
    let debug_stack = args.debug_stack;
    let factory = SharedTermFactory::<CliBuiltins>::default();
    let term = factory.create_let_term(factory.create_int_term(3), factory.create_int_term(4));
    if let Some(term) = factory.match_let_term(&term) {
        println!(
            "INTITIALIZER: {}, BODY: {}",
            term.initializer().as_deref(),
            term.body().as_deref()
        );
    }
    let allocator = DefaultAllocator::default();
    let input_path = args.entry_point;
    let syntax = args.syntax;
    let tls_cert = args
        .tls_cert
        .as_ref()
        .map(|path| load_tls_cert(path.as_path()))
        .transpose()?;
    let https_client = create_https_client(tls_cert)?;
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
                let (evaluate_effect, subscribe_action) =
                    create_query(expression, &factory, &allocator);
                let handlers =
                    default_handlers::<CliAction<CachedSharedTerm<CliBuiltins>>, _, _, _, _, _>(
                        https_client,
                        &factory,
                        &allocator,
                        NoopReconnectTimeout,
                        DefaultHandlersMetricNames::default(),
                    );
                let app = ChainedActor::new(
                    ChainedActor::new(
                        RuntimeActor::new(
                            factory.clone(),
                            allocator.clone(),
                            RuntimeMetricNames::default(),
                        ),
                        BytecodeInterpreter::new(
                            (CompiledProgram::default(), InstructionPointer::default()),
                            compiler_options,
                            interpreter_options,
                            factory.clone(),
                            allocator,
                            BytecodeWorkerMetricNames::default(),
                        ),
                    ),
                    handlers,
                );
                let app = if debug_actions {
                    EitherActor::Left(ChainedActor::new(app, DebugActor::stderr()))
                } else {
                    EitherActor::Right(app)
                };
                let scheduler = TokioScheduler::new(
                    app,
                    SchedulerMiddleware::noop(),
                    TokioSchedulerMetricNames::default(),
                );
                let mut results_stream = tokio::spawn(scheduler.subscribe({
                    let factory = factory.clone();
                    move |action: &CliAction<CachedSharedTerm<CliBuiltins>>| {
                        let EffectEmitAction { updates } = action.match_type()?;
                        let update = updates
                            .iter()
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
                let _ =
                    tokio::spawn(scheduler.dispatch(stream::iter(once(subscribe_action.into()))));
                while let Some(value) = results_stream.next().await {
                    let output = match factory.match_signal_term(&value) {
                        None => format!("{}", value),
                        Some(signal) => format_signal_errors(signal, &factory)
                            .into_iter()
                            .map(|error| format!(" - {}", error))
                            .collect::<Vec<_>>()
                            .join("\n"),
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
) -> (T::Signal<T>, EffectSubscribeAction<T>) {
    let evaluate_effect = create_evaluate_effect(
        String::from("<anonymous>"),
        query,
        QueryEvaluationMode::Standalone,
        QueryInvalidationStrategy::Exact,
        factory,
        allocator,
    );
    let subscribe_action = EffectSubscribeAction {
        effect_type: String::from(EFFECT_TYPE_EVALUATE),
        effects: vec![evaluate_effect.clone()],
    };
    (evaluate_effect, subscribe_action)
}

struct DebugActorLogger<T: std::io::Write> {
    output: T,
}
impl<T: std::io::Write> DebugActorLogger<T> {
    pub fn new(output: T) -> Self {
        Self { output }
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
impl DebugActorLogger<std::io::Stderr> {
    pub fn stderr() -> Self {
        Self::new(std::io::stderr())
    }
}

struct DebugActor<TOutput: std::io::Write> {
    _logger: PhantomData<DebugActorLogger<TOutput>>,
}
impl DebugActor<std::io::Stderr> {
    fn stderr() -> Self {
        Self {
            _logger: Default::default(),
        }
    }
}

struct DebugActorState<TOutput: std::io::Write> {
    logger: DebugActorLogger<TOutput>,
}
impl Default for DebugActorState<std::io::Stderr> {
    fn default() -> Self {
        Self {
            logger: DebugActorLogger::stderr(),
        }
    }
}

impl<TAction, TOutput> Actor<TAction> for DebugActor<TOutput>
where
    TAction: Action + SerializableAction,
    TOutput: std::io::Write,
    DebugActorState<TOutput>: Default,
{
    type State = DebugActorState<TOutput>;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        let mut state = state;
        state.logger.log(action);
        ActorTransition::new(state, Default::default())
    }
}

fn format_signal_errors<'a, T: Expression>(
    signal: &'a SignalTerm<T>,
    factory: &'a impl ExpressionFactory<T>,
) -> impl IntoIterator<Item = String> + 'a {
    signal
        .signals()
        .as_deref()
        .iter()
        .map(|item| item.as_deref())
        .map(|signal| {
            match signal
                .args()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .next()
            {
                Some(payload) => format!("{}", payload),
                None => format!("{}", factory.create_nil_term()),
            }
        })
}

fn read_file(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("Failed to read path {}", path.display()))
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum CliAction<T: Expression> {
    Runtime(RuntimeActions<T>),
    GraphQlHandler(GraphQlHandlerActions),
}
impl<T: Expression> Action for CliAction<T> {}
impl<T: Expression> NamedAction for CliAction<T> {
    fn name(&self) -> &'static str {
        match self {
            Self::Runtime(action) => action.name(),
            Self::GraphQlHandler(action) => action.name(),
        }
    }
}
impl<T: Expression> SerializableAction for CliAction<T> {
    fn to_json(&self) -> SerializedAction {
        match self {
            Self::Runtime(action) => action.to_json(),
            Self::GraphQlHandler(action) => action.to_json(),
        }
    }
}

impl<T: Expression> From<RuntimeActions<T>> for CliAction<T> {
    fn from(value: RuntimeActions<T>) -> Self {
        Self::Runtime(value)
    }
}
impl<T: Expression> From<CliAction<T>> for Option<RuntimeActions<T>> {
    fn from(value: CliAction<T>) -> Self {
        match value {
            CliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a RuntimeActions<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        match value {
            CliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlHandlerActions> for CliAction<T> {
    fn from(value: GraphQlHandlerActions) -> Self {
        Self::GraphQlHandler(value)
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerActions> {
    fn from(value: CliAction<T>) -> Self {
        match value {
            CliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a GraphQlHandlerActions> {
    fn from(value: &'a CliAction<T>) -> Self {
        match value {
            CliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectActions<T>> for CliAction<T> {
    fn from(value: EffectActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectActions<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectActions<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for CliAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for CliAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for CliAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateActions<T>> for CliAction<T> {
    fn from(value: EvaluateActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateActions<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateActions<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for CliAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateUpdateAction<T>> for CliAction<T> {
    fn from(value: EvaluateUpdateAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateUpdateAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateUpdateAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction> for CliAction<T> {
    fn from(value: EvaluateStopAction) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateStopAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateStopAction> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for CliAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryActions<T>> for CliAction<T> {
    fn from(value: QueryActions<T>) -> Self {
        RuntimeActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryActions<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryActions<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for CliAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for CliAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for CliAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryActions<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectSuccessAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketConnectSuccessAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectErrorAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectErrorAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectErrorAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketServerMessageAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        GraphQlHandlerActions::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketServerMessageAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerActions>::from(value).and_then(|value| value.into())
    }
}

fn load_tls_cert(path: &Path) -> Result<Certificate> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to load TLS certificate: {}", path.to_string_lossy()))?;
    Certificate::from_pem(source.as_bytes()).with_context(|| {
        format!(
            "Failed to parse TLS certificate: {}",
            path.to_string_lossy()
        )
    })
}
