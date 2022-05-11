// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use futures::{Stream, StreamExt};
use reflex_dispatcher::{
    compose_actors,
    scheduler::tokio::{TokioScheduler, TokioSchedulerMetricNames},
    Action, Actor, EitherActor, HandlerContext, InboundAction, MessageData, NamedAction, Scheduler,
    SerializableAction, SerializedAction, StateTransition,
};
use reflex_handlers::{
    action::graphql::*,
    default_handlers,
    utils::tls::{create_https_client, native_tls::Certificate},
    DefaultHandlersMetricNames,
};
use reflex_json::{JsonMap, JsonValue};
use reflex_runtime::{
    action::{effect::*, evaluate::*, query::*, RuntimeAction},
    actor::{
        bytecode_interpreter::{BytecodeInterpreter, BytecodeInterpreterMetricNames},
        evaluate_handler::{
            create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
        },
        RuntimeActor, RuntimeMetricNames,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy, StateUpdate,
};
use reflex_utils::reconnect::NoopReconnectTimeout;
use std::{
    fs,
    path::{Path, PathBuf},
};
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{
        hash_program_root, Compiler, CompilerMode, CompilerOptions, InstructionPointer, Program,
    },
    core::{EvaluationResult, Expression, ExpressionFactory, StateCache},
    env::inject_env_vars,
    hash::HashId,
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::{term::SignalTerm, CachedSharedTerm, SharedTermFactory, ValueTerm},
};

use reflex_cli::{builtins::CliBuiltins, create_parser, repl, Syntax, SyntaxParser};

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
    let allocator = DefaultAllocator::default();
    let input_path = args.entry_point;
    let syntax = args.syntax;
    if syntax == Syntax::Bytecode {
        return Err(anyhow!("CLI is not yet supported for bytecode"));
    }
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
            let cache_key = hash_program_root(&program, &entry_point);

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
                let (watcher_middleware, subscribe_action, mut results_stream) =
                    create_query_watcher(expression, &factory, &allocator);
                let handlers =
                    default_handlers::<CliAction<CachedSharedTerm<CliBuiltins>>, _, _, _, _, _>(
                        https_client,
                        &factory,
                        &allocator,
                        NoopReconnectTimeout,
                        DefaultHandlersMetricNames::default(),
                    );
                let app = compose_actors(
                    compose_actors(
                        RuntimeActor::new(
                            factory.clone(),
                            allocator.clone(),
                            RuntimeMetricNames::default(),
                        ),
                        BytecodeInterpreter::new(
                            (Program::new([]), InstructionPointer::default()),
                            compiler_options,
                            interpreter_options,
                            factory.clone(),
                            allocator,
                            BytecodeInterpreterMetricNames::default(),
                        ),
                    ),
                    compose_actors(handlers, watcher_middleware),
                );
                let app = if debug_actions {
                    EitherActor::Left(compose_actors(app, DebugActor::stderr()))
                } else {
                    EitherActor::Right(app)
                };
                let mut scheduler = TokioScheduler::new(app, TokioSchedulerMetricNames::default());
                scheduler.dispatch(subscribe_action.into());
                while let Some(result) = results_stream.next().await {
                    let (result, _dependencies) = result.into_parts();
                    let output = match factory.match_signal_term(&result) {
                        None => format!("{}", result),
                        Some(signal) => format_signal_errors(signal)
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

fn create_query_watcher<
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
>(
    query: T,
    factory: &TFactory,
    allocator: &TAllocator,
) -> (
    QueryWatcherActor<T, TFactory>,
    EffectSubscribeAction<T>,
    impl Stream<Item = EvaluationResult<T>>,
) {
    let evaluate_effect = create_evaluate_effect(
        query,
        QueryEvaluationMode::Standalone,
        QueryInvalidationStrategy::Exact,
        factory,
        allocator,
    );
    let effect_id = evaluate_effect.id();
    let subscribe_action = EffectSubscribeAction {
        effect_type: String::from(EFFECT_TYPE_EVALUATE),
        effects: vec![evaluate_effect],
    };
    let (results_tx, results_rx) = mpsc::unbounded_channel();
    (
        QueryWatcherActor::new(effect_id, results_tx, factory.clone()),
        subscribe_action,
        UnboundedReceiverStream::new(results_rx),
    )
}

struct QueryWatcherActor<T: Expression, TFactory: ExpressionFactory<T>> {
    effect_id: HashId,
    factory: TFactory,
    results: mpsc::UnboundedSender<EvaluationResult<T>>,
}
impl<T, TFactory> QueryWatcherActor<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
{
    fn new(
        effect_id: HashId,
        results: mpsc::UnboundedSender<EvaluationResult<T>>,
        factory: TFactory,
    ) -> Self {
        Self {
            effect_id,
            results,
            factory,
        }
    }
}
impl<T, TFactory, TAction> Actor<TAction> for QueryWatcherActor<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAction: Action + InboundAction<EffectEmitAction<T>>,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_effect_emit(action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default()
    }
}
impl<T, TFactory> QueryWatcherActor<T, TFactory>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
{
    fn handle_effect_emit<TAction>(
        &self,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        for update in action
            .updates
            .iter()
            .filter(|(key, _)| *key == self.effect_id)
            .filter_map(|(_, value)| match value {
                StateUpdate::Value(value) => parse_evaluate_effect_result(value, &self.factory),
                StateUpdate::Patch(_) => None,
            })
        {
            let _ = self.results.send(update);
        }
        None
    }
}

struct DebugActor<T: std::io::Write> {
    output: T,
}
impl<T: std::io::Write> DebugActor<T> {
    pub fn new(output: T) -> Self {
        Self { output }
    }
    fn log(&mut self, action: &impl SerializableAction) {
        let serialized_args = JsonValue::Object(JsonMap::from_iter(action.serialize()));
        let _ = writeln!(
            self.output,
            "[{}] {}",
            action.name(),
            serialized_args.to_string()
        );
    }
}
impl DebugActor<std::io::Stderr> {
    pub fn stderr() -> Self {
        Self::new(std::io::stderr())
    }
}
impl<T, TAction> Actor<TAction> for DebugActor<T>
where
    T: std::io::Write,
    TAction: Action + SerializableAction,
{
    fn handle(
        &mut self,
        action: &TAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        self.log(action);
        None.unwrap_or_default()
    }
}

fn format_signal_errors<T: Expression>(
    signal: &SignalTerm<T>,
) -> impl IntoIterator<Item = String> + '_ {
    signal
        .signals()
        .iter()
        .map(|signal| match signal.args().iter().next() {
            Some(payload) => format!("{}", payload),
            None => format!("{}", ValueTerm::<T::String>::Null),
        })
}

fn read_file(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("Failed to read path {}", path.display()))
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}

enum CliAction<T: Expression> {
    Runtime(RuntimeAction<T>),
    GraphQlHandler(GraphQlHandlerAction),
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
    fn serialize(&self) -> SerializedAction {
        match self {
            Self::Runtime(action) => action.serialize(),
            Self::GraphQlHandler(action) => action.serialize(),
        }
    }
}

impl<T: Expression> From<RuntimeAction<T>> for CliAction<T> {
    fn from(value: RuntimeAction<T>) -> Self {
        Self::Runtime(value)
    }
}
impl<T: Expression> From<CliAction<T>> for Option<RuntimeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        match value {
            CliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a RuntimeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        match value {
            CliAction::Runtime(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<GraphQlHandlerAction> for CliAction<T> {
    fn from(value: GraphQlHandlerAction) -> Self {
        Self::GraphQlHandler(value)
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerAction> {
    fn from(value: CliAction<T>) -> Self {
        match value {
            CliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a GraphQlHandlerAction> {
    fn from(value: &'a CliAction<T>) -> Self {
        match value {
            CliAction::GraphQlHandler(value) => Some(value),
            _ => None,
        }
    }
}

impl<T: Expression> From<EffectAction<T>> for CliAction<T> {
    fn from(value: EffectAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectSubscribeAction<T>> for CliAction<T> {
    fn from(value: EffectSubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectSubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectSubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectUnsubscribeAction<T>> for CliAction<T> {
    fn from(value: EffectUnsubscribeAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectUnsubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectUnsubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EffectEmitAction<T>> for CliAction<T> {
    fn from(value: EffectEmitAction<T>) -> Self {
        EffectAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EffectEmitAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EffectEmitAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EffectAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateAction<T>> for CliAction<T> {
    fn from(value: EvaluateAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStartAction<T>> for CliAction<T> {
    fn from(value: EvaluateStartAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateStartAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateStartAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateStopAction<T>> for CliAction<T> {
    fn from(value: EvaluateStopAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateStopAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateStopAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<EvaluateResultAction<T>> for CliAction<T> {
    fn from(value: EvaluateResultAction<T>) -> Self {
        EvaluateAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<EvaluateResultAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a EvaluateResultAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a EvaluateAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryAction<T>> for CliAction<T> {
    fn from(value: QueryAction<T>) -> Self {
        RuntimeAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a RuntimeAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QuerySubscribeAction<T>> for CliAction<T> {
    fn from(value: QuerySubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QuerySubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QuerySubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryUnsubscribeAction<T>> for CliAction<T> {
    fn from(value: QueryUnsubscribeAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryUnsubscribeAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryUnsubscribeAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<QueryEmitAction<T>> for CliAction<T> {
    fn from(value: QueryEmitAction<T>) -> Self {
        QueryAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<QueryEmitAction<T>> {
    fn from(value: CliAction<T>) -> Self {
        Option::<QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>> for Option<&'a QueryEmitAction<T>> {
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a QueryAction<T>>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectSuccessAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectSuccessAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketConnectSuccessAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectSuccessAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketConnectErrorAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketConnectErrorAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketConnectErrorAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketConnectErrorAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}

impl<T: Expression> From<GraphQlHandlerWebSocketServerMessageAction> for CliAction<T> {
    fn from(value: GraphQlHandlerWebSocketServerMessageAction) -> Self {
        GraphQlHandlerAction::from(value).into()
    }
}
impl<T: Expression> From<CliAction<T>> for Option<GraphQlHandlerWebSocketServerMessageAction> {
    fn from(value: CliAction<T>) -> Self {
        Option::<GraphQlHandlerAction>::from(value).and_then(|value| value.into())
    }
}
impl<'a, T: Expression> From<&'a CliAction<T>>
    for Option<&'a GraphQlHandlerWebSocketServerMessageAction>
{
    fn from(value: &'a CliAction<T>) -> Self {
        Option::<&'a GraphQlHandlerAction>::from(value).and_then(|value| value.into())
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
