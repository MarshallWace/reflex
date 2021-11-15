// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use reflex::{
    compiler::{Compile, CompilerOptions},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        SignalType,
    },
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_cli::{compile_entry_point, Syntax};
use reflex_graphql::{
    parse_graphql_operation, stdlib::Stdlib as GraphQlStdlib, GraphQlOperationPayload,
    NoopGraphQlQueryTransform,
};
use reflex_handlers::{
    debug_signal_handler,
    utils::signal_capture::{SignalPlayback, SignalRecorder},
    EitherHandler, SIGNAL_TYPE_GRAPHQL_EXECUTE, SIGNAL_TYPE_HTTP_FETCH,
};
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, RuntimeCache,
    RuntimeState, SignalHandler, StreamExt,
};

use crate::compile_graphql_query;

pub fn default_captured_signals() -> Vec<String> {
    vec![
        String::from(SIGNAL_TYPE_HTTP_FETCH),
        String::from(SIGNAL_TYPE_GRAPHQL_EXECUTE),
    ]
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ExecuteQueryCliOptions {
    pub graph_root: PathBuf,
    pub syntax: Syntax,
    pub query: String,
    pub args: Option<String>,
    pub capture_signals: Option<PathBuf>,
    pub replay_signals: Option<PathBuf>,
    pub captured_signals: Option<Vec<String>>,
    pub unoptimized: bool,
    pub debug_compiler: bool,
    pub debug_signals: bool,
    pub debug_interpreter: bool,
    pub debug_stack: bool,
}

pub async fn cli<T, TLoader>(
    options: ExecuteQueryCliOptions,
    env: Option<impl IntoIterator<Item = (String, String)>>,
    signal_handler: impl SignalHandler<T>,
    module_loader: Option<TLoader>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<T>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::String: Send + Sync,
    T::Builtin: From<Stdlib> + From<JsStdlib> + From<GraphQlStdlib>,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    let compiler_options = CompilerOptions {
        debug: options.debug_compiler,
        ..if options.unoptimized {
            CompilerOptions::unoptimized()
        } else {
            CompilerOptions::default()
        }
    };
    let interpreter_options = InterpreterOptions {
        debug_instructions: options.debug_interpreter,
        debug_stack: options.debug_stack,
        ..InterpreterOptions::default()
    };
    let graph_root = compile_entry_point(
        options.graph_root.as_path(),
        options.syntax,
        env,
        module_loader,
        &compiler_options,
        factory,
        allocator,
    )?;
    let query = parse_graphql_query(
        &options.query,
        options.args.as_ref().map(|arg| arg.as_str()),
        factory,
        allocator,
    )?;
    let (program, entry_point) =
        compile_graphql_query(query, graph_root, &compiler_options, factory, allocator)
            .map_err(|err| anyhow!("{}", err))
            .with_context(|| format!("Failed to compile GraphQL query"))?;
    let signal_handler = create_signal_handler(signal_handler, &options, factory, allocator)?;
    let state = RuntimeState::default();
    let cache = RuntimeCache::default();
    let runtime = Runtime::new(
        state,
        &signal_handler,
        cache,
        factory,
        allocator,
        interpreter_options,
        compiler_options,
    );
    let subscription = runtime
        .subscribe(program, entry_point)
        .await
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| "GraphQL subscription failed")?;
    let id = subscription.id();
    let result = subscription
        .into_stream()
        .filter(|result| !is_pending_result(result, factory))
        .next()
        .await
        .unwrap();
    runtime.unsubscribe(id).await.unwrap();
    Ok(result)
}

fn create_signal_handler<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    signal_handler: impl SignalHandler<T>,
    options: &ExecuteQueryCliOptions,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<impl SignalHandler<T>>
where
    T::String: Send + Sync,
{
    if options.replay_signals.is_none()
        && options.replay_signals.is_none()
        && !options.debug_signals
    {
        Ok(EitherHandler::Left(signal_handler))
    } else {
        let default_signal_types = default_captured_signals();
        let captured_signals = options
            .captured_signals
            .as_ref()
            .unwrap_or(&default_signal_types);
        let signal_handler = if let Some(path) = &options.replay_signals {
            let signal_playback =
                SignalPlayback::new(captured_signals.iter().cloned(), path.as_path())
                    .map_err(|err| anyhow!("{}", err))
                    .with_context(|| format!("Failed to create signal playback handler"))?;
            EitherHandler::Left(signal_playback.playback_signal_handler(
                signal_handler,
                factory,
                allocator,
            ))
        } else {
            EitherHandler::Right(signal_handler)
        };
        let signal_handler = if let Some(path) = &options.capture_signals {
            let signal_recorder =
                SignalRecorder::new(captured_signals.iter().cloned(), path.as_path())
                    .map_err(|err| anyhow!("{}", err))
                    .with_context(|| format!("Failed to create signal recorder handler"))?;
            EitherHandler::Left(signal_recorder.record_signal_handler(signal_handler, factory))
        } else {
            EitherHandler::Right(signal_handler)
        };
        let signal_handler = if options.debug_signals {
            EitherHandler::Left(debug_signal_handler(signal_handler))
        } else {
            EitherHandler::Right(signal_handler)
        };
        Ok(EitherHandler::Right(signal_handler))
    }
}

fn parse_graphql_query<T: Expression>(
    query: &str,
    args: Option<&str>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T>
where
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let args = args
        .map(|args| {
            serde_json::from_str(&args)
                .map(|value| match value {
                    serde_json::Value::Object(s) => s.into_iter().collect::<Vec<_>>(),
                    _ => unreachable!(),
                })
                .with_context(|| format!("Failed to parse GraphQL arguments: {}", args))
        })
        .transpose()?;
    let operation =
        GraphQlOperationPayload::new(String::from(query), None, args, Option::<Vec<_>>::None);
    parse_graphql_operation(
        &operation,
        factory,
        allocator,
        &NoopGraphQlQueryTransform {},
    )
    .map_err(|err| anyhow!("{}", err))
    .with_context(|| format!("Failed to parse GraphQL operation"))
}

fn is_pending_result<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> bool {
    factory
        .match_signal_term(value)
        .map(|signal| signal.signals().iter().any(is_pending_signal))
        .unwrap_or(false)
}

fn is_pending_signal<T: Expression>(value: &Signal<T>) -> bool {
    matches!(value.signal_type(), SignalType::Pending)
}
