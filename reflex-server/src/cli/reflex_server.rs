// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    convert::Infallible,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{graphql_service, GraphQlHttpQueryTransform};
use anyhow::{anyhow, Context, Result};
use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, Reducible, Rewritable, StringValue},
    interpreter::InterpreterOptions,
    stdlib::Stdlib,
};
use reflex_cli::{compile_entry_point, Syntax};
use reflex_graphql::stdlib::Stdlib as GraphQlStdlib;
use reflex_handlers::{debug_signal_handler, EitherHandler};
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, RuntimeCache,
    RuntimeState, SignalHandler,
};

pub use reflex;
pub use reflex_handlers::builtin_signal_handler;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, imports::builtin_imports, static_module_loader,
};
pub use reflex_runtime::{SignalHandlerResult, SignalResult};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ReflexServerCliOptions {
    pub entry_point: PathBuf,
    pub syntax: Syntax,
    pub port: u16,
    pub unoptimized: bool,
    pub debug_compiler: bool,
    pub debug_signals: bool,
    pub debug_interpreter: bool,
    pub debug_stack: bool,
}

pub async fn cli<
    T: AsyncExpression
        + Expression<String = String>
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + 'static,
>(
    args: ReflexServerCliOptions,
    signal_handler: impl SignalHandler<T>,
    module_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: Option<CompilerOptions>,
    interpreter_options: Option<InterpreterOptions>,
    transform: impl GraphQlHttpQueryTransform,
) -> Result<()>
where
    T::String: Send + Sync,
    T::Builtin: From<Stdlib> + From<JsStdlib> + From<GraphQlStdlib>,
{
    let root_module_path = &args.entry_point;
    let compiler_options = compiler_options.unwrap_or_else(|| CompilerOptions {
        debug: args.debug_compiler,
        normalize: !args.unoptimized,
        ..CompilerOptions::default()
    });
    let graph_root = compile_entry_point(
        root_module_path.as_path(),
        args.syntax,
        Some(std::env::vars()),
        module_loader,
        &compiler_options,
        factory,
        allocator,
    )?;

    let interpreter_options = interpreter_options.unwrap_or_else(|| InterpreterOptions {
        debug_instructions: args.debug_interpreter || args.debug_stack,
        debug_stack: args.debug_stack,
        ..InterpreterOptions::default()
    });
    let state = RuntimeState::default();
    let cache = RuntimeCache::default();
    let signal_handler = if !args.debug_signals {
        EitherHandler::Left(signal_handler)
    } else {
        EitherHandler::Right(debug_signal_handler(signal_handler))
    };
    let runtime = Runtime::new(
        state,
        &signal_handler,
        cache,
        factory,
        allocator,
        interpreter_options,
        compiler_options,
    );
    let address = SocketAddr::from(([0, 0, 0, 0], args.port));
    let server = create_server(
        runtime,
        graph_root,
        factory,
        allocator,
        compiler_options,
        transform,
        &address,
    );
    eprintln!(
        "Listening for incoming HTTP requests on port {}",
        &address.port()
    );
    server.await.with_context(|| "Server error")
}

async fn create_server<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    runtime: Runtime<T>,
    graph_root: (Program, InstructionPointer),
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: impl GraphQlHttpQueryTransform,
    address: &SocketAddr,
) -> Result<()>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let runtime = Arc::new(runtime);
    let graph_root = Arc::new(graph_root);
    let transform = Arc::new(transform);
    let server = Server::bind(&address).serve(make_service_fn(|_socket: &AddrStream| {
        let runtime = Arc::clone(&runtime);
        let graph_root = Arc::clone(&graph_root);
        let transform = Arc::clone(&transform);
        let service = graphql_service(
            runtime,
            graph_root,
            factory,
            allocator,
            compiler_options,
            transform,
        );
        async { Ok::<_, Infallible>(service) }
    }));
    server.await.map_err(|err| anyhow!("{}", err))
}
