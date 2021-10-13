// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    convert::Infallible,
    env, fs,
    io::Write,
    net::SocketAddr,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::Instant,
};

use crate::graphql_service;
use clap::Parser;
use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    compiler::{Compile, Compiler, CompilerMode, CompilerOptions, CompilerOutput, Program},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        StringValue,
    },
    interpreter::InterpreterOptions,
    lang::{NativeFunction, WithCompiledBuiltins},
};
use reflex_handlers::debug_signal_handler;
use reflex_js::{create_js_env, parse_module};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, RuntimeCache,
    RuntimeState, SignalHelpers,
};

pub use reflex;
pub use reflex_handlers::builtin_signal_handler;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, static_module_loader, stdlib::builtin_imports,
};
pub use reflex_runtime::{SignalHandlerResult, SignalResult};

#[derive(Parser)]
struct Opts {
    #[clap(about = "Either a js file or a precompiled bytecode file")]
    entry_point: String,
    #[clap(about = "Port on which to expose a server")]
    port: u16,
    #[clap(long, about = "Interpret entry_point as a precompiled bytecode file")]
    precompiled: bool,
    #[clap(long, about = "Add debug printing of signal handlers")]
    debug_signals: bool,
    #[clap(long, about = "Add debug printing of bytecode output from compiler")]
    debug_compiler: bool,
    #[clap(long, about = "Add debug printing of bytecode execution")]
    debug_interpreter: bool,
    #[clap(long, about = "Add debug printing of stack during execution")]
    debug_stack: bool,
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
    signal_handler: impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
    plugins: impl IntoIterator<Item = NativeFunction<T>>,
    compiler_options: Option<CompilerOptions>,
    interpreter_options: Option<InterpreterOptions>,
) -> Result<(), String>
where
    T::String: StringValue + Send + Sync,
{
    let args: Opts = Opts::parse();
    let config = {
        let entry_point = match PathBuf::from_str(&args.entry_point) {
            Err(_) => Err(format!(
                "Invalid entry point module path: {}",
                &args.entry_point
            )),
            Ok(path) => load_file(&path).map(|source| (path, source)),
        };
        match entry_point {
            Err(error) => Err(error),
            Ok(entry_point) => Ok((
                env::vars(),
                entry_point,
                SocketAddr::from(([0, 0, 0, 0], args.port)),
                args,
            )),
        }
    };
    match config {
        Err(error) => Err(format!("Unable to start server: {}", error)),
        Ok((env_vars, entry_point, address, args)) => {
            let (root_module_path, root_module_source) = entry_point;
            match create_graph_root(
                &root_module_path,
                &root_module_source,
                env_vars,
                custom_loader,
                factory,
                allocator,
            ) {
                Err(error) => Err(format!("Failed to load entry point module: {}", error)),
                Ok(root) => {
                    let compiler_options = compiler_options.unwrap_or_else(|| CompilerOptions {
                        debug: args.debug_compiler,
                        ..CompilerOptions::default()
                    });
                    let interpreter_options =
                        interpreter_options.unwrap_or_else(|| InterpreterOptions {
                            debug_instructions: args.debug_interpreter || args.debug_stack,
                            debug_stack: args.debug_stack,
                            ..InterpreterOptions::default()
                        });
                    let compiled_root =
                        compile_root(root, factory, allocator, plugins, compiler_options)?;
                    let (program, builtins, plugins) = compiled_root.into_parts();
                    let state = RuntimeState::default();
                    let cache = RuntimeCache::default();
                    let runtime = if args.debug_signals {
                        Runtime::new(
                            state,
                            builtins,
                            plugins,
                            debug_signal_handler(signal_handler),
                            cache,
                            factory,
                            allocator,
                            interpreter_options,
                            compiler_options,
                        )
                    } else {
                        Runtime::new(
                            state,
                            builtins,
                            plugins,
                            signal_handler,
                            cache,
                            factory,
                            allocator,
                            interpreter_options,
                            compiler_options,
                        )
                    };
                    let server = create_server(
                        runtime,
                        program,
                        factory,
                        allocator,
                        compiler_options,
                        &address,
                    );
                    println!(
                        "Listening for incoming HTTP requests on port {}",
                        &address.port()
                    );
                    if let Err(error) = server.await {
                        Err(format!("Server error: {}", error))
                    } else {
                        Ok(())
                    }
                }
            }
        }
    }
}

pub fn create_graph_root<T: Expression + 'static>(
    root_module_path: impl AsRef<Path>,
    root_module_source: &str,
    env_args: impl IntoIterator<Item = (String, String)>,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Result<T, String> {
    let env = create_js_env(env_args, factory, allocator);
    let module_loader = create_module_loader(env.clone(), custom_loader, factory, allocator);
    parse_module(
        root_module_source,
        &env,
        root_module_path.as_ref(),
        &module_loader,
        factory,
        allocator,
    )
}

pub fn compile_root<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    root: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: impl IntoIterator<Item = NativeFunction<T>>,
    compiler_options: CompilerOptions,
) -> Result<CompilerOutput<T>, String> {
    print!("Compiling graph root...");
    let _ = std::io::stdout().flush();
    let start_time = Instant::now();
    let compiled = Compiler::new(compiler_options, None).compile(
        &root,
        CompilerMode::Thunk,
        true,
        plugins,
        factory,
        allocator,
    );
    println!(" {:?}", start_time.elapsed());
    // TODO: Error if graph root depends on unrecognized native functions
    compiled
}

async fn create_server<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    runtime: Runtime<T>,
    program: Program,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    address: &SocketAddr,
) -> Result<(), String>
where
    T::String: StringValue + Send + Sync,
{
    let runtime = Arc::new(runtime);
    let program = Arc::new(program);
    let server = Server::bind(&address).serve(make_service_fn(|_socket: &AddrStream| {
        let runtime = Arc::clone(&runtime);
        let program = Arc::clone(&program);
        let service = graphql_service(runtime, program, factory, allocator, compiler_options);
        async { Ok::<_, Infallible>(service) }
    }));
    match server.await {
        Err(error) => Err(format!("{}", error)),
        Ok(()) => Ok(()),
    }
}

fn load_file(path: &Path) -> Result<String, String> {
    match fs::read_to_string(&path) {
        Ok(data) => Ok(data),
        Err(_) => Err(format!(
            "Failed to load {}",
            path.to_str().unwrap_or_else(|| "file")
        )),
    }
}
