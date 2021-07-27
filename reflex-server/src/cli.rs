// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    convert::Infallible,
    env, fs,
    net::SocketAddr,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use crate::graphql_service;
use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    compiler::{Compile, Compiler, CompilerMode, CompilerOptions, NativeFunctionRegistry},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        StringValue,
    },
    interpreter::{Execute, InterpreterOptions},
};
use reflex_cli::parse_cli_args;
use reflex_handlers::debug_signal_handler;
use reflex_js::{create_js_env, parse_module};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, SignalHelpers,
};

pub use reflex;
pub use reflex_handlers::builtin_signal_handler;
pub use reflex_js::{
    compose_module_loaders, create_module_loader, static_module_loader, stdlib::builtin_imports,
};
pub use reflex_runtime::{SignalHandlerResult, SignalResult};

struct CliArgs {
    entry_point: String,
    port: u16,
    debug_signals: bool,
    debug_compiler: bool,
    debug_interpreter: bool,
}

pub async fn cli<
    T: AsyncExpression
        + Expression<String = String>
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + Execute<T>
        + 'static,
>(
    signal_handler: impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    plugins: NativeFunctionRegistry<T>,
    compiler_options: Option<CompilerOptions>,
    interpreter_options: Option<InterpreterOptions>,
) -> Result<(), String>
where
    T::String: StringValue + Send + Sync,
{
    let config = match parse_command_line_args() {
        Err(error) => Err(error),
        Ok(CliArgs {
            entry_point,
            port,
            debug_signals,
            debug_compiler,
            debug_interpreter,
        }) => {
            let entry_point = match PathBuf::from_str(&entry_point) {
                Err(_) => Err(format!("Invalid entry point module path: {}", entry_point)),
                Ok(path) => load_file(&path).map(|source| (path, source)),
            };
            match entry_point {
                Err(error) => Err(error),
                Ok(entry_point) => Ok((
                    env::vars(),
                    entry_point,
                    SocketAddr::from(([0, 0, 0, 0], port)),
                    debug_signals,
                    debug_compiler,
                    debug_interpreter,
                )),
            }
        }
    };
    match config {
        Err(error) => Err(format!("Unable to start server: {}", error)),
        Ok((env_vars, entry_point, address, debug_signals, debug_compiler, debug_interpreter)) => {
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
                        debug: debug_compiler,
                        ..CompilerOptions::default()
                    });
                    let interpreter_options =
                        interpreter_options.unwrap_or_else(|| InterpreterOptions {
                            debug: debug_interpreter,
                            ..InterpreterOptions::default()
                        });
                    let store = if debug_signals {
                        create_store(
                            root,
                            debug_signal_handler(signal_handler),
                            factory,
                            allocator,
                            plugins,
                            compiler_options,
                            interpreter_options,
                        )
                    } else {
                        create_store(
                            root,
                            signal_handler,
                            factory,
                            allocator,
                            plugins,
                            compiler_options,
                            interpreter_options,
                        )
                    }?;
                    let server =
                        create_server(store, factory, allocator, compiler_options, &address);
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

fn create_graph_root<T: Expression + 'static>(
    root_module_path: &Path,
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
        root_module_path,
        &module_loader,
        factory,
        allocator,
    )
}

fn create_store<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
    THandler,
>(
    root: T,
    signal_handler: THandler,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    plugins: NativeFunctionRegistry<T>,
    compiler_options: CompilerOptions,
    interpreter_options: InterpreterOptions,
) -> Result<Runtime<T>, String>
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    let compiled_root = Compiler::new(compiler_options, None).compile(
        &root,
        CompilerMode::Thunk,
        factory,
        allocator,
    )?;
    // TODO: Error if graph root depends on unrecognized native functions
    let (program, _) = compiled_root.into_parts();
    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 1024;
    let result_buffer_size = 1024;
    Ok(Runtime::new(
        program,
        signal_handler,
        factory,
        allocator,
        plugins,
        compiler_options,
        interpreter_options,
        command_buffer_size,
        result_buffer_size,
    ))
}

async fn create_server<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + Execute<T>,
>(
    store: Runtime<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    address: &SocketAddr,
) -> Result<(), String>
where
    T::String: StringValue + Send + Sync,
{
    let store = Arc::new(store);
    let server = Server::bind(&address).serve(make_service_fn(|_socket: &AddrStream| {
        let store = Arc::clone(&store);
        let service = graphql_service(store, factory, allocator, compiler_options);
        async { Ok::<_, Infallible>(service) }
    }));
    match server.await {
        Err(error) => Err(format!("{}", error)),
        Ok(()) => Ok(()),
    }
}

fn parse_command_line_args() -> Result<CliArgs, String> {
    let args = parse_cli_args(env::args().skip(1));
    let port = match args
        .get("port")
        .and_then(|port| port.map(|port| port.parse::<u16>().or_else(|_| Err(port))))
    {
        Some(Ok(value)) => Ok(value),
        None => Err(String::from("Missing --port argument")),
        Some(Err(value)) => Err(format!("Invalid --port argument: {}", value)),
    }?;
    let debug_signals = args.get("debug").is_some();
    let debug_compiler = args.get("bytecode").is_some();
    let debug_interpreter = args.get("vm").is_some();
    let mut args = args.into_iter();
    let entry_point = args.next();
    match entry_point {
        None => Err(String::from("Missing entry point module path")),
        Some(entry_point) => {
            if let Some(_) = args.next() {
                Err(String::from("Multiple entry point modules specified"))
            } else {
                Ok(CliArgs {
                    port,
                    entry_point,
                    debug_signals,
                    debug_compiler,
                    debug_interpreter,
                })
            }
        }
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
