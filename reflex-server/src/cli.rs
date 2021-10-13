// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{
    convert::Infallible, env, fs, io::Write, net::SocketAddr, path::Path, sync::Arc, time::Instant,
};

use crate::graphql_service;
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    compiler::{
        serialization::SerializableCompilerOutput, Compile, Compiler, CompilerMode,
        CompilerOptions, CompilerOutput, Program,
    },
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
) -> Result<()>
where
    T::String: StringValue + Send + Sync,
{
    let args: Opts = Opts::parse();
    let root_module_path = &args.entry_point;
    let compiler_options = compiler_options.unwrap_or_else(|| CompilerOptions {
        debug: args.debug_compiler,
        ..CompilerOptions::default()
    });
    let compiled_root = if args.precompiled {
        deserialize_compiler_output(root_module_path, plugins)?
    } else {
        compile_source(
            root_module_path,
            custom_loader,
            factory,
            allocator,
            plugins,
            compiler_options,
        )?
    };

    let interpreter_options = interpreter_options.unwrap_or_else(|| InterpreterOptions {
        debug_instructions: args.debug_interpreter || args.debug_stack,
        debug_stack: args.debug_stack,
        ..InterpreterOptions::default()
    });
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
    let address = SocketAddr::from(([0, 0, 0, 0], args.port));
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
        Err(anyhow!("Server error: {}", error))
    } else {
        Ok(())
    }
}

fn deserialize_compiler_output<T>(
    bytecode_file: impl AsRef<Path> + std::fmt::Display,
    plugins: impl IntoIterator<Item = NativeFunction<T>>,
) -> Result<CompilerOutput<T>>
where
    T: Expression,
{
    let bytecode_file = fs::File::open(&bytecode_file)
        .with_context(|| format!("Failed to open {}", bytecode_file))?;
    let serialized_output: SerializableCompilerOutput = serde_json::from_reader(bytecode_file)?;
    let plugins = plugins.into_iter().map(|func| (func.uid(), func)).collect();
    serialized_output
        .to_compiler_output(plugins)
        .map_err(|err| anyhow!(err))
}

fn compile_source<T>(
    root_module_path: impl AsRef<Path> + std::fmt::Display,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
    plugins: impl IntoIterator<Item = NativeFunction<T>>,
    compiler_options: CompilerOptions,
) -> Result<CompilerOutput<T>>
where
    T: AsyncExpression
        + Expression<String = String>
        + Rewritable<T>
        + Reducible<T>
        + Applicable<T>
        + Compile<T>
        + 'static,
{
    let root_module_source = fs::read_to_string(&root_module_path)
        .with_context(|| format!("Failed to load {}", root_module_path))?;
    let env_vars = env::vars();
    let root = create_graph_root(
        root_module_path,
        &root_module_source,
        env_vars,
        custom_loader,
        factory,
        allocator,
    )?;
    compile_root(root, factory, allocator, plugins, compiler_options)
}

pub fn create_graph_root<T: Expression + 'static>(
    root_module_path: impl AsRef<Path>,
    root_module_source: &str,
    env_args: impl IntoIterator<Item = (String, String)>,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Result<T> {
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
    .map_err(|err| anyhow!("Failed to parse module: {}", err))
}

pub fn compile_root<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    root: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    plugins: impl IntoIterator<Item = NativeFunction<T>>,
    compiler_options: CompilerOptions,
) -> Result<CompilerOutput<T>> {
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
    compiled.map_err(|err| anyhow!(err))
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
