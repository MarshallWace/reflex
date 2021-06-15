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
    cache::GenerationalGc,
    core::{Expression, SerializedTerm, Term},
    stdlib::value::{StringValue, ValueTerm},
};
use reflex_cli::parse_cli_args;
use reflex_handlers::debug_signal_handler;
use reflex_js::{
    parse_module,
    stdlib::{builtin_globals, global_process},
    Env,
};
use reflex_runtime::{Runtime, SignalHelpers};

pub use reflex;
pub use reflex_handlers::builtin_signal_handler;
pub use reflex_js::{dynamic_module_loader, stdlib::builtin_imports};
pub use reflex_loaders::builtin_loaders;
pub use reflex_runtime::SignalResult;

struct CliArgs {
    entry_point: String,
    port: u16,
    debug: bool,
}

pub async fn cli(
    signal_handler: impl Fn(&str, &[SerializedTerm], &SignalHelpers) -> Option<Result<SignalResult, String>>
        + Send
        + Sync
        + 'static,
    module_loader: impl Fn(&str, &Path) -> Result<Expression, String>,
) -> Result<(), String> {
    let env_vars = parse_env_vars(env::vars());
    let config = match parse_command_line_args() {
        Err(error) => Err(error),
        Ok(CliArgs {
            entry_point,
            port,
            debug,
        }) => {
            let entry_point = match PathBuf::from_str(&entry_point) {
                Err(_) => Err(format!("Invalid entry point module path: {}", entry_point)),
                Ok(path) => load_file(&path).map(|source| (path, source)),
            };
            match entry_point {
                Err(error) => Err(error),
                Ok(entry_point) => Ok((
                    env_vars,
                    entry_point,
                    SocketAddr::from(([127, 0, 0, 1], port)),
                    debug,
                )),
            }
        }
    };
    match config {
        Err(error) => Err(format!("Unable to start server: {}", error)),
        Ok((env_vars, entry_point, address, debug)) => {
            let (root_module_path, root_module_source) = entry_point;
            match create_graph_root(
                &root_module_path,
                &root_module_source,
                env_vars,
                module_loader,
            ) {
                Err(error) => Err(format!("Failed to load entry point module: {}", error)),
                Ok(root) => {
                    let store = if debug {
                        create_store(debug_signal_handler(signal_handler))
                    } else {
                        create_store(signal_handler)
                    };
                    let server = create_server(store, root, &address);
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

fn create_graph_root(
    root_module_path: &Path,
    root_module_source: &str,
    env_args: impl IntoIterator<Item = (String, Expression)>,
    module_loader: impl Fn(&str, &Path) -> Result<Expression, String>,
) -> Result<Expression, String> {
    parse_module(
        root_module_source,
        &create_env(env_args),
        root_module_path,
        &module_loader,
    )
}

fn create_env(env_args: impl IntoIterator<Item = (String, Expression)>) -> Env {
    Env::new()
        .with_globals(builtin_globals())
        .with_global("process", global_process(env_args))
}

fn create_store<THandler>(signal_handler: THandler) -> Runtime
where
    THandler: Fn(&str, &[SerializedTerm], &SignalHelpers) -> Option<Result<SignalResult, String>>
        + Send
        + Sync
        + 'static,
{
    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 1024;
    let result_buffer_size = 1024;
    Runtime::new(
        signal_handler,
        GenerationalGc::new(),
        command_buffer_size,
        result_buffer_size,
    )
}

async fn create_server(
    store: Runtime,
    root: Expression,
    address: &SocketAddr,
) -> Result<(), String> {
    let store = Arc::new(store);
    let server = Server::bind(&address).serve(make_service_fn(|_socket: &AddrStream| {
        let store = Arc::clone(&store);
        let root = Expression::clone(&root);
        let service = graphql_service(store, root);
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
    let debug = args.get("debug").is_some();
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
                    debug,
                })
            }
        }
    }
}

fn parse_env_vars(vars: env::Vars) -> impl IntoIterator<Item = (String, Expression)> {
    vars.into_iter().map(|(name, value)| {
        (
            name,
            Expression::new(Term::Value(ValueTerm::String(StringValue::from(value)))),
        )
    })
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
