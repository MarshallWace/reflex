// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    convert::Infallible, env, fs, net::SocketAddr, path::Path, process, str::FromStr, sync::Arc,
};

use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    core::{Expression, SerializedTerm, Term},
    stdlib::value::{StringValue, ValueTerm},
};
use reflex_handlers::{builtin_signal_handler, debug_signal_handler};
use reflex_js::{
    dynamic_module_loader, parse_module,
    stdlib::{builtin_globals, builtin_imports, global_process},
    Env,
};
use reflex_runtime::{Runtime, SignalResult};
use reflex_server::{graphql_service, loaders::graphql::graphql_module_loader};

struct Args {
    root_path: String,
    debug: bool,
}

#[tokio::main]
pub async fn main() {
    let port = parse_env_var::<u16>("PORT");
    let env_vars = parse_env_vars(env::vars());
    let config = match parse_args() {
        Err(error) => Err(error),
        Ok(Args { root_path, debug }) => match load_file(&Path::new(&root_path)) {
            Err(error) => Err(error),
            Ok(root_source) => match port {
                Err(error) => Err(error),
                Ok(port) => Ok((
                    env_vars,
                    root_path,
                    root_source,
                    SocketAddr::from(([127, 0, 0, 1], port)),
                    debug,
                )),
            },
        },
    };
    let server = match config {
        Err(error) => Err(format!("Unable to start server: {}", error)),
        Ok((env_vars, root_module_path, root_module_source, address, debug)) => {
            match create_graph_root(&Path::new(&root_module_path), &root_module_source, env_vars) {
                Err(error) => Err(format!("Failed to load entry point module: {}", error)),
                Ok(root) => {
                    let signal_handler = create_signal_handler();
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
    };
    process::exit(match server {
        Ok(_) => 0,
        Err(error) => {
            eprintln!("{}", error);
            1
        }
    })
}

fn create_signal_handler(
) -> impl Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static
{
    builtin_signal_handler()
}

fn create_graph_root(
    root_module_path: &Path,
    root_module_source: &str,
    env_args: impl IntoIterator<Item = (String, Expression)>,
) -> Result<Expression, String> {
    parse_module(
        root_module_source,
        &create_env(env_args),
        root_module_path,
        &dynamic_module_loader(vec![graphql_module_loader], Some(builtin_imports())),
    )
}

fn create_env(env_args: impl IntoIterator<Item = (String, Expression)>) -> Env {
    Env::new()
        .with_globals(builtin_globals())
        .with_global("process", global_process(env_args))
}

fn create_store<THandler>(signal_handler: THandler) -> Runtime
where
    THandler:
        Fn(&str, &[SerializedTerm]) -> Option<Result<SignalResult, String>> + Send + Sync + 'static,
{
    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 32;
    let result_buffer_size = 32;
    Runtime::new(signal_handler, command_buffer_size, result_buffer_size)
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

fn parse_args() -> Result<Args, String> {
    let (debug_args, path_args): (Vec<_>, Vec<_>) =
        env::args().skip(1).partition(|arg| arg == "--debug");
    match path_args.len() {
        0 => Err(String::from("Missing entry point module path")),
        1 => Ok(Args {
            root_path: path_args.into_iter().next().unwrap(),
            debug: !debug_args.is_empty(),
        }),
        _ => Err(String::from("Multiple entry point modules specified")),
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

fn parse_env_var<T: FromStr>(key: &str) -> Result<T, String> {
    let value = env::var(key);
    match value {
        Err(error) => match error {
            env::VarError::NotPresent => Err(format!("Missing {} environment variable", key)),
            env::VarError::NotUnicode(_) => Err(format!("Invalid {} environment variable", key)),
        },
        Ok(value) => match value.parse::<T>() {
            Ok(value) => Ok(value),
            Err(_) => Err(format!("Invalid {} environment variable: {}", key, value)),
        },
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
