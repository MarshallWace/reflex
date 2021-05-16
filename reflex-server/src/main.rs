// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{convert::Infallible, env, fs, net::SocketAddr, process, str::FromStr, sync::Arc};

use hyper::{server::conn::AddrStream, service::make_service_fn, Server};
use reflex::{
    core::{Expression, Term},
    stdlib::value::{StringValue, ValueTerm},
};
use reflex_js::{
    parse_module, static_module_loader,
    stdlib::{builtin_globals, builtin_imports, global_process},
    Env,
};
use reflex_runtime::{builtin_signal_handlers, Runtime, SignalHandler};
use reflex_server::graphql_http_request_handler;

#[tokio::main]
pub async fn main() {
    let port = parse_env_var::<u16>("PORT");
    let args = parse_args();
    let config = match args {
        Err(error) => Err(error),
        Ok((root_path, env_args)) => match load_file(&root_path) {
            Err(error) => Err(error),
            Ok(source) => match port {
                Err(error) => Err(error),
                Ok(port) => Ok((env_args, source, SocketAddr::from(([127, 0, 0, 1], port)))),
            },
        },
    };
    let server = match config {
        Err(error) => Err(format!("Unable to start server: {}", error)),
        Ok((env_args, root_module, address)) => match create_graph_root(&root_module, env_args) {
            Err(error) => Err(format!("Failed to load entry point module: {}", error)),
            Ok(root) => {
                let store = create_store();
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
        },
    };
    process::exit(match server {
        Ok(_) => 0,
        Err(error) => {
            eprintln!("{}", error);
            1
        }
    })
}

fn create_graph_root(
    root_module: &str,
    env_args: impl IntoIterator<Item = (String, Expression)>,
) -> Result<Expression, String> {
    parse_module(&root_module, &create_env(env_args), &create_module_loader())
}

fn create_env(env_args: impl IntoIterator<Item = (String, Expression)>) -> Env {
    Env::new()
        .with_globals(builtin_globals())
        .with_global("process", global_process(env_args))
}

fn create_module_loader() -> impl Fn(&str) -> Result<Expression, String> {
    static_module_loader(builtin_imports())
}

fn create_signal_handlers() -> impl IntoIterator<Item = (&'static str, SignalHandler)> {
    builtin_signal_handlers()
}

fn create_store() -> Runtime {
    let signal_handlers = create_signal_handlers();
    // TODO: Establish sensible defaults for channel buffer sizes
    let command_buffer_size = 32;
    let result_buffer_size = 32;
    Runtime::new(signal_handlers, command_buffer_size, result_buffer_size)
}

async fn create_server(
    store: Runtime,
    root: Expression,
    address: &SocketAddr,
) -> Result<(), String> {
    let store = Arc::new(store);
    let http_service_factory = make_service_fn(|_socket: &AddrStream| {
        let store = Arc::clone(&store);
        let root = Expression::clone(&root);
        let service = graphql_http_request_handler(store, root);
        async { Ok::<_, Infallible>(service) }
    });
    let server = Server::bind(&address).serve(http_service_factory);
    match server.await {
        Err(error) => Err(format!("{}", error)),
        Ok(()) => Ok(()),
    }
}

fn parse_args() -> Result<(String, Vec<(String, Expression)>), String> {
    let (path_args, env_args) = env::args().skip(1).fold(
        Ok((Vec::<String>::new(), Vec::<(String, Expression)>::new())),
        |result, arg| {
            let (mut path_args, mut env_args) = result?;
            match parse_env_arg(&arg) {
                None => {
                    path_args.push(arg);
                    Ok((path_args, env_args))
                }
                Some(entry) => match entry {
                    Err(error) => Err(error),
                    Ok((key, value)) => {
                        env_args.push((
                            String::from(key),
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                                value,
                            )))),
                        ));
                        Ok((path_args, env_args))
                    }
                },
            }
        },
    )?;
    match path_args.len() {
        0 => Err(String::from("Missing entry point module path")),
        1 => Ok((path_args.into_iter().next().unwrap(), env_args)),
        _ => Err(String::from("Multiple entry point modules specified")),
    }
}

fn parse_env_arg(arg: &String) -> Option<Result<(&str, &str), String>> {
    let equals_index = arg.find(|char| char == '=')?;
    let key = &arg[0..equals_index];
    let value = &arg[(equals_index + 1)..];
    match validate_env_arg_name(key) {
        true => Some(Ok((key, value))),
        false => Some(Err(format!(
            "Invalid environment variable name: \"{}\"",
            key
        ))),
    }
}

fn validate_env_arg_name(name: &str) -> bool {
    name.chars().enumerate().all(|(index, value)| match value {
        'A'..='Z' | '_' => true,
        '0'..='9' => index > 0,
        _ => false,
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

fn load_file(path: &str) -> Result<String, String> {
    match fs::read_to_string(path) {
        Ok(data) => Ok(data),
        Err(_) => Err(format!("Failed to load {}", path)),
    }
}
