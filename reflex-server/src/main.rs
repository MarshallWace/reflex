// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{env, fs, net::SocketAddr, process, str::FromStr};

use reflex::{
    core::{Expression, Term},
    stdlib::value::{StringValue, ValueTerm},
};
use reflex_js::{
    parse_module, static_module_loader,
    stdlib::{builtin_globals, builtin_imports, global_process},
    Env,
};
use reflex_runtime::builtin_signal_handlers;
use reflex_server::run;

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
        Ok((env_args, root_module, address)) => {
            let env = Env::new()
                .with_globals(builtin_globals())
                .with_global("process", global_process(env_args));
            let loader = static_module_loader(builtin_imports());
            let signal_handlers = builtin_signal_handlers();
            let root = parse_module(&root_module, &env, &loader);
            match root {
                Err(error) => Err(format!("Failed to load entry point module: {}", error)),
                Ok(root) => {
                    let server = run(root, signal_handlers, &address);
                    println!("Listening for incoming HTTP requests on port {}", &address.port());
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
