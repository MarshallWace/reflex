// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{env, net::SocketAddr, str::FromStr};

use reflex::{
    core::{Expression, Term},
    stdlib::value::{StringValue, ValueTerm},
};
use reflex_js::{
    stdlib::{builtin_globals, global_process},
    Env,
};
use reflex_runtime::builtin_signal_handlers;
use reflex_server::run;

#[tokio::main]
pub async fn main() {
    let port = parse_env_var::<u16>("PORT");
    let env_args = parse_env_args();
    let config = match env_args {
        Err(error) => Err(error),
        Ok(env_args) => match port {
            Err(error) => Err(error),
            Ok(port) => Ok((env_args, port)),
        },
    };
    match config {
        Err(error) => eprintln!("Unable to start server: {}", error),
        Ok((env_args, port)) => {
            let address = SocketAddr::from(([127, 0, 0, 1], port));
            let env = Env::new()
                .with_globals(builtin_globals())
                .with_global("process", global_process(env_args));
            let signal_handlers = builtin_signal_handlers();
            let server = run(env, signal_handlers, &address);
            if let Err(error) = server.await {
                eprintln!("Server error: {}", error);
            }
        }
    }
}

fn parse_env_args() -> Result<impl IntoIterator<Item = (String, Expression)>, String> {
    env::args()
        .into_iter()
        .filter_map(|arg| {
            parse_env_arg(&arg).map(|result| {
                result.map(|(key, value)| {
                    (
                        String::from(key),
                        Expression::new(Term::Value(ValueTerm::String(StringValue::from(value)))),
                    )
                })
            })
        })
        .collect::<Result<Vec<_>, _>>()
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
