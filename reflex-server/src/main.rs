// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{env, net::SocketAddr, str::FromStr};

use reflex_js::{
    stdlib::{builtin_globals, builtin_imports},
    Env,
};
use reflex_runtime::builtin_signal_handlers;
use reflex_server::run;

#[tokio::main]
pub async fn main() {
    let port = parse_env_var::<u16>("PORT");
    match port {
        Err(error) => eprintln!("Unable to start server: {}", error),
        Ok(port) => {
            let address = SocketAddr::from(([127, 0, 0, 1], port));
            let env = Env::new()
                .with_globals(builtin_globals())
                .with_imports(builtin_imports());
            let signal_handlers = builtin_signal_handlers();
            let server = run(env, signal_handlers, &address);
            if let Err(error) = server.await {
                eprintln!("Server error: {}", error);
            }
        }
    }
}

fn parse_env_var<T: FromStr>(key: &str) -> Result<T, String> {
    let value = env::var("PORT");
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
