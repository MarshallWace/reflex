// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::process;

use reflex_graphql::graphql_loader;
use reflex_js::{compose_module_loaders, stdlib::imports::builtin_imports_loader};
use reflex_runtime::SignalHelpers;
use reflex_server::cli::{builtin_signal_handler, cli, reflex::core::Expression, SignalResult};

#[tokio::main]
pub async fn main() {
    process::exit(
        match cli(
            create_signal_handler(),
            Some(compose_module_loaders(
                builtin_imports_loader(),
                graphql_loader,
            )),
        )
        .await
        {
            Ok(_) => 0,
            Err(error) => {
                eprintln!("{}", error);
                1
            }
        },
    );
}

fn create_signal_handler(
) -> impl Fn(&str, &[Expression], &SignalHelpers) -> Option<Result<SignalResult, String>>
       + Send
       + Sync
       + 'static {
    builtin_signal_handler()
}
