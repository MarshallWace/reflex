// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::process;

use reflex::{allocator::DefaultAllocator, lang::TermFactory};
use reflex_graphql::{graphql_loader, graphql_plugins};
use reflex_js::{builtin_plugins, compose_module_loaders, stdlib::imports::builtin_imports_loader};
use reflex_server::cli::{builtin_signal_handler, cli};

#[tokio::main]
pub async fn main() {
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    process::exit(
        match cli(
            builtin_signal_handler(&factory, &allocator),
            Some(compose_module_loaders(
                builtin_imports_loader(&factory, &allocator),
                graphql_loader(&factory, &allocator),
            )),
            &factory,
            &allocator,
            builtin_plugins().into_iter().chain(graphql_plugins()),
            None,
            None,
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
