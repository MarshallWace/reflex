// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::process;

use reflex::{allocator::DefaultAllocator, lang::TermFactory};
use reflex_cli::compiler::js::{standard_js_loaders, standard_js_plugins};
use reflex_server::{
    cli::{builtin_signal_handler, cli},
    NoopGraphQlHttpQueryTransform,
};

#[tokio::main]
pub async fn main() {
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    process::exit(
        match cli(
            builtin_signal_handler(&factory, &allocator),
            Some(standard_js_loaders(&factory, &allocator)),
            &factory,
            &allocator,
            standard_js_plugins(),
            None,
            None,
            NoopGraphQlHttpQueryTransform::default(),
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
