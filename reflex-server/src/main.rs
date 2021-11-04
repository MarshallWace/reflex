// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::process;

use reflex::{
    allocator::DefaultAllocator,
    core::{ExpressionFactory, Uid},
    lang::SharedTermFactory,
};
use reflex_cli::compiler::js::standard_js_loaders;
use reflex_server::{
    builtins::ServerBuiltins,
    cli::{builtin_signal_handler, cli},
    NoopGraphQlHttpQueryTransform,
};

#[tokio::main]
pub async fn main() {
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    process::exit(
        match cli(
            builtin_signal_handler(&factory, &allocator),
            Some(standard_js_loaders(&factory, &allocator)),
            &factory,
            &allocator,
            ServerBuiltins::entries()
                .map(|builtin| (builtin.uid(), factory.create_builtin_term(builtin))),
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
