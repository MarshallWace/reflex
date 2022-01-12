// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use reflex::{allocator::DefaultAllocator, compiler::CompilerOptions, lang::SharedTermFactory};
use reflex_cli::{syntax::js::default_js_loaders, Syntax};
use reflex_graphql::graphql_imports;
use reflex_handlers::builtin_signal_handler;
use reflex_server::{
    builtins::ServerBuiltins,
    cli::reflex_server::{cli, ReflexServerCliOptions},
    NoopGraphQlHttpQueryTransform,
};

/// Launch a GraphQL server for the provided graph root
#[derive(Parser)]
struct Args {
    /// Path to graph definition entry point
    entry_point: PathBuf,
    /// Graph definition syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Port on which to expose a GraphQL HTTP server
    #[clap(long)]
    port: u16,
    /// Prevent static compiler optimizations
    #[clap(long)]
    unoptimized: bool,
    /// Log compiler output
    #[clap(long)]
    debug_compiler: bool,
    /// Log runtime signals
    #[clap(long)]
    debug_signals: bool,
    /// Log interpreter instructions
    #[clap(long)]
    debug_interpreter: bool,
    /// Log interpreter stack
    #[clap(long)]
    debug_stack: bool,
}
impl Into<ReflexServerCliOptions> for Args {
    fn into(self) -> ReflexServerCliOptions {
        ReflexServerCliOptions {
            entry_point: self.entry_point,
            syntax: self.syntax,
            port: self.port,
            unoptimized: self.unoptimized,
            debug_compiler: self.debug_compiler,
            debug_signals: self.debug_signals,
            debug_interpreter: self.debug_interpreter,
            debug_stack: self.debug_stack,
        }
    }
}

#[tokio::main]
pub async fn main() -> Result<()> {
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    cli(
        Args::parse().into(),
        builtin_signal_handler(&factory, &allocator),
        Some(default_js_loaders(
            graphql_imports(&factory, &allocator),
            &factory,
            &allocator,
        )),
        &factory,
        &allocator,
        Some(CompilerOptions::unoptimized()),
        None,
        NoopGraphQlHttpQueryTransform::default(),
    )
    .await
}
