// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::PathBuf;

use anyhow::{anyhow, Result};
use clap::Parser;
use reflex::{
    allocator::DefaultAllocator,
    core::{ExpressionFactory, SerializeJson},
    lang::SharedTermFactory,
};
use reflex_cli::{format_signal_result, syntax::js::default_js_loaders, Syntax};
use reflex_graphql::graphql_imports;
use reflex_handlers::builtin_signal_handler;
use reflex_server::{
    builtins::ServerBuiltins,
    cli::execute_query::{cli, default_captured_signals, ExecuteQueryCliOptions},
};

/// Execute a GraphQL query against the provided graph root
#[derive(Parser)]
pub struct Args {
    /// Path to graph definition entry point
    #[clap(long)]
    graph_root: PathBuf,
    /// Graph definition syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// GraphQL query
    #[clap(long)]
    query: String,
    /// JSON-formatted GraphQL query arguments
    #[clap(long)]
    args: Option<String>,
    /// Path to capture runtime signal playback file
    #[clap(long)]
    capture_signals: Option<PathBuf>,
    /// Path to previously-captured signal playback file
    #[clap(long)]
    replay_signals: Option<PathBuf>,
    /// Comma-separated list of captured signal types
    #[clap(long)]
    captured_signals: Option<Vec<String>>,
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
impl Into<ExecuteQueryCliOptions> for Args {
    fn into(self) -> ExecuteQueryCliOptions {
        ExecuteQueryCliOptions {
            graph_root: self.graph_root,
            syntax: self.syntax,
            query: self.query,
            args: self.args,
            capture_signals: self.capture_signals,
            replay_signals: self.replay_signals,
            unoptimized: self.unoptimized,
            debug_compiler: self.debug_compiler,
            debug_signals: self.debug_signals,
            debug_interpreter: self.debug_interpreter,
            debug_stack: self.debug_stack,
            captured_signals: self
                .captured_signals
                .unwrap_or_else(|| default_captured_signals()),
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let signal_handler = builtin_signal_handler(&factory, &allocator);
    let module_loader = Some(default_js_loaders(
        graphql_imports(&factory, &allocator),
        &factory,
        &allocator,
    ));
    let result = cli(
        Args::parse().into(),
        Some(std::env::vars()),
        signal_handler,
        module_loader,
        &factory,
        &allocator,
    )
    .await?;
    let output = if let Some(signal) = factory.match_signal_term(&result) {
        Err(anyhow!("{}", format_signal_result(signal, &factory)))
    } else if let Ok(value) = result.to_json() {
        Ok(format!("{}", value))
    } else {
        Err(anyhow!("Invalid query result: {}", result))
    }?;
    println!("{}", output);
    Ok(())
}
