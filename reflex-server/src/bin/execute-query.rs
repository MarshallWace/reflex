// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use reflex::{
    allocator::DefaultAllocator,
    core::{ExpressionFactory, SerializeJson},
    lang::SharedTermFactory,
};
use reflex_cli::{format_signal_result, syntax::js::default_js_loaders, Syntax};
use reflex_handlers::builtin_signal_handler;
use reflex_server::{
    builtins::ServerBuiltins,
    cli::execute_query::{cli, ExecuteQueryCliOptions},
};

/// Execute a GraphQL query against the provided graph root
#[derive(Parser)]
pub struct Args {
    #[clap(long, about = "Path to graph definition entry point")]
    graph_root: PathBuf,
    #[clap(long, about = "Graph definition syntax", default_value = "javascript")]
    syntax: Syntax,
    #[clap(long, about = "GraphQL query")]
    query: String,
    #[clap(long, about = "JSON-formatted GraphQL query arguments")]
    args: Option<String>,
    #[clap(long, about = "Path to capture runtime signal playback file")]
    capture_signals: Option<PathBuf>,
    #[clap(long, about = "Path to previously-captured signal playback file")]
    replay_signals: Option<PathBuf>,
    #[clap(long, about = "Comma-separated list of captured signal types")]
    captured_signals: Option<Vec<String>>,
    #[clap(long, about = "Prevent static compiler optimizations")]
    unoptimized: bool,
    #[clap(long, about = "Log compiler output")]
    debug_compiler: bool,
    #[clap(long, about = "Log runtime signals")]
    debug_signals: bool,
    #[clap(long, about = "Log interpreter instructions")]
    debug_interpreter: bool,
    #[clap(long, about = "Log interpreter stack")]
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
            captured_signals: self.captured_signals,
            unoptimized: self.unoptimized,
            debug_compiler: self.debug_compiler,
            debug_signals: self.debug_signals,
            debug_interpreter: self.debug_interpreter,
            debug_stack: self.debug_stack,
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let factory = SharedTermFactory::<ServerBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let signal_handler = builtin_signal_handler(&factory, &allocator);
    let module_loader = Some(default_js_loaders(&factory, &allocator));
    let result = cli(
        Args::parse().into(),
        Some(std::env::vars()),
        signal_handler,
        module_loader,
        &factory,
        &allocator,
    )
    .await?;
    if let Some(signal) = factory.match_signal_term(&result) {
        eprintln!("{}", format_signal_result(signal, &factory))
    } else if let Ok(value) = result.to_json() {
        println!("{}", value)
    } else {
        eprintln!("Invalid query result: {}", result)
    }
    Ok(())
}
