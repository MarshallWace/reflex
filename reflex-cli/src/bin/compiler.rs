// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::empty;
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use reflex::allocator::DefaultAllocator;

use reflex::lang::SharedTermFactory;
use reflex_cli::cli::compiler::{cli, CompilerCliOptions, OutputFormat};
use reflex_cli::syntax::js::default_js_loaders;
use reflex_cli::Syntax;
use reflex_js::builtins::JsBuiltins;

/// Reflex bytecode compiler
#[derive(Parser)]
#[clap(name = "reflexc")]
struct Args {
    /// Path to program entry point
    entry_point: PathBuf,
    /// Input file syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Output file serialization format
    #[clap(long, default_value = "rmp")]
    output_format: OutputFormat,
    /// Inline current environment variables into compiled output
    #[clap(long)]
    inline_env: bool,
    /// Prevent static compiler optimizations
    #[clap(long)]
    unoptimized: bool,
    /// Log compiler output
    #[clap(long)]
    debug: bool,
}
impl Into<CompilerCliOptions> for Args {
    fn into(self) -> CompilerCliOptions {
        CompilerCliOptions {
            entry_point: self.entry_point,
            syntax: self.syntax,
            output_format: self.output_format,
            unoptimized: self.unoptimized,
            debug: self.debug,
            env: if self.inline_env {
                Some(std::env::vars().into_iter().collect::<Vec<_>>())
            } else {
                None
            },
        }
    }
}

fn main() -> Result<()> {
    let factory = SharedTermFactory::<JsBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let stdout = std::io::stdout();
    cli(
        Args::parse().into(),
        Some(default_js_loaders(empty(), &factory, &allocator)),
        &factory,
        &allocator,
        stdout.lock(),
    )
}
