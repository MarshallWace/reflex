// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
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
    #[clap(about = "Path to program entry point")]
    entry_point: PathBuf,
    #[clap(long, about = "Input file syntax", default_value = "javascript")]
    syntax: Syntax,
    #[clap(
        long,
        about = "Output file serialization format",
        default_value = "rmp"
    )]
    output_format: OutputFormat,
    #[clap(
        long,
        about = "Inline current environment variables into compiled output"
    )]
    inline_env: bool,
    #[clap(long, about = "Prevent static compiler optimizations")]
    unoptimized: bool,
    #[clap(long, about = "Log compiler output")]
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
        Some(default_js_loaders(&factory, &allocator)),
        &factory,
        &allocator,
        stdout.lock(),
    )
}
