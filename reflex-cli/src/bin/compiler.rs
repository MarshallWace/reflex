// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{io::Write, path::PathBuf, str::FromStr};

use anyhow::{anyhow, Result};
use clap::Parser;
use reflex::allocator::DefaultAllocator;
use reflex::compiler::CompilerOptions;

use reflex::lang::SharedTermFactory;
use reflex_cli::syntax::js::default_js_loaders;
use reflex_cli::{compile_entry_point, Syntax};
use reflex_js::builtins::JsBuiltins;

/// Reflex bytecode compiler
#[derive(Parser)]
#[clap(name = "reflexc")]
struct Opts {
    #[clap(about = "Path to program entry point")]
    in_file: PathBuf,
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

enum OutputFormat {
    Json,
    Rmp,
    Debug,
}
impl FromStr for OutputFormat {
    type Err = anyhow::Error;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.to_lowercase().as_str() {
            "json" => Ok(Self::Json),
            "rmp" => Ok(Self::Rmp),
            "log" | "debug" => Ok(Self::Debug),
            _ => Err(anyhow!("Unknown syntax {}", input)),
        }
    }
}

fn main() -> Result<()> {
    let args: Opts = Opts::parse();
    let path = args.in_file;
    let compiler_options = CompilerOptions {
        debug: args.debug,
        ..if args.unoptimized {
            CompilerOptions::unoptimized()
        } else {
            CompilerOptions::default()
        }
    };
    let env = if args.inline_env {
        Some(std::env::vars())
    } else {
        None
    };
    let factory = SharedTermFactory::<JsBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let (program, _entry_point) = compile_entry_point(
        path.as_path(),
        args.syntax,
        env,
        Some(default_js_loaders(&factory, &allocator)),
        &compiler_options,
        &factory,
        &allocator,
    )
    .map_err(|err| anyhow!("{}", err))?;
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    match args.output_format {
        OutputFormat::Json => serde_json::to_writer(stdout, &program)?,
        OutputFormat::Rmp => rmp_serde::encode::write(&mut stdout, &program)?,
        OutputFormat::Debug => write!(stdout, "{}", &program)?,
    }
    Ok(())
}
