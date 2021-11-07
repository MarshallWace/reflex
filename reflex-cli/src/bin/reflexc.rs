// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use anyhow::{anyhow, Result};
use clap::Parser;
use reflex::compiler::{CompilerMode, CompilerOptions};

use reflex_cli::compiler::js::compile_js_source;
use reflex_cli::Syntax;
use std::str::FromStr;

#[derive(Parser)]
struct Opts {
    #[clap(about = "A file to compile")]
    in_file: String,
    #[clap(long, about = "Syntax of the input file", default_value = "JavaScript")]
    syntax: Syntax,
    #[clap(
        long,
        about = "Serialization format of the output file",
        default_value = "RMP"
    )]
    output_format: OutputFormat,
    #[clap(
        long,
        about = "Mode to run the compiler in",
        default_value = "function"
    )]
    compiler_mode: String,
}

enum OutputFormat {
    Json,
    Rmp,
    Debug,
}

impl FromStr for OutputFormat {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.to_lowercase() == "json" {
            Ok(Self::Json)
        } else if s.to_ascii_lowercase() == "rmp" {
            Ok(Self::Rmp)
        } else if s.to_ascii_lowercase() == "debug" {
            Ok(Self::Debug)
        } else {
            Err(anyhow!("Unknown syntax {}", s))
        }
    }
}

fn main() -> Result<()> {
    let args: Opts = Opts::parse();
    let path = args.in_file;

    let compiler_options = CompilerOptions {
        // TODO        debug: args.print_bytecode,
        ..CompilerOptions::default()
    };

    let compiler_mode = if args.compiler_mode.to_lowercase() == "expression" {
        CompilerMode::Expression
    } else {
        CompilerMode::Function
    };

    let compiler_output = match args.syntax {
        Syntax::JavaScript => compile_js_source(path, compiler_options, compiler_mode)?,
        _ => {
            return Err(anyhow!(
                "Syntax {:?} is not currently supported for bytecode compilation",
                args.syntax
            ))
        }
    };

    let stdout = std::io::stdout();
    let mut stdout_handle = stdout.lock();
    match args.output_format {
        OutputFormat::Json => serde_json::to_writer(stdout_handle, &compiler_output)?,
        OutputFormat::Rmp => rmp_serde::encode::write(&mut stdout_handle, &compiler_output)?,
        OutputFormat::Debug => println!("{}", &compiler_output),
    }

    Ok(())
}
