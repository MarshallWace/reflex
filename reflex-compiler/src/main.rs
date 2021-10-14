// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use anyhow::{anyhow, Result};
use clap::Parser;
use reflex::compiler::{serialization::SerializableCompilerOutput, CompilerOptions};

use reflex_compiler::js::compile_js_source;
// use reflex_server::cli::{compile_root, create_graph_root};
use std::str::FromStr;

#[derive(Parser)]
struct Opts {
    #[clap(about = "A file to compile")]
    in_file: String,
    #[clap(long, about = "Syntax of the input file", default_value = "JavaScript")]
    syntax: Syntax,
}

enum Syntax {
    JavaScript,
}

impl FromStr for Syntax {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.to_lowercase() == "javascript" || s == "js" {
            return Ok(Self::JavaScript);
        } else {
            return Err(anyhow!("Unknown syntax {}", s));
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

    let compiler_output: SerializableCompilerOutput = match args.syntax {
        Syntax::JavaScript => compile_js_source(path, compiler_options)?.into(),
    };

    let stdout = std::io::stdout();
    let stdout_handle = stdout.lock();
    serde_json::to_writer(stdout_handle, &compiler_output)?;

    Ok(())
}
