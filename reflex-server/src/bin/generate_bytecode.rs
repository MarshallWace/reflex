// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use anyhow::{anyhow, Result};
use clap::Parser;
use reflex::{
    allocator::DefaultAllocator,
    compiler::{serialization::SerializableCompilerOutput, CompilerOptions},
    lang::{term::CachedTerm, SharedTerm, TermFactory},
};
use reflex_graphql::{graphql_loader, graphql_plugins};
use reflex_server::cli::{compile_root, create_graph_root};
use std::{env, fs};

use reflex_js::{builtin_plugins, compose_module_loaders, stdlib::imports::builtin_imports_loader};

#[derive(Parser)]
struct Opts {
    #[clap(about = "A js file to compile")]
    in_file: String,
    #[clap(about = "A file to store output compilation artefacts")]
    out_file: String,
    #[clap(long, about = "Add debug printing of bytecode output from compiler")]
    print_bytecode: bool,
}

fn main() -> Result<()> {
    let args: Opts = Opts::parse();
    let path = args.in_file;
    let source = fs::read_to_string(&path)?;
    let env_vars = env::vars();
    let factory = &TermFactory::default();
    let allocator = &DefaultAllocator::<CachedTerm<SharedTerm>>::default();

    let module_loaders = compose_module_loaders(
        builtin_imports_loader(factory, allocator),
        graphql_loader(factory, allocator),
    );

    let plugins = builtin_plugins().into_iter().chain(graphql_plugins());

    let root = create_graph_root(
        &path,
        &source,
        env_vars,
        Some(module_loaders),
        factory,
        allocator,
    )
    .map_err(|err| anyhow!(err))?;

    let compiler_options = CompilerOptions {
        debug: args.print_bytecode,
        ..CompilerOptions::default()
    };
    let compiler_output: SerializableCompilerOutput =
        compile_root(root, factory, allocator, plugins, compiler_options)
            .map_err(|err| anyhow!(err))?
            .into();
    let out_file = fs::File::create(args.out_file)?;

    serde_json::to_writer(out_file, &compiler_output)?;

    Ok(())
}
