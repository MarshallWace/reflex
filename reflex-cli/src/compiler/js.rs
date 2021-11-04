// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    env::{self, Vars},
    fs,
    io::Write,
    path::Path,
    time::Instant,
};

use anyhow::{anyhow, Context, Result};
use reflex::{
    allocator::DefaultAllocator,
    compiler::{Compile, Compiler, CompilerMode, CompilerOptions, Program},
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    lang::SharedTermFactory,
    stdlib::Stdlib,
};

use reflex_graphql::graphql_loader;
use reflex_js::{
    builtins::JsBuiltins, compose_module_loaders, create_js_env, imports::builtin_imports_loader,
    parse_module, stdlib::Stdlib as JsStdlib,
};

use reflex;

use reflex_js::create_module_loader;

pub fn standard_js_loaders<T: Expression + 'static>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    compose_module_loaders(
        builtin_imports_loader(factory, allocator),
        graphql_loader(factory, allocator),
    )
}

/**
Compile Reflex source file using the standard Reflex Javascript plugins and
module loaders, expression factory and heap allocator. If you wish to
customize these, or e.g. add additional environment variables use
[compile_js_source_with_customisation]
*/
pub fn compile_js_source(
    root_module_path: impl AsRef<Path> + std::fmt::Display,
    compiler_options: CompilerOptions,
    compiler_mode: CompilerMode,
) -> Result<Program> {
    let factory = &SharedTermFactory::<JsBuiltins>::default();
    let allocator = &DefaultAllocator::default();
    let env_vars = env::vars();
    let module_loaders = standard_js_loaders(factory, allocator);

    compile_js_source_with_customisation(
        root_module_path,
        Some(module_loaders),
        factory,
        allocator,
        compiler_options,
        compiler_mode,
        env_vars,
    )
}

pub fn compile_js_source_with_customisation<T: Expression + 'static>(
    root_module_path: impl AsRef<Path> + std::fmt::Display,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
    compiler_options: CompilerOptions,
    compiler_mode: CompilerMode,
    env_vars: Vars,
) -> Result<Program>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let root_module_source = fs::read_to_string(&root_module_path)
        .with_context(|| format!("Failed to load {}", root_module_path))?;
    let root = create_graph_root(
        root_module_path,
        &root_module_source,
        env_vars,
        custom_loader,
        factory,
        allocator,
    )?;
    compile_root(root, factory, allocator, compiler_options, compiler_mode)
}

fn create_graph_root<T: Expression + Rewritable<T> + 'static>(
    root_module_path: impl AsRef<Path>,
    root_module_source: &str,
    env_args: impl IntoIterator<Item = (String, String)>,
    custom_loader: Option<impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Result<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let env = create_js_env(env_args, factory, allocator);
    let module_loader = create_module_loader(env.clone(), custom_loader, factory, allocator);
    parse_module(
        root_module_source,
        &env,
        root_module_path.as_ref(),
        &module_loader,
        factory,
        allocator,
    )
    .map_err(|err| anyhow!("Failed to parse module: {}", err))
}

fn compile_root<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>(
    root: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler_options: CompilerOptions,
    compiler_mode: CompilerMode,
) -> Result<Program> {
    eprint!("Compiling graph root...");
    let _ = std::io::stdout().flush();
    let start_time = Instant::now();
    let compiled =
        Compiler::new(compiler_options, None).compile(&root, compiler_mode, factory, allocator);
    eprintln!(" {:?}", start_time.elapsed());
    // TODO: Error if graph root depends on unrecognized native functions
    compiled.map_err(|err| anyhow!(err))
}
