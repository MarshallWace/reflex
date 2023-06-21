// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use reflex::{
    compiler::{Compile, CompilerMode, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    env::inject_env_vars,
    stdlib::Stdlib,
};
use reflex_graphql::graphql_loader;
use reflex_js::{
    builtin_imports, compose_module_loaders, create_js_env, create_module_loader, parse_module,
    static_module_loader, stdlib::Stdlib as JsStdlib,
};

use crate::{compile_graph_root, SyntaxParser};

pub fn default_js_loaders<'a, T: Expression + 'static>(
    imports: impl IntoIterator<
        Item = (&'static str, T),
        IntoIter = impl Iterator<Item = (&'static str, T)> + 'static,
    >,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> + 'static
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    compose_module_loaders(
        static_module_loader(
            imports
                .into_iter()
                .chain(builtin_imports(factory, allocator)),
        ),
        graphql_loader(factory, allocator),
    )
}

pub fn create_js_script_parser<T: Expression + Rewritable<T> + 'static>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let env = create_js_env(factory, allocator);
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |input: &str| reflex_js::parse(input, &env, &factory, &allocator)
}

pub fn create_js_module_parser<T: Expression + Rewritable<T> + 'static>(
    path: &Path,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T>
where
    T::Builtin: From<Stdlib> + From<JsStdlib>,
{
    let env = create_js_env(factory, allocator);
    let loader = create_module_loader(
        env.clone(),
        Some(static_module_loader(builtin_imports(factory, allocator))),
        factory,
        allocator,
    );
    let factory = factory.clone();
    let allocator = allocator.clone();
    let path = path.to_owned();
    move |input: &str| reflex_js::parse_module(input, &env, &path, &loader, &factory, &allocator)
}

pub fn compile_js_entry_point<T: Expression + Rewritable<T> + Reducible<T> + 'static, TLoader>(
    path: &Path,
    env: Option<impl IntoIterator<Item = (String, String)>>,
    module_loader: Option<TLoader>,
    compiler_options: &CompilerOptions,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Result<(Program, InstructionPointer)>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::Builtin: From<Stdlib> + From<JsStdlib>,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    let input = std::fs::read_to_string(path).with_context(|| {
        format!(
            "Failed to load JavaScript graph definition: {}",
            path.display(),
        )
    })?;
    let js_env = create_js_env(factory, allocator);
    let module_loader = create_module_loader(js_env.clone(), module_loader, factory, allocator);
    let root = parse_module(&input, &js_env, path, &module_loader, factory, allocator)
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| {
            format!(
                "Failed to parse JavaScript graph definition: {}",
                path.display()
            )
        })?;
    let root = if let Some(env) = env {
        inject_env_vars(root, env, factory, allocator)
    } else {
        root
    };
    compile_graph_root(
        root,
        factory,
        allocator,
        compiler_options,
        CompilerMode::Function,
    )
}
