// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use reflex::{
    compiler::{Compile, CompilerMode, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    stdlib::Stdlib,
};

use crate::{compile_graph_root, SyntaxParser};

pub fn create_json_parser<T: Expression + Rewritable<T> + Reducible<T>>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T>
where
    T::Builtin: From<Stdlib>,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |input: &str| reflex_json::parse(input, &factory, &allocator)
}

pub fn compile_json_entry_point<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    path: &Path,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(Program, InstructionPointer)>
where
    T::Builtin: From<Stdlib>,
{
    let input = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to load JSON graph definition: {}", path.display(),))?;
    let root = reflex_json::parse(&input, factory, allocator)
        .map_err(|err| anyhow!("{}", err))
        .with_context(|| format!("Failed to parse JSON graph definition: {}", path.display(),))?;
    compile_graph_root(
        root,
        factory,
        allocator,
        compiler_options,
        CompilerMode::Function,
    )
}
