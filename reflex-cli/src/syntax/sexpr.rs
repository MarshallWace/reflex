// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use reflex::core::{
    Applicable, Expression, ExpressionFactory, HeapAllocator, InstructionPointer, Reducible,
    Rewritable,
};
use reflex_interpreter::compiler::{Compile, CompiledProgram, CompilerMode, CompilerOptions};
use reflex_lisp::LispParserBuiltin;

use crate::{compile_graph_root, SyntaxParser};

pub fn create_sexpr_parser<T: Expression + Rewritable<T> + Reducible<T>>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T>
where
    T::Builtin: LispParserBuiltin,
{
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |input: &str| match reflex_lisp::parse(input, &factory, &allocator) {
        Ok(result) => Ok(result),
        Err(error) => Err(format!("{}", error)),
    }
}

pub fn compile_sexpr_entry_point<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    path: &Path,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(CompiledProgram, InstructionPointer)>
where
    T::Builtin: LispParserBuiltin,
{
    let input = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to load Lisp graph definition: {}", path.display(),))?;
    let root = reflex_lisp::parse(&input, factory, allocator)
        .map_err(|error| anyhow!("{}", error))
        .with_context(|| format!("Failed to parse Lisp graph definition: {}", path.display()))?;
    compile_graph_root(
        root,
        factory,
        allocator,
        compiler_options,
        CompilerMode::Function,
    )
}
