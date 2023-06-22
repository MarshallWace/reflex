// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{iter::once, path::Path};

use anyhow::{anyhow, Context, Result};
use reflex::core::{
    Applicable, Expression, ExpressionFactory, HeapAllocator, InstructionPointer, Reducible,
    Rewritable,
};
use reflex_interpreter::compiler::{Compile, CompiledProgram, CompilerMode, CompilerOptions};

use crate::{compile_graph_root, SyntaxParser};

pub fn create_json_parser<T: Expression + Rewritable<T> + Reducible<T>>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |input: &str| reflex_json::parse(input, &factory, &allocator)
}

pub fn json_loader<T: Expression>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl Fn(&str, &Path) -> Option<Result<T, String>> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |import_path: &str, module_path: &Path| {
        if !import_path.ends_with(".json") {
            return None;
        }
        let file_path = module_path
            .parent()
            .map(|parent| parent.join(import_path))
            .unwrap_or_else(|| Path::new(import_path).to_path_buf());
        Some(
            load_json_module(&file_path, &factory, &allocator)
                .map(|value| create_default_module_export(value, &factory, &allocator)),
        )
    }
}

fn load_json_module<T: Expression>(
    path: &Path,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, String> {
    let source = match std::fs::read_to_string(path) {
        Ok(source) => Ok(source),
        Err(error) => Err(format!("{}", error)),
    }?;
    reflex_json::parse(&source, factory, allocator)
}

fn create_default_module_export<T: Expression>(
    value: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_record_term(
        allocator.create_struct_prototype(allocator.create_list(once(
            factory.create_string_term(allocator.create_string(String::from("default"))),
        ))),
        allocator.create_unit_list(value),
    )
}

pub fn compile_json_entry_point<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    path: &Path,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(CompiledProgram, InstructionPointer)> {
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
