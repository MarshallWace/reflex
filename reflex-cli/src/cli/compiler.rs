// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Result};
use reflex::{
    compiler::Compile,
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator},
    stdlib::Stdlib,
};
use reflex::{
    compiler::CompilerOptions,
    core::{Reducible, Rewritable},
};

use crate::{compile_entry_point, Syntax};
use reflex_js::stdlib::Stdlib as JsStdlib;

pub struct CompilerCliOptions {
    pub entry_point: PathBuf,
    pub syntax: Syntax,
    pub output_format: OutputFormat,
    pub env: Option<Vec<(String, String)>>,
    pub unoptimized: bool,
    pub debug: bool,
}

pub enum OutputFormat {
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

pub fn cli<T: Expression, TLoader>(
    options: CompilerCliOptions,
    module_loader: Option<TLoader>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
    mut output: impl std::io::Write,
) -> Result<()>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + 'static,
    T::Builtin: From<Stdlib> + From<JsStdlib>,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    let path = options.entry_point;
    let compiler_options = CompilerOptions {
        debug: options.debug,
        ..if options.unoptimized {
            CompilerOptions::unoptimized()
        } else {
            CompilerOptions::default()
        }
    };
    let (program, _entry_point) = compile_entry_point(
        path.as_path(),
        options.syntax,
        options.env,
        module_loader,
        &compiler_options,
        factory,
        allocator,
    )?;
    match options.output_format {
        OutputFormat::Json => serde_json::to_writer(output, &program)?,
        OutputFormat::Rmp => rmp_serde::encode::write(&mut output, &program)?,
        OutputFormat::Debug => write!(output, "{}", &program)?,
    }
    Ok(())
}
