// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::path::Path;

use anyhow::{Context, Result};
use reflex::compiler::{InstructionPointer, Program};

pub fn compile_bytecode_entry_point(path: &Path) -> Result<(Program, InstructionPointer)> {
    let input = std::fs::File::open(path).with_context(|| {
        format!(
            "Failed to load bytecode graph definition: {}",
            path.display()
        )
    })?;
    let program = rmp_serde::decode::from_read(input).with_context(|| {
        format!(
            "Failed to deserialize bytecode contents: {}",
            path.display(),
        )
    })?;
    Ok((program, InstructionPointer::default()))
}
