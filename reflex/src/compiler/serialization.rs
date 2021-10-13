// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashMap;

use crate::{
    core::Expression,
    lang::{BuiltinTerm, NativeFunction, NativeFunctionId},
};

use super::{CompilerOutput, InstructionPointer, NativeFunctionRegistry, Program};

pub struct SerializableNativeFunctionRegistry {
    values: HashMap<NativeFunctionId, InstructionPointer>,
}

impl SerializableNativeFunctionRegistry {
    pub fn to_registry<T: Expression>(
        mut self,
        native_functions: HashMap<NativeFunctionId, NativeFunction<T>>,
    ) -> Result<NativeFunctionRegistry<T>, String> {
        let mut registry = NativeFunctionRegistry::<T>::default();
        self.values
            .drain()
            .map(|(key, value)| {
                let func = native_functions.get(&key);
                match func {
                    None => Err(format!(
                        "Native function id {} without no corresponding function provided",
                        key
                    )),
                    Some(func) => Ok(registry.add(func.clone(), value)),
                }
            })
            .collect::<Result<Vec<()>, String>>()?;
        Ok(registry)
    }
}

pub struct SerializableCompilerOutput {
    program: Program,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: SerializableNativeFunctionRegistry,
}

impl SerializableCompilerOutput {
    pub fn to_compiler_output<T: Expression>(
        self,
        native_functions: HashMap<NativeFunctionId, NativeFunction<T>>,
    ) -> Result<CompilerOutput<T>, String> {
        Ok(CompilerOutput::new(
            self.program,
            self.builtins,
            self.plugins.to_registry(native_functions)?,
        ))
    }
}
