// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, iter::empty};

use crate::{
    compiler::{compile_expressions, Compile, Compiler, Instruction, Program},
    core::{
        DependencyList, Expression, ExpressionFactory, GraphNode, HeapAllocator, Reducible,
        Rewritable, SerializeJson, Signal, SignalList, StackOffset, VarArgs,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SignalTerm<T: Expression> {
    signals: SignalList<T>,
}
impl<T: Expression> SignalTerm<T> {
    pub fn new(signals: SignalList<T>) -> Self {
        Self { signals }
    }
    pub fn signals(&self) -> &SignalList<T> {
        &self.signals
    }
}
impl<T: Expression> GraphNode for SignalTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        HashSet::new()
    }
    fn dynamic_dependencies(&self, _deep: bool) -> DependencyList {
        DependencyList::empty()
    }
    fn has_dynamic_dependencies(&self, _deep: bool) -> bool {
        false
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        true
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for SignalTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.signals().iter().enumerate().fold(
            Ok(Program::new(empty())),
            |program, (index, signal)| {
                let mut program = program?;
                match compile_signal(signal, stack_offset + index, factory, allocator, compiler) {
                    Err(error) => Err(error),
                    Ok(compiled_signal) => {
                        program.extend(compiled_signal);
                        Ok(program)
                    }
                }
            },
        )
    }
}
impl<T: Expression> std::fmt::Display for SignalTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.signals
                .iter()
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl<T: Expression> SerializeJson for SignalTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        Err(format!("Unable to serialize term: {}", self))
    }
}

fn compile_signal<T: Expression + Compile<T>>(
    signal: &Signal<T>,
    stack_offset: StackOffset,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler: &mut Compiler,
) -> Result<Program, String> {
    let compiled_args = compile_expressions(
        signal.args().iter(),
        VarArgs::Lazy,
        stack_offset,
        factory,
        allocator,
        compiler,
    )?;
    let mut result = compiled_args;
    result.push(Instruction::PushSignal {
        signal_type: signal.signal_type().clone(),
        num_args: signal.args().len(),
    });
    Ok(result)
}
