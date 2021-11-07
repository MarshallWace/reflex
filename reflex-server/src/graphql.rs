// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub(crate) mod http;
pub(crate) mod playground;
pub(crate) mod websocket;

use std::iter::once;

use reflex::{
    compiler::{
        Compile, Compiler, CompilerMode, CompilerOptions, Instruction, InstructionPointer, Program,
    },
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    hash::hash_object,
};

pub(crate) fn compile_graphql_query<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    query: T,
    graph_root: &Program,
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(Program, InstructionPointer), String> {
    let prelude = graph_root.clone();
    let query_address = InstructionPointer::new(prelude.len());
    let mut program = Compiler::new(*compiler_options, Some(prelude)).compile(
        &query,
        CompilerMode::Function,
        factory,
        allocator,
    )?;
    let entry_point = InstructionPointer::new(program.len());
    program.extend(create_query_entry_point_function(
        InstructionPointer::default(),
        query_address,
    ));
    Ok((program, entry_point))
}

fn create_query_entry_point_function(
    graph_factory_address: InstructionPointer,
    query_address: InstructionPointer,
) -> impl IntoIterator<Item = Instruction> {
    let function_body = Program::new([
        Instruction::PushStatic { offset: 0 },
        Instruction::Call {
            target: graph_factory_address,
            num_args: 1,
        },
        Instruction::Call {
            target: query_address,
            num_args: 1,
        },
        Instruction::Squash { depth: 1 },
        Instruction::Return,
    ]);
    once(Instruction::Function {
        hash: hash_object(&function_body),
        required_args: 1,
        optional_args: 0,
    })
    .chain(function_body)
}
