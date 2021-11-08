// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
pub(crate) mod http;
pub(crate) mod playground;
pub(crate) mod websocket;

use reflex::{
    compiler::{
        create_main_function, Compile, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
};

pub(crate) fn compile_graphql_query<
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    query: T,
    graph_root: (Program, InstructionPointer),
    compiler_options: &CompilerOptions,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<(Program, InstructionPointer), String> {
    let (graph_factory, graph_factory_address) = graph_root;
    let query_factory_address = InstructionPointer::new(graph_factory.len());
    let mut program = Compiler::new(*compiler_options, Some(graph_factory)).compile(
        &query,
        CompilerMode::Function,
        factory,
        allocator,
    )?;
    let entry_point = InstructionPointer::new(program.len());
    program.extend(create_query_entry_point_function(
        graph_factory_address,
        query_factory_address,
    ));
    Ok((program, entry_point))
}

fn create_query_entry_point_function(
    graph_factory_address: InstructionPointer,
    query_factory_address: InstructionPointer,
) -> Program {
    create_main_function([
        Instruction::Call {
            target: graph_factory_address,
            num_args: 0,
        },
        Instruction::Call {
            target: query_factory_address,
            num_args: 0,
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::Return,
    ])
}
