// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
pub(crate) mod http;
pub(crate) mod playground;
pub(crate) mod websocket;

use reflex::{
    compiler::{
        create_main_function, Compile, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    hash::HashId,
};

pub fn compile_graphql_query<
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
    let graph_factory_hash = get_compiled_function_hash(graph_factory_address, &program)?;
    let query_factory_hash = get_compiled_function_hash(query_factory_address, &program)?;
    program.extend(create_query_entry_point_function(
        (graph_factory_address, graph_factory_hash),
        (query_factory_address, query_factory_hash),
    ));
    Ok((program, entry_point))
}

fn create_query_entry_point_function(
    graph_factory: (InstructionPointer, HashId),
    query_factory: (InstructionPointer, HashId),
) -> Program {
    let (graph_factory_address, graph_factory_hash) = graph_factory;
    let (query_factory_address, query_factory_hash) = query_factory;
    create_main_function([
        Instruction::Call {
            target_address: graph_factory_address,
            target_hash: graph_factory_hash,
            num_args: 0,
        },
        Instruction::Call {
            target_address: query_factory_address,
            target_hash: query_factory_hash,
            num_args: 0,
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::Return,
    ])
}

fn get_compiled_function_hash(
    address: InstructionPointer,
    program: &Program,
) -> Result<HashId, String> {
    program
        .get(address)
        .and_then(|instruction| match instruction {
            &Instruction::Function { hash, .. } => Some(hash),
            _ => None,
        })
        .ok_or_else(|| format!("Target address is not a function: {:x}", address))
}
