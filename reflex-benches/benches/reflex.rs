// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
#![feature(test)]
extern crate test;

use std::iter::once;

use reflex::{
    cache::SubstitutionCache,
    core::{evaluate, ExpressionFactory, HeapAllocator, InstructionPointer, StateCache, Uid},
};
use reflex_interpreter::{
    compiler::{
        hash_compiled_program, CompiledProgram, Compiler, CompilerMode, CompilerOptions,
        Instruction, Program,
    },
    execute, DefaultInterpreterCache, InterpreterOptions,
};
use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
use reflex_lisp::parse;
use reflex_stdlib::{Add, Collect, Map, Stdlib};
use test::Bencher;

#[bench]
fn nested_expressions(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn nested_expressions_bytecode(b: &mut Bencher) {
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let program = Program::new(vec![
        Instruction::Function {
            hash: 0,
            required_args: 0,
            optional_args: 0,
        },
        Instruction::PushInt { value: 5 },
        Instruction::PushInt { value: 4 },
        Instruction::PushInt { value: -3 },
        Instruction::PushBuiltin {
            target: Uid::uid(&Stdlib::Abs {}),
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::PushBuiltin {
            target: Uid::uid(&Stdlib::Add {}),
        },
        Instruction::Apply { num_args: 2 },
        Instruction::Evaluate,
        Instruction::PushBuiltin {
            target: Uid::uid(&Stdlib::Add {}),
        },
        Instruction::Apply { num_args: 2 },
        Instruction::Evaluate,
        Instruction::Squash { depth: 1 },
        Instruction::Return,
    ]);
    let program = CompiledProgram {
        instructions: program,
        data_section: Default::default(),
    };
    let entry_point = InstructionPointer::default();
    let options = InterpreterOptions::default();
    let cache_key = hash_compiled_program(&program, &entry_point);
    let state = StateCache::default();
    let state_id = 0;
    let mut cache = DefaultInterpreterCache::default();
    b.iter(|| {
        execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &state,
            &factory,
            &allocator,
            &options,
            &mut cache,
        )
    });
}

#[bench]
fn nested_expressions_compiled(b: &mut Bencher) {
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
    let program = Compiler::new(CompilerOptions::unoptimized(), None)
        .compile(&expression, CompilerMode::Function, &factory, &allocator)
        .unwrap();
    let entry_point = InstructionPointer::default();
    let mut cache = DefaultInterpreterCache::default();
    let options = InterpreterOptions::default();
    let cache_key = hash_compiled_program(&program, &entry_point);
    let state = StateCache::default();
    let state_id = 0;
    b.iter(|| {
        execute(
            cache_key,
            &program,
            entry_point,
            state_id,
            &state,
            &factory,
            &allocator,
            &options,
            &mut cache,
        )
    });
}

#[bench]
fn function_application_nullary(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda () 3))", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_unary(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_binary(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar) foo) 3 4)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_ternary(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar baz) foo) 3 4 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_unused_args(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar baz) 2) 3 4 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_argument_scope(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse(
            "((lambda (first second third) ((lambda (one two) ((lambda (foo bar) (+ foo bar)) one two)) first third)) 3 4 5)",
            &factory,
            &allocator,
        )
        .unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn deeply_nested_function_application(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = (1..=100).fold(factory.create_int_term(0), |acc, i| {
        factory.create_application_term(
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(Add),
                    allocator.create_list(vec![factory.create_variable_term(0), acc]),
                ),
            ),
            allocator.create_list(once(factory.create_int_term(i))),
        )
    });
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    })
}

#[bench]
fn function_application_closure(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(((lambda (foo) (lambda () foo)) 3))", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn conditional_expressions(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(if #t 3 4)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn list_transforms(b: &mut Bencher) {
    let state = StateCache::default();
    let factory = SharedTermFactory::<Stdlib>::default();
    let allocator = DefaultAllocator::default();
    let collection = factory.create_list_term(allocator.create_list((0..1000).map(|index| {
        factory.create_application_term(
            factory.create_builtin_term(Add),
            allocator.create_list(vec![
                factory.create_int_term(index),
                factory.create_int_term(1),
            ]),
        )
    })));
    let transform = parse("(lambda (value) (+ value 2))", &factory, &allocator).unwrap();
    let expression = factory.create_application_term(
        factory.create_builtin_term(Collect),
        allocator.create_list(once(factory.create_application_term(
            factory.create_builtin_term(Map),
            allocator.create_list(vec![collection, transform]),
        ))),
    );
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}
