// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]
extern crate test;

use std::iter::once;

use test::Bencher;

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{
        hash_program_root, Compiler, CompilerMode, CompilerOptions, Instruction,
        InstructionPointer, Program,
    },
    core::{evaluate, ExpressionFactory, HeapAllocator, StateCache, Uid},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::SharedTermFactory,
    parser::sexpr::parse,
    stdlib::Stdlib,
};

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
    let builtins = Stdlib::entries()
        .into_iter()
        .map(|builtin| (builtin.uid(), factory.create_builtin_term(builtin)))
        .collect::<Vec<_>>();
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
    let entry_point = InstructionPointer::default();
    let options = InterpreterOptions::default();
    let cache_key = hash_program_root(&program, &entry_point);
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
    let cache_key = hash_program_root(&program, &entry_point);
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
                    factory.create_builtin_term(Stdlib::Add),
                    allocator.create_list(vec![factory.create_static_variable_term(0), acc]),
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
    let collection = factory.create_vector_term(allocator.create_list((0..1000).map(|index| {
        factory.create_application_term(
            factory.create_builtin_term(Stdlib::Add),
            allocator.create_list(vec![
                factory.create_int_term(index),
                factory.create_int_term(1),
            ]),
        )
    })));
    let transform = parse("(lambda (value) (+ value 2))", &factory, &allocator).unwrap();
    let expression = factory.create_application_term(
        factory.create_builtin_term(Stdlib::Collect),
        allocator.create_list(once(factory.create_application_term(
            factory.create_builtin_term(Stdlib::Map),
            allocator.create_list(vec![collection, transform]),
        ))),
    );
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}
