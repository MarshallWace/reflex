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
    compiler::{Instruction, InstructionPointer, NativeFunctionRegistry, Program},
    core::{evaluate, DynamicState, ExpressionFactory, HeapAllocator},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::{BuiltinTerm, TermFactory, ValueTerm},
    parser::sexpr::parse,
};

#[bench]
fn nested_expressions(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(+ (+ (abs -3) 4) 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn nested_expressions_compiled(b: &mut Bencher) {
    let factory = TermFactory::default();
    let program = Program::new(vec![
        Instruction::PushInt { value: 5 },
        Instruction::PushInt { value: 4 },
        Instruction::PushInt { value: -3 },
        Instruction::PushBuiltin {
            target: BuiltinTerm::Abs,
        },
        Instruction::Apply { num_args: 1 },
        Instruction::Evaluate,
        Instruction::PushBuiltin {
            target: BuiltinTerm::Add,
        },
        Instruction::Apply { num_args: 2 },
        Instruction::Evaluate,
        Instruction::PushBuiltin {
            target: BuiltinTerm::Add,
        },
        Instruction::Apply { num_args: 2 },
        Instruction::Evaluate,
        Instruction::Return,
    ]);
    let allocator = DefaultAllocator::default();
    let mut cache = DefaultInterpreterCache::default();
    let state = DynamicState::new();
    let builtins = Vec::new();
    let plugins = NativeFunctionRegistry::default();
    let options = InterpreterOptions::default();
    b.iter(|| {
        execute(
            &program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            &builtins,
            &plugins,
            &options,
            &mut cache,
        )
    });
}

#[bench]
fn function_application_nullary(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda () 3))", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_unary(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo) foo) 3)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_binary(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar) foo) 3 4)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_ternary(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar baz) foo) 3 4 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_unused_args(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("((lambda (foo bar baz) 2) 3 4 5)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn function_application_argument_scope(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
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
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = (1..=100).fold(factory.create_value_term(ValueTerm::Int(0)), |acc, i| {
        factory.create_application_term(
            factory.create_lambda_term(
                1,
                factory.create_application_term(
                    factory.create_builtin_term(BuiltinTerm::Add),
                    allocator.create_list(vec![factory.create_static_variable_term(0), acc]),
                ),
            ),
            allocator.create_list(once(factory.create_value_term(ValueTerm::Int(i)))),
        )
    });
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    })
}

#[bench]
fn function_application_closure(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(((lambda (foo) (lambda () foo)) 3))", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn conditional_expressions(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let expression = parse("(if #t 3 4)", &factory, &allocator).unwrap();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn list_transforms(b: &mut Bencher) {
    let state = DynamicState::new();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let collection = factory.create_vector_term(allocator.create_list((0..1000).map(|index| {
        factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Add),
            allocator.create_list(vec![
                factory.create_value_term(ValueTerm::Int(index)),
                factory.create_value_term(ValueTerm::Int(1)),
            ]),
        )
    })));
    let transform = parse("(lambda (value) (+ value 2))", &factory, &allocator).unwrap();
    let expression = factory.create_application_term(
        factory.create_builtin_term(BuiltinTerm::Collect),
        allocator.create_list(once(factory.create_application_term(
            factory.create_builtin_term(BuiltinTerm::Map),
            allocator.create_list(vec![collection, transform]),
        ))),
    );
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}
