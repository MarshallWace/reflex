// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]
extern crate test;

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{Compiler, CompilerMode, CompilerOptions, InstructionPointer},
    core::{evaluate, DynamicState},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::TermFactory,
};
use reflex_js::{parse, Env};
use test::Bencher;

#[bench]
fn js_interpreted(b: &mut Bencher) {
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let env = Env::new();
    let input = "
        const fullName = (first, last) => `${first} ${last}`;
        const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
        greet({ first: 'John', last: 'Doe' })";
    let expression = parse(input, &env, &factory, &allocator).unwrap();
    let state = DynamicState::new();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn js_compiled(b: &mut Bencher) {
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let env = Env::new();
    let input = "
        const fullName = (first, last) => `${first} ${last}`;
        const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
        greet({ first: 'John', last: 'Doe' })";
    let expression = parse(input, &env, &factory, &allocator).unwrap();
    let compiled = Compiler::new(
        CompilerOptions {
            debug: false,
            hoist_free_variables: true,
            normalize: false,
        },
        None,
    )
    .compile(&expression, CompilerMode::Program, &factory, &allocator)
    .unwrap();
    let state = DynamicState::new();
    let options = InterpreterOptions::default();
    let program = compiled.program();
    let plugins = compiled.plugins();
    b.iter(|| {
        let mut cache = DefaultInterpreterCache::default();
        execute(
            program,
            InstructionPointer::default(),
            &state,
            &factory,
            &allocator,
            plugins,
            &options,
            &mut cache,
        )
    })
}
