// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
#![feature(test)]
extern crate test;

use reflex::{
    cache::SubstitutionCache,
    core::{evaluate, InstructionPointer, StateCache},
};
use reflex_interpreter::{
    compiler::{hash_compiled_program, Compiler, CompilerMode, CompilerOptions},
    execute, DefaultInterpreterCache, InterpreterOptions,
};
use reflex_js::{builtins::JsBuiltins, parse, Env};
use reflex_lang::{allocator::DefaultAllocator, SharedTermFactory};
use test::Bencher;

#[bench]
fn js_interpreted(b: &mut Bencher) {
    let factory = SharedTermFactory::<JsBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let env = Env::new();
    let input = "
        const fullName = (first, last) => `${first} ${last}`;
        const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
        greet({ first: 'John', last: 'Doe' })";
    let expression = parse(input, &env, &factory, &allocator).unwrap();
    let state = StateCache::default();
    b.iter(|| {
        let mut cache = SubstitutionCache::new();
        evaluate(&expression, &state, &factory, &allocator, &mut cache)
    });
}

#[bench]
fn js_compiled(b: &mut Bencher) {
    let factory = SharedTermFactory::<JsBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let env = Env::new();
    let input = "
        const fullName = (first, last) => `${first} ${last}`;
        const greet = (user) => `Hello, ${fullName(user.first, user.last)}!`;
        greet({ first: 'John', last: 'Doe' })";
    let expression = parse(input, &env, &factory, &allocator).unwrap();
    let program = Compiler::new(
        CompilerOptions {
            debug: false,
            hoist_free_variables: true,
            inline_static_data: true,
            normalize: false,
        },
        None,
    )
    .compile(&expression, CompilerMode::Function, &factory, &allocator)
    .unwrap();
    let state = StateCache::default();
    let state_id = 0;
    let options = InterpreterOptions::default();
    b.iter(|| {
        let mut cache = DefaultInterpreterCache::default();
        let entry_point = InstructionPointer::default();
        let cache_key = hash_compiled_program(&program, &entry_point);
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
    })
}
