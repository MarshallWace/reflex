// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    env, fs,
    io::{self, Write},
    iter::empty,
    path::PathBuf,
    process,
    str::FromStr,
    sync::Mutex,
};

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{Compiler, CompilerMode, CompilerOptions, InstructionPointer, Program},
    core::{DynamicState, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::TermFactory,
};
use reflex_cli::parse_cli_args;
use reflex_handlers::{builtin_signal_handler, debug_signal_handler};
use reflex_js::{
    self, create_js_env, create_module_loader, stdlib::imports::builtin_imports_loader,
};
use reflex_runtime::{Runtime, StreamExt};
use repl::ReplParser;

mod repl;

#[tokio::main]
pub async fn main() {
    let args = parse_cli_args(env::args().skip(1));
    let debug_signals = args.get("debug").is_some();
    let debug_compiler = args.get("bytecode").is_some();
    let debug_interpreter = args.get("vm").is_some();
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let parser = match args.get("syntax") {
        Some(value) => match value {
            None => Err(String::from("Empty --syntax argument")),
            Some(value) => match value {
                "js" | "javascript" => Ok(match args.iter().next() {
                    None => create_js_script_parser(&factory, &allocator),
                    Some(path) => create_js_module_parser(path, env::vars(), &factory, &allocator),
                }),
                "sexpr" | "lisp" => Ok(create_sexpr_parser(&factory, &allocator)),
                _ => Err(format!("Invalid --syntax argument: {}", value)),
            },
        },
        None => Err(String::from("Missing --syntax argument")),
    };
    let input_path = match args.len() {
        0 => Ok(None),
        1 => Ok(Some(args.into_iter().next().unwrap())),
        _ => Err(format!(
            "Multiple input files specified:\n{}",
            args.into_iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join("\n"),
        )),
    };
    let result = match parser {
        Err(error) => Err(error),
        Ok(parser) => match input_path {
            Err(error) => Err(error),
            Ok(input_path) => match input_path {
                None => {
                    let mut state = DynamicState::new();
                    let mut cache = SubstitutionCache::new();
                    repl::run(parser, &mut state, &factory, &allocator, &mut cache)
                        .or_else(|error| Err(format!("{}", error)))
                }
                Some(input_path) => match read_file(&input_path) {
                    Err(error) => Err(error),
                    Ok(source) => {
                        let compiler_options = CompilerOptions {
                            debug: debug_compiler,
                            ..CompilerOptions::default()
                        };
                        match parser(&source).and_then(|expression| {
                            Compiler::new(compiler_options, None).compile(
                                &expression,
                                CompilerMode::Program,
                                &factory,
                                &allocator,
                            )
                        }) {
                            Err(error) => Err(error),
                            Ok(compiled) => {
                                let mut stdout = io::stdout();
                                let state = DynamicState::new();
                                let (program, plugins) = compiled.into_parts();
                                let interpreter_options = InterpreterOptions {
                                    debug: debug_interpreter,
                                    ..InterpreterOptions::default()
                                };
                                let mut interpreter_cache = DefaultInterpreterCache::default();
                                match execute(
                                    &program,
                                    InstructionPointer::default(),
                                    &state,
                                    &factory,
                                    &allocator,
                                    &plugins,
                                    &interpreter_options,
                                    &mut interpreter_cache,
                                ) {
                                    Err(error) => Err(error),
                                    Ok((result, _cache_key)) => {
                                        let (output, dependencies) = result.into_parts();
                                        if dependencies.is_empty() {
                                            writeln!(stdout, "{}", output)
                                                .or_else(|error| Err(format!("{}", error)))
                                        } else {
                                            // TODO: Establish sensible defaults for channel buffer sizes
                                            let command_buffer_size = 32;
                                            let result_buffer_size = 32;
                                            let signal_handler =
                                                builtin_signal_handler(&factory, &allocator);
                                            let runtime = if debug_signals {
                                                Runtime::new(
                                                    Program::new(empty()),
                                                    debug_signal_handler(signal_handler),
                                                    &factory,
                                                    &allocator,
                                                    plugins,
                                                    compiler_options,
                                                    interpreter_options,
                                                    command_buffer_size,
                                                    result_buffer_size,
                                                )
                                            } else {
                                                Runtime::new(
                                                    Program::new(empty()),
                                                    signal_handler,
                                                    &factory,
                                                    &allocator,
                                                    plugins,
                                                    compiler_options,
                                                    interpreter_options,
                                                    command_buffer_size,
                                                    result_buffer_size,
                                                )
                                            };
                                            let stdout = Mutex::new(stdout);
                                            async move {
                                                match runtime
                                                    .subscribe(
                                                        program,
                                                        InstructionPointer::default(),
                                                        &factory,
                                                        &allocator,
                                                    )
                                                    .await
                                                {
                                                    Err(error) => {
                                                        writeln!(
                                                            stdout.lock().unwrap(),
                                                            "{}",
                                                            error
                                                        )
                                                        .unwrap();
                                                    }
                                                    Ok(mut subscription) => {
                                                        while let Some(result) =
                                                            subscription.next().await
                                                        {
                                                            let output = match result {
                                                                Ok(result) => format!("{}", result),
                                                                Err(errors) => errors.join("\n"),
                                                            };
                                                            writeln!(
                                                                stdout.lock().unwrap(),
                                                                "{}{}",
                                                                clear_escape_sequence(),
                                                                output
                                                            )
                                                            .unwrap();
                                                        }
                                                    }
                                                }
                                            }
                                            .await;
                                            Ok(())
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
            },
        },
    };
    process::exit(match result {
        Ok(_) => 0,
        Err(error) => {
            println!("{}", error);
            1
        }
    })
}

fn create_js_script_parser<T: Expression + 'static>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> ReplParser<T> {
    let env =
        reflex_js::Env::new().with_globals(reflex_js::stdlib::builtin_globals(factory, allocator));
    let factory = factory.clone();
    let allocator = allocator.clone();
    Box::new(move |input: &str| reflex_js::parse(input, &env, &factory, &allocator))
}

fn create_js_module_parser<T: Expression + 'static>(
    path: &str,
    env_vars: impl IntoIterator<Item = (String, String)>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> ReplParser<T> {
    let path = PathBuf::from_str(&path).unwrap();
    let env = create_js_env(env_vars, factory, allocator);
    let loader = create_module_loader(
        env.clone(),
        Some(builtin_imports_loader(factory, allocator)),
        factory,
        allocator,
    );
    let factory = factory.clone();
    let allocator = allocator.clone();
    Box::new(move |input: &str| {
        reflex_js::parse_module(input, &env, &path, &loader, &factory, &allocator)
    })
}

fn create_sexpr_parser<T: Expression + Rewritable<T> + Reducible<T>>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> ReplParser<T> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    Box::new(
        move |input: &str| match reflex::parser::sexpr::parse(input, &factory, &allocator) {
            Ok(result) => Ok(result),
            Err(error) => Err(format!("{}", error)),
        },
    )
}

fn read_file(path: &str) -> Result<String, String> {
    match fs::read_to_string(path) {
        Ok(contents) => Ok(contents),
        Err(error) => Err(format!("Failed to read path {}: {}", path, error)),
    }
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}
