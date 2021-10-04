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
    compiler::{
        hash_program_root, Compile, Compiler, CompilerMode, CompilerOptions, InstructionPointer,
        NativeFunctionRegistry,
    },
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        StateCache, StringValue,
    },
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::{term::SignalTerm, BuiltinTerm, TermFactory, ValueTerm, WithCompiledBuiltins},
};
use reflex_cli::parse_cli_args;
use reflex_handlers::{builtin_signal_handler, debug_signal_handler};
use reflex_js::{
    self, create_js_env, create_module_loader, stdlib::imports::builtin_imports_loader,
};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, RuntimeCache,
    RuntimeState, SignalHandlerResult, SignalHelpers, StreamExt,
};
use repl::ReplParser;

mod repl;

#[tokio::main]
pub async fn main() {
    let args = parse_cli_args(env::args().skip(1));
    let debug_signals = args.get("debug").is_some();
    let debug_compiler = args.get("bytecode").is_some();
    let debug_interpreter = args.get("vm").is_some();
    let debug_stack = args.get("stack").is_some();
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
                    let state = StateCache::default();
                    let mut cache = SubstitutionCache::new();
                    repl::run(parser, &state, &factory, &allocator, &mut cache)
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
                                CompilerMode::Expression,
                                true,
                                empty(),
                                &factory,
                                &allocator,
                            )
                        }) {
                            Err(error) => Err(error),
                            Ok(compiled) => {
                                let mut stdout = io::stdout();
                                let state = StateCache::default();
                                let (program, builtins, plugins) = compiled.into_parts();
                                let interpreter_options = InterpreterOptions {
                                    debug_instructions: debug_interpreter || debug_stack,
                                    debug_stack: debug_stack,
                                    ..InterpreterOptions::default()
                                };
                                let mut interpreter_cache = DefaultInterpreterCache::default();
                                let entry_point = InstructionPointer::default();
                                let cache_key = hash_program_root(&program, &entry_point);
                                match execute(
                                    cache_key,
                                    &program,
                                    entry_point,
                                    &state,
                                    &factory,
                                    &allocator,
                                    &builtins,
                                    &plugins,
                                    &interpreter_options,
                                    &mut interpreter_cache,
                                ) {
                                    Err(error) => Err(error),
                                    Ok((result, cache_entries)) => {
                                        let (output, dependencies) = result.into_parts();
                                        if dependencies.is_empty() {
                                            writeln!(stdout, "{}", output)
                                                .or_else(|error| Err(format!("{}", error)))
                                        } else {
                                            let signal_handler =
                                                builtin_signal_handler(&factory, &allocator);
                                            let state = RuntimeState::default();
                                            let cache = RuntimeCache::new(cache_entries);
                                            let runtime = create_runtime(
                                                state,
                                                builtins,
                                                plugins,
                                                signal_handler,
                                                cache,
                                                &factory,
                                                &allocator,
                                                interpreter_options,
                                                compiler_options,
                                                debug_signals,
                                            );
                                            let stdout = Mutex::new(stdout);
                                            async move {
                                                match runtime
                                                    .subscribe(
                                                        program,
                                                        InstructionPointer::default(),
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
                                                            let output = match factory
                                                                .match_signal_term(&result)
                                                            {
                                                                None => format!("{}", result),
                                                                Some(signal) => {
                                                                    format_signal_errors(signal)
                                                                        .into_iter()
                                                                        .map(|error| {
                                                                            format!(" - {}", error)
                                                                        })
                                                                        .collect::<Vec<_>>()
                                                                        .join("\n")
                                                                }
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

fn create_runtime<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    THandler,
>(
    state: RuntimeState<T>,
    builtins: Vec<(BuiltinTerm, InstructionPointer)>,
    plugins: NativeFunctionRegistry<T>,
    signal_handler: THandler,
    cache: RuntimeCache<T>,
    factory: &(impl AsyncExpressionFactory<T> + WithCompiledBuiltins),
    allocator: &impl AsyncHeapAllocator<T>,
    interpreter_options: InterpreterOptions,
    compiler_options: CompilerOptions,
    debug_signals: bool,
) -> Runtime<T>
where
    T::String: StringValue + Send + Sync,
    THandler: Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T>
        + Send
        + Sync
        + 'static,
{
    if debug_signals {
        create_runtime(
            state,
            builtins,
            plugins,
            debug_signal_handler(signal_handler),
            cache,
            factory,
            allocator,
            interpreter_options,
            compiler_options,
            false,
        )
    } else {
        Runtime::new(
            state,
            builtins,
            plugins,
            signal_handler,
            cache,
            factory,
            allocator,
            interpreter_options,
            compiler_options,
        )
    }
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

fn format_signal_errors<T: Expression>(
    signal: &SignalTerm<T>,
) -> impl IntoIterator<Item = String> + '_ {
    signal
        .signals()
        .iter()
        .map(|signal| match signal.args().iter().next() {
            Some(payload) => format!("{}", payload),
            None => format!("{}", ValueTerm::<T::String>::Null),
        })
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
