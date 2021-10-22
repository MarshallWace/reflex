// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use std::{
    env, fs,
    io::{self, Write},
    iter::once,
    path::PathBuf,
    str::FromStr,
    sync::Mutex,
};

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{hash_program_root, Compiler, CompilerMode, CompilerOptions, InstructionPointer},
    core::{Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, StateCache},
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::{
        term::{NativeFunctionId, SignalTerm},
        TermFactory, ValueTerm,
    },
};

use reflex_cli::Syntax;
use reflex_handlers::{builtin_signal_handler, debug_signal_handler};
use reflex_js::{
    self, create_js_env, create_module_loader, stdlib::imports::builtin_imports_loader,
};
use reflex_runtime::{Runtime, RuntimeCache, RuntimeState, StreamExt};
use repl::ReplParser;

mod repl;

#[derive(Parser)]
struct Opts {
    #[clap(about = "Optional script to run, if not given will start in repl mode")]
    entry_point: Option<String>,
    #[clap(
        long,
        about = "What syntax to interpret input in",
        default_value = "javascript"
    )]
    syntax: Syntax,
    #[clap(long, about = "Prevent static optimizations")]
    unoptimized: bool,
    #[clap(long, about = "Add debug printing of signal handlers")]
    debug_signals: bool,
    #[clap(long, about = "Add debug printing of bytecode output from compiler")]
    debug_compiler: bool,
    #[clap(long, about = "Add debug printing of bytecode execution")]
    debug_interpreter: bool,
    #[clap(long, about = "Add debug printing of stack during execution")]
    debug_stack: bool,
}

#[tokio::main]
pub async fn main() -> Result<()> {
    let args = Opts::parse();
    let unoptimized = args.unoptimized;
    let debug_signals = args.debug_signals;
    let debug_compiler = args.debug_compiler;
    let debug_interpreter = args.debug_interpreter;
    let debug_stack = args.debug_stack;
    let factory = TermFactory::default();
    let allocator = DefaultAllocator::default();
    let input_path = args.entry_point;
    let syntax = args.syntax;
    if syntax == Syntax::ByteCode {
        return Err(anyhow!("CLI is not yet supported for bytecode"));
    }
    match input_path {
        None => {
            let state = StateCache::default();
            let mut cache = SubstitutionCache::new();
            let parser = create_parser(syntax, None, &factory, &allocator);
            repl::run(parser, &state, &factory, &allocator, &mut cache)?;
        }
        Some(input_path) => {
            let compiler_options = CompilerOptions {
                debug: debug_compiler,
                normalize: !unoptimized,
                ..CompilerOptions::default()
            };
            let source = read_file(&input_path)?;
            let parser = create_parser(syntax, Some(input_path.to_owned()), &factory, &allocator);
            let expression = parser(&source)
                .map_err(|err| anyhow!("Failed to parse source at {}: {}", input_path, err))?;
            let plugins = find_expression_plugins(&expression, &factory);
            let program = Compiler::new(compiler_options, None)
                .compile(&expression, CompilerMode::Expression, &factory, &allocator)
                .map_err(|err| anyhow!("Failed to compile source at {}: {}", input_path, err))?;

            let mut stdout = io::stdout();
            let state = StateCache::default();
            let interpreter_options = InterpreterOptions {
                debug_instructions: debug_interpreter || debug_stack,
                debug_stack,
                ..InterpreterOptions::default()
            };
            let mut interpreter_cache = DefaultInterpreterCache::default();
            let entry_point = InstructionPointer::default();
            let cache_key = hash_program_root(&program, &entry_point);

            let (result, cache_entries) = execute(
                cache_key,
                &program,
                entry_point,
                &state,
                &factory,
                &allocator,
                &plugins,
                &interpreter_options,
                &mut interpreter_cache,
            )
            .map_err(|err| anyhow!("Failed to execute program at {}: {}", &input_path, err))?;

            let (output, dependencies) = result.into_parts();
            if dependencies.is_empty() {
                writeln!(stdout, "{}", output).or_else(|error| Err(anyhow!("{}", error)))?;
            } else {
                let signal_handler = builtin_signal_handler(&factory, &allocator);
                let state = RuntimeState::default();
                let cache = RuntimeCache::new(cache_entries);
                let runtime = if debug_signals {
                    Runtime::new(
                        state,
                        plugins,
                        debug_signal_handler(signal_handler),
                        cache,
                        &factory,
                        &allocator,
                        interpreter_options,
                        compiler_options,
                    )
                } else {
                    Runtime::new(
                        state,
                        plugins,
                        debug_signal_handler(signal_handler),
                        cache,
                        &factory,
                        &allocator,
                        interpreter_options,
                        compiler_options,
                    )
                };
                let stdout = Mutex::new(stdout);

                match runtime
                    .subscribe(program, InstructionPointer::default())
                    .await
                {
                    Err(error) => {
                        writeln!(stdout.lock().unwrap(), "{}", error).unwrap();
                    }
                    Ok(mut subscription) => {
                        while let Some(result) = subscription.next().await {
                            let output = match factory.match_signal_term(&result) {
                                None => format!("{}", result),
                                Some(signal) => format_signal_errors(signal)
                                    .into_iter()
                                    .map(|error| format!(" - {}", error))
                                    .collect::<Vec<_>>()
                                    .join("\n"),
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
        }
    }

    Ok(())
}

fn create_parser<T>(
    syntax: Syntax,
    entry_path: Option<String>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Box<dyn Fn(&str) -> Result<T, String>>
where
    T: Expression + Reducible<T> + Rewritable<T> + 'static,
{
    match (syntax, entry_path) {
        (Syntax::JavaScript, None) => create_js_script_parser(factory, allocator),
        (Syntax::JavaScript, Some(entry_path)) => {
            create_js_module_parser(&entry_path, env::vars(), factory, allocator)
        }
        (Syntax::Lisp, _) => create_sexpr_parser(factory, allocator),
        (Syntax::ByteCode, _) => todo!(),
    }
}

fn find_expression_plugins<T: Expression + Rewritable<T>>(
    expression: &T,
    factory: &impl ExpressionFactory<T>,
) -> Vec<(NativeFunctionId, T)> {
    if let Some(term) = factory.match_native_function_term(expression) {
        once((term.uid(), expression.clone())).collect::<Vec<_>>()
    } else {
        expression
            .subexpressions()
            .into_iter()
            .flat_map(|expression| find_expression_plugins(expression, factory))
            .collect::<Vec<_>>()
    }
}

fn create_js_script_parser<T: Expression + Rewritable<T> + 'static>(
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> ReplParser<T> {
    let env =
        reflex_js::Env::new().with_globals(reflex_js::stdlib::builtin_globals(factory, allocator));
    let factory = factory.clone();
    let allocator = allocator.clone();
    Box::new(move |input: &str| reflex_js::parse(input, &env, &factory, &allocator))
}

fn create_js_module_parser<T: Expression + Rewritable<T> + 'static>(
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

fn read_file(path: &str) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("Failed to read path {}", path))
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}
