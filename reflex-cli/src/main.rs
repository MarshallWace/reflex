// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use std::{
    io::Write,
    path::{Path, PathBuf},
    sync::Mutex,
};

use reflex::{
    allocator::DefaultAllocator,
    cache::SubstitutionCache,
    compiler::{hash_program_root, Compiler, CompilerMode, CompilerOptions, InstructionPointer},
    core::{Expression, ExpressionFactory, StateCache},
    env::inject_env_vars,
    interpreter::{execute, DefaultInterpreterCache, InterpreterOptions},
    lang::{term::SignalTerm, SharedTermFactory, ValueTerm},
};

use reflex_cli::{create_parser, Syntax, SyntaxParser};
use reflex_handlers::{builtin_signal_handler, debug_signal_handler, EitherHandler};
use reflex_js::builtins::JsBuiltins;
use reflex_runtime::{Runtime, RuntimeCache, RuntimeState, StreamExt};

mod repl;

/// Reflex runtime evaluator
#[derive(Parser)]
struct Args {
    /// Optional entry point module to evaluate (defaults to REPL)
    entry_point: Option<PathBuf>,
    /// Entry point syntax
    #[clap(long, default_value = "javascript")]
    syntax: Syntax,
    /// Prevent static optimizations
    #[clap(long)]
    unoptimized: bool,
    /// Add debug printing of signal handlers
    #[clap(long)]
    debug_signals: bool,
    /// Add debug printing of bytecode output from compiler
    #[clap(long)]
    debug_compiler: bool,
    /// Add debug printing of bytecode execution
    #[clap(long)]
    debug_interpreter: bool,
    /// Add debug printing of stack during execution
    #[clap(long)]
    debug_stack: bool,
}

#[tokio::main]
pub async fn main() -> Result<()> {
    let args = Args::parse();
    let unoptimized = args.unoptimized;
    let debug_signals = args.debug_signals;
    let debug_compiler = args.debug_compiler;
    let debug_interpreter = args.debug_interpreter;
    let debug_stack = args.debug_stack;
    let factory = SharedTermFactory::<JsBuiltins>::default();
    let allocator = DefaultAllocator::default();
    let input_path = args.entry_point;
    let syntax = args.syntax;
    if syntax == Syntax::Bytecode {
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
                ..if unoptimized {
                    CompilerOptions::unoptimized()
                } else {
                    CompilerOptions::default()
                }
            };
            let source = read_file(&input_path)?;
            let parser = create_parser(syntax, Some(&input_path), &factory, &allocator);
            let expression = parser
                .parse(&source)
                .map(|expression| {
                    inject_env_vars(expression, std::env::vars(), &factory, &allocator)
                })
                .map_err(|err| {
                    anyhow!(
                        "Failed to parse source at {}: {}",
                        input_path.display(),
                        err
                    )
                })?;
            let program = Compiler::new(compiler_options, None)
                .compile(&expression, CompilerMode::Function, &factory, &allocator)
                .map_err(|err| {
                    anyhow!(
                        "Failed to compile source at {}: {}",
                        input_path.display(),
                        err
                    )
                })?;

            let mut stdout = std::io::stdout();
            let state = StateCache::default();
            let state_id = 0;
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
                state_id,
                &state,
                &factory,
                &allocator,
                &interpreter_options,
                &mut interpreter_cache,
            )
            .map_err(|err| {
                anyhow!(
                    "Failed to execute program at {}: {}",
                    input_path.display(),
                    err
                )
            })?;

            let (output, dependencies) = result.into_parts();
            if dependencies.is_empty() {
                writeln!(stdout, "{}", output).or_else(|error| Err(anyhow!("{}", error)))?;
            } else {
                let signal_handler = builtin_signal_handler(&factory, &allocator);
                let signal_handler = if !debug_signals {
                    EitherHandler::Left(signal_handler)
                } else {
                    EitherHandler::Right(debug_signal_handler(signal_handler))
                };
                let state = RuntimeState::default();
                let mut cache = RuntimeCache::default();
                cache.apply_updates(cache_entries, state_id, state_id);
                let runtime = Runtime::new(
                    state,
                    &signal_handler,
                    cache,
                    &factory,
                    &allocator,
                    interpreter_options,
                    compiler_options,
                );
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

fn read_file(path: &Path) -> Result<String> {
    std::fs::read_to_string(path).with_context(|| format!("Failed to read path {}", path.display()))
}

fn clear_escape_sequence() -> &'static str {
    "\x1b[2J\x1b[H"
}
