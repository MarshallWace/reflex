// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    env, fs,
    io::{self, Write},
    path::PathBuf,
    process,
    str::FromStr,
    sync::Mutex,
};

use reflex_cli::parse_cli_args;
use reflex::{
    cache::GenerationalGc,
    core::{DynamicState, Expression},
};
use reflex_handlers::builtin_signal_handler;
use reflex_js::{self, dynamic_module_loader, stdlib::builtin_imports};
use reflex_loaders::builtin_loaders;
use reflex_runtime::Runtime;
use repl::ReplParser;

mod repl;

#[tokio::main]
pub async fn main() {
    let args = parse_cli_args(env::args().skip(1));
    let parser = match args.get("syntax") {
        Some(value) => match value {
            None => Err(String::from("Empty --syntax argument")),
            Some(value) => match value {
                "js" | "javascript" => Ok(match args.iter().next() {
                    None => create_js_script_parser(),
                    Some(path) => create_js_module_parser(path),
                }),
                "sexpr" | "lisp" => Ok(create_sexpr_parser()),
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
                    let mut cache = GenerationalGc::new();
                    repl::run(parser, &mut state, &mut cache)
                        .or_else(|error| Err(format!("{}", error)))
                }
                Some(input_path) => match read_file(&input_path) {
                    Err(error) => Err(error),
                    Ok(source) => match parser(&source) {
                        Err(error) => Err(error),
                        Ok(expression) => {
                            let mut stdout = io::stdout();
                            let state = DynamicState::new();
                            let mut cache = GenerationalGc::new();
                            let (output, dependencies) =
                                repl::eval(Expression::clone(&expression), &state, &mut cache);
                            if dependencies.is_empty() {
                                writeln!(stdout, "{}", output)
                                    .or_else(|error| Err(format!("{}", error)))
                            } else {
                                let signal_handler = builtin_signal_handler();
                                let runtime = Runtime::new(signal_handler, cache, 32, 32);
                                let stdout = Mutex::new(stdout);
                                async move {
                                    let expression = Expression::clone(&expression);
                                    match runtime.subscribe(expression).await {
                                        Err(error) => {
                                            writeln!(stdout.lock().unwrap(), "{}", error).unwrap();
                                        }
                                        Ok(mut subscription) => {
                                            while let Some(result) = subscription.next().await {
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
                    },
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

fn create_js_script_parser() -> ReplParser {
    let env = reflex_js::Env::new().with_globals(reflex_js::stdlib::builtin_globals());
    Box::new(move |input: &str| reflex_js::parse(input, &env))
}

fn create_js_module_parser(path: &str) -> ReplParser {
    let path = PathBuf::from_str(&path).unwrap();
    let env = reflex_js::Env::new().with_globals(reflex_js::stdlib::builtin_globals());
    let loader = dynamic_module_loader(builtin_loaders(), Some(builtin_imports()));
    Box::new(move |input: &str| reflex_js::parse_module(input, &env, &path, &loader))
}

fn create_sexpr_parser() -> ReplParser {
    Box::new(|input: &str| match reflex::parser::sexpr::parse(input) {
        Ok(result) => Ok(result),
        Err(error) => Err(format!("{}", error)),
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
