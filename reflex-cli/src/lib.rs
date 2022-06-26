// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{path::Path, str::FromStr};

use anyhow::{anyhow, Result};
use reflex::{
    compiler::{Compile, Compiler, CompilerMode, CompilerOptions, InstructionPointer, Program},
    core::{
        Applicable, Expression, ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal,
        SignalType, StringValue,
    },
    lang::term::SignalTerm,
    stdlib::Stdlib,
};
use reflex_handlers::stdlib::Stdlib as HandlersStdlib;
use reflex_js::stdlib::Stdlib as JsStdlib;
use reflex_json::stdlib::Stdlib as JsonStdlib;

use crate::syntax::{
    js::compile_js_entry_point,
    js::{create_js_module_parser, create_js_script_parser},
    json::compile_json_entry_point,
    json::create_json_parser,
    sexpr::compile_sexpr_entry_point,
    sexpr::create_sexpr_parser,
};

pub mod builtins;
pub mod cli {
    pub mod compiler;
}
pub mod repl;
pub mod syntax {
    pub mod js;
    pub mod json;
    pub mod sexpr;
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Syntax {
    JavaScript,
    Json,
    Lisp,
}
impl FromStr for Syntax {
    type Err = anyhow::Error;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.to_lowercase().as_str() {
            "javascript" | "js" => Ok(Self::JavaScript),
            "json" => Ok(Self::Json),
            "sexpr" | "lisp" => Ok(Self::Lisp),
            _ => Err(anyhow!("Unknown syntax: {}", input)),
        }
    }
}

pub trait SyntaxParser<T: Expression> {
    fn parse(&self, input: &str) -> Result<T, String>;
}
impl<T, F> SyntaxParser<T> for F
where
    T: Expression,
    F: Fn(&str) -> Result<T, String>,
{
    fn parse(&self, input: &str) -> Result<T, String> {
        self(input)
    }
}

pub fn create_parser<T>(
    syntax: Syntax,
    entry_path: Option<&Path>,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> impl SyntaxParser<T>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + 'static,
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<JsStdlib> + From<HandlersStdlib>,
{
    match (syntax, entry_path) {
        (Syntax::JavaScript, None) => {
            GenericSyntaxParser::new(create_js_script_parser(factory, allocator))
        }
        (Syntax::JavaScript, Some(entry_path)) => {
            GenericSyntaxParser::new(create_js_module_parser(entry_path, factory, allocator))
        }
        (Syntax::Json, _) => GenericSyntaxParser::new(create_json_parser(factory, allocator)),
        (Syntax::Lisp, _) => GenericSyntaxParser::new(create_sexpr_parser(factory, allocator)),
    }
}
struct GenericSyntaxParser<T: Expression> {
    parser: Box<dyn SyntaxParser<T>>,
}
impl<T: Expression> GenericSyntaxParser<T> {
    fn new(parser: impl SyntaxParser<T> + 'static) -> Self {
        Self {
            parser: Box::new(parser),
        }
    }
}
impl<T: Expression> SyntaxParser<T> for GenericSyntaxParser<T> {
    fn parse(&self, input: &str) -> Result<T, String> {
        self.parser.parse(input)
    }
}

pub fn compile_entry_point<T, TLoader>(
    path: &Path,
    syntax: Syntax,
    env: Option<impl IntoIterator<Item = (String, String)>>,
    module_loader: Option<TLoader>,
    compiler_options: &CompilerOptions,
    factory: &(impl ExpressionFactory<T> + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Clone + 'static),
) -> Result<(Program, InstructionPointer)>
where
    T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T> + 'static,
    T::Builtin: From<Stdlib> + From<JsonStdlib> + From<JsStdlib>,
    TLoader: Fn(&str, &Path) -> Option<Result<T, String>> + 'static,
{
    match syntax {
        Syntax::JavaScript => compile_js_entry_point(
            path,
            env,
            module_loader,
            compiler_options,
            factory,
            allocator,
        ),
        Syntax::Json => compile_json_entry_point(path, compiler_options, factory, allocator),
        Syntax::Lisp => compile_sexpr_entry_point(path, compiler_options, factory, allocator),
    }
}

fn compile_graph_root<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>>(
    expression: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    compiler_options: &CompilerOptions,
    compiler_mode: CompilerMode,
) -> Result<(Program, InstructionPointer)> {
    let program = Compiler::new(*compiler_options, None)
        .compile(&expression, compiler_mode, factory, allocator)
        .map_err(|err| anyhow!("{}", err))?;
    Ok((program, InstructionPointer::default()))
}

pub fn format_signal_result<T: Expression>(
    result: &SignalTerm<T>,
    factory: &impl ExpressionFactory<T>,
) -> String {
    result
        .signals()
        .iter()
        .map(|signal| format_signal(signal, factory))
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_signal<T: Expression>(signal: &Signal<T>, factory: &impl ExpressionFactory<T>) -> String {
    match signal.signal_type() {
        SignalType::Error => {
            let (message, args) = {
                let args = signal.args();
                match args.get(0).map(|arg| match factory.match_string_term(arg) {
                    Some(message) => Some(String::from(message.value.as_str())),
                    _ => None,
                }) {
                    Some(message) => (message, Some(&args[1..])),
                    _ => (None, Some(&args[..])),
                }
            };
            format!(
                "Error: {}",
                match message {
                    Some(message) => match args {
                        None => format!("{}", message),
                        Some(args) => format!(
                            "{} {}",
                            message,
                            args.iter()
                                .map(|arg| format!("{}", arg))
                                .collect::<Vec<_>>()
                                .join(" ")
                        ),
                    },
                    None => String::from("<unknown>"),
                }
            )
        }
        SignalType::Custom(signal_type) => format!(
            "<{}>{}",
            signal_type,
            format!(
                " {}",
                signal
                    .args()
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        ),
        SignalType::Pending => String::from("<pending>"),
    }
}
