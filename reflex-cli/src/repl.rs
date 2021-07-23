// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{
    cache::EvaluationCache,
    core::{DependencyList, DynamicState, Expression, SignalTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};

pub(crate) type ReplParser = Box<dyn Fn(&str) -> Result<Expression, String>>;

pub(crate) fn run(
    parser: ReplParser,
    state: &mut DynamicState,
    cache: &mut impl EvaluationCache,
) -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;

        let input = {
            let mut input = String::new();
            stdin.read_line(&mut input)?;
            input
        };

        if input == "exit\n" {
            break;
        }

        match parser(&input) {
            Ok(expression) => {
                let (output, _) = eval(expression, state, cache);
                writeln!(stdout, "{}", output)
            }
            Err(error) => writeln!(stderr, "Syntax error: {}", error),
        }?;
    }
    Ok(())
}

pub(crate) fn eval(
    expression: Expression,
    state: &DynamicState,
    cache: &mut impl EvaluationCache,
) -> (String, DependencyList) {
    let (result, dependencies) = expression.evaluate(state, cache).into_parts();
    let output = match result.value() {
        Term::Signal(signal) => format_signal_output(signal),
        value => format!("{}", value),
    };
    (output, dependencies)
}

fn format_signal_output(signal: &SignalTerm) -> String {
    signal
        .signals()
        .into_iter()
        .map(|signal| match signal.signal_type() {
            SignalType::Error => {
                let (message, args) = {
                    let args = signal.args();
                    match args.get(0).map(|arg| arg.value()) {
                        Some(Term::Value(ValueTerm::String(message))) => {
                            (Some(message.clone()), Some(&args[1..]))
                        }
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
        })
        .collect::<Vec<_>>()
        .join("\n")
}
