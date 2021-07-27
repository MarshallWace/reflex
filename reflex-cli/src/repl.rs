// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{
    core::{
        DependencyList, DynamicState, Evaluate, EvaluationCache, EvaluationResult, Expression,
        ExpressionFactory, HeapAllocator, Reducible, Rewritable, Signal, SignalType, StringValue,
    },
    lang::ValueTerm,
};

pub(crate) type ReplParser<T> = Box<dyn Fn(&str) -> Result<T, String>>;

pub(crate) fn run<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>>(
    parser: ReplParser<T>,
    state: &mut DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
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
                let (output, _) = eval(&expression, state, factory, allocator, cache);
                writeln!(stdout, "{}", output)
            }
            Err(error) => writeln!(stderr, "Syntax error: {}", error),
        }?;
    }
    Ok(())
}

pub(crate) fn eval<T: Expression + Evaluate<T>>(
    expression: &T,
    state: &DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> (String, DependencyList) {
    let (result, dependencies) = expression
        .evaluate(state, factory, allocator, cache)
        .unwrap_or_else(|| EvaluationResult::new(expression.clone(), DependencyList::empty()))
        .into_parts();
    let output = if let Some(result) = factory.match_signal_term(&result) {
        result
            .signals()
            .iter()
            .map(|signal| format_signal_output(signal, factory))
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        format!("{}", result)
    };
    (output, dependencies)
}

fn format_signal_output<T: Expression>(
    signal: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> String {
    match signal.signal_type() {
        SignalType::Error => {
            let (message, args) = {
                let args = signal.args();
                match args.get(0).map(|arg| match factory.match_value_term(arg) {
                    Some(ValueTerm::String(message)) => Some(String::from(message.as_str())),
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
