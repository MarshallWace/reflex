// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{
    cache::EvaluationCache,
    core::{DynamicState, Signal},
    parser::sexpr::parse,
    stdlib::{signal::SignalType, value::ValueTerm},
};

pub fn run() -> io::Result<()> {
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

        let mut cache = EvaluationCache::new();
        let state = DynamicState::new();

        match parse(&input) {
            Ok(expression) => {
                let (result, _) = expression.evaluate(&state, &mut cache).unwrap();
                match result {
                    Ok(result) => writeln!(stdout, "{}", result)?,
                    Err(signals) => {
                        let (errors, signals): (Vec<Signal>, Vec<Signal>) = signals
                            .into_iter()
                            .map(|foo| foo)
                            .partition(|signal| signal.is_type(SignalType::Error));
                        for error in errors {
                            let message = match error.args() {
                                Some(args) => match args.get(0) {
                                    Some(ValueTerm::String(value)) => value,
                                    _ => "<invalid>",
                                },
                                _ => "<unknown>",
                            };
                            writeln!(stdout, "Error: {}", message)?;
                        }
                        for signal in signals {
                            writeln!(stdout, "{}", signal)?;
                        }
                    }
                }
            }
            Err(err) => {
                writeln!(stderr, "Syntax error: {}", err.message())?;
            }
        };
    }

    Ok(())
}
