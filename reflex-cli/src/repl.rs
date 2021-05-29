// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{
    cache::GenerationalGc,
    core::{DynamicState, SerializedTerm, Term},
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

        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();

        match parse(&input) {
            Ok(expression) => {
                let (result, _) = expression.evaluate(&state, &mut cache).unwrap();
                match result.value() {
                    Term::Signal(signal) => {
                        for signal in signal.signals() {
                            let message = match signal.get_type() {
                                SignalType::Error => {
                                    let (message, args) = {
                                        let args = signal.args();
                                        match args.get(0) {
                                            Some(SerializedTerm::Value(ValueTerm::String(
                                                message,
                                            ))) => (Some(message.clone()), Some(&args[1..])),
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
                                                        .map(SerializedTerm::stringify)
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
                                            .map(SerializedTerm::stringify)
                                            .collect::<Vec<_>>()
                                            .join(" ")
                                    )
                                ),
                                SignalType::Pending => String::from("<pending>"),
                            };
                            writeln!(stdout, "{}", message)?;
                        }
                    }
                    _ => writeln!(stdout, "{}", result)?,
                }
            }
            Err(err) => {
                writeln!(stderr, "Syntax error: {}", err.message())?;
            }
        };
    }

    Ok(())
}
