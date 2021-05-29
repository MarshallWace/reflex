// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{env, error::Error, fs, path::Path};

use reflex::{
    cache::GenerationalGc,
    core::{DynamicState, SerializedTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_js::{
    parse_module, static_module_loader,
    stdlib::{builtin_globals, builtin_imports},
    Env,
};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().skip(1).collect::<Vec<String>>();
    if args.len() < 1 {
        return Err("Missing input filename".into());
    }
    if args.len() > 1 {
        return Err("Multiple input filenames".into());
    }
    let filename = &args[0];
    let path = Path::new(filename);
    let src = fs::read_to_string(path).expect("Failed to read input file");
    let env = Env::new().with_globals(builtin_globals());
    let loader = static_module_loader(builtin_imports());
    let expression = match parse_module(&src, &env, &path, &loader) {
        Err(error) => panic!(error),
        Ok(expression) => expression,
    };
    let mut cache = GenerationalGc::new();
    let state = DynamicState::new();
    let (result, _) = expression.evaluate(&state, &mut cache).unwrap();
    match result.value() {
        Term::Signal(signal) => {
            for signal in signal.signals() {
                let message = match signal.get_type() {
                    SignalType::Error => {
                        let (message, args) = {
                            let args = signal.args();
                            match args.get(0) {
                                Some(SerializedTerm::Value(ValueTerm::String(message))) => {
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
                };
                println!("{}", message);
            }
        }
        _ => {
            println!("{}", result);
        }
    }
    Ok(())
}
