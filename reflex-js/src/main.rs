// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{env, error::Error, fs};

use reflex::{
    cache::EvaluationCache,
    core::{DynamicState, Signal},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_js::{builtin_globals, builtin_imports, parse, Env};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        return Err("Missing input filename".into());
    }
    if args.len() > 2 {
        return Err("Multiple input filenames".into());
    }
    let filename = &args[1];
    let src = fs::read_to_string(filename).expect("Failed to read input file");
    let env = Env::new()
        .with_globals(builtin_globals())
        .with_imports(builtin_imports());
    let expression = match parse(&src, &env) {
        Err(error) => panic!(error),
        Ok(expression) => expression,
    };
    let mut cache = EvaluationCache::new();
    let state = DynamicState::new();
    let (result, _) = expression.evaluate(&state, &mut cache).unwrap();
    match result {
        Ok(result) => {
            println!("{}", result);
        }
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
                println!("Error: {}", message);
            }
            for signal in signals {
                println!("{}", signal);
            }
        }
    }
    Ok(())
}
