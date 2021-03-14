// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{env::Env, node::parser};

pub fn start() -> io::Result<()> {
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

        let env = Env::new();

        match parser::parse(&input) {
            Ok(expression) => {
                let result = expression.evaluate(&env);
                writeln!(stdout, "{}", result.value())?;
            }
            Err(err) => {
                writeln!(stderr, "Syntax error: {}", err.message())?;
            }
        };
    }

    Ok(())
}
