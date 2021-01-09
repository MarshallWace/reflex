// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{env::Env, node::Node, operation::evaluate::Evaluate, parser::parse};

pub fn start() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    let env = Env::new();

    let mut input = String::new();
    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;

        input.clear();
        stdin.read_line(&mut input)?;

        if input == "exit\n" { break; }

        match parse(&input, &Node::factory) {
            Ok(expression) => {
                writeln!(stdout, "{}", expression.evaluate(&env))?;
            },
            Err(err) => {
                writeln!(stderr, "Syntax error: {}", err)?;
            },
        };
    }

    Ok(())
}
