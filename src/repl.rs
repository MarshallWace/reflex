// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::{
    env::Env,
    expression::{Expression, RuntimeState},
    node::{
        core::{signals::with_error_handler, CoreNode, ValueNode},
        parser, Node,
    },
    signal::EffectHandler,
};

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
        let state = RuntimeState::new();

        let error_handler = EffectHandler::new(1, handle_errors);

        match parser::parse(&input) {
            Ok(expression) => {
                let result = with_error_handler(error_handler, expression).evaluate(&env, &state);
                writeln!(stdout, "{}", result)?;
            }
            Err(err) => {
                writeln!(stderr, "Syntax error: {}", err.message())?;
            }
        };
    }

    Ok(())
}

fn handle_errors(errors: &[&[Expression<Node>]]) -> Expression<Node> {
    let mut stdout = io::stdout();
    for args in errors {
        let message = &args[0];
        writeln!(stdout, "Error: {}", message).unwrap();
    }
    Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
}
