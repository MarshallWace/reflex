// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::io::{self, Write};

use reflex::core::{
    DependencyList, DynamicState, Evaluate, EvaluationCache, EvaluationResult, Expression,
    ExpressionFactory, HeapAllocator, Reducible, Rewritable,
};

use crate::{format_signal_result, SyntaxParser};

pub fn run<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>>(
    parser: impl SyntaxParser<T>,
    state: &impl DynamicState<T>,
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

        match parser.parse(&input) {
            Ok(expression) => {
                let (output, _) = eval(&expression, state, factory, allocator, cache);
                writeln!(stdout, "{}", output)
            }
            Err(error) => writeln!(stderr, "Syntax error: {}", error),
        }?;
    }
    Ok(())
}

pub fn eval<T: Expression + Evaluate<T>>(
    expression: &T,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> (String, DependencyList) {
    let (result, dependencies) = expression
        .evaluate(state, factory, allocator, cache)
        .unwrap_or_else(|| EvaluationResult::new(expression.clone(), DependencyList::empty()))
        .into_parts();
    let output = if let Some(result) = factory.match_signal_term(&result) {
        format_signal_result(result)
    } else {
        format!("{}", result)
    };
    (output, dependencies)
}
