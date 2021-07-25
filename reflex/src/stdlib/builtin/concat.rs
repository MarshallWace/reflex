// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, Signal, SignalTerm, Term, VarArgs},
    stdlib::{
        builtin::BuiltinFunction,
        signal::SignalType,
        value::{StringValue, ValueTerm},
    },
};

pub struct Concat {}
impl BuiltinFunction for Concat {
    fn arity() -> Arity {
        Arity::from(2, 0, Some(VarArgs::Eager))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() == 0 {
            return Expression::new(Term::Value(ValueTerm::String(StringValue::from(""))));
        }
        let result = args.into_iter().fold(Ok(String::from("")), |result, arg| {
            let mut result = result?;
            match arg.value() {
                Term::Value(ValueTerm::String(value)) => {
                    result.push_str(value);
                    Ok(result)
                }
                _ => Err(format!("Expected string arguments, received {}", arg)),
            }
        });
        match result {
            Ok(result) => Expression::new(Term::Value(ValueTerm::String(result))),
            Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![Expression::new(Term::Value(ValueTerm::String(error)))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::SubstitutionCache,
        core::{DependencyList, DynamicState, EvaluationResult, Expression, Term},
        parser::sexpr::parse,
        stdlib::value::{StringValue, ValueTerm},
    };

    #[test]
    fn concat_expressions() {
        let mut cache = SubstitutionCache::new();
        let state = DynamicState::new();
        let expression = parse("(concat \"\" \"\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("")))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(concat \"foo\" \"\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("foo")))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(concat \"\" \"bar\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("bar")))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(concat \"foo\" \"bar\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from("foobar")))),
                DependencyList::empty(),
            )
        );
        let expression = parse("(concat \"foo\" \"bar\" \"baz\")").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                    "foobarbaz"
                )))),
                DependencyList::empty(),
            )
        );
    }
}
