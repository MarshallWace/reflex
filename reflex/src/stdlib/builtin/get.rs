// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{
        Arity, DataStructureTerm, Expression, NativeFunction, Signal, SignalTerm,
        StructFieldOffset, Term,
    },
    stdlib::{signal::SignalType, value::ValueTerm},
};

pub struct Get {}
impl NativeFunction for Get {
    fn arity() -> Arity {
        Arity::from(2, 0, None)
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        if args.len() != 2 {
            return Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Expected 2 arguments, received {}",
                    args.len(),
                ))],
            ))));
        }
        let mut args = args.into_iter();
        let target = args.next().unwrap();
        let field_offset = args.next().unwrap();
        match (target.value(), field_offset.value()) {
            (
                Term::DataStructure(DataStructureTerm::Struct(target)),
                Term::Value(ValueTerm::Int(field_offset)),
            ) => match target.get(*field_offset as StructFieldOffset) {
                Some(expression) => expression,
                None => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![ValueTerm::String(format!(
                        "Invalid field offset: {} on struct {}",
                        field_offset, target
                    ))],
                )))),
            },
            _ => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Error,
                vec![ValueTerm::String(format!(
                    "Invalid field access: Expected (<struct>, Int), received ({}, {})",
                    target, field_offset,
                ))],
            )))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            ApplicationTerm, DataStructureTerm, DependencyList, DynamicState, EvaluationResult,
            Expression, StructTerm, Term,
        },
        stdlib::builtin::BuiltinTerm,
        stdlib::value::ValueTerm,
    };

    #[test]
    fn get_expressions() {
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Get)),
            vec![
                Expression::new(Term::DataStructure(DataStructureTerm::Struct(
                    StructTerm::new(vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ]),
                ))),
                Expression::new(Term::Value(ValueTerm::Int(1))),
            ],
        )));
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(4)))),
                DependencyList::empty(),
            )
        )
    }
}
