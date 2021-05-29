// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    core::{Arity, Expression, StructTerm, Term, VarArgs},
    stdlib::builtin::BuiltinFunction,
};

pub struct Tuple {}
impl BuiltinFunction for Tuple {
    fn arity() -> Arity {
        Arity::from(0, 0, Some(VarArgs::Lazy))
    }
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        Expression::new(Term::Struct(StructTerm::new(
            None,
            args.into_iter().collect(),
        )))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cache::GenerationalGc,
        core::{
            ApplicationTerm, DependencyList, DynamicState, EvaluationResult, Expression,
            StructTerm, Term,
        },
        parser::sexpr::parse,
        stdlib::{builtin::BuiltinTerm, value::ValueTerm},
    };

    #[test]
    fn tuple_expressions() {
        let mut cache = GenerationalGc::new();
        let state = DynamicState::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Tuple)),
            vec![],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(None, Vec::new()))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Tuple)),
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![
                        Expression::new(Term::Value(ValueTerm::Int(3))),
                        Expression::new(Term::Value(ValueTerm::Int(4))),
                        Expression::new(Term::Value(ValueTerm::Int(5))),
                    ],
                ))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::Tuple)),
            vec![
                parse("(+ 3 1)").unwrap(),
                parse("(+ 4 1)").unwrap(),
                parse("(+ 5 1)").unwrap(),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Struct(StructTerm::new(
                    None,
                    vec![
                        parse("(+ 3 1)").unwrap(),
                        parse("(+ 4 1)").unwrap(),
                        parse("(+ 5 1)").unwrap(),
                    ],
                ))),
                DependencyList::empty(),
            ),
        );
    }
}
