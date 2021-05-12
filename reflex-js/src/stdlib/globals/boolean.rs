// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        ApplicationTerm, Arity, Expression, LambdaTerm, StaticVariableTerm, Term, VariableTerm,
    },
    stdlib::{builtin::BuiltinTerm, value::ValueTerm},
};

pub(crate) fn global_boolean() -> Expression {
    Expression::new(Term::Lambda(LambdaTerm::new(
        Arity::from(1, 0, None),
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Builtin(BuiltinTerm::If)),
            vec![
                Expression::new(Term::Variable(VariableTerm::Static(
                    StaticVariableTerm::new(0),
                ))),
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
            ],
        ))),
    )))
}
