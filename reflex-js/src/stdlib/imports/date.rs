// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{ApplicationTerm, Arity, Expression, LambdaTerm, Term, VariableTerm},
    stdlib::{
        builtin::BuiltinTerm,
        value::{StringValue, ValueTerm},
    },
};

use crate::stdlib::imports::create_struct;

pub(crate) fn import_date() -> Expression {
    create_struct(vec![(
        "timestamp",
        Expression::new(Term::Lambda(LambdaTerm::new(
            Arity::from(0, 1, None),
            Expression::new(Term::Application(ApplicationTerm::new(
                Expression::new(Term::Builtin(BuiltinTerm::Effect)),
                vec![
                    Expression::new(Term::Value(ValueTerm::String(StringValue::from(
                        "reflex::date::timestamp",
                    )))),
                    Expression::new(Term::Application(ApplicationTerm::new(
                        Expression::new(Term::Builtin(BuiltinTerm::Get)),
                        vec![
                            Expression::new(Term::Variable(VariableTerm::scoped(0))),
                            Expression::new(Term::Value(ValueTerm::String(StringValue::from("interval")))),
                        ]
                    )))
                ],
            ))),
        ))),
    )])
}
