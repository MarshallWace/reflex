// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, Term},
    stdlib::builtin::BuiltinTerm,
};

use crate::stdlib::globals::create_struct;

pub fn global_math() -> Expression {
    create_struct(vec![
        (
            String::from("abs"),
            Expression::new(Term::Builtin(BuiltinTerm::Abs)),
        ),
        (
            String::from("ceil"),
            Expression::new(Term::Builtin(BuiltinTerm::Ceil)),
        ),
        (
            String::from("floor"),
            Expression::new(Term::Builtin(BuiltinTerm::Floor)),
        ),
        (
            String::from("max"),
            Expression::new(Term::Builtin(BuiltinTerm::Max)),
        ),
        (
            String::from("min"),
            Expression::new(Term::Builtin(BuiltinTerm::Min)),
        ),
        (
            String::from("round"),
            Expression::new(Term::Builtin(BuiltinTerm::Round)),
        ),
    ])
}
