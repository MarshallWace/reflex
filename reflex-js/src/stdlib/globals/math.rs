// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, Term},
    stdlib::builtin::BuiltinTerm,
};

use crate::stdlib::globals::create_struct;

pub(crate) fn global_math() -> Expression {
    create_struct(vec![
        ("abs", Expression::new(Term::Builtin(BuiltinTerm::Abs))),
        ("ceil", Expression::new(Term::Builtin(BuiltinTerm::Ceil))),
        ("floor", Expression::new(Term::Builtin(BuiltinTerm::Floor))),
        ("max", Expression::new(Term::Builtin(BuiltinTerm::Max))),
        ("min", Expression::new(Term::Builtin(BuiltinTerm::Min))),
        ("round", Expression::new(Term::Builtin(BuiltinTerm::Round))),
    ])
}
