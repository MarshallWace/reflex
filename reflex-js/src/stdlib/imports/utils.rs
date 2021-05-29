// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Arity, Expression, LambdaTerm, RecursiveTerm, Term, VariableTerm};

use crate::stdlib::imports::create_struct;
mod types;
use types::import_types;

pub(crate) fn import_utils() -> Expression {
    create_struct(vec![
        (
            "graph",
            Expression::new(Term::Lambda(LambdaTerm::new(
                Arity::from(0, 1, None),
                Expression::new(Term::Recursive(RecursiveTerm::new(Expression::new(
                    Term::Variable(VariableTerm::scoped(0)),
                )))),
            ))),
        ),
        ("Types", import_types()),
    ])
}
