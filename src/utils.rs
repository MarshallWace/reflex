// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::expression::{Closure, Expression, Function};

pub fn format_type(expression: &Expression) -> String {
    match expression {
        Expression::Pending => String::from("Pending"),
        Expression::Error(message) => format!("Error({:?})", message),
        Expression::Value(value) => format!("{:?}", value),
        Expression::Reference(value) => format!("{:?}", value),
        Expression::Bound(_, value) => format!("Bound({})", format_type(value)),
        Expression::Function(Function { arity, captures: _, body }) => {
            format!("Function({} -> {})", arity, format_type(body))
        }
        Expression::Closure(Closure { env: _, arity, body }) => {
            format!("Closure({} -> {})", arity, format_type(body))
        }
        Expression::Node(node) => format!("{:?}", node),
    }
}
