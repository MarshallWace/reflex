// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::expression::Expression;

pub fn format_type(expression: &Expression) -> String {
    match expression {
        Expression::Pending => String::from("Pending"),
        Expression::Error(message) => format!("Error({:?})", message),
        Expression::Value(value) => format!("{:?}", value),
        Expression::Function(args, body) => {
            format!("Function({} -> {})", args.join(" "), format_type(body))
        }
        Expression::Reference(value) => format!("{:?}", value),
        Expression::Node(node) => format!("{:?}", node),
    }
}
