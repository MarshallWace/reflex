// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::expression::Expression;
use crate::value::Value;

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Nil => String::from("Nil"),
        Value::Boolean(value) => format!("{:?}", value),
        Value::Int(value) => format!("{:?}", value),
        Value::Float(value) => format!("{:?}", value),
        Value::String(value) => format!("{:?}", value.get()),
    }
}

pub fn format_type(expression: &Expression) -> String {
    match expression {
        Expression::Pending => String::from("Pending"),
        Expression::Error(message) => format!("Error({:?})", message),
        Expression::Value(value) => format!("{:?}", value),
        Expression::Node(node) => format!("{:?}", node),
    }
}

#[cfg(test)]
mod tests {
    use crate::value::{StringValue, Value};

    use super::format_value;

    #[test]
    fn format_values() {
        assert_eq!(format_value(&Value::Nil), "Nil");
        assert_eq!(format_value(&Value::Boolean(true)), "true");
        assert_eq!(format_value(&Value::Boolean(false)), "false");
        assert_eq!(format_value(&Value::Int(0)), "0");
        assert_eq!(format_value(&Value::Int(3)), "3");
        assert_eq!(format_value(&Value::Int(-3)), "-3");
        assert_eq!(format_value(&Value::Float(0.0)), "0.0");
        assert_eq!(format_value(&Value::Float(3.0)), "3.0");
        assert_eq!(format_value(&Value::Float(-3.0)), "-3.0");
        assert_eq!(
            format_value(&Value::String(StringValue::literal("foo"))),
            "\"foo\""
        );
        assert_eq!(
            format_value(&Value::String(StringValue::new(String::from("foo")))),
            "\"foo\""
        );
    }
}
