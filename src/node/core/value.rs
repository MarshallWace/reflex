// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::Node,
};

#[derive(Debug, PartialEq, Clone)]
pub enum ValueNode {
    Nil,
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(StringValue),
}
impl NodeType<Node> for ValueNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        Vec::new()
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
    }
}
impl fmt::Display for ValueNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = match self {
            ValueNode::Nil => String::from("Nil"),
            ValueNode::Boolean(value) => format!("{:?}", value),
            ValueNode::Int(value) => format!("{:?}", value),
            ValueNode::Float(value) => format!("{:?}", value),
            ValueNode::String(value) => format!("{}", value),
        };
        write!(f, "{}", formatted)
    }
}

#[derive(Clone)]
pub enum StringValue {
    Literal(&'static str),
    Runtime(Rc<String>),
}
impl StringValue {
    pub fn literal(value: &'static str) -> StringValue {
        StringValue::Literal(value)
    }
    pub fn new(value: String) -> Self {
        StringValue::Runtime(Rc::new(value))
    }
    pub fn get(&self) -> &str {
        match self {
            StringValue::Literal(value) => value,
            StringValue::Runtime(value) => value,
        }
    }
}
impl PartialEq for StringValue {
    fn eq(&self, other: &StringValue) -> bool {
        match (self, other) {
            (&StringValue::Literal(left), &StringValue::Literal(right)) => {
                std::ptr::eq(left, right)
            }
            (left, right) => left.get() == right.get(),
        }
    }
}
impl fmt::Display for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.get())
    }
}
impl fmt::Debug for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({:?})",
            match self {
                StringValue::Literal(_) => "Literal",
                StringValue::Runtime(_) => "Runtime",
            },
            self.get()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{core::CoreNode, parser, Node},
    };

    use super::{StringValue, ValueNode};

    #[test]
    fn static_values_nil() {
        let env = Env::new();
        let expression = parser::parse("null").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
        );
    }

    #[test]
    fn static_values_boolean() {
        let env = Env::new();
        let expression = parser::parse("true").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("false").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }

    #[test]
    fn static_values_int() {
        let env = Env::new();
        let expression = parser::parse("3").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );
    }

    #[test]
    fn static_values_float() {
        let env = Env::new();
        let expression = parser::parse("3.0").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.0))))
        );
    }

    #[test]
    fn static_values_string() {
        let env = Env::new();
        let expression = parser::parse("\"foo\"").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::new(String::from("foo"))
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
            StringValue::literal("foo"),
        ))));
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::literal("foo")
            ))))
        );
    }

    #[test]
    fn string_equality() {
        let left = StringValue::literal("foo");
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("foo"));
        assert_eq!(left, right);
        let left = StringValue::literal("foo");
        let right = StringValue::new(String::from("bar"));
        assert_ne!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("foo");
        assert_eq!(left, right);
        let left = StringValue::new(String::from("foo"));
        let right = StringValue::literal("bar");
        assert_ne!(left, right);
    }

    #[test]
    fn format_values() {
        assert_eq!(format!("{}", ValueNode::Nil), "Nil");
        assert_eq!(format!("{}", ValueNode::Boolean(true)), "true");
        assert_eq!(format!("{}", ValueNode::Boolean(false)), "false");
        assert_eq!(format!("{}", ValueNode::Int(0)), "0");
        assert_eq!(format!("{}", ValueNode::Int(3)), "3");
        assert_eq!(format!("{}", ValueNode::Int(-3)), "-3");
        assert_eq!(format!("{}", ValueNode::Float(0.0)), "0.0");
        assert_eq!(format!("{}", ValueNode::Float(3.0)), "3.0");
        assert_eq!(format!("{}", ValueNode::Float(-3.0)), "-3.0");
        assert_eq!(
            format!("{}", ValueNode::String(StringValue::literal("foo"))),
            "\"foo\""
        );
        assert_eq!(
            format!(
                "{}",
                ValueNode::String(StringValue::new(String::from("foo")))
            ),
            "\"foo\""
        );
    }
}
