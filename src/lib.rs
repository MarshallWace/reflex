// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]

mod reflex {
    use std::{fmt, rc::Rc};

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Pending,
        Error(String),
        Value(Value),
        Node(Node),
    }
    impl Evaluate for Expression {
        fn evaluate(&self, store: &Store) -> Expression {
            match self {
                Expression::Pending => Expression::Pending,
                Expression::Error(message) => Expression::Error(message.to_owned()),
                Expression::Value(value) => Expression::Value(value.clone()),
                Expression::Node(node) => node.evaluate(store),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Value {
        Nil,
        Boolean(bool),
        Int(i32),
        Float(f64),
        String(StringValue),
    }
    impl Clone for Value {
        fn clone(&self) -> Self {
            match self {
                Value::Nil => Value::Nil,
                Value::Boolean(value) => Value::Boolean(*value),
                Value::Int(value) => Value::Int(*value),
                Value::Float(value) => Value::Float(*value),
                Value::String(value) => Value::String(value.clone()),
            }
        }
    }

    pub enum StringValue {
        Literal(&'static str),
        Runtime(Rc<String>),
    }
    impl StringValue {
        pub fn literal(value: &'static str) -> StringValue {
            StringValue::Literal(value)
        }
        pub fn new(value: String) -> StringValue {
            StringValue::Runtime(Rc::new(value))
        }
        fn get(&self) -> &str {
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
    impl Clone for StringValue {
        fn clone(&self) -> Self {
            match self {
                StringValue::Literal(value) => StringValue::Literal(value),
                StringValue::Runtime(value) => StringValue::Runtime(Rc::clone(value)),
            }
        }
    }
    impl fmt::Debug for StringValue {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                StringValue::Literal(value) => write!(formatter, "{:?}", value),
                StringValue::Runtime(value) => write!(formatter, "{:?}", value),
            }
        }
    }

    pub struct Store {}
    impl Store {
        pub fn new() -> Store {
            Store {}
        }
        pub fn evaluate(&self, expression: &Expression) -> Expression {
            expression.evaluate(&self)
        }
    }

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

    pub trait Evaluate {
        fn evaluate(&self, store: &Store) -> Expression;
    }

    pub trait Evaluate1: Evaluate {
        fn dependencies(&self) -> &Expression;
        fn run(&self, store: &Store, dep1: &Expression) -> Expression;
        fn evaluate(&self, store: &Store) -> Expression {
            let dep1 = self.dependencies();
            let dep1 = dep1.evaluate(store);
            match &dep1 {
                Expression::Error(_) | Expression::Pending => dep1,
                dep1 => self.run(store, dep1).evaluate(store),
            }
        }
    }

    pub trait Evaluate2: Evaluate {
        fn dependencies(&self) -> (&Expression, &Expression);
        fn run(&self, store: &Store, dep1: &Expression, dep2: &Expression) -> Expression;
        fn evaluate(&self, store: &Store) -> Expression {
            let (dep1, dep2) = self.dependencies();
            let dep1 = dep1.evaluate(store);
            let dep2 = dep2.evaluate(store);
            match (&dep1, &dep2) {
                (Expression::Error(_), _) => dep1,
                (_, Expression::Error(_)) => dep2,
                (Expression::Pending, _) | (_, Expression::Pending) => Expression::Pending,
                (dep1, dep2) => self.run(store, dep1, dep2).evaluate(store),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Node {
        Abs(AbsNode),
        Add(AddNode),
    }
    impl Evaluate for Node {
        fn evaluate(&self, store: &Store) -> Expression {
            match self {
                Node::Abs(node) => Evaluate::evaluate(node, store),
                Node::Add(node) => Evaluate::evaluate(node, store),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct AbsNode {
        target: Box<Expression>,
    }
    impl AbsNode {
        pub fn new(target: Expression) -> AbsNode {
            AbsNode {
                target: Box::new(target),
            }
        }
    }
    impl Evaluate for AbsNode {
        fn evaluate(&self, store: &Store) -> Expression {
            Evaluate1::evaluate(self, store)
        }
    }
    impl Evaluate1 for AbsNode {
        fn dependencies(&self) -> &Expression {
            &self.target
        }
        fn run(&self, _store: &Store, target: &Expression) -> Expression {
            match target {
                Expression::Value(Value::Int(target)) => {
                    Expression::Value(Value::Int(target.abs()))
                }
                Expression::Value(Value::Float(target)) => {
                    Expression::Value(Value::Float(target.abs()))
                }
                target => Expression::Error(format!(
                    "Expected Int or Float, received {}",
                    format_type(target),
                )),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct AddNode {
        left: Box<Expression>,
        right: Box<Expression>,
    }
    impl AddNode {
        pub fn new(left: Expression, right: Expression) -> AddNode {
            AddNode {
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }
    impl Evaluate for AddNode {
        fn evaluate(&self, store: &Store) -> Expression {
            Evaluate2::evaluate(self, store)
        }
    }
    impl Evaluate2 for AddNode {
        fn dependencies(&self) -> (&Expression, &Expression) {
            (&self.left, &self.right)
        }
        fn run(&self, _store: &Store, left: &Expression, right: &Expression) -> Expression {
            match (left, right) {
                (Expression::Value(Value::Int(left)), Expression::Value(Value::Int(right))) => {
                    Expression::Value(Value::Int(left + right))
                }
                (Expression::Value(Value::Float(left)), Expression::Value(Value::Float(right))) => {
                    Expression::Value(Value::Float(left + right))
                }
                (left, right) => Expression::Error(format!(
                    "Expected (Int, Int) or (Float, Float), received ({}, {})",
                    format_type(left),
                    format_type(right),
                )),
            }
        }
    }
}

#[cfg(test)]
mod benchmarks {
    extern crate test;
    use test::Bencher;

    use crate::reflex::{AbsNode, AddNode, Expression, Node, Store, Value};

    #[bench]
    fn nested_expressions(b: &mut Bencher) {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Node(Node::Add(AddNode::new(
                Expression::Node(Node::Abs(AbsNode::new(Expression::Value(Value::Int(-3))))),
                Expression::Value(Value::Int(4)),
            ))),
            Expression::Value(Value::Int(5)),
        )));
        b.iter(|| store.evaluate(&target));
    }
}

#[cfg(test)]
mod tests {

    use crate::reflex::{
        format_value, AbsNode, AddNode, Expression, Node, Store, StringValue, Value,
    };

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

    #[test]
    fn static_values_nil() {
        let store = Store::new();
        let target = Expression::Value(Value::Nil);
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Nil));
    }

    #[test]
    fn static_values_boolean() {
        let store = Store::new();
        let target = Expression::Value(Value::Boolean(true));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Boolean(true)));
        let target = Expression::Value(Value::Boolean(false));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Boolean(false)));
    }

    #[test]
    fn static_values_int() {
        let store = Store::new();
        let target = Expression::Value(Value::Int(3));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Int(3)));
    }

    #[test]
    fn static_values_float() {
        let store = Store::new();
        let target = Expression::Value(Value::Float(3.0));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Float(3.0)));
    }

    #[test]
    fn static_values_string() {
        let store = Store::new();
        let target = Expression::Value(Value::String(StringValue::literal("foo")));
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::String(StringValue::literal("foo")))
        );
        let store = Store::new();
        let target = Expression::Value(Value::String(StringValue::new(String::from("foo"))));
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::String(StringValue::new(String::from("foo"))))
        );
    }

    #[test]
    fn static_errors() {
        let store = Store::new();
        let target = Expression::Error(String::from("foo"));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn static_pendings() {
        let store = Store::new();
        let target = Expression::Pending;
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
    }

    #[test]
    fn dynamic_expressions() {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Value(Value::Int(3)),
            Expression::Value(Value::Int(4)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn nested_expressions() {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Node(Node::Add(AddNode::new(
                Expression::Node(Node::Abs(AbsNode::new(Expression::Value(Value::Int(-3))))),
                Expression::Value(Value::Int(4)),
            ))),
            Expression::Value(Value::Int(5)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::Int(((-3 as i32).abs() + 4) + 5))
        );
    }

    #[test]
    fn short_circuit_errors() {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Error(String::from("foo")),
            Expression::Value(Value::Int(3)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Value(Value::Int(3)),
            Expression::Error(String::from("foo")),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Node(Node::Add(AddNode::new(
                Expression::Error(String::from("foo")),
                Expression::Value(Value::Int(3)),
            ))),
            Expression::Value(Value::Int(3)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Value(Value::Int(3)),
            Expression::Node(Node::Add(AddNode::new(
                Expression::Error(String::from("foo")),
                Expression::Value(Value::Int(3)),
            ))),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn short_circuit_pendings() {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Pending,
            Expression::Value(Value::Int(3)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Value(Value::Int(3)),
            Expression::Pending,
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Node(Node::Add(AddNode::new(
                Expression::Pending,
                Expression::Value(Value::Int(3)),
            ))),
            Expression::Value(Value::Int(3)),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Value(Value::Int(3)),
            Expression::Node(Node::Add(AddNode::new(
                Expression::Pending,
                Expression::Value(Value::Int(3)),
            ))),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
    }

    #[test]
    fn short_circuit_error_priority() {
        let store = Store::new();
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Error(String::from("foo")),
            Expression::Pending,
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Pending,
            Expression::Error(String::from("foo")),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Node(Node::Add(AddNode::new(
                Expression::Error(String::from("foo")),
                Expression::Pending,
            ))),
            Expression::Pending,
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = Expression::Node(Node::Add(AddNode::new(
            Expression::Pending,
            Expression::Node(Node::Add(AddNode::new(
                Expression::Error(String::from("foo")),
                Expression::Pending,
            ))),
        )));
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }
}
