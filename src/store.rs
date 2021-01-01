// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::types::Expression;

pub struct Store {}
impl Store {
    pub fn new() -> Store {
        Store {}
    }
    pub fn evaluate(&self, expression: &Expression) -> Expression {
        expression.evaluate(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::{node::{Node, abs::AbsNode, add::AddNode}, store::Store, types::{Expression, StringValue, Value}};


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

#[cfg(test)]
mod benchmarks {
    extern crate test;
    use test::Bencher;

    use crate::{
        node::{abs::AbsNode, add::AddNode, Node},
        store::Store,
        types::{Expression, Value},
    };

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
