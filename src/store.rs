// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::expression::Expression;

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
    use crate::{
        expression::Expression,
        node::Node,
        parser,
        store::Store,
        value::{StringValue, Value},
    };

    #[test]
    fn static_values_nil() {
        let store = Store::new();
        let target = parser::parse("null", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Nil));
    }

    #[test]
    fn static_values_boolean() {
        let store = Store::new();
        let target = parser::parse("true", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Boolean(true)));
        let target = parser::parse("false", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Boolean(false)));
    }

    #[test]
    fn static_values_int() {
        let store = Store::new();
        let target = parser::parse("3", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Int(3)));
    }

    #[test]
    fn static_values_float() {
        let store = Store::new();
        let target = parser::parse("3.0", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Float(3.0)));
    }

    #[test]
    fn static_values_string() {
        let store = Store::new();
        let target = parser::parse("\"foo\"", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::String(StringValue::new(String::from("foo"))))
        );
        let target = Expression::Value(Value::String(StringValue::literal("foo")));
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::String(StringValue::literal("foo")))
        );
    }

    #[test]
    fn static_errors() {
        let store = Store::new();
        let target = parser::parse("(throw \"foo\")", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn static_pendings() {
        let store = Store::new();
        let target = parser::parse("(await)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
    }

    #[test]
    fn dynamic_expressions() {
        let store = Store::new();
        let target = parser::parse("(add 3 4)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn nested_expressions() {
        let store = Store::new();
        let target = parser::parse("(add (add (abs -3) 4) 5)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(
            result,
            Expression::Value(Value::Int(((-3 as i32).abs() + 4) + 5))
        );
    }

    #[test]
    fn short_circuit_errors() {
        let store = Store::new();
        let target = parser::parse("(add (throw \"foo\") 3)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = parser::parse("(add 3 (throw \"foo\"))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn short_circuit_pendings() {
        let store = Store::new();
        let target = parser::parse("(add (await) 3)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = parser::parse("(add 3 (await))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = parser::parse("(add (add (await) 3) 3)", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
        let target = parser::parse("(add 3 (add (await) 3))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Pending);
    }

    #[test]
    fn short_circuit_error_priority() {
        let store = Store::new();
        let target = parser::parse("(add (throw \"foo\") (await))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target = parser::parse("(add (await) (throw \"foo\"))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target =
            parser::parse("(add (add (throw \"foo\") (await)) (await))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
        let target =
            parser::parse("(add (await) (add (await) (throw \"foo\")))", &Node::new).unwrap();
        let result = store.evaluate(&target);
        assert_eq!(result, Expression::Error(String::from("foo")));
    }
}

#[cfg(test)]
mod benchmarks {
    extern crate test;
    use test::Bencher;

    use crate::{node::Node, parser, store::Store};

    #[bench]
    fn nested_expressions(b: &mut Bencher) {
        let store = Store::new();
        let target = parser::parse("(add (add (abs -3) 4) 5)", &Node::new).unwrap();
        b.iter(|| store.evaluate(&target));
    }
}
