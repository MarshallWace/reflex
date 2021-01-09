// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
#![feature(test)]

pub mod env;
pub mod expression;
pub mod node;
pub mod operation;
pub mod parser;
pub mod utils;
pub mod value;

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{env::Env, expression::Expression, node::Node, operation::evaluate::Evaluate, parser, value::{StringValue, Value}};

    #[test]
    fn static_values_nil() {
        let env = Env::new();
        let expression = parser::parse("null", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Nil));
    }

    #[test]
    fn static_values_boolean() {
        let env = Env::new();
        let expression = parser::parse("true", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(true)));
        let expression = parser::parse("false", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Boolean(false)));
    }

    #[test]
    fn static_values_int() {
        let env = Env::new();
        let expression = parser::parse("3", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3)));
    }

    #[test]
    fn static_values_float() {
        let env = Env::new();
        let expression = parser::parse("3.0", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Float(3.0)));
    }

    #[test]
    fn static_values_string() {
        let env = Env::new();
        let expression = parser::parse("\"foo\"", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Value(Value::String(StringValue::new(String::from("foo"))))
        );
        let expression = Rc::new(Expression::Value(Value::String(StringValue::literal("foo"))));
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Value(Value::String(StringValue::literal("foo")))
        );
    }

    #[test]
    fn static_errors() {
        let env = Env::new();
        let expression = parser::parse("(throw \"foo\")", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn static_pendings() {
        let env = Env::new();
        let expression = parser::parse("(await)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Pending);
    }

    #[test]
    fn dynamic_expressions() {
        let env = Env::new();
        let expression = parser::parse("(add 3 4)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Value(Value::Int(3 + 4)));
    }

    #[test]
    fn nested_expressions() {
        let env = Env::new();
        let expression = parser::parse("(add (add (abs -3) 4) 5)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result,
            Expression::Value(Value::Int(((-3 as i32).abs() + 4) + 5))
        );
    }

    #[test]
    fn short_circuit_errors() {
        let env = Env::new();
        let expression = parser::parse("(add (throw \"foo\") 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
        let expression = parser::parse("(add 3 (throw \"foo\"))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
    }

    #[test]
    fn short_circuit_pendings() {
        let env = Env::new();
        let expression = parser::parse("(add (await) 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Pending);
        let expression = parser::parse("(add 3 (await))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Pending);
        let expression = parser::parse("(add (add (await) 3) 3)", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Pending);
        let expression = parser::parse("(add 3 (add (await) 3))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Pending);
    }

    #[test]
    fn short_circuit_error_priority() {
        let env = Env::new();
        let expression = parser::parse("(add (throw \"foo\") (await))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
        let expression = parser::parse("(add (await) (throw \"foo\"))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
        let expression =
            parser::parse("(add (add (throw \"foo\") (await)) (await))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
        let expression =
            parser::parse("(add (await) (add (await) (throw \"foo\")))", &Node::factory).unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(*result, Expression::Error(String::from("foo")));
    }
}

#[cfg(test)]
mod benchmarks {
    extern crate test;
    use test::Bencher;

    use crate::{env::Env, node::Node, operation::evaluate::Evaluate, parser};

    #[bench]
    fn nested_expressions(b: &mut Bencher) {
        let env = Env::new();
        let expression = parser::parse("(add (add (abs -3) 4) 5)", &Node::factory).unwrap();
        b.iter(|| expression.evaluate(&env));
    }
}
