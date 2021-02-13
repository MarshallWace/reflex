// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, Expression, NodeFactoryResult, NodeType},
    node::core::{CoreNode, Node, ValueNode},
};

#[derive(PartialEq, Clone)]
pub struct ErrorNode {
    message: String,
}
impl ErrorNode {
    pub fn new(message: &str) -> Self {
        ErrorNode {
            message: String::from(message),
        }
    }
}
impl AstNode<Node> for ErrorNode {
    fn factory(args: &Vec<Expression<Node>>) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let message = args.next().unwrap();
        match message.value() {
            Node::Core(CoreNode::Value(ValueNode::String(value))) => Ok(Self::new(value.get())),
            _ => Err(format!("Invalid argument")),
        }
    }
}
impl NodeType<Node> for ErrorNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        Vec::new()
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
    }
}
impl fmt::Display for ErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<error:{:?}>", self.message)
    }
}
impl fmt::Debug for ErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{core::CoreNode, parser, Node},
    };

    use super::ErrorNode;

    #[test]
    fn static_errors() {
        let env = Env::new();
        let expression = parser::parse("(error \"foo\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }

    #[test]
    fn short_circuit_errors() {
        let env = Env::new();
        let expression = parser::parse("(add (error \"foo\") 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(add 3 (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }

    #[test]
    fn short_circuit_error_priority() {
        let env = Env::new();
        let expression = parser::parse("(add (error \"foo\") (pending))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(add (pending) (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(add (add (error \"foo\") (pending)) (pending))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(add (pending) (add (pending) (error \"foo\")))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }
}
