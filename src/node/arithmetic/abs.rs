// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct AbsNode {
    target: Expression<Node>,
}
impl AbsNode {
    pub fn new(target: Expression<Node>) -> Self {
        AbsNode { target }
    }
}
impl AstNode<Node> for AbsNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl NodeType<Node> for AbsNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for AbsNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, _env: &Env<Node>, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Int(target))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(target.abs()))))
            }
            Node::Core(CoreNode::Value(ValueNode::Float(target))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(target.abs()))))
            }
            target => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected Int or Float, received {}",
                target,
            ))))),
        }
    }
}
impl fmt::Display for AbsNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser, Node,
        },
    };

    #[test]
    fn abs_expressions() {
        let env = Env::new();
        let expression = parser::parse("(abs 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0))))
        );
        let expression = parser::parse("(abs -0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0))))
        );
        let expression = parser::parse("(abs 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );
        let expression = parser::parse("(abs -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );

        let expression = parser::parse("(abs 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(0.0))))
        );
        let expression = parser::parse("(abs -0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(0.0))))
        );
        let expression = parser::parse("(abs 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.142))))
        );
        let expression = parser::parse("(abs -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.142))))
        );
    }

    #[test]
    fn invalid_abs_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(abs null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Int or Float, received Nil"
            ))))
        );
        let expression = parser::parse("(abs false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Int or Float, received false"
            ))))
        );
        let expression = parser::parse("(abs \"3\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Int or Float, received \"3\""
            ))))
        );
    }
}
