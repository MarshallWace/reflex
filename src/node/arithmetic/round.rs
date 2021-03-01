// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct RoundNode {
    target: Expression<Node>,
}
impl RoundNode {
    pub fn new(target: Expression<Node>) -> Self {
        RoundNode { target }
    }
}
impl AstNode<Node> for RoundNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl NodeType<Node> for RoundNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for RoundNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Float(target))) => Expression::new(Node::Core(
                CoreNode::Value(ValueNode::Float(target.round())),
            )),
            target => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected Float, received {}",
                target,
            ))))),
        }
    }
}
impl fmt::Display for RoundNode {
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
    fn round_expressions() {
        let env = Env::new();
        let expression = parser::parse("(round 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(0.0))))
        );
        let expression = parser::parse("(round 2.718)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.0))))
        );
        let expression = parser::parse("(round 3.000)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.0))))
        );
        let expression = parser::parse("(round 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(3.0))))
        );
        let expression = parser::parse("(round -0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(0.0))))
        );
        let expression = parser::parse("(round -2.718)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(-3.0))))
        );
        let expression = parser::parse("(round -3.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(-3.0))))
        );
        let expression = parser::parse("(round -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(-3.0))))
        );
    }

    #[test]
    fn invalid_round_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(round 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Float, received 3"
            ))))
        );
        let expression = parser::parse("(round null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Float, received Nil"
            ))))
        );
        let expression = parser::parse("(round false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Float, received false"
            ))))
        );
        let expression = parser::parse("(round \"3\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected Float, received \"3\""
            ))))
        );
    }
}
