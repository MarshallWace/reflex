// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        Evaluate2, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct AndNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl AndNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        AndNode { left, right }
    }
}
impl AstNode<Node> for AndNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        Ok(Self::new(left, right))
    }
}
impl NodeType<Node> for AndNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.left, &self.right]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for AndNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(
        &self,
        _env: &Env<Node>,
        left: &Expression<Node>,
        right: &Expression<Node>,
    ) -> Expression<Node> {
        match (left.value(), right.value()) {
            (
                Node::Core(CoreNode::Value(ValueNode::Boolean(left))),
                Node::Core(CoreNode::Value(ValueNode::Boolean(right))),
            ) => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(
                *left && *right,
            )))),
            (left, right) => {
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                    "Expected (Boolean, Boolean), received ({}, {})",
                    left, right,
                )))))
            }
        }
    }
}
impl fmt::Display for AndNode {
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
    fn and_expressions() {
        let env = Env::new();
        let expression = parser::parse("(and false false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(and false true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(and true false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(and true true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
    }

    #[test]
    fn invalid_and_expression_arguments() {
        let env = Env::new();
        let expression = parser::parse("(and true null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (true, Nil)"
            ))))
        );
        let expression = parser::parse("(and true 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (true, 0)"
            ))))
        );
        let expression = parser::parse("(and true 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (true, 0.0)"
            ))))
        );
        let expression = parser::parse("(and true \"\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (true, \"\")"
            ))))
        );

        let expression = parser::parse("(and null true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (Nil, true)"
            ))))
        );
        let expression = parser::parse("(and 0 true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (0, true)"
            ))))
        );
        let expression = parser::parse("(and 0.0 true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (0.0, true)"
            ))))
        );
        let expression = parser::parse("(and \"\" true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Boolean, Boolean), received (\"\", true)"
            ))))
        );
    }
}
