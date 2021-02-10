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
pub struct PowNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl PowNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        PowNode { left, right }
    }
}
impl AstNode<Node> for PowNode {
    fn factory(args: &Vec<Expression<Node>>) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let left = args.next().unwrap();
        let right = args.next().unwrap();
        Ok(Self::new(left, right))
    }
}
impl NodeType<Node> for PowNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.left, &self.right]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for PowNode {
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
            (Node::Core(CoreNode::Value(ValueNode::Int(left))), Node::Core(CoreNode::Value(ValueNode::Int(right)))) => {
                let left = *left;
                let right = *right;
                Expression::new(Node::Core(CoreNode::Value(if right < 0 {
                    ValueNode::Float((left as f64).powi(right))
                } else {
                    ValueNode::Int(left.pow(right as u32))
                })))
            }
            (Node::Core(CoreNode::Value(ValueNode::Float(left))), Node::Core(CoreNode::Value(ValueNode::Int(right)))) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(left.powi(*right)))))
            }
            (Node::Core(CoreNode::Value(ValueNode::Int(left))), Node::Core(CoreNode::Value(ValueNode::Float(right)))) => {
                let left = *left;
                let right = *right;
                if left < 0 && !is_integer(right) {
                    Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!("Invalid exponentiation operands: ({}, {})", left, right)))))
                } else {
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Float((left as f64).powf(right)))))
                }
            }
            (Node::Core(CoreNode::Value(ValueNode::Float(left))), Node::Core(CoreNode::Value(ValueNode::Float(right)))) => {
                let left = *left;
                let right = *right;
                if left < 0.0 && !is_integer(right) {
                    Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!("Invalid exponentiation operands: ({}, {})", left, right)))))
                } else {
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(left.powf(right)))))
                }
            }
            (left, right) => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received ({}, {})",
                left, right,
            ))))),
        }
    }
}
impl fmt::Display for PowNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

fn is_integer(value: f64) -> bool {
    value == (value as u32) as f64
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
    fn exponentiation_expressions() {
        let env = Env::new();
        let expression = parser::parse("(pow 0 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (0 as i32).pow(0)
            ))))
        );
        let expression = parser::parse("(pow 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (3 as i32).pow(4)
            ))))
        );
        let expression = parser::parse("(pow -3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (-3 as i32).pow(4)
            ))))
        );
        let expression = parser::parse("(pow 3 -4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (3 as f64).powf(-4 as f64)
            ))))
        );
        let expression = parser::parse("(pow -3 -4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (-3 as f64).powf(-4 as f64)
            ))))
        );

        let expression = parser::parse("(pow 0.0 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (0.0 as f64).powf(0.0)
            ))))
        );
        let expression = parser::parse("(pow 2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (2.718 as f64).powf(3.142)
            ))))
        );
        let expression = parser::parse("(pow -2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Invalid exponentiation operands: (-2.718, 3.142)"
            ))))
        );
        let expression = parser::parse("(pow 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (2.718 as f64).powf(-3.142)
            ))))
        );
        let expression = parser::parse("(pow -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Invalid exponentiation operands: (-2.718, -3.142)"
            ))))
        );

        let expression = parser::parse("(pow 0 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (0 as f64).powf(0.0)
            ))))
        );
        let expression = parser::parse("(pow 3 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (3 as f64).powf(3.142)
            ))))
        );
        let expression = parser::parse("(pow 3 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (3 as f64).powf(-3.142)
            ))))
        );
        let expression = parser::parse("(pow -3 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Invalid exponentiation operands: (-3, 3.142)"
            ))))
        );
        let expression = parser::parse("(pow -3 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Invalid exponentiation operands: (-3, -3.142)"
            ))))
        );

        let expression = parser::parse("(pow 0.0 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (0.0 as f64).powi(0)
            ))))
        );
        let expression = parser::parse("(pow 3.142 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (3.142 as f64).powi(3)
            ))))
        );
        let expression = parser::parse("(pow -3.142 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (-3.142 as f64).powi(3)
            ))))
        );
        let expression = parser::parse("(pow 3.142 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (3.142 as f64).powi(-3)
            ))))
        );
        let expression = parser::parse("(pow -3.142 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (-3.142 as f64).powi(-3)
            ))))
        );
    }

    #[test]
    fn invalid_exponentiation_expression_operands() {
        let env = Env::new();

        let expression = parser::parse("(pow 3 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3, Nil)"
            ))))
        );
        let expression = parser::parse("(pow 3 false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3, false)"
            ))))
        );
        let expression = parser::parse("(pow 3 \"3\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3, \"3\")"
            ))))
        );

        let expression = parser::parse("(pow null 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (Nil, 3)"
            ))))
        );
        let expression = parser::parse("(pow false 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (false, 3)"
            ))))
        );
        let expression = parser::parse("(pow \"3\" 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (\"3\", 3)"
            ))))
        );

        let expression = parser::parse("(pow 3.142 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3.142, Nil)"
            ))))
        );
        let expression = parser::parse("(pow 3.142 false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3.142, false)"
            ))))
        );
        let expression = parser::parse("(pow 3.142 \"3\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (3.142, \"3\")"
            ))))
        );

        let expression = parser::parse("(pow null 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (Nil, 3.142)"
            ))))
        );
        let expression = parser::parse("(pow false 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (false, 3.142)"
            ))))
        );
        let expression = parser::parse("(pow \"3\" 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Int) or (Int, Float) or (Float, Float), received (\"3\", 3.142)"
            ))))
        );
    }
}
