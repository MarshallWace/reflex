// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        Evaluate2, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct DivideNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl DivideNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        DivideNode { left, right }
    }
}
impl AstNode<Node> for DivideNode {
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
impl NodeType<Node> for DivideNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.left, &self.right]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for DivideNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(
        &self,
        left: &Expression<Node>,
        right: &Expression<Node>,
    ) -> Expression<Node> {
        match (left.value(), right.value()) {
            (
                Node::Core(CoreNode::Value(ValueNode::Int(left))),
                Node::Core(CoreNode::Value(ValueNode::Int(right))),
            ) => {
                if *right == 0 {
                    Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                        "Division by zero: ({:?} / {:?})",
                        left, right
                    )))))
                } else {
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(left / right))))
                }
            }
            (
                Node::Core(CoreNode::Value(ValueNode::Float(left))),
                Node::Core(CoreNode::Value(ValueNode::Float(right))),
            ) => {
                if *right == 0.0 {
                    Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                        "Division by zero: ({:?} / {:?})",
                        left, right
                    )))))
                } else {
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(left / right))))
                }
            }
            (left, right) => {
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                    "Expected (Int, Int) or (Float, Float), received ({}, {})",
                    left, right,
                )))))
            }
        }
    }
}
impl fmt::Display for DivideNode {
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
    fn division_expressions() {
        let env = Env::new();
        let expression = parser::parse("(/ 0 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0 / 3))))
        );
        let expression = parser::parse("(/ 12 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(12 / 3))))
        );
        let expression = parser::parse("(/ -12 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(-12 / 3))))
        );
        let expression = parser::parse("(/ 12 -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(12 / -3))))
        );
        let expression = parser::parse("(/ -12 -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(-12 / -3))))
        );

        let expression = parser::parse("(/ 0.0 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(0.0 / 3.142))))
        );
        let expression = parser::parse("(/ 2.718 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(2.718 / 3.142))))
        );
        let expression = parser::parse("(/ -2.718 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                -2.718 / 3.142
            ))))
        );
        let expression = parser::parse("(/ 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                2.718 / -3.142
            ))))
        );
        let expression = parser::parse("(/ -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                -2.718 / -3.142
            ))))
        );
    }

    #[test]
    fn integer_division_rounding() {
        let env = Env::new();
        let expression = parser::parse("(/ 5 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1))))
        );
    }

    #[test]
    fn division_by_zero() {
        let env = Env::new();
        let expression = parser::parse("(/ 3 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3 / 0)"
            ))))
        );
        let expression = parser::parse("(/ 3 -0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3 / 0)"
            ))))
        );
        let expression = parser::parse("(/ 0 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (0 / 0)"
            ))))
        );

        let expression = parser::parse("(/ 3.142 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3.142 / 0.0)"
            ))))
        );
        let expression = parser::parse("(/ 3.142 -0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3.142 / -0.0)"
            ))))
        );
        let expression = parser::parse("(/ 0.0 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (0.0 / 0.0)"
            ))))
        );
    }

    #[test]
    fn invalid_division_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(/ 3 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, 3.142)"
            ))))
        );
        let expression = parser::parse("(/ 3.142 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, 3)"
            ))))
        );

        let expression = parser::parse("(/ 3 null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, Nil)"
            ))))
        );
        let expression = parser::parse("(/ 3 false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, false)"
            ))))
        );
        let expression = parser::parse("(/ 3 \"0\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, \"0\")"
            ))))
        );

        let expression = parser::parse("(/ null 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (Nil, 3)"
            ))))
        );
        let expression = parser::parse("(/ false 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (false, 3)"
            ))))
        );
        let expression = parser::parse("(/ \"0\" 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"0\", 3)"
            ))))
        );

        let expression = parser::parse("(/ 3.142 null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, Nil)"
            ))))
        );
        let expression = parser::parse("(/ 3.142 false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, false)"
            ))))
        );
        let expression = parser::parse("(/ 3.142 \"0\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, \"0\")"
            ))))
        );

        let expression = parser::parse("(/ null 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (Nil, 3.142)"
            ))))
        );
        let expression = parser::parse("(/ false 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (false, 3.142)"
            ))))
        );
        let expression = parser::parse("(/ \"0\" 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"0\", 3.142)"
            ))))
        );
    }
}
