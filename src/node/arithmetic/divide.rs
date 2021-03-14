// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
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
impl<'a> CompoundNode<'a> for DivideNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.left).chain(once(&self.right))
    }
}
impl NodeType<Node> for DivideNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate2::evaluate(self, env)
    }
}
impl Evaluate2 for DivideNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(&self, left: &Expression<Node>, right: &Expression<Node>) -> Expression<Node> {
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
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser, Node,
        },
    };

    #[test]
    fn divide_expressions() {
        let env = Env::new();
        let expression = parser::parse("(/ 0 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(0 / 3)))
        );
        let expression = parser::parse("(/ 12 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(12 / 3)))
        );
        let expression = parser::parse("(/ -12 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-12 / 3)))
        );
        let expression = parser::parse("(/ 12 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(12 / -3)))
        );
        let expression = parser::parse("(/ -12 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-12 / -3)))
        );

        let expression = parser::parse("(/ 0.0 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(0.0 / 3.142)))
        );
        let expression = parser::parse("(/ 2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(2.718 / 3.142)))
        );
        let expression = parser::parse("(/ -2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-2.718 / 3.142)))
        );
        let expression = parser::parse("(/ 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(2.718 / -3.142)))
        );
        let expression = parser::parse("(/ -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-2.718 / -3.142)))
        );
    }

    #[test]
    fn integer_divide_rounding() {
        let env = Env::new();
        let expression = parser::parse("(/ 5 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(1)))
        );
    }

    #[test]
    fn divide_by_zero() {
        let env = Env::new();
        let expression = parser::parse("(/ 3 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new("Division by zero: (3 / 0)")))
        );
        let expression = parser::parse("(/ 3 -0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new("Division by zero: (3 / 0)")))
        );
        let expression = parser::parse("(/ 0 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new("Division by zero: (0 / 0)")))
        );

        let expression = parser::parse("(/ 3.142 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3.142 / 0.0)"
            )))
        );
        let expression = parser::parse("(/ 3.142 -0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (3.142 / -0.0)"
            )))
        );
        let expression = parser::parse("(/ 0.0 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Division by zero: (0.0 / 0.0)"
            )))
        );
    }

    #[test]
    fn invalid_divide_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(/ 3 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, 3.142)"
            )))
        );
        let expression = parser::parse("(/ 3.142 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, 3)"
            )))
        );

        let expression = parser::parse("(/ 3 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, null)"
            )))
        );
        let expression = parser::parse("(/ 3 #f)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, #f)"
            )))
        );
        let expression = parser::parse("(/ 3 \"0\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, \"0\")"
            )))
        );

        let expression = parser::parse("(/ null 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (null, 3)"
            )))
        );
        let expression = parser::parse("(/ #f 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (#f, 3)"
            )))
        );
        let expression = parser::parse("(/ \"0\" 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"0\", 3)"
            )))
        );

        let expression = parser::parse("(/ 3.142 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, null)"
            )))
        );
        let expression = parser::parse("(/ 3.142 #f)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, #f)"
            )))
        );
        let expression = parser::parse("(/ 3.142 \"0\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, \"0\")"
            )))
        );

        let expression = parser::parse("(/ null 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (null, 3.142)"
            )))
        );
        let expression = parser::parse("(/ #f 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (#f, 3.142)"
            )))
        );
        let expression = parser::parse("(/ \"0\" 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"0\", 3.142)"
            )))
        );
    }
}
