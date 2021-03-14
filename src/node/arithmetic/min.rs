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
pub struct MinNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl MinNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        MinNode { left, right }
    }
}
impl AstNode<Node> for MinNode {
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
impl<'a> CompoundNode<'a> for MinNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.left).chain(once(&self.right))
    }
}
impl NodeType<Node> for MinNode {
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
impl Evaluate2 for MinNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(&self, left: &Expression<Node>, right: &Expression<Node>) -> Expression<Node> {
        match (left.value(), right.value()) {
            (
                Node::Core(CoreNode::Value(ValueNode::Int(left))),
                Node::Core(CoreNode::Value(ValueNode::Int(right))),
            ) => Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (*left).min(*right),
            )))),
            (
                Node::Core(CoreNode::Value(ValueNode::Float(left))),
                Node::Core(CoreNode::Value(ValueNode::Float(right))),
            ) => Expression::new(Node::Core(CoreNode::Value(ValueNode::Float(
                (*left).min(*right),
            )))),
            (left, right) => {
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                    "Expected (Int, Int) or (Float, Float), received ({}, {})",
                    left, right,
                )))))
            }
        }
    }
}
impl fmt::Display for MinNode {
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
    fn min_expressions() {
        let env = Env::new();
        let expression = parser::parse("(min 0 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(0)))
        );
        let expression = parser::parse("(min -0 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(0)))
        );
        let expression = parser::parse("(min 0 -0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(0)))
        );
        let expression = parser::parse("(min -0 -0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(0)))
        );
        let expression = parser::parse("(min 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(3)))
        );
        let expression = parser::parse("(min 4 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(3)))
        );
        let expression = parser::parse("(min -3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-3)))
        );
        let expression = parser::parse("(min 4 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-3)))
        );
        let expression = parser::parse("(min 3 -4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-4)))
        );
        let expression = parser::parse("(min -4 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-4)))
        );
        let expression = parser::parse("(min -3 -4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-4)))
        );
        let expression = parser::parse("(min -4 -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Int(-4)))
        );

        let expression = parser::parse("(min 0.0 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(0.0)))
        );
        let expression = parser::parse("(min -0.0 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(0.0)))
        );
        let expression = parser::parse("(min 0.0 -0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(0.0)))
        );
        let expression = parser::parse("(min -0.0 -0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(0.0)))
        );
        let expression = parser::parse("(min 2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(2.718)))
        );
        let expression = parser::parse("(min 3.142 2.718)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(2.718)))
        );
        let expression = parser::parse("(min -2.718 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-2.718)))
        );
        let expression = parser::parse("(min 3.142 -2.718)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-2.718)))
        );
        let expression = parser::parse("(min 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-3.142)))
        );
        let expression = parser::parse("(min -3.142 2.718)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-3.142)))
        );
        let expression = parser::parse("(min -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-3.142)))
        );
        let expression = parser::parse("(min -3.142 -2.718)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Value(ValueNode::Float(-3.142)))
        );
    }

    #[test]
    fn invalid_min_expression_operands() {
        let env = Env::new();
        let expression = parser::parse("(min 3 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, 3.142)"
            )))
        );
        let expression = parser::parse("(min 3.142 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, 3)"
            )))
        );

        let expression = parser::parse("(min 3 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, null)"
            )))
        );
        let expression = parser::parse("(min 3 #f)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, #f)"
            )))
        );
        let expression = parser::parse("(min 3 \"3\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3, \"3\")"
            )))
        );

        let expression = parser::parse("(min null 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (null, 3)"
            )))
        );
        let expression = parser::parse("(min #f 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (#f, 3)"
            )))
        );
        let expression = parser::parse("(min \"3\" 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"3\", 3)"
            )))
        );

        let expression = parser::parse("(min 3.142 null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, null)"
            )))
        );
        let expression = parser::parse("(min 3.142 #f)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, #f)"
            )))
        );
        let expression = parser::parse("(min 3.142 \"3\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (3.142, \"3\")"
            )))
        );

        let expression = parser::parse("(min null 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (null, 3.142)"
            )))
        );
        let expression = parser::parse("(min #f 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (#f, 3.142)"
            )))
        );
        let expression = parser::parse("(min \"3\" 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            *result.value(),
            Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected (Int, Int) or (Float, Float), received (\"3\", 3.142)"
            )))
        );
    }
}
