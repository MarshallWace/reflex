// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
        RuntimeState,
    },
    node::{
        core::{CoreNode, ValueNode},
        evaluate::EvaluateResult,
        Evaluate2, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct GteNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl GteNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        GteNode { left, right }
    }
}
impl AstNode<Node> for GteNode {
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
impl<'a> CompoundNode<'a, Node> for GteNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.left).chain(once(&self.right))
    }
}
impl NodeType<Node> for GteNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate2::evaluate(self, env, state)
    }
}
impl Evaluate2 for GteNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(&self, left: &Expression<Node>, right: &Expression<Node>) -> EvaluateResult {
        match (left.value(), right.value()) {
            (
                Node::Core(CoreNode::Value(ValueNode::Int(left))),
                Node::Core(CoreNode::Value(ValueNode::Int(right))),
            ) => Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(left >= right),
            )))),
            (
                Node::Core(CoreNode::Value(ValueNode::Float(left))),
                Node::Core(CoreNode::Value(ValueNode::Float(right))),
            ) => Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(left >= right),
            )))),
            (left, right) => Err(format!(
                "Expected (Int, Int) or (Float, Float), received ({}, {})",
                left, right,
            )),
        }
    }
}
impl fmt::Display for GteNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{EvaluationResult, Expression, RuntimeState},
        node::{
            core::{CoreNode, ValueNode},
            parser, Node,
        },
        signal::Signal,
    };

    #[test]
    fn gte_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(>= 0 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= 3 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3 -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= 3 -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= 3 4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= -3 4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= 3 -4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3 -4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(>= 0.0 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= 3.142 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3.142 -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= 3.142 -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3.142 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= 3.142 2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= 3.142 -2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -3.142 2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= -3.142 -2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= 2.718 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= -2.718 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(>= 2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(>= -2.718 -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
    }

    #[test]
    fn invalid_gte_expression_operands() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(>= 3 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3, 3.142)"
            )))
        );
        let expression = parser::parse("(>= 3.142 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3.142, 3)"
            )))
        );

        let expression = parser::parse("(>= 3 null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3, null)"
            )))
        );
        let expression = parser::parse("(>= 3 #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3, #f)"
            )))
        );
        let expression = parser::parse("(>= 3 \"3\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3, \"3\")"
            )))
        );

        let expression = parser::parse("(>= null 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (null, 3)"
            )))
        );
        let expression = parser::parse("(>= #f 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (#f, 3)"
            )))
        );
        let expression = parser::parse("(>= \"3\" 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (\"3\", 3)"
            )))
        );

        let expression = parser::parse("(>= 3.142 null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3.142, null)"
            )))
        );
        let expression = parser::parse("(>= 3.142 #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3.142, #f)"
            )))
        );
        let expression = parser::parse("(>= 3.142 \"3\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (3.142, \"3\")"
            )))
        );

        let expression = parser::parse("(>= null 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (null, 3.142)"
            )))
        );
        let expression = parser::parse("(>= #f 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (#f, 3.142)"
            )))
        );
        let expression = parser::parse("(>= \"3\" 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Int, Int) or (Float, Float), received (\"3\", 3.142)"
            )))
        );
    }
}
