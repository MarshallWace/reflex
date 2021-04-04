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
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct FloorNode {
    target: Expression<Node>,
}
impl FloorNode {
    pub fn new(target: Expression<Node>) -> Self {
        FloorNode { target }
    }
}
impl AstNode<Node> for FloorNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for FloorNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for FloorNode {
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
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for FloorNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Float(target))) => Ok(Expression::new(
                Node::Core(CoreNode::Value(ValueNode::Float(target.floor()))),
            )),
            target => Err(format!("Expected Float, received {}", target)),
        }
    }
}
impl fmt::Display for FloorNode {
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
    fn floor_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(floor 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(0.0)
            ))))
        );
        let expression = parser::parse("(floor 2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(2.0)
            ))))
        );
        let expression = parser::parse("(floor 3.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(3.0)
            ))))
        );
        let expression = parser::parse("(floor 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(3.0)
            ))))
        );
        let expression = parser::parse("(floor -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(0.0)
            ))))
        );
        let expression = parser::parse("(floor -2.718)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-3.0)
            ))))
        );
        let expression = parser::parse("(floor -3.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-3.0)
            ))))
        );
        let expression = parser::parse("(floor -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Float(-4.0)
            ))))
        );
    }

    #[test]
    fn invalid_floor_expression_operands() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(floor 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected Float, received 3")))
        );
        let expression = parser::parse("(floor null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected Float, received null")))
        );
        let expression = parser::parse("(floor #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected Float, received #f")))
        );
        let expression = parser::parse("(floor \"3\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected Float, received \"3\""
            )))
        );
    }
}
