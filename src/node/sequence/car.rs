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
    node::{evaluate::EvaluateResult, sequence::SequenceNode, Evaluate1, Node},
};

#[derive(Debug, PartialEq, Clone)]
pub struct CarNode {
    target: Expression<Node>,
}
impl CarNode {
    pub fn new(target: Expression<Node>) -> Self {
        CarNode { target }
    }
}
impl AstNode<Node> for CarNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for CarNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for CarNode {
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
impl Evaluate1 for CarNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        match target.value() {
            Node::Sequence(SequenceNode::Cons(node)) => Ok(Expression::clone(node.head())),
            _ => Err(format!("Expected pair, received {}", target)),
        }
    }
}
impl fmt::Display for CarNode {
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
    fn car_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(car (cons 3 4))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            )),))
        );
        let expression = parser::parse("(car (cons (+ 1 2) (+ 3 4)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(1 + 2)
            )),))
        );
        let expression =
            parser::parse("(car ((lambda (foo) foo) (cons (+ 1 2) (+ 3 4))))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(1 + 2)
            )),))
        );
        let expression = parser::parse("(car (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            )),))
        );
    }

    #[test]
    fn lazy_evaluation() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(car (cons (+ 3 4) (error \"foo\")))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            ))))
        );
    }

    #[test]
    fn invalid_car_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(car 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected pair, received 3"))),
        );
        let expression = parser::parse("(car (list))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected pair, received null"))),
        );
    }
}
