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
pub struct OrNode {
    left: Expression<Node>,
    right: Expression<Node>,
}
impl OrNode {
    pub fn new(left: Expression<Node>, right: Expression<Node>) -> Self {
        OrNode { left, right }
    }
}
impl AstNode<Node> for OrNode {
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
impl<'a> CompoundNode<'a, Node> for OrNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.left).chain(once(&self.right))
    }
}
impl NodeType<Node> for OrNode {
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
impl Evaluate2 for OrNode {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>) {
        (&self.left, &self.right)
    }
    fn run(&self, left: &Expression<Node>, right: &Expression<Node>) -> EvaluateResult {
        match (left.value(), right.value()) {
            (
                Node::Core(CoreNode::Value(ValueNode::Boolean(left))),
                Node::Core(CoreNode::Value(ValueNode::Boolean(right))),
            ) => Ok(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(*left && *right),
            )))),
            (left, right) => Err(format!(
                "Expected (Boolean, Boolean), received ({}, {})",
                left, right,
            )),
        }
    }
}
impl fmt::Display for OrNode {
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
    fn or_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(or #f #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(or #f #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(or #t #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(or #t #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
    }

    #[test]
    fn invalid_or_expression_arguments() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(or #t null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (#t, null)"
            )))
        );
        let expression = parser::parse("(or #t 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (#t, 0)"
            )))
        );
        let expression = parser::parse("(or #t 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (#t, 0.0)"
            )))
        );
        let expression = parser::parse("(or #t \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (#t, \"\")"
            )))
        );

        let expression = parser::parse("(or null #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (null, #t)"
            )))
        );
        let expression = parser::parse("(or 0 #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (0, #t)"
            )))
        );
        let expression = parser::parse("(or 0.0 #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (0.0, #t)"
            )))
        );
        let expression = parser::parse("(or \"\" #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected (Boolean, Boolean), received (\"\", #t)"
            )))
        );
    }
}
