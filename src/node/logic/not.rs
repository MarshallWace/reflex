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
pub struct NotNode {
    target: Expression<Node>,
}
impl NotNode {
    pub fn new(target: Expression<Node>) -> Self {
        NotNode { target }
    }
}
impl AstNode<Node> for NotNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for NotNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for NotNode {
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
impl Evaluate1 for NotNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Boolean(target))) => Ok(Expression::new(
                Node::Core(CoreNode::Value(ValueNode::Boolean(!*target))),
            )),
            target => Err(format!("Expected Boolean, received {}", target,)),
        }
    }
}
impl fmt::Display for NotNode {
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
    fn not_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(not #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(not #f)").unwrap();
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
        let expression = parser::parse("(not null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected Boolean, received null"
            )))
        );
        let expression = parser::parse("(not 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected Boolean, received 0")))
        );
        let expression = parser::parse("(not 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected Boolean, received 0.0"
            )))
        );
        let expression = parser::parse("(not \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from(
                "Expected Boolean, received \"\""
            )))
        );
    }
}
