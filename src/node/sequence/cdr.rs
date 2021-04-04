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
pub struct CdrNode {
    target: Expression<Node>,
}
impl CdrNode {
    pub fn new(target: Expression<Node>) -> Self {
        CdrNode { target }
    }
}
impl AstNode<Node> for CdrNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for CdrNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for CdrNode {
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
impl Evaluate1 for CdrNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        match target.value() {
            Node::Sequence(SequenceNode::Cons(node)) => Ok(Expression::clone(node.tail())),
            _ => Err(format!("Expected pair, received {}", target)),
        }
    }
}
impl fmt::Display for CdrNode {
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
            parser,
            sequence::{ConsNode, SequenceNode},
            Node,
        },
        signal::Signal,
    };

    #[test]
    fn cdr_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(cdr (cons 3 4))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(4)
            )),))
        );
        let expression = parser::parse("(cdr (cons (+ 1 2) (+ 3 4)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
        let expression = parser::parse("(cdr (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Sequence(SequenceNode::Cons(
                ConsNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                    )))),
                )
            )))),
        );
        let expression =
            parser::parse("(cdr ((lambda (foo) foo) (cons (+ 1 2) (+ 3 4))))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
    }

    #[test]
    fn invalid_cdr_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(cdr 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("Expected pair, received 3")),),
        );
    }

    #[test]
    fn lazy_evaluation() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(cdr (cons (error \"foo\") (+ 3 4)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            )),))
        );
    }
}
