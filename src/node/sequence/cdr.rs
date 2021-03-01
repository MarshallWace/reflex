// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ErrorNode},
        sequence::SequenceNode,
        Evaluate1, Node,
    },
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
impl NodeType<Node> for CdrNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for CdrNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, _env: &Env<Node>, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Sequence(SequenceNode::Cons(node)) => Expression::clone(node.tail()),
            _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected pair, received {}",
                target
            ))))),
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
        expression::Expression,
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser,
            sequence::{ConsNode, SequenceNode},
            Node,
        },
    };

    #[test]
    fn cdr_expressions() {
        let env = Env::new();
        let expression = parser::parse("(cdr (cons 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
        );
        let expression = parser::parse("(cdr (cons (add 1 2) (add 3 4)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse("(cdr (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                )))),
            )))),
        );
        let expression =
            parser::parse("(cdr ((lambda (foo) foo) (cons (add 1 2) (add 3 4))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
    }

    #[test]
    fn invalid_cdr_expressions() {
        let env = Env::new();
        let expression = parser::parse("(cdr 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected pair, received 3"
            )))),
        );
    }
}
