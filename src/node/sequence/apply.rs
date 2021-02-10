// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{ApplicationNode, CoreNode, ErrorNode, ValueNode},
        sequence::{ListNode, SequenceNode},
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ApplyNode {
    target: Expression<Node>,
    args: Expression<Node>,
}
impl ApplyNode {
    pub fn new(target: Expression<Node>, args: Expression<Node>) -> Self {
        ApplyNode {
            target,
            args: ListNode::collect(args),
        }
    }
}
impl AstNode<Node> for ApplyNode {
    fn factory(args: &Vec<Expression<Node>>) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        let args = args.next().unwrap();
        Ok(Self::new(target, args))
    }
}
impl NodeType<Node> for ApplyNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target, &self.args]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for ApplyNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.args
    }
    fn run(&self, _env: &Env<Node>, args: &Expression<Node>) -> Expression<Node> {
        match args.value() {
            Node::Core(CoreNode::Value(ValueNode::Nil)) => {
                Expression::new(Node::Core(CoreNode::Application(ApplicationNode::new(
                    Expression::clone(&self.target),
                    Vec::new(),
                ))))
            }
            Node::Sequence(SequenceNode::List(args)) => {
                Expression::new(Node::Core(CoreNode::Application(ApplicationNode::new(
                    Expression::clone(&self.target),
                    args.items().iter().map(Expression::clone).collect(),
                ))))
            }
            _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected list, received {}",
                args,
            ))))),
        }
    }
}
impl fmt::Display for ApplyNode {
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
    fn apply_expressions() {
        let env = Env::new();
        let expression = parser::parse("(apply (lambda () 3) (list))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
        let expression =
            parser::parse("(apply (lambda (foo bar) (add foo bar)) (list 3 4))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse("(apply (lambda (first second third) (add (abs first) (add second third))) (list -3 4 5))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (-3 as i32).abs() + (4 + 5)
            )))),
        );
    }

    #[test]
    fn invalid_apply_expressions() {
        let env = Env::new();
        let expression = parser::parse("(apply 3 (list))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Target expression is not a function: 3"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected list, received 3"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected 1 arguments, received 0"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list 3 4))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected 1 arguments, received 2"
            ))))
        );
    }
}
