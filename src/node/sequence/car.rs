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
impl NodeType<Node> for CarNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for CarNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Sequence(SequenceNode::Cons(node)) => Expression::clone(node.head()),
            _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected pair, received {}",
                target
            ))))),
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
        expression::Expression,
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser, Node,
        },
    };

    #[test]
    fn car_expressions() {
        let env = Env::new();
        let expression = parser::parse("(car (cons 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
        let expression = parser::parse("(car (cons (+ 1 2) (+ 3 4)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1 + 2)))),
        );
        let expression =
            parser::parse("(car ((lambda (foo) foo) (cons (+ 1 2) (+ 3 4))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1 + 2)))),
        );
        let expression = parser::parse("(car (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
    }

    #[test]
    fn lazy_evaluation() {
        let env = Env::new();
        let expression = parser::parse("(car (cons (+ 3 4) (error \"foo\")))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
    }

    #[test]
    fn invalid_car_expressions() {
        let env = Env::new();
        let expression = parser::parse("(car 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected pair, received 3"
            )))),
        );
        let expression = parser::parse("(car (list))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected pair, received Nil"
            )))),
        );
    }
}
