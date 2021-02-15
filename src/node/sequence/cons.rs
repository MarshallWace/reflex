// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{CoreNode, ValueNode},
        sequence::SequenceNode,
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ConsNode {
    head: Expression<Node>,
    tail: Expression<Node>,
}
impl ConsNode {
    pub fn new(head: Expression<Node>, tail: Expression<Node>) -> Self {
        ConsNode { head, tail }
    }
    pub fn head(&self) -> &Expression<Node> {
        &self.head
    }
    pub fn tail(&self) -> &Expression<Node> {
        &self.tail
    }
}
impl AstNode<Node> for ConsNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let head = args.next().unwrap();
        let tail = args.next().unwrap();
        Ok(Self::new(head, tail))
    }
}
impl NodeType<Node> for ConsNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.head, &self.tail]
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
    }
}
impl fmt::Display for ConsNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsPairNode {
    target: Expression<Node>,
}
impl IsPairNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsPairNode { target }
    }
    pub fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl NodeType<Node> for IsPairNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IsPairNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, _env: &Env<Node>, target: &Expression<Node>) -> Expression<Node> {
        Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(
            match target.value() {
                Node::Sequence(SequenceNode::Cons(_)) => true,
                Node::Sequence(SequenceNode::List(_)) => true,
                _ => false,
            },
        ))))
    }
}
impl fmt::Display for IsPairNode {
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
            arithmetic::{AddNode, ArithmeticNode},
            core::{CoreNode, ValueNode},
            parser,
            sequence::SequenceNode,
            Node,
        },
    };

    use super::ConsNode;

    #[test]
    fn cons_expressions() {
        let env = Env::new();
        let expression = parser::parse("(cons 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
            )))),
        );
        let expression = parser::parse("(cons (add 1 2) (add 3 4))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )))),
            )))),
        );
        let expression = parser::parse("(cons 3 (cons 4 (cons 5 null)))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                    )))),
                )))),
            )))),
        );
    }

    #[test]
    fn is_pair_expressions() {
        let env = Env::new();
        let expression = parser::parse("(pair? (cons 3 4))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (cons 3 (cons 4 5)))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (cons (cons 1 2) (cons 3 4)))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list 3))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? ((lambda (foo) foo) (cons 3 4)))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(pair? null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? \"\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? \"foo\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }
}
