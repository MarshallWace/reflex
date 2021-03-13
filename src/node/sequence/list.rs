// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{CoreNode, ValueNode},
        evaluate::Evaluate0,
        sequence::{ConsNode, SequenceNode},
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ListNode {
    items: Vec<Expression<Node>>,
}
impl ListNode {
    pub fn new(items: Vec<Expression<Node>>) -> Self {
        ListNode { items }
    }
    pub fn items(&self) -> &Vec<Expression<Node>> {
        &self.items
    }
}
impl AstNode<Node> for ListNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        let args = &mut args.iter().map(Expression::clone);
        let items = args.collect::<Vec<_>>();
        Ok(Self::new(items))
    }
}
impl<'a> CompoundNode<'a> for ListNode {
    type Expressions = std::slice::Iter<'a, Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        self.items.iter()
    }
}
impl NodeType<Node> for ListNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate0::evaluate(self, env)
    }
}
impl Evaluate0 for ListNode {
    fn run(&self) -> Expression<Node> {
        if self.items.is_empty() {
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
        } else {
            self.items.iter().rev().fold(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                |tail, head| {
                    Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                        Expression::clone(head),
                        tail,
                    ))))
                },
            )
        }
    }
}
impl fmt::Display for ListNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsListNode {
    target: Expression<Node>,
}
impl IsListNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsListNode { target }
    }
    pub fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        let args = &mut args.iter().map(Expression::clone);
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a> for IsListNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsListNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IsListNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> Expression<Node> {
        Expression::new(match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Nil)) => {
                Node::Core(CoreNode::Value(ValueNode::Boolean(true)))
            }
            Node::Sequence(SequenceNode::Cons(node)) => {
                if node.is_static_list() {
                    Node::Core(CoreNode::Value(ValueNode::Boolean(true)))
                } else {
                    Node::Sequence(SequenceNode::IsList(IsListNode::new(Expression::clone(
                        node.tail(),
                    ))))
                }
            }
            _ => Node::Core(CoreNode::Value(ValueNode::Boolean(false))),
        })
    }
}
impl fmt::Display for IsListNode {
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
            sequence::{ConsNode, SequenceNode},
            Node,
        },
    };

    #[test]
    fn list_expressions() {
        let env = Env::new();
        let expression = parser::parse("(list 3 4 5)").unwrap();
        let result = expression.evaluate(&env).expression;
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
        let expression = parser::parse("(list (+ 1 2) (+ 3 4) (+ 5 6))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                )))),
                Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    )))),
                    Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                        )))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                    )))),
                )))),
            )))),
        );
    }

    #[test]
    fn empty_list_expressions() {
        let env = Env::new();
        let expression = parser::parse("(list)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
        );
    }

    #[test]
    fn is_list_expressions() {
        let env = Env::new();
        let expression = parser::parse("(list? (list))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(list? (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(list? null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(list? (cons 3 null))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(list? (cons 3 (cons 4 (cons 5 null))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression =
            parser::parse("(list? (cons 3 (cons 4 (cons 5 ((lambda (foo) foo) null)))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse(
    "(list? ((lambda (first) ((lambda (second) ((lambda (third) (cons first (cons second (cons third null)))) 5)) 4)) 3))"
        ).unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression =
            parser::parse("(list? (cons 3 (cons 4 (cons 5 ((lambda (foo) foo) 6)))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? (cons 1 2))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? #t)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? \"\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }
}
