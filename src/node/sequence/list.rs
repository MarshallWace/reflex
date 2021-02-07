// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
        sequence::SequenceNode,
        Node,
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
    pub fn factory(args: &Vec<Expression<Node>>) -> Result<Self, String> {
        let args = &mut args.iter().map(Expression::clone);
        let items = args.collect::<Vec<_>>();
        Ok(ListNode::new(items))
    }
    pub fn collect(target: Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Sequence(SequenceNode::List(_)) | Node::Core(CoreNode::Value(ValueNode::Nil)) => {
                target
            }
            _ => Expression::new(Node::Sequence(SequenceNode::Collect(CollectNode::new(
                target,
            )))),
        }
    }
    pub fn items(&self) -> &Vec<Expression<Node>> {
        &self.items
    }
}
impl NodeType<Node> for ListNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.items.iter().collect()
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        if self.items().is_empty() {
            Some(Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))))
        } else {
            None
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
    pub fn factory(args: &Vec<Expression<Node>>) -> Result<Self, String> {
        let args = &mut args.iter().map(Expression::clone);
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let target = args.next().unwrap();
        Ok(IsListNode::new(target))
    }
}
impl NodeType<Node> for IsListNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Some(is_list(&self.target, env))
    }
}
impl fmt::Display for IsListNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CollectNode {
    target: Expression<Node>,
}
impl CollectNode {
    fn new(target: Expression<Node>) -> Self {
        CollectNode { target }
    }
}
impl NodeType<Node> for CollectNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Some(collect_list_items(&self.target, env, Vec::new()))
    }
}
impl fmt::Display for CollectNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

fn is_list(target: &Expression<Node>, env: &Env<Node>) -> Expression<Node> {
    let target = target.evaluate(env);
    match target.value() {
        Node::Core(CoreNode::Pending(_)) | Node::Core(CoreNode::Error(_)) => target,
        Node::Core(CoreNode::Value(ValueNode::Nil)) | Node::Sequence(SequenceNode::List(_)) => {
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        }
        Node::Sequence(SequenceNode::Cons(node)) => is_list(node.tail(), env),
        _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
    }
}

fn collect_list_items(
    target: &Expression<Node>,
    env: &Env<Node>,
    mut results: Vec<Expression<Node>>,
) -> Expression<Node> {
    let target = target.evaluate(env);
    match target.value() {
        Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => target,
        Node::Sequence(SequenceNode::List(_)) => target,
        Node::Core(CoreNode::Value(ValueNode::Nil)) => Expression::new(if results.is_empty() {
            Node::Core(CoreNode::Value(ValueNode::Nil))
        } else {
            Node::Sequence(SequenceNode::List(ListNode::new(results)))
        }),
        Node::Sequence(SequenceNode::Cons(node)) => {
            results.push(Expression::clone(node.head()));
            collect_list_items(node.tail(), env, results)
        }
        _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
            "Expected list, received {}",
            target
        ))))),
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
            sequence::SequenceNode,
            Node,
        },
    };

    use super::ListNode;

    #[test]
    fn list_expressions() {
        let env = Env::new();
        let expression = Node::parse("(list 3 4 5)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(vec![
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
            ])))),
        );
        let expression = Node::parse("(list (add 1 2) (add 3 4) (add 5 6))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(vec![
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                )))),
            ])))),
        );
    }

    #[test]
    fn empty_list_expressions() {
        let env = Env::new();
        let expression = Node::parse("(list)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
        );
    }

    #[test]
    fn is_list() {
        let env = Env::new();
        let expression = Node::parse("(list? (list))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = Node::parse("(list? (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = Node::parse("(list? null)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = Node::parse("(list? (cons 3 null))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = Node::parse("(list? (cons 3 (cons 4 (cons 5 null))))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression =
            Node::parse("(list? (cons 3 (cons 4 (cons 5 ((lambda (foo) foo) null)))))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression =
            Node::parse("(list? (cons 3 (cons 4 (cons 5 ((lambda (foo) foo) 6)))))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? (cons 1 2))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? false)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? true)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? \"\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? \"foo\")").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? 0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? -3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? 0.0)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? 3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = Node::parse("(list? -3.142)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }

    #[test]
    fn collect_static_lists() {
        let env = Env::new();
        let expression = ListNode::collect(Node::parse("null").unwrap());
        assert_eq!(
            expression,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil)))
        );
        let expression = ListNode::collect(Node::parse("(list)").unwrap());
        assert_eq!(
            expression,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(Vec::new()))))
        );
        let expression = ListNode::collect(Node::parse("(list 3 4 5)").unwrap());
        assert_eq!(
            expression,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(vec![
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
            ])))),
        );
        let expression =
            ListNode::collect(Node::parse("(list (add 1 2) (add 3 4) (add 5 6))").unwrap());
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(vec![
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                )))),
            ])))),
        );
    }

    #[test]
    fn collect_dynamic_cons_lists() {
        let env = Env::new();
        let expression = ListNode::collect(
            Node::parse(
                "(cons (add 1 2) (cons (add 3 4) (cons (add 5 6) ((lambda (foo) foo) null))))",
            )
            .unwrap(),
        );
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::List(ListNode::new(vec![
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                )))),
            ])))),
        );
    }
}
