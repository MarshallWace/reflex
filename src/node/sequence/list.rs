// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        with_dependencies, AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{CoreNode, ErrorNode, ValueNode},
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
impl NodeType<Node> for ListNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.items.iter().collect()
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate0::evaluate(self, env)
    }
}
impl Evaluate0 for ListNode {
    fn run(&self, _env: &Env<Node>) -> Expression<Node> {
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
impl NodeType<Node> for IsListNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IsListNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, _env: &Env, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Core(CoreNode::Value(ValueNode::Nil)) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            Node::Sequence(SequenceNode::Cons(node)) if node.is_list().unwrap_or(false) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            Node::Sequence(SequenceNode::Cons(node)) if node.is_list().is_none() => {
                Expression::new(Node::Sequence(SequenceNode::IsList(IsListNode::new(
                    Expression::clone(node.tail()),
                ))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        }
    }
}
impl fmt::Display for IsListNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

pub fn collect_list_items(
    target: &Expression<Node>,
    env: &Env<Node>,
    combine: impl Fn(Vec<Expression<Node>>) -> Expression<Node>,
) -> EvaluationResult<Node> {
    collect_list_items_iter(target, env, combine, Vec::new())
}

fn collect_list_items_iter(
    target: &Expression<Node>,
    env: &Env<Node>,
    combine: impl Fn(Vec<Expression<Node>>) -> Expression<Node>,
    mut results: Vec<Expression<Node>>,
) -> EvaluationResult<Node> {
    let target = target.evaluate(env);
    match target.expression.value() {
        Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => target,
        Node::Core(CoreNode::Value(ValueNode::Nil)) => {
            EvaluationResult::new(combine(results), target.dependencies)
        }
        Node::Sequence(SequenceNode::Cons(node)) if node.is_list().unwrap_or(false) => {
            results.extend(node.iter());
            EvaluationResult::new(combine(results), target.dependencies)
        }
        Node::Sequence(SequenceNode::Cons(node)) if node.is_list().is_none() => {
            results.push(Expression::clone(node.head()));
            with_dependencies(
                target.dependencies,
                collect_list_items_iter(node.tail(), env, combine, results),
            )
        }
        _ => EvaluationResult::new(
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected list, received {}",
                target.expression
            ))))),
            target.dependencies,
        ),
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

    use super::collect_list_items;

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
        let expression = parser::parse("(list (add 1 2) (add 3 4) (add 5 6))").unwrap();
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
        let expression = parser::parse("(list? false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(list? true)").unwrap();
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

    #[test]
    fn collect_static_lists() {
        let env = Env::new();
        let sum = |items: Vec<Expression<Node>>| {
            items.into_iter().fold(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
                |acc, item| {
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        acc, item,
                    ))))
                },
            )
        };
        let result = collect_list_items(&parser::parse("null").unwrap(), &env, sum);
        assert_eq!(
            result.expression,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0))))
        );
        let result = collect_list_items(&parser::parse("(list)").unwrap(), &env, sum);
        assert_eq!(
            result.expression,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0))))
        );
        let result = collect_list_items(&parser::parse("(list 3 4 5)").unwrap(), &env, sum);
        assert_eq!(
            result.expression,
            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    )))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                )))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
            ))))
        );
        let result = collect_list_items(
            &parser::parse("(list (add 1 2) (add 3 4) (add 5 6))").unwrap(),
            &env,
            sum,
        );
        assert_eq!(
            result.expression,
            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                        )))),
                    )))),
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    )))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                )))),
            ))))
        );
    }

    #[test]
    fn collect_dynamic_lists() {
        let env = Env::new();
        let sum = |items: Vec<Expression<Node>>| {
            items.into_iter().fold(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
                |acc, item| {
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        acc, item,
                    ))))
                },
            )
        };
        let result = collect_list_items(
            &parser::parse(
                "(cons (add 1 2) (cons (add 3 4) (cons (add 5 6) ((lambda (foo) foo) (cons (add 7 8) null)))))",
            )
            .unwrap(),
            &env,
            sum,
        );
        assert_eq!(
            result.expression,
            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1)))),
                                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(2)))),
                            )))),
                        )))),
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                        )))),
                    )))),
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(6)))),
                    )))),
                )))),
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(7)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(8)))),
                )))),
            )))),
        );
    }
}
