// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, num::NonZeroUsize};

use crate::{
    env::Env,
    expression::{
        with_dependencies, AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{BoundNode, CoreNode, ErrorNode, ValueNode},
        sequence::SequenceNode,
        Evaluate1, Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ConsNode {
    head: Expression<Node>,
    tail: Expression<Node>,
    static_list_length: Option<NonZeroUsize>,
}
impl ConsNode {
    pub fn new(head: Expression<Node>, tail: Expression<Node>) -> Self {
        let static_list_length = match tail.value() {
            Node::Core(CoreNode::Value(ValueNode::Nil)) => NonZeroUsize::new(1),
            Node::Sequence(SequenceNode::Cons(ConsNode {
                static_list_length: Some(tail_length),
                ..
            })) => NonZeroUsize::new(tail_length.get() + 1),
            _ => None,
        };
        ConsNode {
            head,
            tail,
            static_list_length,
        }
    }
    pub fn head(&self) -> &Expression<Node> {
        &self.head
    }
    pub fn tail(&self) -> &Expression<Node> {
        &self.tail
    }
    pub fn is_static_list(&self) -> bool {
        self.static_list_length.is_some()
    }
    pub fn static_list_length(&self) -> Option<usize> {
        self.static_list_length.map(|value| value.get())
    }
    fn iter(&self) -> ConsIterator {
        match self.static_list_length {
            Some(length) => ConsIterator {
                current: ConsIteratorItem::Some(self, None),
                length: length.get(),
            },
            None => panic!("Target sequence is not a static list"),
        }
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
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        if self.head.capture_depth() == 0 && self.tail.capture_depth() == 0 {
            return None;
        }
        Some(EvaluationResult::new(
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode {
                head: BoundNode::bind(&self.head, env),
                tail: BoundNode::bind(&self.tail, env),
                static_list_length: self.static_list_length,
            }))),
            None,
        ))
    }
}
impl fmt::Display for ConsNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

pub struct ConsIterator<'a> {
    current: ConsIteratorItem<'a>,
    length: usize,
}
enum ConsIteratorItem<'a> {
    Some(&'a ConsNode, Option<&'a Env<Node>>),
    None,
}
impl<'a> ExactSizeIterator for ConsIterator<'a> {
    fn len(&self) -> usize {
        self.length
    }
}
impl<'a> Iterator for ConsIterator<'a> {
    type Item = Expression<Node>;
    fn next(&mut self) -> Option<Expression<Node>> {
        match self.current {
            ConsIteratorItem::Some(ConsNode { head, tail, .. }, env) => {
                self.current = get_next_iterator_item(tail, env);
                Some(match env {
                    Some(env) => BoundNode::bind(head, env),
                    None => Expression::clone(head),
                })
            }
            ConsIteratorItem::None => None,
        }
    }
}
fn get_next_iterator_item<'a>(
    tail: &'a Expression<Node>,
    env: Option<&'a Env<Node>>,
) -> ConsIteratorItem<'a> {
    match tail.value() {
        Node::Sequence(SequenceNode::Cons(next)) => ConsIteratorItem::Some(next, env),
        Node::Core(CoreNode::Bound(bound)) => match bound.target().value() {
            Node::Sequence(SequenceNode::Cons(next)) => {
                ConsIteratorItem::Some(next, Some(bound.env()))
            }
            _ => ConsIteratorItem::None,
        },
        _ => ConsIteratorItem::None,
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
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IsPairNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> Expression<Node> {
        Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(
            match target.value() {
                Node::Sequence(SequenceNode::Cons(_)) => true,
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
        Node::Sequence(SequenceNode::Cons(node)) => {
            if node.is_static_list() {
                results.extend(node.iter());
                EvaluationResult::new(combine(results), target.dependencies)
            } else {
                results.push(Expression::clone(node.head()));
                with_dependencies(
                    target.dependencies,
                    collect_list_items_iter(node.tail(), env, combine, results),
                )
            }
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
            core::{CoreNode, ErrorNode, ValueNode},
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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
            )))),
        );
        let expression = parser::parse("(cons (+ 1 2) (+ 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
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
    }

    #[test]
    fn cons_expression_scope() {
        let env = Env::new();
        let expression = parser::parse("(car ((lambda (foo bar) (cons foo bar)) 3 null))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
        let expression = parser::parse("(cdr ((lambda (foo bar) (cons foo bar)) 3 null))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
        );
        let expression = parser::parse("(car ((lambda (first) (cons first ((lambda (second) (cons second ((lambda (third) (cons third null)) 5))) 4))) 3))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
        let expression = parser::parse("(car (cdr ((lambda (first) (cons first ((lambda (second) (cons second ((lambda (third) (cons third null)) 5))) 4))) 3)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
        );
        let expression = parser::parse("(car (cdr (cdr ((lambda (first) (cons first ((lambda (second) (cons second ((lambda (third) (cons third null)) 5))) 4))) 3))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
        );
    }

    #[test]
    fn lazy_evaluation() {
        let env = Env::new();
        let expression = parser::parse("(cons (error \"foo\") (error \"bar\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo")))),
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("bar")))),
            ))))
        )
    }

    #[test]
    fn is_pair_expressions() {
        let env = Env::new();
        let expression = parser::parse("(pair? (cons 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (cons 3 (cons 4 5)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (cons (cons 1 2) (cons 3 4)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list 3))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? (list 3 4 5))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let env = Env::new();
        let expression = parser::parse("(pair? ((lambda (foo) foo) (cons 3 4)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(pair? null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? true)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? \"\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pair? -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }

    #[test]
    fn cons_iterator() {
        let expression = ConsNode::new(
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
            Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                Expression::new(Node::Sequence(SequenceNode::Cons(ConsNode::new(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Nil))),
                )))),
            )))),
        );
        let mut iterator = expression.iter();
        assert_eq!(iterator.len(), 3);
        assert_eq!(
            iterator.next(),
            Some(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3)
            ))))
        );
        assert_eq!(
            iterator.next(),
            Some(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(4)
            ))))
        );
        assert_eq!(
            iterator.next(),
            Some(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(5)
            ))))
        );
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn cons_iterator_scope() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo) (cons foo null)) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        if let Node::Sequence(SequenceNode::Cons(node)) = result.value() {
            let mut iterator = node.iter();
            assert_eq!(
                iterator.next().unwrap().evaluate(&env).expression,
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
            );
            assert_eq!(iterator.next(), None);
        } else {
            panic!()
        }
        let expression = parser::parse("((lambda (first) ((lambda (second) ((lambda (third) (cons first (cons second (cons third null)))) 5)) 4)) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        if let Node::Sequence(SequenceNode::Cons(node)) = result.value() {
            let mut iterator = node.iter();
            assert_eq!(iterator.len(), 3);
            let first = iterator.next();
            let second = iterator.next();
            let third = iterator.next();
            assert_eq!(
                first.unwrap().evaluate(&env).expression,
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
            );
            assert_eq!(
                second.unwrap().evaluate(&env).expression,
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
            );
            assert_eq!(
                third.unwrap().evaluate(&env).expression,
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
            );
            assert_eq!(iterator.next(), None);
        } else {
            panic!()
        }
    }
}
