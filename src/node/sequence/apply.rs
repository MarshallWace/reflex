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
        core::{CoreNode, FunctionApplicationNode},
        sequence::cons::collect_list_items,
        Node,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct ApplyNode {
    target: Expression<Node>,
    args: Expression<Node>,
}
impl ApplyNode {
    pub fn new(target: Expression<Node>, args: Expression<Node>) -> Self {
        ApplyNode { target, args }
    }
}
impl AstNode<Node> for ApplyNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
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
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let result = collect_list_items(&self.args, env, |args| {
            Expression::new(Node::Core(CoreNode::FunctionApplication(
                FunctionApplicationNode::new(Expression::clone(&self.target), args),
            )))
        });
        Some(with_dependencies(
            result.dependencies,
            result.expression.evaluate(env),
        ))
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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        );
        let expression =
            parser::parse("(apply (lambda (foo bar) (+ foo bar)) (list 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse("(apply (lambda (first second third) (+ (abs first) (+ second third))) (list -3 4 5))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (-3 as i32).abs() + (4 + 5)
            )))),
        );
    }

    #[test]
    fn dynamic_argument_lists() {
        let env = Env::new();
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) (cons 3 ((lambda () (cons 4 null)))))",
        )
        .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) (cons 3 ((lambda (foo) (cons 4 foo)) null)))",
        )
        .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse(
            "(apply (lambda (foo bar) (+ foo bar)) ((lambda (foo) foo) (cons ((lambda (first) first) 3) ((lambda (third) (cons ((lambda (second) second) 4) third)) null))))",
        )
        .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4)))),
        );
        let expression = parser::parse(
    "(apply (lambda (first second third) (+ first (+ second third))) ((lambda (first) ((lambda (second) ((lambda (third) (cons first (cons second (cons third null)))) 5)) 4)) 3))"
        ).unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4 + 5)))),
        )
    }

    #[test]
    fn invalid_apply_expressions() {
        let env = Env::new();
        let expression = parser::parse("(apply 3 (list))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Target expression is not a function: 3"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected list, received 3"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected 1 arguments, received 0"
            ))))
        );
        let expression = parser::parse("(apply (lambda (foo) foo) (list 3 4))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(
                "Expected 1 arguments, received 2"
            ))))
        );
    }
}
