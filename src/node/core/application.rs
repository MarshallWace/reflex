// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once, rc::Rc};

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::{
        core::{CoreNode, ErrorNode},
        Evaluate1, Node,
    },
};

#[derive(PartialEq, Clone)]
pub struct ApplicationNode {
    target: Expression<Node>,
    args: Vec<Expression<Node>>,
}
impl ApplicationNode {
    pub fn new(target: Expression<Node>, args: Vec<Expression<Node>>) -> Self {
        ApplicationNode { target, args }
    }
}
impl NodeType<Node> for ApplicationNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        once(&self.target).chain(self.args.iter()).collect()
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for ApplicationNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, env: &Env<Node>, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Core(CoreNode::Function(node)) => {
                let arity = node.arity();
                let body = node.body();
                apply_function(arity, body, &self.args, env, None)
            }
            Node::Core(CoreNode::BoundFunction(node)) => {
                let function = node.function();
                let arity = function.arity();
                let body = function.body();
                let captured_env = node.env();
                apply_function(arity, body, &self.args, env, Some(captured_env))
            }
            _ => Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Target expression is not a function: {}",
                target
            ))))),
        }
    }
}
impl fmt::Display for ApplicationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<application:{}:{}>",
            self.target,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
impl fmt::Debug for ApplicationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

fn apply_function(
    arity: usize,
    body: &Expression<Node>,
    args: &Vec<Expression<Node>>,
    env: &Env<Node>,
    captured_env: Option<&Env<Node>>,
) -> Expression<Node> {
    if args.len() != arity {
        return Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
            "Expected {} arguments, received {}",
            arity,
            args.len()
        )))));
    }
    if arity == 0 {
        return body.evaluate(match captured_env {
            Some(captured_env) => captured_env,
            None => env,
        });
    }
    let inner_env = match captured_env {
        Some(captured_env) => {
            let arg_env = Rc::new(env.capture());
            captured_env.extend(args.iter().map(|arg| {
                Expression::new(Node::Core(CoreNode::BoundArgument(BoundArgumentNode {
                    target: Expression::clone(arg),
                    env: Rc::clone(&arg_env),
                })))
            }))
        }
        None => env.extend(args.iter().map(Expression::clone)),
    };
    body.evaluate(&inner_env)
}

#[derive(PartialEq, Clone)]
pub struct BoundArgumentNode {
    target: Expression<Node>,
    env: Rc<Env<Node>>,
}
impl NodeType<Node> for BoundArgumentNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.target.value().expressions()
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        Some(self.target.evaluate(&self.env))
    }
}
impl fmt::Display for BoundArgumentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound:{}>", self.target)
    }
}
impl fmt::Debug for BoundArgumentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            arithmetic::{AddNode, ArithmeticNode},
            core::{CoreNode, FunctionNode, ReferenceNode, ValueNode},
            parser, Node,
        },
    };

    use super::ApplicationNode;

    #[test]
    fn nullary_functions() {
        let env = Env::new();
        let expression = parser::parse("((lambda () (add 3 4)))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn unary_functions() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo) (add foo 4)) 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn binary_functions() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo bar) (add foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn ternary_functions() {
        let env = Env::new();
        let expression =
            parser::parse("((lambda (foo bar baz) (add foo (add bar baz))) 3 4 5)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4 + 5))))
        );
    }

    #[test]
    fn immediately_invoked_closures() {
        let env = Env::new();
        let expression =
            parser::parse("((lambda (three four) ((lambda (one two) (add one three)) 1 2)) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1 + 3))))
        );
        let expression =
            parser::parse("(((lambda (one two) (lambda (three four) (add one three))) 1 2) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1 + 3))))
        );
    }

    #[test]
    fn deferred_closures() {
        let env = Env::new();
        let expression = parser::parse(
            "((lambda (transform value) (transform (value))) ((lambda (constant) (lambda (value) (add value constant))) 4) ((lambda (value) (lambda () value)) 3))",
        )
        .unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn deeply_nested_functions() {
        let env = Env::new();
        let expression = (1..=100).fold(
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(0)))),
            |acc, i| {
                Expression::new(Node::Core(CoreNode::Application(ApplicationNode::new(
                    Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                        1,
                        Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                            Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                            acc,
                        )))),
                    )))),
                    vec![Expression::new(Node::Core(CoreNode::Value(
                        ValueNode::Int(i),
                    )))],
                ))))
            },
        );
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (1..=100).fold(0, |acc, i| i + acc)
            ))))
        );
    }
}
