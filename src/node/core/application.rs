// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{with_dependencies, EvaluationResult, Expression, NodeType},
    node::{
        core::{CoreNode, ErrorNode},
        Node,
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
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let target = self.target.evaluate(env);
        match target.expression.value() {
            Node::Core(CoreNode::Function(node)) => {
                let arity = node.arity();
                let body = node.body();
                Some(with_dependencies(
                    target.dependencies,
                    apply_function(arity, body, &self.args, env, None),
                ))
            }
            Node::Core(CoreNode::BoundFunction(node)) => {
                let arity = node.arity();
                let body = node.body();
                let captured_env = node.env();
                Some(with_dependencies(
                    target.dependencies,
                    apply_function(arity, body, &self.args, env, Some(captured_env)),
                ))
            }
            _ => Some(EvaluationResult::new(
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                    "Target expression is not a function: {}",
                    target.expression
                ))))),
                target.dependencies,
            )),
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
    arg_env: &Env<Node>,
    captured_env: Option<&Env<Node>>,
) -> EvaluationResult<Node> {
    if args.len() != arity {
        return EvaluationResult::new(
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                "Expected {} arguments, received {}",
                arity,
                args.len()
            ))))),
            None,
        );
    }
    if arity == 0 {
        return match captured_env {
            Some(captured_env) => body.evaluate(captured_env),
            None => body.evaluate(&Env::new()),
        };
    }
    let args = args.iter().map(|arg| match arg.capture_depth() {
        0 => Expression::clone(arg),
        depth => match arg.value() {
            Node::Core(CoreNode::Reference(node)) => Expression::clone(arg_env.get(node.offset())),
            _ => Expression::new(Node::Core(CoreNode::BoundArgument(BoundArgumentNode {
                target: Expression::clone(arg),
                env: arg_env.capture(depth),
            }))),
        },
    });
    let inner_env = match captured_env {
        Some(captured_env) => captured_env.extend(args),
        None => Env::from(args),
    };
    body.evaluate(&inner_env)
}

#[derive(PartialEq, Clone)]
pub struct BoundArgumentNode {
    target: Expression<Node>,
    env: Env<Node>,
}
impl NodeType<Node> for BoundArgumentNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Some(self.target.evaluate(&self.env))
    }
    fn capture_depth(&self) -> usize {
        0
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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn unary_functions() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo) (add foo 4)) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn binary_functions() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo bar) (add foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&env).expression;
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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4 + 5))))
        );
    }

    #[test]
    fn argument_scope() {
        let env = Env::new();
        let expression = parser::parse(
            "((lambda (first second third) ((lambda (foo bar) (add foo bar)) second third)) 3 4 5)",
        )
        .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4 + 5)))),
        );
        let env = Env::new();
        let expression = parser::parse(
            "((lambda (first second third) ((lambda (one two) ((lambda (foo bar) (add foo bar)) one two)) first third)) 3 4 5)",
        ).unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 5)))),
        );
    }

    #[test]
    fn immediately_invoked_closures() {
        let env = Env::new();
        let expression =
            parser::parse("((lambda (three four) ((lambda (one two) (add one three)) 1 2)) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(1 + 3))))
        );
        let expression =
            parser::parse("(((lambda (one two) (lambda (three four) (add one three))) 1 2) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env).expression;
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
        let result = expression.evaluate(&env).expression;
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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(
                (1..=100).fold(0, |acc, i| i + acc)
            ))))
        );
    }
}
