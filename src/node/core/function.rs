// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        with_dependencies, AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{BoundNode, CoreNode, ErrorNode, ValueNode},
        Evaluate1, Node,
    },
};

#[derive(PartialEq, Clone)]
pub struct FunctionNode {
    arity: usize,
    body: Expression<Node>,
}
impl FunctionNode {
    pub fn new(arity: usize, body: Expression<Node>) -> Self {
        FunctionNode { arity, body }
    }
    pub fn arity(&self) -> usize {
        self.arity
    }
    pub fn body(&self) -> &Expression<Node> {
        &self.body
    }
}
impl NodeType<Node> for FunctionNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.body]
    }
    fn capture_depth(&self) -> usize {
        let depth = self.body.capture_depth();
        let arity = self.arity;
        if depth <= arity {
            0
        } else {
            depth - arity
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        match self.capture_depth() {
            0 => None,
            depth => Some(EvaluationResult::new(
                Expression::new(Node::Core(CoreNode::BoundFunction(BoundFunctionNode {
                    arity: self.arity,
                    body: Expression::clone(&self.body),
                    env: env.capture(depth),
                }))),
                None,
            )),
        }
    }
}
impl fmt::Display for FunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function:{} -> {}>", self.arity, self.body)
    }
}
impl fmt::Debug for FunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[derive(PartialEq, Clone)]
pub struct BoundFunctionNode {
    arity: usize,
    body: Expression<Node>,
    env: Env<Node>,
}
impl BoundFunctionNode {
    pub fn arity(&self) -> usize {
        self.arity
    }
    pub fn body(&self) -> &Expression<Node> {
        &self.body
    }
    pub fn env(&self) -> &Env<Node> {
        &self.env
    }
}
impl NodeType<Node> for BoundFunctionNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.body]
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        None
    }
}
impl fmt::Display for BoundFunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound:{} -> {}>", self.arity, self.body)
    }
}
impl fmt::Debug for BoundFunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}
#[derive(PartialEq, Clone)]
pub struct FunctionApplicationNode {
    target: Expression<Node>,
    args: Vec<Expression<Node>>,
}
impl FunctionApplicationNode {
    pub fn new(target: Expression<Node>, args: Vec<Expression<Node>>) -> Self {
        FunctionApplicationNode { target, args }
    }
}
impl NodeType<Node> for FunctionApplicationNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        once(&self.target).chain(self.args.iter()).collect()
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let target = self.target.evaluate(env);
        Some(match target.expression.value() {
            Node::Core(CoreNode::Error(_)) | Node::Core(CoreNode::Pending(_)) => target,
            Node::Core(CoreNode::Function(node)) => {
                let arity = node.arity();
                let body = node.body();
                with_dependencies(
                    target.dependencies,
                    apply_function(arity, body, &self.args, env, None),
                )
            }
            Node::Core(CoreNode::BoundFunction(node)) => {
                let arity = node.arity();
                let body = node.body();
                let captured_env = node.env();
                with_dependencies(
                    target.dependencies,
                    apply_function(arity, body, &self.args, env, Some(captured_env)),
                )
            }
            _ => EvaluationResult::new(
                Expression::new(Node::Core(CoreNode::Error(ErrorNode::new(&format!(
                    "Target expression is not a function: {}",
                    target.expression
                ))))),
                target.dependencies,
            ),
        })
    }
}
impl fmt::Display for FunctionApplicationNode {
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
impl fmt::Debug for FunctionApplicationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub fn apply_function(
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
            _ => Expression::new(Node::Core(CoreNode::Bound(BoundNode::new(
                Expression::clone(arg),
                arg_env.capture(depth),
            )))),
        },
    });
    let inner_env = match captured_env {
        Some(captured_env) => captured_env.extend(args),
        None => Env::from(args),
    };
    body.evaluate(&inner_env)
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsFunctionNode {
    target: Expression<Node>,
}
impl IsFunctionNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsFunctionNode { target }
    }
}
impl AstNode<Node> for IsFunctionNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl NodeType<Node> for IsFunctionNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env)
    }
}
impl Evaluate1 for IsFunctionNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> Expression<Node> {
        match target.value() {
            Node::Core(CoreNode::Function(_)) | Node::Core(CoreNode::BoundFunction(_)) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        }
    }
}
impl fmt::Display for IsFunctionNode {
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
            core::{CoreNode, ErrorNode, PendingNode, ReferenceNode, ValueNode},
            parser, Node,
        },
    };

    use super::{BoundFunctionNode, FunctionApplicationNode, FunctionNode};

    #[test]
    fn function_definitions() {
        let env = Env::new();
        let expression = parser::parse("(lambda (foo bar) (add foo bar))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                2,
                Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                ))))
            ))))
        );
    }

    #[test]
    fn closure_definitions() {
        let env = Env::new();
        let expression =
            parser::parse("((lambda (first second third) (lambda () (add second third))) 3 4 5)")
                .unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::BoundFunction(BoundFunctionNode {
                arity: 0,
                body: Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                )))),
                env: Env::from(vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                ]),
            })))
        );
        let expression =
            parser::parse("((lambda (foo bar) (lambda (baz) (add foo baz))) 3 4)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::BoundFunction(BoundFunctionNode {
                arity: 1,
                body: Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                    Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                )))),
                env: Env::from(vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ]),
            })))
        );
    }

    #[test]
    fn nullary_function_application() {
        let env = Env::new();
        let expression = parser::parse("((lambda () (add 3 4)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn unary_function_application() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo) (add foo 4)) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn binary_function_application() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo bar) (add foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn ternary_function_application() {
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
    fn pending_function_application() {
        let env = Env::new();
        let expression = parser::parse("((pending) 3 4 5)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new()))),
        );
    }

    #[test]
    fn error_function_application() {
        let env = Env::new();
        let expression = parser::parse("((error \"foo\") 3 4 5)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo")))),
        );
    }

    #[test]
    fn nested_argument_scope() {
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
                Expression::new(Node::Core(CoreNode::FunctionApplication(
                    FunctionApplicationNode::new(
                        Expression::new(Node::Core(CoreNode::Function(FunctionNode::new(
                            1,
                            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                                Expression::new(Node::Core(CoreNode::Reference(
                                    ReferenceNode::new(0),
                                ))),
                                acc,
                            )))),
                        )))),
                        vec![Expression::new(Node::Core(CoreNode::Value(
                            ValueNode::Int(i),
                        )))],
                    ),
                )))
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

    #[test]
    fn is_function_expressions() {
        let env = Env::new();
        let expression = parser::parse("(function? (lambda () 3))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );
        let expression = parser::parse("(function? ((lambda (foo) (lambda () foo)) 3))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
        );

        let expression = parser::parse("(function? null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? true)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? -0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? -0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? \"\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(function? \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }
}
