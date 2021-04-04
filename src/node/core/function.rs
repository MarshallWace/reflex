// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, EvaluationResultType, Expression,
        NodeFactoryResult, NodeType, RuntimeState,
    },
    hash::{combine_hashes, prefix_hash},
    node::{
        core::{BoundNode, CoreNode, ReferenceNode, ValueNode},
        evaluate::EvaluateResult,
        Evaluate1, Node,
    },
    signal::{Signal, SignalResult},
};

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionNode {
    arity: usize,
    body: Expression<Node>,
}
impl FunctionNode {
    pub fn new(arity: usize, body: Expression<Node>) -> Self {
        FunctionNode { arity, body }
    }
    pub fn identity() -> Self {
        FunctionNode {
            arity: 1,
            body: Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
        }
    }
    pub fn arity(&self) -> usize {
        self.arity
    }
    pub fn body(&self) -> &Expression<Node> {
        &self.body
    }
}
impl NodeType<Node> for FunctionNode {
    fn hash(&self) -> u32 {
        prefix_hash(self.arity as u8, self.body.hash())
    }
    fn capture_depth(&self) -> usize {
        self.body.capture_depth().saturating_sub(self.arity)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        _state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        match self.capture_depth() {
            0 => None,
            _ => Some(EvaluationResult::new(Expression::new(Node::Core(
                CoreNode::BoundFunction(BoundFunctionNode {
                    arity: self.arity,
                    body: Expression::clone(&self.body),
                    env: env.capture(),
                }),
            )))),
        }
    }
}
impl fmt::Display for FunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function:{} -> {}>", self.arity, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    fn hash(&self) -> u32 {
        prefix_hash(
            self.arity as u8,
            combine_hashes(self.body.hash(), self.env.hash()),
        )
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(
        &self,
        _env: &Env<Node>,
        _state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        None
    }
}
impl fmt::Display for BoundFunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound:{} -> {}>", self.arity, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionApplicationNode {
    target: Expression<Node>,
    args: Vec<Expression<Node>>,
}
impl FunctionApplicationNode {
    pub fn new(target: Expression<Node>, args: Vec<Expression<Node>>) -> Self {
        FunctionApplicationNode { target, args }
    }
}
impl<'a> CompoundNode<'a, Node> for FunctionApplicationNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::slice::Iter<'a, Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target).chain(self.args.iter())
    }
}
impl NodeType<Node> for FunctionApplicationNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Some(apply_function(&self.target, &self.args, env, state))
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

pub fn apply_function(
    target: &Expression<Node>,
    args: &[Expression<Node>],
    env: &Env<Node>,
    state: &RuntimeState<Node>,
) -> EvaluationResult<Node> {
    let (result, dependencies) = target.evaluate(env, state).unwrap();
    match result {
        EvaluationResultType::Signal(signal) => {
            EvaluationResult::signal_with_dependencies(signal, dependencies)
        }
        EvaluationResultType::Expression(expression) => match expression.value() {
            Node::Core(CoreNode::Function(node)) => {
                let arity = node.arity();
                let body = node.body();
                apply_function_body(arity, body, args, env, None, state)
                    .add_dependencies(dependencies)
            }
            Node::Core(CoreNode::BoundFunction(node)) => {
                let arity = node.arity();
                let body = node.body();
                let captured_env = node.env();
                apply_function_body(arity, body, args, env, Some(captured_env), state)
                    .add_dependencies(dependencies)
            }
            target => EvaluationResult::signal_with_dependencies(
                SignalResult::single(Signal::error(format!(
                    "Target expression is not a function: {}",
                    target
                ))),
                dependencies,
            ),
        },
    }
}

fn apply_function_body(
    arity: usize,
    body: &Expression<Node>,
    args: &[Expression<Node>],
    arg_env: &Env<Node>,
    captured_env: Option<&Env<Node>>,
    state: &RuntimeState<Node>,
) -> EvaluationResult<Node> {
    if args.len() != arity {
        return EvaluationResult::signal(Signal::error(format!(
            "Expected {} arguments, received {}",
            arity,
            args.len()
        )));
    }
    if arity == 0 {
        return match captured_env {
            Some(captured_env) => body.evaluate(captured_env, state),
            None => body.evaluate(&Env::new(), state),
        };
    }
    let args = args.iter().map(|arg| BoundNode::bind(arg, arg_env));
    let inner_env = match captured_env {
        Some(captured_env) => captured_env.extend(args),
        None => Env::from(args),
    };
    body.evaluate(&inner_env, state)
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
impl<'a> CompoundNode<'a, Node> for IsFunctionNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsFunctionNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Evaluate1::evaluate(self, env, state)
    }
}
impl Evaluate1 for IsFunctionNode {
    fn dependencies(&self) -> &Expression<Node> {
        &self.target
    }
    fn run(&self, target: &Expression<Node>) -> EvaluateResult {
        Ok(match target.value() {
            Node::Core(CoreNode::Function(_)) | Node::Core(CoreNode::BoundFunction(_)) => {
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true))))
            }
            _ => Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        })
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
        expression::{EvaluationResult, Expression, RuntimeState},
        node::{
            arithmetic::{AddNode, ArithmeticNode},
            core::{CoreNode, ReferenceNode, ValueNode},
            parser, Node,
        },
        signal::Signal,
    };

    use super::{BoundFunctionNode, FunctionApplicationNode, FunctionNode};

    #[test]
    fn function_definitions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(lambda (foo bar) (+ foo bar))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Function(
                FunctionNode::new(
                    2,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    ))))
                )
            ))))
        );
    }

    #[test]
    fn closure_definitions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression =
            parser::parse("((lambda (first second third) (lambda () (+ second third))) 3 4 5)")
                .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::BoundFunction(
                BoundFunctionNode {
                    arity: 0,
                    body: Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    )))),
                    env: env.extend(
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        ]
                        .into_iter()
                    ),
                }
            ))))
        );
        let expression =
            parser::parse("((lambda (foo bar) (lambda (baz) (+ foo baz))) 3 4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::BoundFunction(
                BoundFunctionNode {
                    arity: 1,
                    body: Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    )))),
                    env: Env::from(vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                    ]),
                }
            ))))
        );
    }

    #[test]
    fn nullary_function_application() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("((lambda () (+ 3 4)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            ))))
        );
    }

    #[test]
    fn unary_function_application() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("((lambda (foo) (+ foo 4)) 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            ))))
        );
    }

    #[test]
    fn binary_function_application() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("((lambda (foo bar) (+ foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            ))))
        );
    }

    #[test]
    fn ternary_function_application() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression =
            parser::parse("((lambda (foo bar baz) (+ foo (+ bar baz))) 3 4 5)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4 + 5)
            ))))
        );
    }

    #[test]
    fn error_function_application() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("((error \"foo\") 3 4 5)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("foo"))),
        );
    }

    #[test]
    fn nested_argument_scope() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse(
            "((lambda (first second third) ((lambda (foo bar) (+ foo bar)) second third)) 3 4 5)",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(4 + 5)
            )),))
        );
        let env = Env::new();
        let expression = parser::parse(
            "((lambda (first second third) ((lambda (one two) ((lambda (foo bar) (+ foo bar)) one two)) first third)) 3 4 5)",
        ).unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 5)
            )),))
        );
    }

    #[test]
    fn immediately_invoked_closures() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression =
            parser::parse("((lambda (three four) ((lambda (one two) (+ one three)) 1 2)) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(1 + 3)
            ))))
        );
        let expression =
            parser::parse("(((lambda (one two) (lambda (three four) (+ one three))) 1 2) 3 4)")
                .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(1 + 3)
            ))))
        );
    }

    #[test]
    fn deferred_closures() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse(
            "((lambda (transform value) (transform (value))) ((lambda (constant) (lambda (value) (+ value constant))) 4) ((lambda (value) (lambda () value)) 3))",
        )
        .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(3 + 4)
            ))))
        );
    }

    #[test]
    fn deeply_nested_functions() {
        let env = Env::new();
        let state = RuntimeState::new();
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
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int((1..=100).fold(0, |acc, i| i + acc))
            ))))
        );
    }

    #[test]
    fn is_function_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(function? (lambda () 3))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(function? ((lambda (foo) (lambda () foo)) 3))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(function? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(function? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }
}
