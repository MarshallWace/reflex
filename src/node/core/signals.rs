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
    hash::{combine_hashes, hash_seed, hash_string},
    node::{
        core::{function::apply_function, CoreNode, StringValue, ValueNode},
        Node,
    },
    signal::{EffectHandler, Signal, SignalName, SignalResult},
};

#[derive(Debug, PartialEq, Clone)]
pub struct SignalHandlerNode {
    name: SignalName,
    handler: EffectHandler<Node>,
    expression: Expression<Node>,
}
impl SignalHandlerNode {
    pub fn new(
        name: SignalName,
        handler: EffectHandler<Node>,
        expression: Expression<Node>,
    ) -> Self {
        SignalHandlerNode {
            name,
            handler,
            expression,
        }
    }
}
impl<'a> CompoundNode<'a, Node> for SignalHandlerNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.expression)
    }
}
impl NodeType<Node> for SignalHandlerNode {
    fn hash(&self) -> u32 {
        combine_hashes(hash_string(&self.name), CompoundNode::hash(self))
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        let (result, dependencies) = self.expression.evaluate(env, state).unwrap();
        Some(match result {
            EvaluationResultType::Signal(signal) if signal.contains(self.name) => match signal {
                SignalResult::Single(signal) => self
                    .handler
                    .handle(&vec![signal.args()])
                    .evaluate(env, state)
                    .add_dependencies(dependencies),
                SignalResult::Multiple(signal) => {
                    let (signals, remaining) = signal.remove(self.name);
                    let signals = signals.unwrap();
                    let args = signals
                        .iter()
                        .map(|signal| signal.args())
                        .collect::<Vec<_>>();
                    let handled_result = self.handler.handle(&args).evaluate(env, state);
                    match remaining {
                        None => handled_result.add_dependencies(dependencies),
                        Some(signal) => {
                            let (_, handled_dependencies) = handled_result.unwrap();
                            EvaluationResult::signal_with_dependencies(signal, dependencies)
                                .add_dependencies(handled_dependencies)
                        }
                    }
                }
            },
            result => EvaluationResult::from(result, dependencies),
        })
    }
}
impl fmt::Display for SignalHandlerNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<handler:{}>", self.name)
    }
}

const ERROR_SIGNAL: SignalName = "*error*";

impl Signal<Node> {
    pub fn error(message: String) -> Self {
        Signal::new(
            ERROR_SIGNAL,
            vec![Expression::new(Node::Core(CoreNode::Value(
                ValueNode::String(StringValue::new(message)),
            )))],
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ErrorNode {
    signal: Signal<Node>,
}
impl ErrorNode {
    pub fn new(signal: Signal<Node>) -> Self {
        ErrorNode { signal }
    }
}
impl AstNode<Node> for ErrorNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let arg = args.next().unwrap();
        Ok(Self::new(Signal::new(ERROR_SIGNAL, vec![arg])))
    }
}
impl NodeType<Node> for ErrorNode {
    fn hash(&self) -> u32 {
        self.signal.hash()
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(
        &self,
        _env: &Env<Node>,
        _state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Some(EvaluationResult::signal(self.signal.clone()))
    }
}
impl fmt::Display for ErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.signal, f)
    }
}

pub fn with_error_handler(
    handler: EffectHandler<Node>,
    expression: Expression<Node>,
) -> Expression<Node> {
    Expression::new(Node::Core(CoreNode::SignalHandler(SignalHandlerNode::new(
        ERROR_SIGNAL,
        handler,
        expression,
    ))))
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchNode {
    handler: Expression<Node>,
    expression: Expression<Node>,
}
impl CatchNode {
    pub fn new(handler: Expression<Node>, expression: Expression<Node>) -> Self {
        CatchNode {
            handler,
            expression,
        }
    }
}
impl AstNode<Node> for CatchNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 2 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let handler = args.next().unwrap();
        let expression = args.next().unwrap();
        Ok(Self::new(handler, expression))
    }
}
impl<'a> CompoundNode<'a, Node> for CatchNode {
    type Expressions = std::iter::Chain<
        std::iter::Once<&'a Expression<Node>>,
        std::iter::Once<&'a Expression<Node>>,
    >;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.handler).chain(once(&self.expression))
    }
}
impl NodeType<Node> for CatchNode {
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
        let (result, dependencies) = self.expression.evaluate(env, state).unwrap();
        Some(match result {
            EvaluationResultType::Signal(signal) if signal.contains(ERROR_SIGNAL) => match signal {
                SignalResult::Single(signal) => {
                    apply_function(&self.handler, signal.args(), env, state)
                        .add_dependencies(dependencies)
                }
                SignalResult::Multiple(signal) => {
                    let (signals, remaining) = signal.remove(ERROR_SIGNAL);
                    let signals = signals.unwrap();
                    let signal = signals.iter().next().unwrap();
                    let handled_result = apply_function(&self.handler, signal.args(), env, state);
                    match remaining {
                        None => handled_result.add_dependencies(dependencies),
                        Some(signal) => {
                            let (_, handled_dependencies) = handled_result.unwrap();
                            EvaluationResult::signal_with_dependencies(signal, dependencies)
                                .add_dependencies(handled_dependencies)
                        }
                    }
                }
            },
            result => EvaluationResult::from(result, dependencies),
        })
    }
}
impl fmt::Display for CatchNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsErrorNode {
    target: Expression<Node>,
}
impl IsErrorNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsErrorNode { target }
    }
}
impl AstNode<Node> for IsErrorNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsErrorNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsErrorNode {
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
        let (result, dependencies) = self.target.evaluate(env, state).unwrap();
        Some(match result {
            EvaluationResultType::Signal(signal) if signal.contains(ERROR_SIGNAL) => {
                EvaluationResult::with_dependencies(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
                    dependencies,
                )
            }
            EvaluationResultType::Signal(signal) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            _ => EvaluationResult::with_dependencies(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
                dependencies,
            ),
        })
    }
}
impl fmt::Display for IsErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

const PENDING_SIGNAL: SignalName = "*pending*";

impl Signal<Node> {
    pub fn pending() -> Self {
        Signal::new(PENDING_SIGNAL, Vec::new())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PendingNode {}
impl PendingNode {
    pub fn new() -> Self {
        PendingNode {}
    }
}
impl AstNode<Node> for PendingNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 0 {
            return Err(String::from("Invalid number of arguments"));
        }
        Ok(Self::new())
    }
}
impl NodeType<Node> for PendingNode {
    fn hash(&self) -> u32 {
        hash_seed()
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(
        &self,
        _env: &Env<Node>,
        _state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Some(EvaluationResult::signal(Signal::pending()))
    }
}
impl fmt::Display for PendingNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsPendingNode {
    target: Expression<Node>,
}
impl IsPendingNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsPendingNode { target }
    }
}
impl AstNode<Node> for IsPendingNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a, Node> for IsPendingNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsPendingNode {
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
        let (result, dependencies) = self.target.evaluate(env, state).unwrap();
        Some(match result {
            EvaluationResultType::Signal(signal) if signal.contains(PENDING_SIGNAL) => {
                EvaluationResult::with_dependencies(
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
                    dependencies,
                )
            }
            EvaluationResultType::Signal(signal) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            _ => EvaluationResult::with_dependencies(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
                dependencies,
            ),
        })
    }
}
impl fmt::Display for IsPendingNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{EvaluationResult, EvaluationResultType, Expression, RuntimeState},
        node::{
            core::{CoreNode, ValueNode},
            parser, Node,
        },
        signal::Signal,
    };

    #[test]
    fn static_error_signals() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(error \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("foo")))
        );
    }

    #[test]
    fn short_circuit_error_signals() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(+ (error \"foo\") 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("foo")))
        );
        let expression = parser::parse("(+ 3 (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("foo")))
        );
    }

    #[test]
    fn is_error_expression() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(error? (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(error? (/ 3 0))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(error? (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));

        let expression = parser::parse("(error? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(error? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }

    #[test]
    fn catch_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression =
            parser::parse("(catch (lambda (error) (+ error 1)) (+ 3 (error 4)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Int(4 + 1)
            )))),
        );
        let expression =
            parser::parse("(catch (lambda (error) (+ error 1)) (+ (error 3) (pending)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
        let expression =
            parser::parse("(catch (lambda (error) (+ error 1)) (+ (error 3) (error 4)))").unwrap();
        let (result, _dependencies) = expression.evaluate(&env, &state).unwrap();
        match result {
            EvaluationResultType::Expression(result) => match result.value() {
                Node::Core(CoreNode::Value(ValueNode::Int(result))) => {
                    assert!(*result == (3 + 1) || *result == (4 + 1));
                }
                _ => {
                    panic!();
                }
            },
            _ => {
                panic!();
            }
        }
    }

    #[test]
    fn pending_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(pending)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
    }

    #[test]
    fn short_circuit_pending_expressions() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(+ (pending) 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
        let expression = parser::parse("(+ 3 (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
        let expression = parser::parse("(+ (+ (pending) 3) 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
        let expression = parser::parse("(+ 3 (+ (pending) 3))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
    }

    #[test]
    fn is_pending_expression() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(pending? (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );
        let expression = parser::parse("(pending? (+ 3 (pending)))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(true)
            ))))
        );

        let expression = parser::parse("(pending? (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal(Signal::error(String::from("foo")))
        );

        let expression = parser::parse("(pending? null)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? #t)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? #f)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? 0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? -0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? 3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? -3)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? 0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? -0.0)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? 3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? -3.142)").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? \"\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
        let expression = parser::parse("(pending? \"foo\")").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::new(Expression::new(Node::Core(CoreNode::Value(
                ValueNode::Boolean(false)
            ))))
        );
    }
}
