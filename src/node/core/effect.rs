// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        CompoundNode, DependencyList, EvaluateDependencies, EvaluationResult, Expression, NodeType,
        RuntimeState,
    },
    hash::{combine_hashes, hash_string},
    node::Node,
    signal::{hash_signal, Signal, SignalName, SignalResult},
};

#[derive(Debug, PartialEq, Clone)]
pub struct EffectNode {
    name: SignalName,
    args: Vec<Expression<Node>>,
}
impl EffectNode {
    pub fn new(name: SignalName, args: Vec<Expression<Node>>) -> Self {
        EffectNode { name, args }
    }
}
impl<'a> CompoundNode<'a, Node> for EffectNode {
    type Expressions = std::slice::Iter<'a, Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        self.args.iter()
    }
}
impl NodeType<Node> for EffectNode {
    fn hash(&self) -> u32 {
        combine_hashes(hash_string(self.name), CompoundNode::hash(self))
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        EvaluateDependencies::evaluate(self, env, state)
    }
}
impl<'a> EvaluateDependencies<'a, Node> for EffectNode {
    type Dependencies = std::slice::Iter<'a, Expression<Node>>;
    fn dependencies(&'a self) -> Self::Dependencies {
        self.args.iter()
    }
    fn run(
        &self,
        args: &[Expression<Node>],
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> EvaluationResult<Node> {
        let state_key = hash_signal(self.name, args);
        match state.get(state_key) {
            Some(expression) => expression.evaluate(env, state).add_dependency(state_key),
            None => EvaluationResult::signal_with_dependencies(
                SignalResult::single(Signal::new(
                    self.name,
                    args.iter().map(Expression::clone).collect(),
                )),
                DependencyList::of(state_key),
            ),
        }
    }
}
impl fmt::Display for EffectNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<effect:{}:{}>",
            self.name,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{
            DependencyList, EvaluationResult, EvaluationResultType, Expression, RuntimeState,
        },
        node::{
            core::{CoreNode, StringValue, ValueNode},
            parser, Node,
        },
        signal::{Signal, SignalResult},
    };

    use super::EffectNode;

    #[test]
    fn effect_expressions() {
        let env = Env::new();
        let mut state = RuntimeState::new();
        let expression = Expression::new(Node::Core(CoreNode::Effect(EffectNode::new(
            "fetch",
            vec![
                parser::parse("3").unwrap(),
                parser::parse("4").unwrap(),
                parser::parse("5").unwrap(),
            ],
        ))));
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::single(Signal::new(
                    "fetch",
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    ]
                )),
                DependencyList::of(
                    Signal::new(
                        "fetch",
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        ]
                    )
                    .hash()
                )
            )
        );
        let expression = Expression::new(Node::Core(CoreNode::Effect(EffectNode::new(
            "fetch",
            vec![
                parser::parse("(+ 1 2)").unwrap(),
                parser::parse("(+ 1 3)").unwrap(),
                parser::parse("(+ 1 4)").unwrap(),
            ],
        ))));
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::single(Signal::new(
                    "fetch",
                    vec![
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                    ]
                )),
                DependencyList::of(
                    Signal::new(
                        "fetch",
                        vec![
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(5)))),
                        ]
                    )
                    .hash(),
                ),
            ),
        );
        let (result, _dependencies) = result.unwrap();
        match result {
            EvaluationResultType::Signal(SignalResult::Single(signal)) => {
                let response = Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                    StringValue::new(String::from("foo")),
                ))));
                state.set(signal.hash(), response);
                let result = expression.evaluate(&env, &state);
                assert_eq!(
                    result,
                    EvaluationResult::with_dependency(
                        Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                            StringValue::new(String::from("foo")),
                        )))),
                        signal.hash(),
                    )
                )
            }
            _ => {
                panic!();
            }
        }
    }
}
