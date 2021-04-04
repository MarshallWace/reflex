// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{EvaluationResult, Expression, NodeType, RuntimeState, StateToken},
    node::Node,
};

#[derive(Debug, PartialEq, Clone)]
pub struct StateNode {
    id: StateToken,
    initial_value: Expression<Node>,
}
impl StateNode {
    pub fn new(id: StateToken, initial_value: Expression<Node>) -> Self {
        StateNode { id, initial_value }
    }
    pub fn id(&self) -> StateToken {
        self.id
    }
}
impl NodeType<Node> for StateNode {
    fn hash(&self) -> StateToken {
        self.id
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Some(match state.get(self.id) {
            Some(expression) => expression.evaluate(env, state).add_dependency(self.id),
            None => self
                .initial_value
                .evaluate(env, state)
                .add_dependency(self.id),
        })
    }
}
impl fmt::Display for StateNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<state:{}>", self.id)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{DependencyList, EvaluationResult, Expression, RuntimeState},
        node::{
            core::{CoreNode, ValueNode},
            Node,
        },
    };

    use super::StateNode;

    #[test]
    fn state_expressions() {
        let env = Env::new();
        let mut state = RuntimeState::new();
        let expression = Expression::new(Node::Core(CoreNode::State(StateNode::new(
            123,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
        ))));
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::with_dependencies(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                DependencyList::of(123),
            ),
        );
        state.set(
            123,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
        );
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::with_dependencies(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                DependencyList::of(123),
            ),
        );
    }
}
