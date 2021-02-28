// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{EvaluationResult, Expression, NodeType},
    node::Node,
};

#[derive(PartialEq, Clone)]
pub struct BoundNode {
    target: Expression<Node>,
    env: Env<Node>,
}
impl BoundNode {
    pub fn new(target: Expression<Node>, env: Env<Node>) -> Self {
        BoundNode { target, env }
    }
}
impl NodeType<Node> for BoundNode {
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
impl fmt::Display for BoundNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound:{}>", self.target)
    }
}
impl fmt::Debug for BoundNode {
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
            arithmetic::{ArithmeticNode, AddNode},
            core::{CoreNode, ReferenceNode, ValueNode},
            Node,
        },
    };

    use super::BoundNode;

    #[test]
    fn bound_expressions() {
        let env = Env::new();
        let expression = Expression::new(Node::Core(CoreNode::Bound(BoundNode::new(
            Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
            )))),
            env.extend(vec![
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
            ]),
        ))));
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }
}
