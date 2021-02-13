// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::{Env, StackOffset},
    expression::{Expression, NodeType},
    node::Node,
};

#[derive(PartialEq, Clone)]
pub struct ReferenceNode {
    offset: StackOffset,
}
impl ReferenceNode {
    pub fn new(offset: StackOffset) -> Self {
        ReferenceNode { offset }
    }
    pub fn offset(&self) -> StackOffset {
        self.offset
    }
}
impl NodeType<Node> for ReferenceNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        Vec::new()
    }
    fn capture_depth(&self) -> usize {
        self.offset + 1
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Some(env.get(self.offset).evaluate(env))
    }
}
impl fmt::Display for ReferenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<ref:{}>", self.offset)
    }
}
impl fmt::Debug for ReferenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        env::Env,
        expression::Expression,
        node::{
            core::{CoreNode, StringValue, ValueNode},
            Node,
        },
    };

    use super::ReferenceNode;

    #[test]
    fn references() {
        let env = Env::from(vec![
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::literal("first"),
            )))),
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::literal("second"),
            )))),
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::literal("third"),
            )))),
        ]);
        let expression = Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2))));
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("first")
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1))));
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("second")
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))));
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("third")
            ))))
        );
    }
}
