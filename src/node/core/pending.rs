// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::Node,
};

#[derive(PartialEq, Clone)]
pub struct PendingNode {}
impl PendingNode {
    pub fn new() -> Self {
        PendingNode {}
    }
    pub fn factory(args: &Vec<Expression<Node>>) -> Result<Self, String> {
        if args.len() != 0 {
            return Err(String::from("Invalid number of arguments"));
        }
        Ok(PendingNode::new())
    }
}
impl NodeType<Node> for PendingNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        Vec::new()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
    }
}
impl fmt::Display for PendingNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<pending>")
    }
}
impl fmt::Debug for PendingNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{core::CoreNode, Node},
    };

    use super::PendingNode;

    #[test]
    fn static_pendings() {
        let env = Env::new();
        let expression = Node::parse("(pending)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
    }

    #[test]
    fn short_circuit_pendings() {
        let env = Env::new();
        let expression = Node::parse("(add (pending) 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = Node::parse("(add 3 (pending))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = Node::parse("(add (add (pending) 3) 3)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = Node::parse("(add 3 (add (pending) 3))").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
    }
}
