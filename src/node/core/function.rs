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
        self.body.value().expressions()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
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

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            arithmetic::{AddNode, ArithmeticNode},
            core::{CoreNode, ReferenceNode},
            Node,
        },
    };

    use super::FunctionNode;

    #[test]
    fn functions() {
        let env = Env::new();
        let expression = Node::parse("(lambda (foo bar) (add foo bar))").unwrap();
        let result = expression.evaluate(&env);
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
}
