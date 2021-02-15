// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::{CoreNode, Node},
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
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self.capture_depth() {
            0 => None,
            depth => Some(Expression::new(Node::Core(CoreNode::BoundFunction(
                BoundFunctionNode {
                    arity: self.arity,
                    body: Expression::clone(&self.body),
                    env: env.capture(depth),
                },
            )))),
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
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
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

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            arithmetic::{AddNode, ArithmeticNode},
            core::{CoreNode, ReferenceNode, ValueNode},
            parser, Node,
        },
    };

    use super::{BoundFunctionNode, FunctionNode};

    #[test]
    fn functions() {
        let env = Env::new();
        let expression = parser::parse("(lambda (foo bar) (add foo bar))").unwrap();
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

    #[test]
    fn closures() {
        let env = Env::new();
        let expression =
            parser::parse("((lambda (first second third) (lambda () (add second third))) 3 4 5)")
                .unwrap();
        let result = expression.evaluate(&env);
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
        let result = expression.evaluate(&env);
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
}