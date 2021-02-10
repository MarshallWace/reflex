// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{
    env::{Env, StackOffset},
    expression::{Expression, NodeType},
    node::{
        core::{CoreNode, FunctionNode},
        Node,
    },
};

#[derive(PartialEq, Clone)]
pub struct ClosureNode {
    captures: Vec<StackOffset>,
    function: Rc<FunctionNode>,
}
impl ClosureNode {
    pub fn new(captures: Vec<StackOffset>, function: FunctionNode) -> Self {
        ClosureNode {
            captures,
            function: Rc::new(function),
        }
    }
    pub fn captures(&self) -> &Vec<StackOffset> {
        &self.captures
    }
    pub fn function(&self) -> &FunctionNode {
        &self.function
    }
}
impl NodeType<Node> for ClosureNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.function.expressions()
    }
    fn is_static(&self) -> bool {
        false
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        Some(Expression::new(Node::Core(CoreNode::BoundFunction(
            BoundFunctionNode {
                function: Rc::clone(&self.function),
                env: env.capture(),
            },
        ))))
    }
}
impl fmt::Display for ClosureNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure:{}>", self.function)
    }
}
impl fmt::Debug for ClosureNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(PartialEq, Clone)]
pub struct BoundFunctionNode {
    function: Rc<FunctionNode>,
    env: Env<Node>,
}
impl BoundFunctionNode {
    pub fn function(&self) -> &FunctionNode {
        &self.function
    }
    pub fn env(&self) -> &Env<Node> {
        &self.env
    }
}
impl NodeType<Node> for BoundFunctionNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.function.expressions()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<Expression<Node>> {
        None
    }
}
impl fmt::Display for BoundFunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound:{}>", self.function)
    }
}
impl fmt::Debug for BoundFunctionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{env::Env, expression::Expression, node::{Node, arithmetic::{AddNode, ArithmeticNode}, core::{CoreNode, FunctionNode, ReferenceNode, ValueNode}, parser}};

    use super::BoundFunctionNode;

    #[test]
    fn closures() {
        let env = Env::new();
        let expression = parser::parse("((lambda (foo bar) (lambda () (add foo bar))) 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::BoundFunction(BoundFunctionNode {
                env: env.extend(vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ]),
                function: Rc::new(FunctionNode::new(
                    0,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    ))))
                )),
            })))
        );
        let expression =
            parser::parse("((lambda (foo bar) (lambda (baz) (add foo baz))) 3 4)").unwrap();
        let result = expression.evaluate(&env);
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::BoundFunction(BoundFunctionNode {
                env: env.extend(vec![
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                    Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
                ]),
                function: Rc::new(FunctionNode::new(
                    1,
                    Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(2)))),
                        Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
                    ))))
                )),
            })))
        );
    }
}
