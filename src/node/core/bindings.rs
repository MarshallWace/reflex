// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::{Env, StackOffset},
    expression::{AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType},
    node::{
        core::{function::apply_function, CoreNode},
        Node,
    },
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
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
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

#[derive(PartialEq, Clone)]
pub struct BoundNode {
    target: Expression<Node>,
    env: Env<Node>,
}
impl BoundNode {
    pub fn bind(target: &Expression<Node>, env: &Env<Node>) -> Expression<Node> {
        let capture_depth = target.capture_depth();
        if capture_depth == 0 {
            return Expression::clone(target);
        }
        match target.value() {
            Node::Core(CoreNode::Reference(ReferenceNode { offset })) => {
                Expression::clone(env.get(*offset))
            }
            _ => Expression::new(Node::Core(CoreNode::Bound(BoundNode {
                target: Expression::clone(target),
                env: env.capture(capture_depth),
            }))),
        }
    }
    pub fn target(&self) -> &Expression<Node> {
        &self.target
    }
    pub fn env(&self) -> &Env<Node> {
        &self.env
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

#[derive(Debug, PartialEq, Clone)]
pub struct LetNode {
    initializers: Vec<Expression<Node>>,
    body: Expression<Node>,
}
impl LetNode {
    pub fn new(initializers: Vec<Expression<Node>>, body: Expression<Node>) -> Self {
        LetNode { initializers, body }
    }
}
impl AstNode<Node> for LetNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() < 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let (body, initializers) = args.split_last().unwrap();
        let initializers = initializers.iter().map(Expression::clone).collect();
        let body = Expression::clone(body);
        Ok(Self::new(initializers, body))
    }
}
impl NodeType<Node> for LetNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        self.initializers.iter().chain(once(&self.body)).collect()
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        Some(apply_function(
            self.initializers.len(),
            &self.body,
            &self.initializers,
            env,
            Some(env),
        ))
    }
}
impl fmt::Display for LetNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::Expression,
        node::{
            arithmetic::{AddNode, ArithmeticNode},
            core::{
                value::{StringValue, ValueNode},
                CoreNode, Node,
            },
            parser,
        },
    };

    use super::{BoundNode, ReferenceNode};

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
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("first")
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1))));
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("second")
            ))))
        );
        let expression = Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0))));
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::String(
                StringValue::Literal("third")
            ))))
        );
    }

    #[test]
    fn bound_expressions() {
        let env = Env::new();
        let expression = Expression::new(Node::Core(CoreNode::Bound(BoundNode {
            target: Expression::new(Node::Arithmetic(ArithmeticNode::Add(AddNode::new(
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(1)))),
                Expression::new(Node::Core(CoreNode::Reference(ReferenceNode::new(0)))),
            )))),
            env: env.extend(vec![
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3)))),
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(4)))),
            ]),
        })));
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }

    #[test]
    fn let_expressions() {
        let env = Env::new();
        let expression = parser::parse("(let () 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );
        let expression = parser::parse("(let ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3))))
        );
        let expression = parser::parse("(let ((foo 3) (bar 4)) (add foo bar))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
        let expression = parser::parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (add third fourth))))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Int(3 + 4))))
        );
    }
}
