// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        AstNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::{
        core::{CoreNode, ValueNode},
        Node,
    },
};

#[derive(PartialEq, Clone)]
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
    fn expressions(&self) -> Vec<&Expression<Node>> {
        Vec::new()
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<EvaluationResult<Node>> {
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
impl NodeType<Node> for IsPendingNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        vec![&self.target]
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let target = self.target.evaluate(env);
        match target.expression.value() {
            Node::Core(CoreNode::Error(_)) => Some(target),
            Node::Core(CoreNode::Pending(_)) => Some(EvaluationResult::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
                target.dependencies,
            )),
            _ => Some(EvaluationResult::new(
                Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
                target.dependencies,
            )),
        }
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
        expression::Expression,
        node::{
            core::{CoreNode, ErrorNode, ValueNode},
            parser, Node,
        },
    };

    use super::PendingNode;

    #[test]
    fn static_pendings() {
        let env = Env::new();
        let expression = parser::parse("(pending)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
    }

    #[test]
    fn short_circuit_pendings() {
        let env = Env::new();
        let expression = parser::parse("(+ (pending) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = parser::parse("(+ 3 (pending))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = parser::parse("(+ (+ (pending) 3) 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
        let expression = parser::parse("(+ 3 (+ (pending) 3))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new())))
        );
    }

    #[test]
    fn is_pending_expression() {
        let env = Env::new();
        let expression = parser::parse("(pending? (pending))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
        );
        let expression = parser::parse("(pending? (+ 3 (pending)))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
        );

        let expression = parser::parse("(pending? (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo")))),
        );

        let expression = parser::parse("(pending? null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        );
        let expression = parser::parse("(pending? true)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? false)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? -0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? -0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? \"\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(pending? \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }
}
