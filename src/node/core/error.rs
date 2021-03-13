// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, iter::once};

use crate::{
    env::Env,
    expression::{
        AstNode, CompoundNode, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    hash::hash_string,
    node::{
        core::{CoreNode, ValueNode},
        Node,
    },
};

#[derive(PartialEq, Clone)]
pub struct ErrorNode {
    message: String,
}
impl ErrorNode {
    pub fn new(message: &str) -> Self {
        ErrorNode {
            message: String::from(message),
        }
    }
}
impl AstNode<Node> for ErrorNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let message = args.next().unwrap();
        match message.value() {
            Node::Core(CoreNode::Value(ValueNode::String(value))) => Ok(Self::new(value.get())),
            _ => Err(format!("Invalid argument")),
        }
    }
}
impl NodeType<Node> for ErrorNode {
    fn hash(&self) -> u32 {
        hash_string(&self.message)
    }
    fn capture_depth(&self) -> usize {
        0
    }
    fn evaluate(&self, _env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        None
    }
}
impl fmt::Display for ErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<error:{:?}>", self.message)
    }
}
impl fmt::Debug for ErrorNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsErrorNode {
    target: Expression<Node>,
}
impl IsErrorNode {
    pub fn new(target: Expression<Node>) -> Self {
        IsErrorNode { target }
    }
}
impl AstNode<Node> for IsErrorNode {
    fn factory(args: &[Expression<Node>]) -> NodeFactoryResult<Self> {
        if args.len() != 1 {
            return Err(String::from("Invalid number of arguments"));
        }
        let args = &mut args.iter().map(Expression::clone);
        let target = args.next().unwrap();
        Ok(Self::new(target))
    }
}
impl<'a> CompoundNode<'a> for IsErrorNode {
    type Expressions = std::iter::Once<&'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions {
        once(&self.target)
    }
}
impl NodeType<Node> for IsErrorNode {
    fn hash(&self) -> u32 {
        CompoundNode::hash(self)
    }
    fn capture_depth(&self) -> usize {
        CompoundNode::capture_depth(self)
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        let target = self.target.evaluate(env);
        match target.expression.value() {
            Node::Core(CoreNode::Pending(_)) => Some(target),
            Node::Core(CoreNode::Error(_)) => Some(EvaluationResult::new(
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
impl fmt::Display for IsErrorNode {
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
            core::{CoreNode, PendingNode, ValueNode},
            parser, Node,
        },
    };

    use super::ErrorNode;

    #[test]
    fn static_errors() {
        let env = Env::new();
        let expression = parser::parse("(error \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }

    #[test]
    fn short_circuit_errors() {
        let env = Env::new();
        let expression = parser::parse("(+ (error \"foo\") 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(+ 3 (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }

    #[test]
    fn short_circuit_error_priority() {
        let env = Env::new();
        let expression = parser::parse("(+ (error \"foo\") (pending))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(+ (pending) (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(+ (+ (error \"foo\") (pending)) (pending))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
        let expression = parser::parse("(+ (pending) (+ (pending) (error \"foo\")))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Error(ErrorNode::new("foo"))))
        );
    }

    #[test]
    fn is_error_expression() {
        let env = Env::new();
        let expression = parser::parse("(error? (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
        );
        let expression = parser::parse("(error? (/ 3 0))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(true)))),
        );

        let expression = parser::parse("(error? (pending))").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Pending(PendingNode::new()))),
        );

        let expression = parser::parse("(error? null)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false)))),
        );
        let expression = parser::parse("(error? #t)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? #f)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? 0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? -0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? 3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? -3)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? 0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? -0.0)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? 3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? -3.142)").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? \"\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
        let expression = parser::parse("(error? \"foo\")").unwrap();
        let result = expression.evaluate(&env).expression;
        assert_eq!(
            result,
            Expression::new(Node::Core(CoreNode::Value(ValueNode::Boolean(false))))
        );
    }
}
