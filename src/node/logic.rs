// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, AstNodePackage, Expression, NodeFactoryResult, NodeType},
    node::Node,
};

pub mod and;
pub mod conditional;
pub mod not;
pub mod or;

pub use self::and::AndNode;
pub use self::conditional::ConditionalNode;
pub use self::not::NotNode;
pub use self::or::OrNode;

#[derive(PartialEq, Clone)]
pub enum LogicNode {
    And(AndNode),
    Conditional(ConditionalNode),
    Not(NotNode),
    Or(OrNode),
}
impl AstNodePackage<Node> for LogicNode {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "and" => Some(AndNode::factory(args).map(Self::And)),
            "if" => Some(ConditionalNode::factory(args).map(Self::Conditional)),
            "not" => Some(NotNode::factory(args).map(Self::Not)),
            "or" => Some(OrNode::factory(args).map(Self::Or)),
            _ => None,
        }
    }
}
impl NodeType<Node> for LogicNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            LogicNode::And(node) => node.expressions(),
            LogicNode::Conditional(node) => node.expressions(),
            LogicNode::Not(node) => node.expressions(),
            LogicNode::Or(node) => node.expressions(),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            LogicNode::And(node) => node.capture_depth(),
            LogicNode::Conditional(node) => node.capture_depth(),
            LogicNode::Not(node) => node.capture_depth(),
            LogicNode::Or(node) => node.capture_depth(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            LogicNode::And(node) => node.evaluate(env),
            LogicNode::Conditional(node) => node.evaluate(env),
            LogicNode::Not(node) => node.evaluate(env),
            LogicNode::Or(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for LogicNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicNode::And(node) => fmt::Display::fmt(node, f),
            LogicNode::Conditional(node) => fmt::Display::fmt(node, f),
            LogicNode::Not(node) => fmt::Display::fmt(node, f),
            LogicNode::Or(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for LogicNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicNode::And(node) => fmt::Debug::fmt(node, f),
            LogicNode::Conditional(node) => fmt::Debug::fmt(node, f),
            LogicNode::Not(node) => fmt::Debug::fmt(node, f),
            LogicNode::Or(node) => fmt::Debug::fmt(node, f),
        }
    }
}
