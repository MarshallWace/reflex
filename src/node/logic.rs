// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::Node,
};

pub mod and;
pub mod conditional;
pub mod not;
pub mod or;

pub use and::AndNode;
pub use conditional::ConditionalNode;
pub use not::NotNode;
pub use or::OrNode;

#[derive(PartialEq, Clone)]
pub enum LogicNode {
    And(AndNode),
    Conditional(ConditionalNode),
    Not(NotNode),
    Or(OrNode),
}
impl LogicNode {
    pub fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Result<Option<Self>, String> {
        match type_name {
            "and" => AndNode::factory(args).map(|node| Some(LogicNode::And(node))),
            "if" => ConditionalNode::factory(args).map(|node| Some(LogicNode::Conditional(node))),
            "not" => NotNode::factory(args).map(|node| Some(LogicNode::Not(node))),
            "or" => OrNode::factory(args).map(|node| Some(LogicNode::Or(node))),
            _ => Ok(None),
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
    fn is_static(&self) -> bool {
        match self {
            LogicNode::And(node) => node.is_static(),
            LogicNode::Conditional(node) => node.is_static(),
            LogicNode::Not(node) => node.is_static(),
            LogicNode::Or(node) => node.is_static(),
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
