// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::Node,
};

pub mod application;
pub mod closure;
pub mod error;
pub mod function;
pub mod pending;
pub mod reference;
pub mod value;

pub use application::{ApplicationNode, BoundArgumentNode};
pub use closure::{BoundFunctionNode, ClosureNode};
pub use error::ErrorNode;
pub use function::FunctionNode;
pub use pending::PendingNode;
pub use reference::ReferenceNode;
pub use value::{StringValue, ValueNode};

#[derive(PartialEq, Clone)]
pub enum CoreNode {
    Application(ApplicationNode),
    BoundArgument(BoundArgumentNode),
    BoundFunction(BoundFunctionNode),
    Closure(ClosureNode),
    Function(FunctionNode),
    Pending(PendingNode),
    Reference(ReferenceNode),
    Error(ErrorNode),
    Value(ValueNode),
}
impl CoreNode {
    pub fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Result<Option<Self>, String> {
        match type_name {
            "error" => ErrorNode::factory(args).map(|node| Some(CoreNode::Error(node))),
            "pending" => PendingNode::factory(args).map(|node| Some(CoreNode::Pending(node))),
            _ => Ok(None),
        }
    }
}
impl NodeType<Node> for CoreNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            CoreNode::Application(node) => node.expressions(),
            CoreNode::BoundArgument(node) => node.expressions(),
            CoreNode::BoundFunction(node) => node.expressions(),
            CoreNode::Closure(node) => node.expressions(),
            CoreNode::Error(node) => node.expressions(),
            CoreNode::Function(node) => node.expressions(),
            CoreNode::Pending(node) => node.expressions(),
            CoreNode::Reference(node) => node.expressions(),
            CoreNode::Value(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            CoreNode::Application(node) => node.is_static(),
            CoreNode::BoundArgument(node) => node.is_static(),
            CoreNode::BoundFunction(node) => node.is_static(),
            CoreNode::Closure(node) => node.is_static(),
            CoreNode::Error(node) => node.is_static(),
            CoreNode::Function(node) => node.is_static(),
            CoreNode::Pending(node) => node.is_static(),
            CoreNode::Reference(node) => node.is_static(),
            CoreNode::Value(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            CoreNode::Application(node) => node.evaluate(env),
            CoreNode::BoundArgument(node) => node.evaluate(env),
            CoreNode::BoundFunction(node) => node.evaluate(env),
            CoreNode::Closure(node) => node.evaluate(env),
            CoreNode::Error(node) => node.evaluate(env),
            CoreNode::Function(node) => node.evaluate(env),
            CoreNode::Pending(node) => node.evaluate(env),
            CoreNode::Reference(node) => node.evaluate(env),
            CoreNode::Value(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CoreNode::Application(node) => fmt::Display::fmt(node, f),
            CoreNode::BoundArgument(node) => fmt::Display::fmt(node, f),
            CoreNode::BoundFunction(node) => fmt::Display::fmt(node, f),
            CoreNode::Closure(node) => fmt::Display::fmt(node, f),
            CoreNode::Error(node) => fmt::Display::fmt(node, f),
            CoreNode::Function(node) => fmt::Display::fmt(node, f),
            CoreNode::Pending(node) => fmt::Display::fmt(node, f),
            CoreNode::Value(node) => fmt::Display::fmt(node, f),
            CoreNode::Reference(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CoreNode::Application(node) => fmt::Debug::fmt(node, f),
            CoreNode::BoundArgument(node) => fmt::Debug::fmt(node, f),
            CoreNode::BoundFunction(node) => fmt::Debug::fmt(node, f),
            CoreNode::Closure(node) => fmt::Debug::fmt(node, f),
            CoreNode::Error(node) => fmt::Debug::fmt(node, f),
            CoreNode::Function(node) => fmt::Debug::fmt(node, f),
            CoreNode::Pending(node) => fmt::Debug::fmt(node, f),
            CoreNode::Value(node) => fmt::Debug::fmt(node, f),
            CoreNode::Reference(node) => fmt::Debug::fmt(node, f),
        }
    }
}
