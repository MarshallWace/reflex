// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, AstNodePackage, Expression, NodeFactoryResult, NodeType},
    node::Node,
};

pub mod application;
pub mod closure;
pub mod error;
pub mod function;
pub mod pending;
pub mod reference;
pub mod value;

pub use self::application::{ApplicationNode, BoundArgumentNode};
pub use self::closure::{BoundFunctionNode, ClosureNode};
pub use self::error::ErrorNode;
pub use self::function::FunctionNode;
pub use self::pending::PendingNode;
pub use self::reference::ReferenceNode;
pub use self::value::{StringValue, ValueNode};

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
impl AstNodePackage<Node> for CoreNode {
    fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "error" => Some(ErrorNode::factory(args).map(Self::Error)),
            "pending" => Some(PendingNode::factory(args).map(Self::Pending)),
            _ => None,
        }
    }
}
impl NodeType<Node> for CoreNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Self::Application(node) => node.expressions(),
            Self::BoundArgument(node) => node.expressions(),
            Self::BoundFunction(node) => node.expressions(),
            Self::Closure(node) => node.expressions(),
            Self::Error(node) => node.expressions(),
            Self::Function(node) => node.expressions(),
            Self::Pending(node) => node.expressions(),
            Self::Reference(node) => node.expressions(),
            Self::Value(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Application(node) => node.is_static(),
            Self::BoundArgument(node) => node.is_static(),
            Self::BoundFunction(node) => node.is_static(),
            Self::Closure(node) => node.is_static(),
            Self::Error(node) => node.is_static(),
            Self::Function(node) => node.is_static(),
            Self::Pending(node) => node.is_static(),
            Self::Reference(node) => node.is_static(),
            Self::Value(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            Self::Application(node) => node.evaluate(env),
            Self::BoundArgument(node) => node.evaluate(env),
            Self::BoundFunction(node) => node.evaluate(env),
            Self::Closure(node) => node.evaluate(env),
            Self::Error(node) => node.evaluate(env),
            Self::Function(node) => node.evaluate(env),
            Self::Pending(node) => node.evaluate(env),
            Self::Reference(node) => node.evaluate(env),
            Self::Value(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Application(node) => fmt::Display::fmt(node, f),
            Self::BoundArgument(node) => fmt::Display::fmt(node, f),
            Self::BoundFunction(node) => fmt::Display::fmt(node, f),
            Self::Closure(node) => fmt::Display::fmt(node, f),
            Self::Error(node) => fmt::Display::fmt(node, f),
            Self::Function(node) => fmt::Display::fmt(node, f),
            Self::Pending(node) => fmt::Display::fmt(node, f),
            Self::Value(node) => fmt::Display::fmt(node, f),
            Self::Reference(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Application(node) => fmt::Debug::fmt(node, f),
            Self::BoundArgument(node) => fmt::Debug::fmt(node, f),
            Self::BoundFunction(node) => fmt::Debug::fmt(node, f),
            Self::Closure(node) => fmt::Debug::fmt(node, f),
            Self::Error(node) => fmt::Debug::fmt(node, f),
            Self::Function(node) => fmt::Debug::fmt(node, f),
            Self::Pending(node) => fmt::Debug::fmt(node, f),
            Self::Value(node) => fmt::Debug::fmt(node, f),
            Self::Reference(node) => fmt::Debug::fmt(node, f),
        }
    }
}
