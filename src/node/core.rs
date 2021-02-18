// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        AstNode, AstNodePackage, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    node::Node,
};

pub mod application;
pub mod error;
pub mod function;
pub mod pending;
pub mod reference;
pub mod value;

pub use self::application::{ApplicationNode, BoundArgumentNode};
pub use self::error::{ErrorNode, IsErrorNode};
pub use self::function::{BoundFunctionNode, FunctionNode, IsFunctionNode};
pub use self::pending::{IsPendingNode, PendingNode};
pub use self::reference::ReferenceNode;
pub use self::value::{
    IsBooleanNode, IsFloatNode, IsIntegerNode, IsNullNode, IsStringNode, StringValue, ValueNode,
};

#[derive(PartialEq, Clone)]
pub enum CoreNode {
    Application(ApplicationNode),
    BoundArgument(BoundArgumentNode),
    BoundFunction(BoundFunctionNode),
    Function(FunctionNode),
    Pending(PendingNode),
    Reference(ReferenceNode),
    Error(ErrorNode),
    Value(ValueNode),
    IsNull(IsNullNode),
    IsBoolean(IsBooleanNode),
    IsInteger(IsIntegerNode),
    IsFloat(IsFloatNode),
    IsString(IsStringNode),
    IsFunction(IsFunctionNode),
    IsError(IsErrorNode),
    IsPending(IsPendingNode),
}
impl AstNodePackage<Node> for CoreNode {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "error" => Some(ErrorNode::factory(args).map(Self::Error)),
            "pending" => Some(PendingNode::factory(args).map(Self::Pending)),
            "null?" => Some(IsNullNode::factory(args).map(Self::IsNull)),
            "boolean?" => Some(IsBooleanNode::factory(args).map(Self::IsBoolean)),
            "integer?" => Some(IsIntegerNode::factory(args).map(Self::IsInteger)),
            "float?" => Some(IsFloatNode::factory(args).map(Self::IsFloat)),
            "string?" => Some(IsStringNode::factory(args).map(Self::IsString)),
            "function?" => Some(IsFunctionNode::factory(args).map(Self::IsFunction)),
            "error?" => Some(IsErrorNode::factory(args).map(Self::IsError)),
            "pending?" => Some(IsPendingNode::factory(args).map(Self::IsPending)),
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
            Self::Error(node) => node.expressions(),
            Self::Function(node) => node.expressions(),
            Self::Pending(node) => node.expressions(),
            Self::Reference(node) => node.expressions(),
            Self::Value(node) => node.expressions(),
            Self::IsNull(node) => node.expressions(),
            Self::IsBoolean(node) => node.expressions(),
            Self::IsInteger(node) => node.expressions(),
            Self::IsFloat(node) => node.expressions(),
            Self::IsString(node) => node.expressions(),
            Self::IsFunction(node) => node.expressions(),
            Self::IsError(node) => node.expressions(),
            Self::IsPending(node) => node.expressions(),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            Self::Application(node) => node.capture_depth(),
            Self::BoundArgument(node) => node.capture_depth(),
            Self::BoundFunction(node) => node.capture_depth(),
            Self::Error(node) => node.capture_depth(),
            Self::Function(node) => node.capture_depth(),
            Self::Pending(node) => node.capture_depth(),
            Self::Reference(node) => node.capture_depth(),
            Self::Value(node) => node.capture_depth(),
            Self::IsNull(node) => node.capture_depth(),
            Self::IsBoolean(node) => node.capture_depth(),
            Self::IsInteger(node) => node.capture_depth(),
            Self::IsFloat(node) => node.capture_depth(),
            Self::IsString(node) => node.capture_depth(),
            Self::IsFunction(node) => node.capture_depth(),
            Self::IsError(node) => node.capture_depth(),
            Self::IsPending(node) => node.capture_depth(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        match self {
            Self::Application(node) => node.evaluate(env),
            Self::BoundArgument(node) => node.evaluate(env),
            Self::BoundFunction(node) => node.evaluate(env),
            Self::Error(node) => node.evaluate(env),
            Self::Function(node) => node.evaluate(env),
            Self::Pending(node) => node.evaluate(env),
            Self::Reference(node) => node.evaluate(env),
            Self::Value(node) => node.evaluate(env),
            Self::IsNull(node) => node.evaluate(env),
            Self::IsBoolean(node) => node.evaluate(env),
            Self::IsInteger(node) => node.evaluate(env),
            Self::IsFloat(node) => node.evaluate(env),
            Self::IsString(node) => node.evaluate(env),
            Self::IsFunction(node) => node.evaluate(env),
            Self::IsError(node) => node.evaluate(env),
            Self::IsPending(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Application(node) => fmt::Display::fmt(node, f),
            Self::BoundArgument(node) => fmt::Display::fmt(node, f),
            Self::BoundFunction(node) => fmt::Display::fmt(node, f),
            Self::Error(node) => fmt::Display::fmt(node, f),
            Self::Function(node) => fmt::Display::fmt(node, f),
            Self::Pending(node) => fmt::Display::fmt(node, f),
            Self::Reference(node) => fmt::Display::fmt(node, f),
            Self::Value(node) => fmt::Display::fmt(node, f),
            Self::IsNull(node) => fmt::Display::fmt(node, f),
            Self::IsBoolean(node) => fmt::Display::fmt(node, f),
            Self::IsInteger(node) => fmt::Display::fmt(node, f),
            Self::IsFloat(node) => fmt::Display::fmt(node, f),
            Self::IsString(node) => fmt::Display::fmt(node, f),
            Self::IsFunction(node) => fmt::Display::fmt(node, f),
            Self::IsError(node) => fmt::Display::fmt(node, f),
            Self::IsPending(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Application(node) => fmt::Debug::fmt(node, f),
            Self::BoundArgument(node) => fmt::Debug::fmt(node, f),
            Self::BoundFunction(node) => fmt::Debug::fmt(node, f),
            Self::Error(node) => fmt::Debug::fmt(node, f),
            Self::Function(node) => fmt::Debug::fmt(node, f),
            Self::Pending(node) => fmt::Debug::fmt(node, f),
            Self::Reference(node) => fmt::Debug::fmt(node, f),
            Self::Value(node) => fmt::Debug::fmt(node, f),
            Self::IsNull(node) => fmt::Debug::fmt(node, f),
            Self::IsBoolean(node) => fmt::Debug::fmt(node, f),
            Self::IsInteger(node) => fmt::Debug::fmt(node, f),
            Self::IsFloat(node) => fmt::Debug::fmt(node, f),
            Self::IsString(node) => fmt::Debug::fmt(node, f),
            Self::IsFunction(node) => fmt::Debug::fmt(node, f),
            Self::IsError(node) => fmt::Debug::fmt(node, f),
            Self::IsPending(node) => fmt::Debug::fmt(node, f),
        }
    }
}
