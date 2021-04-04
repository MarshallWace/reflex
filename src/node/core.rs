// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        AstNode, AstNodePackage, EvaluationResult, Expression, NodeFactoryResult, NodeType,
        RuntimeState,
    },
    hash::prefix_hash,
    node::Node,
};

pub mod bindings;
pub mod effect;
pub mod eq;
pub mod function;
pub mod signals;
pub mod state;
pub mod value;

pub use self::bindings::{
    BoundNode, LetNode, LetRecBindingNode, LetRecNode, LetStarNode, ReferenceNode,
};
pub use self::effect::EffectNode;
pub use self::eq::EqNode;
pub use self::function::{
    BoundFunctionNode, FunctionApplicationNode, FunctionNode, IsFunctionNode,
};
pub use self::signals::{
    CatchNode, ErrorNode, IsErrorNode, IsPendingNode, PendingNode, SignalHandlerNode,
};
pub use self::state::StateNode;
pub use self::value::{
    IsBooleanNode, IsFloatNode, IsIntegerNode, IsNullNode, IsStringNode, StringValue, ValueNode,
};

#[derive(PartialEq, Clone)]
pub enum CoreNode {
    Bound(BoundNode),
    BoundFunction(BoundFunctionNode),
    Catch(CatchNode),
    Effect(EffectNode),
    Eq(EqNode),
    Error(ErrorNode),
    Function(FunctionNode),
    FunctionApplication(FunctionApplicationNode),
    Let(LetNode),
    LetRec(LetRecNode),
    LetRecBinding(LetRecBindingNode),
    LetStar(LetStarNode),
    Pending(PendingNode),
    Reference(ReferenceNode),
    SignalHandler(SignalHandlerNode),
    State(StateNode),
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
            "catch" => Some(CatchNode::factory(args).map(Self::Catch)),
            "eq" => Some(EqNode::factory(args).map(Self::Eq)),
            "error" => Some(ErrorNode::factory(args).map(Self::Error)),
            "pending" => Some(PendingNode::factory(args).map(Self::Pending)),
            "let" => Some(LetNode::factory(args).map(Self::Let)),
            "letrec" => Some(LetRecNode::factory(args).map(Self::LetRec)),
            "let*" => Some(LetStarNode::factory(args).map(Self::LetStar)),
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
    fn hash(&self) -> u32 {
        match self {
            Self::Bound(node) => prefix_hash(0, node.hash()),
            Self::BoundFunction(node) => prefix_hash(1, node.hash()),
            Self::Catch(node) => prefix_hash(2, node.hash()),
            Self::Effect(node) => prefix_hash(3, node.hash()),
            Self::Eq(node) => prefix_hash(4, node.hash()),
            Self::Error(node) => prefix_hash(5, node.hash()),
            Self::Function(node) => prefix_hash(6, node.hash()),
            Self::FunctionApplication(node) => prefix_hash(7, node.hash()),
            Self::Let(node) => prefix_hash(8, node.hash()),
            Self::LetRec(node) => prefix_hash(9, node.hash()),
            Self::LetRecBinding(node) => prefix_hash(10, node.hash()),
            Self::LetStar(node) => prefix_hash(11, node.hash()),
            Self::Pending(node) => prefix_hash(12, node.hash()),
            Self::Reference(node) => prefix_hash(13, node.hash()),
            Self::SignalHandler(node) => prefix_hash(14, node.hash()),
            Self::State(node) => prefix_hash(15, node.hash()),
            Self::Value(node) => prefix_hash(16, node.hash()),
            Self::IsNull(node) => prefix_hash(17, node.hash()),
            Self::IsBoolean(node) => prefix_hash(18, node.hash()),
            Self::IsInteger(node) => prefix_hash(19, node.hash()),
            Self::IsFloat(node) => prefix_hash(20, node.hash()),
            Self::IsString(node) => prefix_hash(21, node.hash()),
            Self::IsFunction(node) => prefix_hash(22, node.hash()),
            Self::IsError(node) => prefix_hash(23, node.hash()),
            Self::IsPending(node) => prefix_hash(24, node.hash()),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            Self::Bound(node) => node.capture_depth(),
            Self::BoundFunction(node) => node.capture_depth(),
            Self::Catch(node) => node.capture_depth(),
            Self::Effect(node) => node.capture_depth(),
            Self::Eq(node) => node.capture_depth(),
            Self::Error(node) => node.capture_depth(),
            Self::Function(node) => node.capture_depth(),
            Self::FunctionApplication(node) => node.capture_depth(),
            Self::Let(node) => node.capture_depth(),
            Self::LetRec(node) => node.capture_depth(),
            Self::LetRecBinding(node) => node.capture_depth(),
            Self::LetStar(node) => node.capture_depth(),
            Self::Pending(node) => node.capture_depth(),
            Self::Reference(node) => node.capture_depth(),
            Self::SignalHandler(node) => node.capture_depth(),
            Self::State(node) => node.capture_depth(),
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
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        match self {
            Self::Bound(node) => node.evaluate(env, state),
            Self::BoundFunction(node) => node.evaluate(env, state),
            Self::Catch(node) => node.evaluate(env, state),
            Self::Effect(node) => node.evaluate(env, state),
            Self::Eq(node) => node.evaluate(env, state),
            Self::Error(node) => node.evaluate(env, state),
            Self::Function(node) => node.evaluate(env, state),
            Self::FunctionApplication(node) => node.evaluate(env, state),
            Self::Let(node) => node.evaluate(env, state),
            Self::LetRec(node) => node.evaluate(env, state),
            Self::LetRecBinding(node) => node.evaluate(env, state),
            Self::LetStar(node) => node.evaluate(env, state),
            Self::Pending(node) => node.evaluate(env, state),
            Self::Reference(node) => node.evaluate(env, state),
            Self::SignalHandler(node) => node.evaluate(env, state),
            Self::State(node) => node.evaluate(env, state),
            Self::Value(node) => node.evaluate(env, state),
            Self::IsNull(node) => node.evaluate(env, state),
            Self::IsBoolean(node) => node.evaluate(env, state),
            Self::IsInteger(node) => node.evaluate(env, state),
            Self::IsFloat(node) => node.evaluate(env, state),
            Self::IsString(node) => node.evaluate(env, state),
            Self::IsFunction(node) => node.evaluate(env, state),
            Self::IsError(node) => node.evaluate(env, state),
            Self::IsPending(node) => node.evaluate(env, state),
        }
    }
}
impl fmt::Display for CoreNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bound(node) => fmt::Display::fmt(node, f),
            Self::BoundFunction(node) => fmt::Display::fmt(node, f),
            Self::Catch(node) => fmt::Display::fmt(node, f),
            Self::Effect(node) => fmt::Display::fmt(node, f),
            Self::Eq(node) => fmt::Display::fmt(node, f),
            Self::Error(node) => fmt::Display::fmt(node, f),
            Self::Function(node) => fmt::Display::fmt(node, f),
            Self::FunctionApplication(node) => fmt::Display::fmt(node, f),
            Self::Let(node) => fmt::Display::fmt(node, f),
            Self::LetRec(node) => fmt::Display::fmt(node, f),
            Self::LetRecBinding(node) => fmt::Display::fmt(node, f),
            Self::LetStar(node) => fmt::Display::fmt(node, f),
            Self::Pending(node) => fmt::Display::fmt(node, f),
            Self::Reference(node) => fmt::Display::fmt(node, f),
            Self::SignalHandler(node) => fmt::Display::fmt(node, f),
            Self::State(node) => fmt::Display::fmt(node, f),
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
            Self::Bound(node) => fmt::Debug::fmt(node, f),
            Self::BoundFunction(node) => fmt::Debug::fmt(node, f),
            Self::Catch(node) => fmt::Debug::fmt(node, f),
            Self::Effect(node) => fmt::Debug::fmt(node, f),
            Self::Eq(node) => fmt::Debug::fmt(node, f),
            Self::Error(node) => fmt::Debug::fmt(node, f),
            Self::Function(node) => fmt::Debug::fmt(node, f),
            Self::FunctionApplication(node) => fmt::Debug::fmt(node, f),
            Self::Let(node) => fmt::Debug::fmt(node, f),
            Self::LetRec(node) => fmt::Debug::fmt(node, f),
            Self::LetRecBinding(node) => fmt::Debug::fmt(node, f),
            Self::LetStar(node) => fmt::Debug::fmt(node, f),
            Self::Pending(node) => fmt::Debug::fmt(node, f),
            Self::Reference(node) => fmt::Debug::fmt(node, f),
            Self::SignalHandler(node) => fmt::Debug::fmt(node, f),
            Self::State(node) => fmt::Debug::fmt(node, f),
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
