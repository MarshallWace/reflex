// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt::{self, Debug};

use crate::{
    env::Env,
    expression::{AstNode, AstNodePackage, Expression, NodeFactoryResult, NodeType},
    node::Node,
};

pub mod abs;
pub mod add;
pub mod ceil;
pub mod divide;
pub mod floor;
pub mod multiply;
pub mod pow;
pub mod remainder;
pub mod subtract;

pub use self::abs::AbsNode;
pub use self::add::AddNode;
pub use self::ceil::CeilNode;
pub use self::divide::DivideNode;
pub use self::floor::FloorNode;
pub use self::multiply::MultiplyNode;
pub use self::pow::PowNode;
pub use self::remainder::RemainderNode;
pub use self::subtract::SubtractNode;

#[derive(PartialEq, Clone)]
pub enum ArithmeticNode {
    Abs(AbsNode),
    Add(AddNode),
    Ceil(CeilNode),
    Divide(DivideNode),
    Floor(FloorNode),
    Multiply(MultiplyNode),
    Pow(PowNode),
    Remainder(RemainderNode),
    Subtract(SubtractNode),
}
impl AstNodePackage<Node> for ArithmeticNode {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "abs" => Some(AbsNode::factory(args).map(Self::Abs)),
            "add" => Some(AddNode::factory(args).map(Self::Add)),
            "ceil" => Some(CeilNode::factory(args).map(Self::Ceil)),
            "divide" => Some(DivideNode::factory(args).map(Self::Divide)),
            "floor" => Some(FloorNode::factory(args).map(Self::Floor)),
            "multiply" => Some(MultiplyNode::factory(args).map(Self::Multiply)),
            "pow" => Some(PowNode::factory(args).map(Self::Pow)),
            "remainder" => Some(RemainderNode::factory(args).map(Self::Remainder)),
            "subtract" => Some(SubtractNode::factory(args).map(Self::Subtract)),
            _ => None,
        }
    }
}
impl NodeType<Node> for ArithmeticNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Self::Abs(node) => node.expressions(),
            Self::Add(node) => node.expressions(),
            Self::Ceil(node) => node.expressions(),
            Self::Divide(node) => node.expressions(),
            Self::Floor(node) => node.expressions(),
            Self::Multiply(node) => node.expressions(),
            Self::Pow(node) => node.expressions(),
            Self::Remainder(node) => node.expressions(),
            Self::Subtract(node) => node.expressions(),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            Self::Abs(node) => node.capture_depth(),
            Self::Add(node) => node.capture_depth(),
            Self::Ceil(node) => node.capture_depth(),
            Self::Divide(node) => node.capture_depth(),
            Self::Floor(node) => node.capture_depth(),
            Self::Multiply(node) => node.capture_depth(),
            Self::Pow(node) => node.capture_depth(),
            Self::Remainder(node) => node.capture_depth(),
            Self::Subtract(node) => node.capture_depth(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            Self::Abs(node) => node.evaluate(env),
            Self::Add(node) => node.evaluate(env),
            Self::Ceil(node) => node.evaluate(env),
            Self::Divide(node) => node.evaluate(env),
            Self::Floor(node) => node.evaluate(env),
            Self::Multiply(node) => node.evaluate(env),
            Self::Pow(node) => node.evaluate(env),
            Self::Remainder(node) => node.evaluate(env),
            Self::Subtract(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abs(node) => Debug::fmt(node, f),
            Self::Add(node) => Debug::fmt(node, f),
            Self::Ceil(node) => Debug::fmt(node, f),
            Self::Divide(node) => Debug::fmt(node, f),
            Self::Floor(node) => Debug::fmt(node, f),
            Self::Multiply(node) => Debug::fmt(node, f),
            Self::Pow(node) => Debug::fmt(node, f),
            Self::Remainder(node) => Debug::fmt(node, f),
            Self::Subtract(node) => Debug::fmt(node, f),
        }
    }
}
impl fmt::Debug for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Abs(node) => fmt::Display::fmt(node, f),
            Self::Add(node) => fmt::Display::fmt(node, f),
            Self::Ceil(node) => fmt::Display::fmt(node, f),
            Self::Divide(node) => fmt::Display::fmt(node, f),
            Self::Floor(node) => fmt::Display::fmt(node, f),
            Self::Multiply(node) => fmt::Display::fmt(node, f),
            Self::Pow(node) => fmt::Display::fmt(node, f),
            Self::Remainder(node) => fmt::Display::fmt(node, f),
            Self::Subtract(node) => fmt::Display::fmt(node, f),
        }
    }
}
