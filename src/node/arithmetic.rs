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

pub mod abs;
pub mod add;
pub mod ceil;
pub mod divide;
pub mod floor;
pub mod max;
pub mod min;
pub mod multiply;
pub mod pow;
pub mod remainder;
pub mod round;
pub mod subtract;

pub use self::abs::AbsNode;
pub use self::add::AddNode;
pub use self::ceil::CeilNode;
pub use self::divide::DivideNode;
pub use self::floor::FloorNode;
pub use self::max::MaxNode;
pub use self::min::MinNode;
pub use self::multiply::MultiplyNode;
pub use self::pow::PowNode;
pub use self::remainder::RemainderNode;
pub use self::round::RoundNode;
pub use self::subtract::SubtractNode;

#[derive(PartialEq, Clone)]
pub enum ArithmeticNode {
    Add(AddNode),
    Subtract(SubtractNode),
    Multiply(MultiplyNode),
    Divide(DivideNode),
    Remainder(RemainderNode),
    Abs(AbsNode),
    Floor(FloorNode),
    Ceil(CeilNode),
    Round(RoundNode),
    Max(MaxNode),
    Min(MinNode),
    Pow(PowNode),
}
impl AstNodePackage<Node> for ArithmeticNode {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "add" => Some(AddNode::factory(args).map(Self::Add)),
            "subtract" => Some(SubtractNode::factory(args).map(Self::Subtract)),
            "multiply" => Some(MultiplyNode::factory(args).map(Self::Multiply)),
            "divide" => Some(DivideNode::factory(args).map(Self::Divide)),
            "remainder" => Some(RemainderNode::factory(args).map(Self::Remainder)),
            "abs" => Some(AbsNode::factory(args).map(Self::Abs)),
            "floor" => Some(FloorNode::factory(args).map(Self::Floor)),
            "ceil" => Some(CeilNode::factory(args).map(Self::Ceil)),
            "round" => Some(RoundNode::factory(args).map(Self::Round)),
            "max" => Some(MaxNode::factory(args).map(Self::Max)),
            "min" => Some(MinNode::factory(args).map(Self::Min)),
            "pow" => Some(PowNode::factory(args).map(Self::Pow)),
            _ => None,
        }
    }
}
impl NodeType<Node> for ArithmeticNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Self::Add(node) => node.expressions(),
            Self::Subtract(node) => node.expressions(),
            Self::Multiply(node) => node.expressions(),
            Self::Divide(node) => node.expressions(),
            Self::Remainder(node) => node.expressions(),
            Self::Abs(node) => node.expressions(),
            Self::Floor(node) => node.expressions(),
            Self::Ceil(node) => node.expressions(),
            Self::Round(node) => node.expressions(),
            Self::Max(node) => node.expressions(),
            Self::Min(node) => node.expressions(),
            Self::Pow(node) => node.expressions(),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            Self::Add(node) => node.capture_depth(),
            Self::Subtract(node) => node.capture_depth(),
            Self::Multiply(node) => node.capture_depth(),
            Self::Divide(node) => node.capture_depth(),
            Self::Remainder(node) => node.capture_depth(),
            Self::Abs(node) => node.capture_depth(),
            Self::Floor(node) => node.capture_depth(),
            Self::Ceil(node) => node.capture_depth(),
            Self::Round(node) => node.capture_depth(),
            Self::Max(node) => node.capture_depth(),
            Self::Min(node) => node.capture_depth(),
            Self::Pow(node) => node.capture_depth(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        match self {
            Self::Add(node) => node.evaluate(env),
            Self::Subtract(node) => node.evaluate(env),
            Self::Multiply(node) => node.evaluate(env),
            Self::Divide(node) => node.evaluate(env),
            Self::Remainder(node) => node.evaluate(env),
            Self::Abs(node) => node.evaluate(env),
            Self::Floor(node) => node.evaluate(env),
            Self::Ceil(node) => node.evaluate(env),
            Self::Round(node) => node.evaluate(env),
            Self::Max(node) => node.evaluate(env),
            Self::Min(node) => node.evaluate(env),
            Self::Pow(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add(node) => fmt::Display::fmt(node, f),
            Self::Subtract(node) => fmt::Display::fmt(node, f),
            Self::Multiply(node) => fmt::Display::fmt(node, f),
            Self::Divide(node) => fmt::Display::fmt(node, f),
            Self::Remainder(node) => fmt::Display::fmt(node, f),
            Self::Abs(node) => fmt::Display::fmt(node, f),
            Self::Floor(node) => fmt::Display::fmt(node, f),
            Self::Ceil(node) => fmt::Display::fmt(node, f),
            Self::Round(node) => fmt::Display::fmt(node, f),
            Self::Max(node) => fmt::Display::fmt(node, f),
            Self::Min(node) => fmt::Display::fmt(node, f),
            Self::Pow(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add(node) => fmt::Display::fmt(node, f),
            Self::Subtract(node) => fmt::Display::fmt(node, f),
            Self::Multiply(node) => fmt::Display::fmt(node, f),
            Self::Divide(node) => fmt::Display::fmt(node, f),
            Self::Remainder(node) => fmt::Display::fmt(node, f),
            Self::Abs(node) => fmt::Display::fmt(node, f),
            Self::Floor(node) => fmt::Display::fmt(node, f),
            Self::Ceil(node) => fmt::Display::fmt(node, f),
            Self::Round(node) => fmt::Display::fmt(node, f),
            Self::Max(node) => fmt::Display::fmt(node, f),
            Self::Min(node) => fmt::Display::fmt(node, f),
            Self::Pow(node) => fmt::Display::fmt(node, f),
        }
    }
}
