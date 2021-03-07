// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{
        AstNode, AstNodePackage, EvaluationResult, Expression, NodeFactoryResult, NodeType,
    },
    hash::prefix_hash,
    node::Node,
};

pub mod abs;
pub mod add;
pub mod ceil;
pub mod divide;
pub mod equal;
pub mod floor;
pub mod gt;
pub mod gte;
pub mod lt;
pub mod lte;
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
pub use self::equal::EqualNode;
pub use self::floor::FloorNode;
pub use self::gt::GtNode;
pub use self::gte::GteNode;
pub use self::lt::LtNode;
pub use self::lte::LteNode;
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
    Equal(EqualNode),
    Remainder(RemainderNode),
    Abs(AbsNode),
    Floor(FloorNode),
    Gt(GtNode),
    Gte(GteNode),
    Lt(LtNode),
    Lte(LteNode),
    Ceil(CeilNode),
    Round(RoundNode),
    Max(MaxNode),
    Min(MinNode),
    Pow(PowNode),
}
impl AstNodePackage<Node> for ArithmeticNode {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "+" => Some(AddNode::factory(args).map(Self::Add)),
            "-" => Some(SubtractNode::factory(args).map(Self::Subtract)),
            "*" => Some(MultiplyNode::factory(args).map(Self::Multiply)),
            "/" => Some(DivideNode::factory(args).map(Self::Divide)),
            "=" => Some(EqualNode::factory(args).map(Self::Equal)),
            ">" => Some(GtNode::factory(args).map(Self::Gt)),
            ">=" => Some(GteNode::factory(args).map(Self::Gte)),
            "<" => Some(LtNode::factory(args).map(Self::Lt)),
            "<=" => Some(LteNode::factory(args).map(Self::Lte)),
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
    fn hash(&self) -> u32 {
        match self {
            Self::Add(node) => prefix_hash(0, node.hash()),
            Self::Subtract(node) => prefix_hash(1, node.hash()),
            Self::Multiply(node) => prefix_hash(2, node.hash()),
            Self::Divide(node) => prefix_hash(3, node.hash()),
            Self::Equal(node) => prefix_hash(4, node.hash()),
            Self::Remainder(node) => prefix_hash(5, node.hash()),
            Self::Abs(node) => prefix_hash(6, node.hash()),
            Self::Floor(node) => prefix_hash(7, node.hash()),
            Self::Gt(node) => prefix_hash(8, node.hash()),
            Self::Gte(node) => prefix_hash(9, node.hash()),
            Self::Lt(node) => prefix_hash(10, node.hash()),
            Self::Lte(node) => prefix_hash(11, node.hash()),
            Self::Ceil(node) => prefix_hash(12, node.hash()),
            Self::Round(node) => prefix_hash(13, node.hash()),
            Self::Max(node) => prefix_hash(14, node.hash()),
            Self::Min(node) => prefix_hash(15, node.hash()),
            Self::Pow(node) => prefix_hash(16, node.hash()),
        }
    }
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Self::Add(node) => node.expressions(),
            Self::Subtract(node) => node.expressions(),
            Self::Multiply(node) => node.expressions(),
            Self::Divide(node) => node.expressions(),
            Self::Equal(node) => node.expressions(),
            Self::Remainder(node) => node.expressions(),
            Self::Abs(node) => node.expressions(),
            Self::Floor(node) => node.expressions(),
            Self::Gt(node) => node.expressions(),
            Self::Gte(node) => node.expressions(),
            Self::Lt(node) => node.expressions(),
            Self::Lte(node) => node.expressions(),
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
            Self::Equal(node) => node.capture_depth(),
            Self::Remainder(node) => node.capture_depth(),
            Self::Abs(node) => node.capture_depth(),
            Self::Floor(node) => node.capture_depth(),
            Self::Gt(node) => node.capture_depth(),
            Self::Gte(node) => node.capture_depth(),
            Self::Lt(node) => node.capture_depth(),
            Self::Lte(node) => node.capture_depth(),
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
            Self::Equal(node) => node.evaluate(env),
            Self::Remainder(node) => node.evaluate(env),
            Self::Abs(node) => node.evaluate(env),
            Self::Floor(node) => node.evaluate(env),
            Self::Gt(node) => node.evaluate(env),
            Self::Gte(node) => node.evaluate(env),
            Self::Lt(node) => node.evaluate(env),
            Self::Lte(node) => node.evaluate(env),
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
            Self::Equal(node) => fmt::Display::fmt(node, f),
            Self::Remainder(node) => fmt::Display::fmt(node, f),
            Self::Abs(node) => fmt::Display::fmt(node, f),
            Self::Floor(node) => fmt::Display::fmt(node, f),
            Self::Gt(node) => fmt::Display::fmt(node, f),
            Self::Gte(node) => fmt::Display::fmt(node, f),
            Self::Lt(node) => fmt::Display::fmt(node, f),
            Self::Lte(node) => fmt::Display::fmt(node, f),
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
            Self::Equal(node) => fmt::Display::fmt(node, f),
            Self::Remainder(node) => fmt::Display::fmt(node, f),
            Self::Abs(node) => fmt::Display::fmt(node, f),
            Self::Floor(node) => fmt::Display::fmt(node, f),
            Self::Gt(node) => fmt::Display::fmt(node, f),
            Self::Gte(node) => fmt::Display::fmt(node, f),
            Self::Lt(node) => fmt::Display::fmt(node, f),
            Self::Lte(node) => fmt::Display::fmt(node, f),
            Self::Ceil(node) => fmt::Display::fmt(node, f),
            Self::Round(node) => fmt::Display::fmt(node, f),
            Self::Max(node) => fmt::Display::fmt(node, f),
            Self::Min(node) => fmt::Display::fmt(node, f),
            Self::Pow(node) => fmt::Display::fmt(node, f),
        }
    }
}
