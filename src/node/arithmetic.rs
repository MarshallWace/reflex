// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt::{self, Debug};

use crate::{
    env::Env,
    expression::{Expression, NodeType},
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

pub use abs::AbsNode;
pub use add::AddNode;
pub use ceil::CeilNode;
pub use divide::DivideNode;
pub use floor::FloorNode;
pub use multiply::MultiplyNode;
pub use pow::PowNode;
pub use remainder::RemainderNode;
pub use subtract::SubtractNode;

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
impl ArithmeticNode {
    pub fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Result<Option<Self>, String> {
        match type_name {
            "abs" => AbsNode::factory(args).map(|node| Some(ArithmeticNode::Abs(node))),
            "add" => AddNode::factory(args).map(|node| Some(ArithmeticNode::Add(node))),
            "ceil" => CeilNode::factory(args).map(|node| Some(ArithmeticNode::Ceil(node))),
            "divide" => DivideNode::factory(args).map(|node| Some(ArithmeticNode::Divide(node))),
            "floor" => FloorNode::factory(args).map(|node| Some(ArithmeticNode::Floor(node))),
            "multiply" => {
                MultiplyNode::factory(args).map(|node| Some(ArithmeticNode::Multiply(node)))
            }
            "pow" => PowNode::factory(args).map(|node| Some(ArithmeticNode::Pow(node))),
            "remainder" => {
                RemainderNode::factory(args).map(|node| Some(ArithmeticNode::Remainder(node)))
            }
            "subtract" => {
                SubtractNode::factory(args).map(|node| Some(ArithmeticNode::Subtract(node)))
            }
            _ => Ok(None),
        }
    }
}
impl NodeType<Node> for ArithmeticNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            ArithmeticNode::Abs(node) => node.expressions(),
            ArithmeticNode::Add(node) => node.expressions(),
            ArithmeticNode::Ceil(node) => node.expressions(),
            ArithmeticNode::Divide(node) => node.expressions(),
            ArithmeticNode::Floor(node) => node.expressions(),
            ArithmeticNode::Multiply(node) => node.expressions(),
            ArithmeticNode::Pow(node) => node.expressions(),
            ArithmeticNode::Remainder(node) => node.expressions(),
            ArithmeticNode::Subtract(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            ArithmeticNode::Abs(node) => node.is_static(),
            ArithmeticNode::Add(node) => node.is_static(),
            ArithmeticNode::Ceil(node) => node.is_static(),
            ArithmeticNode::Divide(node) => node.is_static(),
            ArithmeticNode::Floor(node) => node.is_static(),
            ArithmeticNode::Multiply(node) => node.is_static(),
            ArithmeticNode::Pow(node) => node.is_static(),
            ArithmeticNode::Remainder(node) => node.is_static(),
            ArithmeticNode::Subtract(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            ArithmeticNode::Abs(node) => node.evaluate(env),
            ArithmeticNode::Add(node) => node.evaluate(env),
            ArithmeticNode::Ceil(node) => node.evaluate(env),
            ArithmeticNode::Divide(node) => node.evaluate(env),
            ArithmeticNode::Floor(node) => node.evaluate(env),
            ArithmeticNode::Multiply(node) => node.evaluate(env),
            ArithmeticNode::Pow(node) => node.evaluate(env),
            ArithmeticNode::Remainder(node) => node.evaluate(env),
            ArithmeticNode::Subtract(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticNode::Abs(node) => Debug::fmt(node, f),
            ArithmeticNode::Add(node) => Debug::fmt(node, f),
            ArithmeticNode::Ceil(node) => Debug::fmt(node, f),
            ArithmeticNode::Divide(node) => Debug::fmt(node, f),
            ArithmeticNode::Floor(node) => Debug::fmt(node, f),
            ArithmeticNode::Multiply(node) => Debug::fmt(node, f),
            ArithmeticNode::Pow(node) => Debug::fmt(node, f),
            ArithmeticNode::Remainder(node) => Debug::fmt(node, f),
            ArithmeticNode::Subtract(node) => Debug::fmt(node, f),
        }
    }
}
impl fmt::Debug for ArithmeticNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticNode::Abs(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Add(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Ceil(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Divide(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Floor(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Multiply(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Pow(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Remainder(node) => fmt::Display::fmt(node, f),
            ArithmeticNode::Subtract(node) => fmt::Display::fmt(node, f),
        }
    }
}
