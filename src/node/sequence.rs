// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNode, AstNodePackage, Expression, NodeFactoryResult, NodeType},
    node::Node,
};

pub mod apply;
pub mod car;
pub mod cdr;
pub mod cons;
pub mod list;

pub use self::apply::ApplyNode;
pub use self::car::CarNode;
pub use self::cdr::CdrNode;
pub use self::cons::{ConsNode, IsPairNode};
pub use self::list::{CollectNode, IsListNode, ListNode};

#[derive(PartialEq, Clone)]
pub enum SequenceNode {
    Apply(ApplyNode),
    Car(CarNode),
    Cdr(CdrNode),
    Collect(CollectNode),
    Cons(ConsNode),
    IsList(IsListNode),
    IsPair(IsPairNode),
    List(ListNode),
}
impl AstNodePackage<Node> for SequenceNode {
    fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Option<NodeFactoryResult<Self>> {
        match type_name {
            "apply" => Some(ApplyNode::factory(args).map(Self::Apply)),
            "car" => Some(CarNode::factory(args).map(Self::Car)),
            "cdr" => Some(CdrNode::factory(args).map(Self::Cdr)),
            "cons" => Some(ConsNode::factory(args).map(Self::Cons)),
            "list" => Some(ListNode::factory(args).map(Self::List)),
            "list?" => Some(IsListNode::factory(args).map(Self::IsList)),
            "pair?" => Some(IsPairNode::factory(args).map(Self::IsPair)),
            _ => None,
        }
    }
}
impl NodeType<Node> for SequenceNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Self::Apply(node) => node.expressions(),
            Self::Car(node) => node.expressions(),
            Self::Cdr(node) => node.expressions(),
            Self::Cons(node) => node.expressions(),
            Self::Collect(node) => node.expressions(),
            Self::IsList(node) => node.expressions(),
            Self::List(node) => node.expressions(),
            Self::IsPair(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Apply(node) => node.is_static(),
            Self::Car(node) => node.is_static(),
            Self::Cdr(node) => node.is_static(),
            Self::Cons(node) => node.is_static(),
            Self::Collect(node) => node.is_static(),
            Self::IsList(node) => node.is_static(),
            Self::List(node) => node.is_static(),
            Self::IsPair(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            Self::Apply(node) => node.evaluate(env),
            Self::Car(node) => node.evaluate(env),
            Self::Cdr(node) => node.evaluate(env),
            Self::Cons(node) => node.evaluate(env),
            Self::Collect(node) => node.evaluate(env),
            Self::IsList(node) => node.evaluate(env),
            Self::List(node) => node.evaluate(env),
            Self::IsPair(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for SequenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Apply(node) => fmt::Display::fmt(node, f),
            Self::Car(node) => fmt::Display::fmt(node, f),
            Self::Cdr(node) => fmt::Display::fmt(node, f),
            Self::Cons(node) => fmt::Display::fmt(node, f),
            Self::Collect(node) => fmt::Display::fmt(node, f),
            Self::IsList(node) => fmt::Display::fmt(node, f),
            Self::List(node) => fmt::Display::fmt(node, f),
            Self::IsPair(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for SequenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Apply(node) => fmt::Debug::fmt(node, f),
            Self::Car(node) => fmt::Debug::fmt(node, f),
            Self::Cdr(node) => fmt::Debug::fmt(node, f),
            Self::Cons(node) => fmt::Debug::fmt(node, f),
            Self::Collect(node) => fmt::Debug::fmt(node, f),
            Self::IsList(node) => fmt::Debug::fmt(node, f),
            Self::List(node) => fmt::Debug::fmt(node, f),
            Self::IsPair(node) => fmt::Debug::fmt(node, f),
        }
    }
}
