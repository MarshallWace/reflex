// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
    node::Node,
};

pub mod apply;
pub mod car;
pub mod cdr;
pub mod cons;
pub mod list;

pub use apply::ApplyNode;
pub use car::CarNode;
pub use cdr::CdrNode;
pub use cons::{ConsNode, IsPairNode};
pub use list::{CollectNode, IsListNode, ListNode};

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
impl SequenceNode {
    pub fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Result<Option<Self>, String> {
        match type_name {
            "apply" => ApplyNode::factory(args).map(SequenceNode::Apply).map(Some),
            "car" => CarNode::factory(args).map(SequenceNode::Car).map(Some),
            "cdr" => CdrNode::factory(args).map(SequenceNode::Cdr).map(Some),
            "cons" => ConsNode::factory(args).map(SequenceNode::Cons).map(Some),
            "list" => ListNode::factory(args).map(SequenceNode::List).map(Some),
            "list?" => IsListNode::factory(args)
                .map(SequenceNode::IsList)
                .map(Some),
            "pair?" => IsPairNode::factory(args)
                .map(SequenceNode::IsPair)
                .map(Some),
            _ => Ok(None),
        }
    }
}
impl NodeType<Node> for SequenceNode {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            SequenceNode::Apply(node) => node.expressions(),
            SequenceNode::Car(node) => node.expressions(),
            SequenceNode::Cdr(node) => node.expressions(),
            SequenceNode::Cons(node) => node.expressions(),
            SequenceNode::Collect(node) => node.expressions(),
            SequenceNode::IsList(node) => node.expressions(),
            SequenceNode::List(node) => node.expressions(),
            SequenceNode::IsPair(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            SequenceNode::Apply(node) => node.is_static(),
            SequenceNode::Car(node) => node.is_static(),
            SequenceNode::Cdr(node) => node.is_static(),
            SequenceNode::Cons(node) => node.is_static(),
            SequenceNode::Collect(node) => node.is_static(),
            SequenceNode::IsList(node) => node.is_static(),
            SequenceNode::List(node) => node.is_static(),
            SequenceNode::IsPair(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            SequenceNode::Apply(node) => node.evaluate(env),
            SequenceNode::Car(node) => node.evaluate(env),
            SequenceNode::Cdr(node) => node.evaluate(env),
            SequenceNode::Cons(node) => node.evaluate(env),
            SequenceNode::Collect(node) => node.evaluate(env),
            SequenceNode::IsList(node) => node.evaluate(env),
            SequenceNode::List(node) => node.evaluate(env),
            SequenceNode::IsPair(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for SequenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SequenceNode::Apply(node) => fmt::Display::fmt(node, f),
            SequenceNode::Car(node) => fmt::Display::fmt(node, f),
            SequenceNode::Cdr(node) => fmt::Display::fmt(node, f),
            SequenceNode::Cons(node) => fmt::Display::fmt(node, f),
            SequenceNode::Collect(node) => fmt::Display::fmt(node, f),
            SequenceNode::IsList(node) => fmt::Display::fmt(node, f),
            SequenceNode::List(node) => fmt::Display::fmt(node, f),
            SequenceNode::IsPair(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for SequenceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SequenceNode::Apply(node) => fmt::Debug::fmt(node, f),
            SequenceNode::Car(node) => fmt::Debug::fmt(node, f),
            SequenceNode::Cdr(node) => fmt::Debug::fmt(node, f),
            SequenceNode::Cons(node) => fmt::Debug::fmt(node, f),
            SequenceNode::Collect(node) => fmt::Debug::fmt(node, f),
            SequenceNode::IsList(node) => fmt::Debug::fmt(node, f),
            SequenceNode::List(node) => fmt::Debug::fmt(node, f),
            SequenceNode::IsPair(node) => fmt::Debug::fmt(node, f),
        }
    }
}
