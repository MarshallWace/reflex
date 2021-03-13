// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{AstNodePackage, EvaluationResult, Expression, NodeFactoryResult, NodeType},
    hash::prefix_hash,
};

mod evaluate;
pub use evaluate::{Evaluate1, Evaluate2};

mod lexer;
pub mod parser;

pub mod arithmetic;
pub mod core;
pub mod logic;
pub mod sequence;

use self::arithmetic::ArithmeticNode;
use self::core::CoreNode;
use self::logic::LogicNode;
use self::sequence::SequenceNode;

#[derive(PartialEq, Clone)]
pub enum Node {
    Core(CoreNode),
    Logic(LogicNode),
    Sequence(SequenceNode),
    Arithmetic(ArithmeticNode),
}
impl AstNodePackage<Node> for Node {
    fn factory(type_name: &str, args: &[Expression<Node>]) -> Option<NodeFactoryResult<Self>> {
        None.or_else(|| wrap(CoreNode::factory, Self::Core)(type_name, args))
            .or_else(|| wrap(LogicNode::factory, Self::Logic)(type_name, args))
            .or_else(|| wrap(SequenceNode::factory, Self::Sequence)(type_name, args))
            .or_else(|| wrap(ArithmeticNode::factory, Self::Arithmetic)(type_name, args))
    }
}
impl NodeType<Node> for Node {
    fn hash(&self) -> u32 {
        match self {
            Self::Core(node) => prefix_hash(0, node.hash()),
            Self::Logic(node) => prefix_hash(0, node.hash()),
            Self::Sequence(node) => prefix_hash(0, node.hash()),
            Self::Arithmetic(node) => prefix_hash(0, node.hash()),
        }
    }
    fn capture_depth(&self) -> usize {
        match self {
            Self::Core(node) => node.capture_depth(),
            Self::Logic(node) => node.capture_depth(),
            Self::Sequence(node) => node.capture_depth(),
            Self::Arithmetic(node) => node.capture_depth(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<EvaluationResult<Node>> {
        match self {
            Self::Core(node) => node.evaluate(env),
            Self::Logic(node) => node.evaluate(env),
            Self::Sequence(node) => node.evaluate(env),
            Self::Arithmetic(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Core(node) => fmt::Display::fmt(node, f),
            Self::Logic(node) => fmt::Display::fmt(node, f),
            Self::Sequence(node) => fmt::Display::fmt(node, f),
            Self::Arithmetic(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Core(node) => fmt::Debug::fmt(node, f),
            Self::Logic(node) => fmt::Debug::fmt(node, f),
            Self::Sequence(node) => fmt::Debug::fmt(node, f),
            Self::Arithmetic(node) => fmt::Debug::fmt(node, f),
        }
    }
}

fn wrap<
    T: NodeType<T>,
    V: NodeType<T>,
    F: Fn(&str, &[Expression<T>]) -> Option<NodeFactoryResult<V>>,
    W: Fn(V) -> T,
>(
    factory: F,
    wrapper: W,
) -> impl Fn(&str, &[Expression<T>]) -> Option<NodeFactoryResult<T>> {
    move |type_name, args| match factory(type_name, args) {
        Some(Ok(result)) => Some(Ok(wrapper(result))),
        Some(Err(err)) => Some(Err(err)),
        None => None,
    }
}
