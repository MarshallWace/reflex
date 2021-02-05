// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{
    env::Env,
    expression::{Expression, NodeType},
};

mod evaluate;
pub use evaluate::{Evaluate1, Evaluate2};

mod parser;

pub mod arithmetic;
pub mod core;
pub mod logic;

#[derive(PartialEq, Clone)]
pub enum Node {
    Core(core::CoreNode),
    Logic(logic::LogicNode),
    Arithmetic(arithmetic::ArithmeticNode),
}
impl Node {
    pub fn factory(type_name: &str, args: &Vec<Expression<Node>>) -> Result<Option<Node>, String> {
        let result = core::CoreNode::factory(type_name, args)?;
        if result.is_some() {
            return Ok(result.map(Node::Core));
        }

        let result = logic::LogicNode::factory(type_name, args)?;
        if result.is_some() {
            return Ok(result.map(Node::Logic));
        }

        let result = arithmetic::ArithmeticNode::factory(type_name, args)?;
        if result.is_some() {
            return Ok(result.map(Node::Arithmetic));
        }

        return Ok(None);
    }
    pub fn parse(input: &str) -> Result<Expression<Node>, String> {
        parser::parse(input)
    }
}
impl NodeType<Node> for Node {
    fn expressions(&self) -> Vec<&Expression<Node>> {
        match self {
            Node::Core(node) => node.expressions(),
            Node::Logic(node) => node.expressions(),
            Node::Arithmetic(node) => node.expressions(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Node::Core(node) => node.is_static(),
            Node::Logic(node) => node.is_static(),
            Node::Arithmetic(node) => node.is_static(),
        }
    }
    fn evaluate(&self, env: &Env<Node>) -> Option<Expression<Node>> {
        match self {
            Node::Core(node) => node.evaluate(env),
            Node::Logic(node) => node.evaluate(env),
            Node::Arithmetic(node) => node.evaluate(env),
        }
    }
}
impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Core(node) => fmt::Display::fmt(node, f),
            Node::Logic(node) => fmt::Display::fmt(node, f),
            Node::Arithmetic(node) => fmt::Display::fmt(node, f),
        }
    }
}
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Core(node) => fmt::Debug::fmt(node, f),
            Node::Logic(node) => fmt::Debug::fmt(node, f),
            Node::Arithmetic(node) => fmt::Debug::fmt(node, f),
        }
    }
}
