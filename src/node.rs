// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{expression::Expression, operation::evaluate::Evaluate, env::Env};

use self::{abs::AbsNode, add::AddNode};

pub mod abs;
pub mod add;

#[derive(Debug, PartialEq)]
pub enum Node {
    Abs(self::abs::AbsNode),
    Add(self::add::AddNode),
}

const ABS: &str = AbsNode::name();
const ADD: &str = AddNode::name();

pub enum NodeFactoryResult {
    Some(Node),
    None(Vec<Expression>),
    Err(String),
}

impl Node {
    pub fn evaluate(&self, env: &Env) -> Expression {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, env),
            Node::Add(node) => Evaluate::evaluate(node, env),
        }
    }
    pub fn factory(type_name: &str, args: Vec<Expression>) -> NodeFactoryResult {
        match type_name {
            ABS => AbsNode::factory(args),
            ADD => AddNode::factory(args),
            _ => NodeFactoryResult::None(args),
        }
    }
}
