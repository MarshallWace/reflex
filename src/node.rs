// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::{Evaluate, Expression},
};

use self::{abs::AbsNode, add::AddNode, apply::ApplyNode};

pub mod abs;
pub mod add;
pub mod apply;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Abs(self::abs::AbsNode),
    Add(self::add::AddNode),
    Apply(self::apply::ApplyNode),
}

const ABS: &str = AbsNode::name();
const ADD: &str = AddNode::name();
const APPLY: &str = ApplyNode::name();

pub type NodeFactoryResult = Result<Node, String>;

pub trait CompoundNode {
    fn factory(args: Vec<Rc<Expression>>) -> NodeFactoryResult;
    fn expressions(&self) -> Vec<&Rc<Expression>>;
}

impl Node {
    pub fn factory(type_name: &str, args: Vec<Rc<Expression>>) -> Result<Option<Node>, String> {
        match type_name {
            ABS => AbsNode::factory(args).map(Some),
            ADD => AddNode::factory(args).map(Some),
            APPLY => ApplyNode::factory(args).map(Some),
            _ => Ok(None),
        }
    }
    pub fn children(&self) -> Option<Vec<&Rc<Expression>>> {
        match self {
            Node::Abs(node) => Some(CompoundNode::expressions(node)),
            Node::Add(node) => Some(CompoundNode::expressions(node)),
            Node::Apply(node) => Some(CompoundNode::expressions(node)),
        }
    }
    pub fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, env),
            Node::Add(node) => Evaluate::evaluate(node, env),
            Node::Apply(node) => Evaluate::evaluate(node, env),
        }
    }
}
