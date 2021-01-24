// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::rc::Rc;

use crate::{
    env::Env,
    expression::{Evaluate, Expression},
};

pub mod abs;
pub mod add;
pub mod and;
pub mod apply;
pub mod get;
pub mod _if;
pub mod not;
pub mod or;

pub use abs::AbsNode;
pub use add::AddNode;
pub use and::AndNode;
pub use apply::ApplyNode;
pub use get::GetNode;
pub use _if::IfNode;
pub use not::NotNode;
pub use or::OrNode;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Abs(AbsNode),
    Add(AddNode),
    And(AndNode),
    Apply(ApplyNode),
    Get(GetNode),
    If(IfNode),
    Not(NotNode),
    Or(OrNode),
}

const ABS: &str = AbsNode::name();
const ADD: &str = AddNode::name();
const AND: &str = AndNode::name();
const APPLY: &str = ApplyNode::name();
const GET: &str = GetNode::name();
const IF: &str = IfNode::name();
const NOT: &str = NotNode::name();
const OR: &str = OrNode::name();

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
            AND => AndNode::factory(args).map(Some),
            APPLY => ApplyNode::factory(args).map(Some),
            GET => GetNode::factory(args).map(Some),
            IF => IfNode::factory(args).map(Some),
            NOT => NotNode::factory(args).map(Some),
            OR => OrNode::factory(args).map(Some),
            _ => Ok(None),
        }
    }
    pub fn children(&self) -> Option<Vec<&Rc<Expression>>> {
        match self {
            Node::Abs(node) => Some(CompoundNode::expressions(node)),
            Node::Add(node) => Some(CompoundNode::expressions(node)),
            Node::And(node) => Some(CompoundNode::expressions(node)),
            Node::Apply(node) => Some(CompoundNode::expressions(node)),
            Node::Get(node) => Some(CompoundNode::expressions(node)),
            Node::If(node) => Some(CompoundNode::expressions(node)),
            Node::Not(node) => Some(CompoundNode::expressions(node)),
            Node::Or(node) => Some(CompoundNode::expressions(node)),
        }
    }
    pub fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, env),
            Node::Add(node) => Evaluate::evaluate(node, env),
            Node::And(node) => Evaluate::evaluate(node, env),
            Node::Apply(node) => Evaluate::evaluate(node, env),
            Node::Get(node) => Evaluate::evaluate(node, env),
            Node::If(node) => Evaluate::evaluate(node, env),
            Node::Not(node) => Evaluate::evaluate(node, env),
            Node::Or(node) => Evaluate::evaluate(node, env),
        }
    }
}
