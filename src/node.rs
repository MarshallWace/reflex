// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{operation::evaluate::Evaluate, store::Store, expression::Expression};

use self::{abs::AbsNode, add::AddNode};

pub mod abs;
pub mod add;

#[derive(Debug, PartialEq)]
pub enum Node {
    Abs(self::abs::AbsNode),
    Add(self::add::AddNode),
}
impl Node {
    pub fn evaluate(&self, store: &Store) -> Expression {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, store),
            Node::Add(node) => Evaluate::evaluate(node, store),
        }
    }
    pub fn new(type_name: &str, args: Vec<Expression>) -> Result<Node, &'static str> {
        match type_name {
            "abs" => {
                if args.len() != 1 { return Err("Invalid number of arguments"); }
                let args = &mut args.into_iter();
                let target = args.next().unwrap();
                return Ok(Node::Abs(AbsNode::new(target)))
            },
            "add" => {
                if args.len() != 2 { return Err("Invalid number of arguments"); }
                let args = &mut args.into_iter();
                let left = args.next().unwrap();
                let right = args.next().unwrap();
                Ok(Node::Add(AddNode::new(left, right)))
            },
            _ => Err("Unknown node type")
        }
    }
}
