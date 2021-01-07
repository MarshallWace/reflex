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
pub enum NodeFactoryResult {
    Some(Node),
    None(Vec<Expression>)
}
impl Node {
    pub fn evaluate(&self, env: &Env) -> Expression {
        match self {
            Node::Abs(node) => Evaluate::evaluate(node, env),
            Node::Add(node) => Evaluate::evaluate(node, env),
        }
    }
    pub fn new(type_name: &str, args: Vec<Expression>) -> Result<NodeFactoryResult, &'static str> {
        match type_name {
            "abs" => {
                if args.len() != 1 {
                    return Err("Invalid number of arguments");
                }
                let args = &mut args.into_iter();
                let target = args.next().unwrap();
                Ok(NodeFactoryResult::Some(Node::Abs(AbsNode::new(target))))
            }
            "add" => {
                if args.len() != 2 {
                    return Err("Invalid number of arguments");
                }
                let args = &mut args.into_iter();
                let left = args.next().unwrap();
                let right = args.next().unwrap();
                Ok(NodeFactoryResult::Some(Node::Add(AddNode::new(left, right))))
            },
            _ => Ok(NodeFactoryResult::None(args)),
        }
    }
}
