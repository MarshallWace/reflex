// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{operation::evaluate::Evaluate, store::Store, types::Expression};

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
}
