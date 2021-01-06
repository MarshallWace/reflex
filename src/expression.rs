// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::fmt;

use crate::{node::Node, store::Store, value::Value};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Pending,
    Error(String),
    Value(Value),
    Node(Node),
}
impl Expression {
    pub fn evaluate(&self, store: &Store) -> Expression {
        match self {
            Expression::Pending => Expression::Pending,
            Expression::Error(message) => Expression::Error(message.to_owned()),
            Expression::Value(value) => Expression::Value(value.clone()),
            Expression::Node(node) => node.evaluate(store),
        }
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Pending => write!(f, "{}", "<pending>"),
            Expression::Error(message) => write!(f, "Error: {}", message),
            Expression::Value(value) => write!(f, "{}", value),
            Expression::Node(value) => write!(f, "{:?}", value),
        }
    }
}
