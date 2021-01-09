// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{env::Env, node::Node, operation::evaluate::Evaluate, value::Value};

#[derive(PartialEq,Clone)]
pub enum Expression {
    Pending,
    Error(String),
    Value(Value),
    Node(Node),
}
impl Evaluate for Rc<Expression> {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match &**self {
            Expression::Pending => Rc::clone(self),
            Expression::Error(_) => Rc::clone(self),
            Expression::Value(_) => Rc::clone(self),
            Expression::Node(node) => node.evaluate(env).unwrap_or_else(|| Rc::clone(self)),
        }
    }
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Pending => write!(f, "{}", "Pending"),
            Expression::Error(message) => write!(f, "Error({:?})", message),
            Expression::Value(value) => write!(f, "{:?}", value),
            Expression::Node(value) => write!(f, "{:?}", value),
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
