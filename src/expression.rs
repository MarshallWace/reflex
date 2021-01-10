// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{env::Env, node::Node, operation::evaluate::Evaluate, value::Value};

#[derive(PartialEq, Clone)]
pub enum Expression {
    Pending,
    Error(String),
    Value(Value),
    Reference(String),
    Function(Vec<String>, Rc<Expression>),
    Node(Node),
}
impl Evaluate for Rc<Expression> {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match &**self {
            Expression::Reference(key) => match env.get(&key) {
                Some(value) => value.evaluate(env),
                None => Rc::new(Expression::Error(format!("Invalid reference: {}", key))),
            },
            Expression::Node(node) => node.evaluate(env),
            _ => Rc::clone(self),
        }
    }
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Pending => write!(f, "{}", "Pending"),
            Expression::Error(message) => write!(f, "Error({:?})", message),
            Expression::Value(value) => write!(f, "{:?}", value),
            Expression::Function(args, body) => {
                write!(f, "Function({} -> {})", args.join(" "), body)
            }
            Expression::Reference(node) => write!(f, "Reference({:?})", node),
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
            Expression::Function(args, body) => {
                write!(f, "Function({} -> {:?})", args.join(" "), body)
            }
            Expression::Reference(value) => write!(f, "{}", value),
            Expression::Node(value) => write!(f, "{:?}", value),
        }
    }
}
