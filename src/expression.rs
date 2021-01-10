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
    Reference(EnvPath),
    Function(Function),
    Node(Node),
}

type EnvPath = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub arity: usize,
    pub body: Rc<Expression>,
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({} -> {})", self.arity, self.body)
    }
}

impl Evaluate for Rc<Expression> {
    fn evaluate(&self, env: &Env) -> Rc<Expression> {
        match &**self {
            Expression::Reference(index) => match env.get(*index) {
                Some(value) => value.evaluate(env),
                None => panic!("Invalid reference target"),
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
            Expression::Function(value) => write!(f, "{:?}", value),
            Expression::Reference(node) => write!(f, "Reference({})", node),
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
            Expression::Function(value) => write!(f, "{}", value),
            Expression::Reference(value) => write!(f, "<env:{}>", value),
            Expression::Node(value) => write!(f, "{:?}", value),
        }
    }
}
