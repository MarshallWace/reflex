// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::env::Env;

pub trait NodeType<T: NodeType<T>>: PartialEq + Clone + fmt::Display + fmt::Debug {
    fn expressions(&self) -> Vec<&Expression<T>>;
    fn is_static(&self) -> bool {
        let expressions = self.expressions();
        expressions.is_empty() || expressions.iter().all(|child| child.value().is_static())
    }
    fn evaluate(&self, env: &Env<T>) -> Option<Expression<T>>;
}

pub type NodeFactory<T, V> = dyn Fn(&Vec<Expression<T>>) -> Result<V, String>;
pub type NodeFactoryError = String;
pub type NodeFactoryResult<T> = Result<T, NodeFactoryError>;

pub trait AstNodePackage<T: NodeType<T>>: NodeType<T> {
    fn factory(type_name: &str, args: &Vec<Expression<T>>) -> Option<NodeFactoryResult<Self>>;
}

pub trait AstNode<T: NodeType<T>>: NodeType<T> {
    fn factory(args: &Vec<Expression<T>>) -> NodeFactoryResult<Self>;
}

#[derive(PartialEq, Clone)]
pub struct Expression<T: NodeType<T>> {
    value: Rc<T>,
}
impl<T: NodeType<T>> Expression<T> {
    pub fn new(value: T) -> Self {
        Expression {
            value: Rc::new(value),
        }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
    pub fn clone(&self) -> Self {
        Expression {
            value: Rc::clone(&self.value),
        }
    }
    pub fn evaluate(&self, env: &Env<T>) -> Expression<T> {
        self.value()
            .evaluate(env)
            .unwrap_or_else(|| Expression::clone(self))
    }
}
impl<T: NodeType<T>> fmt::Debug for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
impl<T: NodeType<T>> fmt::Display for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:}", self.value)
    }
}
