// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::env::Env;

pub trait NodeType<T: NodeType<T>>: PartialEq + Clone + fmt::Display + fmt::Debug {
    fn expressions(&self) -> Vec<&Expression<T>>;
    fn capture_depth(&self) -> usize {
        self.expressions()
            .iter()
            .fold(0, |acc, child| acc.max(child.capture_depth()))
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
    capture_depth: usize,
}
impl<T: NodeType<T>> Expression<T> {
    pub fn new(value: T) -> Self {
        let capture_depth = value.capture_depth();
        Expression {
            value: Rc::new(value),
            capture_depth,
        }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
    pub fn clone(&self) -> Self {
        Expression {
            value: Rc::clone(&self.value),
            capture_depth: self.capture_depth,
        }
    }
    pub fn evaluate(&self, env: &Env<T>) -> Expression<T> {
        self.value()
            .evaluate(env)
            .unwrap_or_else(|| Expression::clone(self))
    }
    pub fn capture_depth(&self) -> usize {
        self.capture_depth
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
