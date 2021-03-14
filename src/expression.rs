// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{fmt, rc::Rc};

use crate::{env::Env, hash::hash_sequence, node::Node};

pub trait NodeType<T: NodeType<T>>: PartialEq + Clone + fmt::Display + fmt::Debug {
    fn hash(&self) -> u32;
    fn capture_depth(&self) -> usize;
    fn evaluate(&self, env: &Env<T>) -> Option<EvaluationResult<T>>;
}

pub trait CompoundNode<'a> {
    type Expressions: Iterator<Item = &'a Expression<Node>>;
    fn expressions(&'a self) -> Self::Expressions;
    fn hash(&'a self) -> u32 {
        hash_sequence(self.expressions().map(|expression| expression.hash()))
    }
    fn capture_depth(&'a self) -> usize {
        self.expressions()
            .fold(0, |acc, child| acc.max(child.capture_depth()))
    }
}

pub type NodeFactory<T, V> = dyn Fn(&[Expression<T>]) -> Result<V, String>;
pub type NodeFactoryError = String;
pub type NodeFactoryResult<T> = Result<T, NodeFactoryError>;

pub trait AstNodePackage<T: NodeType<T>>: NodeType<T> {
    fn factory(type_name: &str, args: &[Expression<T>]) -> Option<NodeFactoryResult<Self>>;
}

pub trait AstNode<T: NodeType<T>>: NodeType<T> {
    fn factory(args: &[Expression<T>]) -> NodeFactoryResult<Self>;
}

#[derive(PartialEq, Clone)]
pub struct Expression<T: NodeType<T>> {
    value: Rc<T>,
    hash: u32,
    capture_depth: usize,
}
impl<T: NodeType<T>> Expression<T> {
    pub fn new(value: T) -> Self {
        let hash = value.hash();
        let capture_depth = value.capture_depth();
        Expression {
            value: Rc::new(value),
            hash,
            capture_depth,
        }
    }
    pub fn hash(&self) -> u32 {
        self.hash
    }
    pub fn value(&self) -> &T {
        &self.value
    }
    pub fn evaluate(&self, env: &Env<T>) -> EvaluationResult<T> {
        self.value()
            .evaluate(env)
            .unwrap_or_else(|| EvaluationResult::new(Expression::clone(self)))
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

pub type StateToken = u32;

#[derive(Debug, PartialEq, Clone)]
pub struct EvaluationResult<T: NodeType<T>> {
    expression: Expression<T>,
    dependencies: DependencyList,
}
impl<T: NodeType<T>> EvaluationResult<T> {
    pub fn new(expression: Expression<T>) -> Self {
        EvaluationResult {
            expression,
            dependencies: DependencyList::new(),
        }
    }
    pub fn with_dependencies_from(self, other: EvaluationResult<T>) -> Self {
        EvaluationResult {
            expression: self.expression,
            dependencies: self.dependencies.extend(other.dependencies),
        }
    }
    pub fn expression(&self) -> &Expression<T> {
        &self.expression
    }
    pub fn dependencies(&self) -> &DependencyList {
        &self.dependencies
    }
    pub fn value(&self) -> &T {
        self.expression.value()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DependencyList(Option<Vec<StateToken>>);
impl DependencyList {
    pub fn new() -> Self {
        DependencyList(None)
    }
    pub fn extend(self, target: DependencyList) -> Self {
        match self.0 {
            None => target,
            Some(mut base_dependencies) => match target.0 {
                None => DependencyList(Some(base_dependencies)),
                Some(target_dependencies) => {
                    base_dependencies.extend(target_dependencies);
                    DependencyList(Some(base_dependencies))
                }
            },
        }
    }
}
