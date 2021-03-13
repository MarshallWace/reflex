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

#[derive(Debug, PartialEq, Clone)]
pub struct EvaluationResult<T: NodeType<T>> {
    pub expression: Expression<T>,
    pub dependencies: Option<Vec<StateToken>>,
}
impl<T: NodeType<T>> EvaluationResult<T> {
    pub fn new(expression: Expression<T>, dependencies: Option<Vec<StateToken>>) -> Self {
        EvaluationResult {
            expression,
            dependencies,
        }
    }
}

pub type StateToken = usize;

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
            .unwrap_or_else(|| EvaluationResult::new(Expression::clone(self), None))
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

pub fn with_dependencies<T: NodeType<T>>(
    base_dependencies: Option<Vec<usize>>,
    result: EvaluationResult<T>,
) -> EvaluationResult<T> {
    match base_dependencies {
        None => result,
        Some(_) => EvaluationResult::new(
            result.expression,
            combine_dependency_lists(base_dependencies, result.dependencies),
        ),
    }
}

pub fn combine_dependency_lists(
    base_dependencies: Option<Vec<usize>>,
    extra_dependencies: Option<Vec<usize>>,
) -> Option<Vec<usize>> {
    match base_dependencies {
        None => extra_dependencies,
        Some(mut base_dependencies) => match extra_dependencies {
            None => Some(base_dependencies),
            Some(extra_dependencies_items) => {
                base_dependencies.extend(extra_dependencies_items);
                Some(base_dependencies)
            }
        },
    }
}
