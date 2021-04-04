// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashMap, fmt, rc::Rc};

use crate::{
    env::Env,
    hash::hash_sequence,
    node::Node,
    signal::{Signal, SignalResult},
};

pub trait NodeType<T: NodeType<T>>: PartialEq + Clone + fmt::Display + fmt::Debug {
    fn hash(&self) -> u32;
    fn capture_depth(&self) -> usize;
    fn evaluate(&self, env: &Env<T>, state: &RuntimeState<T>) -> Option<EvaluationResult<T>>;
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

pub trait CompoundNode<'a, T: NodeType<T> + 'a> {
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
    pub fn evaluate(&self, env: &Env<T>, state: &RuntimeState<T>) -> EvaluationResult<T> {
        self.value()
            .evaluate(env, state)
            .unwrap_or_else(|| EvaluationResult::new(Expression::clone(self)))
    }
    pub fn capture_depth(&self) -> usize {
        self.capture_depth
    }
}
impl<T: NodeType<T>> fmt::Debug for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.value, f)
    }
}
impl<T: NodeType<T>> fmt::Display for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EvaluationResult<T: NodeType<T>> {
    result: EvaluationResultType<T>,
    dependencies: DependencyList,
}
impl<T: NodeType<T>> EvaluationResult<T> {
    pub fn from(result: EvaluationResultType<T>, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result,
            dependencies,
        }
    }
    pub fn new(expression: Expression<T>) -> Self {
        EvaluationResult {
            result: EvaluationResultType::Expression(expression),
            dependencies: DependencyList::new(),
        }
    }
    pub fn with_dependencies(expression: Expression<T>, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result: EvaluationResultType::Expression(expression),
            dependencies,
        }
    }
    pub fn with_dependency(expression: Expression<T>, key: StateToken) -> Self {
        EvaluationResult {
            result: EvaluationResultType::Expression(expression),
            dependencies: DependencyList::of(key),
        }
    }
    pub fn signal_with_dependencies(signal: SignalResult<T>, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result: EvaluationResultType::Signal(signal),
            dependencies,
        }
    }
    pub fn signal(signal: Signal<T>) -> Self {
        EvaluationResult::signal_with_dependencies(
            SignalResult::Single(signal),
            DependencyList::new(),
        )
    }
    pub fn add_dependency(self, key: StateToken) -> Self {
        EvaluationResult {
            result: self.result,
            dependencies: self.dependencies.append(key),
        }
    }
    pub fn add_dependencies(self, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result: self.result,
            dependencies: self.dependencies.extend(dependencies),
        }
    }
    pub fn unwrap(self) -> (EvaluationResultType<T>, DependencyList) {
        (self.result, self.dependencies)
    }
}
impl<T: NodeType<T>> fmt::Display for EvaluationResult<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.result, f)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvaluationResultType<T: NodeType<T>> {
    Signal(SignalResult<T>),
    Expression(Expression<T>),
}
impl<T: NodeType<T>> fmt::Display for EvaluationResultType<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvaluationResultType::Expression(expression) => fmt::Display::fmt(expression, f),
            EvaluationResultType::Signal(signal) => fmt::Display::fmt(signal, f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DependencyList(Option<Vec<StateToken>>);
impl DependencyList {
    pub fn new() -> Self {
        DependencyList(None)
    }
    pub fn of(key: StateToken) -> Self {
        DependencyList(Some(vec![key]))
    }
    pub fn append(self, key: StateToken) -> Self {
        match self.0 {
            None => DependencyList::of(key),
            Some(mut dependencies) => {
                dependencies.push(key);
                DependencyList(Some(dependencies))
            }
        }
    }
    pub fn extend(self, target: Self) -> Self {
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

pub type StateToken = u32;

pub struct RuntimeState<T: NodeType<T>> {
    state: HashMap<StateToken, Expression<T>>,
}
impl<T: NodeType<T>> RuntimeState<T> {
    pub fn new() -> Self {
        RuntimeState {
            state: HashMap::new(),
        }
    }
    pub fn get(&self, key: StateToken) -> Option<Expression<T>> {
        let value = self.state.get(&key)?;
        Some(Expression::clone(value))
    }
    pub fn set(&mut self, key: StateToken, value: Expression<T>) {
        self.state.insert(key, value);
    }
}

pub trait EvaluateDependencies<'a, T: NodeType<T> + 'a> {
    type Dependencies: Iterator<Item = &'a Expression<T>>;
    fn dependencies(&'a self) -> Self::Dependencies;
    fn run(
        &self,
        deps: &[Expression<T>],
        env: &Env<T>,
        state: &RuntimeState<T>,
    ) -> EvaluationResult<T>;
    fn evaluate(&'a self, env: &Env<T>, state: &RuntimeState<T>) -> Option<EvaluationResult<T>> {
        let deps = self.dependencies();
        let (results, dependencies) = deps.into_iter().fold(
            (Ok(Vec::new()), DependencyList::new()),
            |(combined_results, combined_dependencies): (
                Result<Vec<Expression<T>>, SignalResult<T>>,
                DependencyList,
            ),
             dep| {
                let (result, dependencies) = dep.evaluate(env, state).unwrap();
                let combined_results = match combined_results {
                    Err(combined_signal) => Err(match result {
                        EvaluationResultType::Signal(signal) => combined_signal.combine(signal),
                        _ => combined_signal,
                    }),
                    Ok(mut results) => match result {
                        EvaluationResultType::Signal(signal) => Err(signal),
                        EvaluationResultType::Expression(expression) => {
                            results.push(expression);
                            Ok(results)
                        }
                    },
                };
                let combined_dependencies = combined_dependencies.extend(dependencies);
                (combined_results, combined_dependencies)
            },
        );
        Some(match results {
            Err(signal) => EvaluationResult::signal_with_dependencies(signal, dependencies),
            Ok(values) => self
                .run(&values[..], env, state)
                .add_dependencies(dependencies),
        })
    }
}
