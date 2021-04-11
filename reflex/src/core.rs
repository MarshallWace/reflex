// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, HashSet},
    fmt,
    iter::once,
    rc::Rc,
};

use crate::{
    hash::{
        combine_hashes, hash_bytes, hash_sequence, hash_u32, hash_unordered_sequence, prefix_hash,
        HashId, Hashable,
    },
    stdlib::{
        builtin::BuiltinTerm, collection::CollectionTerm, signal::SignalType, value::ValueTerm,
    },
};

pub trait NativeFunction {
    fn arity() -> Arity;
    fn apply(args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression;
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum VarArgs {
    Eager,
    Lazy,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Arity {
    eager: u8,
    lazy: u8,
    variadic: Option<VarArgs>,
}
impl Hashable for Arity {
    fn hash(&self) -> HashId {
        hash_bytes(&[
            self.eager,
            self.lazy,
            match self.variadic {
                None => 0,
                Some(var_args) => match var_args {
                    VarArgs::Eager => 1,
                    VarArgs::Lazy => 2,
                },
            },
        ])
    }
}
impl Arity {
    pub fn from(eager: u8, lazy: u8, variadic: Option<VarArgs>) -> Self {
        Self {
            eager,
            lazy,
            variadic,
        }
    }
    fn eager(&self) -> usize {
        self.eager as usize
    }
    fn required(&self) -> usize {
        (self.eager + self.lazy) as usize
    }
}
impl fmt::Display for Arity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.eager, self.lazy)
    }
}

pub(crate) trait Rewritable {
    fn capture_depth(&self) -> StackOffset;
    fn dynamic_dependencies(&self) -> DependencyList;
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression>;
    fn optimize(&self) -> Option<Expression>;
}

pub(crate) trait Reducible {
    fn reduce(&self) -> Option<Expression>;
}

pub(crate) enum Substitutions<'a> {
    Static(StaticSubstitutions<'a>),
    Dynamic(&'a DynamicState),
}

pub(crate) enum StaticSubstitutions<'a> {
    Some(TargetedStaticSubstitutions<'a>),
    All(WildcardStaticSubstitutions),
}
impl<'a> StaticSubstitutions<'a> {
    pub fn new(substitutions: &'a [(StackOffset, Expression)]) -> Self {
        Self::Some(TargetedStaticSubstitutions::new(substitutions))
    }
    pub fn all(offset: StackOffset) -> Self {
        Self::All(WildcardStaticSubstitutions::new(offset))
    }
    fn offset(&self, offset: StackOffset) -> Self {
        match self {
            Self::Some(substitutions) => Self::Some(substitutions.offset(offset)),
            Self::All(substitutions) => Self::All(substitutions.offset(offset)),
        }
    }
    fn can_skip(&self, expression: &Expression) -> bool {
        match self {
            Self::Some(substitutions) => substitutions.can_skip(expression),
            Self::All(substitutions) => substitutions.can_skip(expression),
        }
    }
    fn get(&self, offset: StackOffset) -> Option<Expression> {
        match self {
            Self::Some(substitutions) => substitutions.get(offset),
            Self::All(substitutions) => substitutions.get(offset),
        }
    }
}

pub(crate) struct TargetedStaticSubstitutions<'a> {
    substitutions: &'a [(StackOffset, Expression)],
    min_depth: StackOffset,
    offset: StackOffset,
}
impl<'a> TargetedStaticSubstitutions<'a> {
    pub fn new(substitutions: &'a [(StackOffset, Expression)]) -> Self {
        Self {
            substitutions,
            min_depth: substitutions
                .iter()
                .fold(StackOffset::MAX, |acc, (offset, _)| acc.min(*offset)),
            offset: 0,
        }
    }
    fn offset(&self, offset: StackOffset) -> Self {
        Self {
            substitutions: self.substitutions,
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    fn can_skip(&self, expression: &Expression) -> bool {
        let capture_depth = expression.capture_depth();
        capture_depth == 0 || (capture_depth - 1 < self.min_depth + self.offset)
    }
    fn get(&self, offset: StackOffset) -> Option<Expression> {
        if offset < self.offset {
            return None;
        }
        let target_offset = offset - self.offset;
        self.substitutions
            .iter()
            .find(|(offset, _)| (*offset == target_offset))
            .map(|(_, expression)| {
                if expression.capture_depth() > 0 {
                    expression
                        .substitute(&Substitutions::Static(StaticSubstitutions::all(
                            self.offset,
                        )))
                        .unwrap_or_else(|| Expression::clone(expression))
                } else {
                    Expression::clone(expression)
                }
            })
    }
}

pub(crate) struct WildcardStaticSubstitutions {
    offset: StackOffset,
}
impl WildcardStaticSubstitutions {
    pub fn new(offset: StackOffset) -> Self {
        Self { offset }
    }
    pub fn offset(&self, offset: StackOffset) -> Self {
        Self {
            offset: self.offset + offset,
        }
    }
    pub fn can_skip(&self, expression: &Expression) -> bool {
        expression.capture_depth() == 0
    }
    pub fn get(&self, offset: StackOffset) -> Option<Expression> {
        Some(Expression::new(Term::Variable(VariableTerm::Static(
            StaticVariableTerm::new(self.offset + offset),
        ))))
    }
}

#[derive(PartialEq, Clone)]
pub struct Expression {
    value: Rc<Term>,
    hash: HashId,
    capture_depth: StackOffset,
    is_reduced: bool,
    is_optimized: bool,
    dynamic_dependencies: DependencyList,
}
impl Hashable for Expression {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl Expression {
    pub fn new(value: Term) -> Self {
        let is_reducible = value.is_reducible();
        let is_reduced = !is_reducible;
        let is_optimized = false;
        Self::create(value, is_reduced, is_optimized)
    }
    pub fn value(&self) -> &Term {
        &self.value
    }
    pub fn compile(&self) -> Expression {
        self.optimize().unwrap_or_else(|| Expression::clone(self))
    }
    pub fn evaluate(&self, state: &DynamicState) -> EvaluationResult {
        let substitutions = Substitutions::Dynamic(state);
        let (result, dependencies) = evaluate_recursive(
            Expression::clone(self),
            &substitutions,
            DependencyList::empty(),
        );
        let result = match result.value() {
            Term::Signal(signal) => Err(signal.flatten()),
            _ => Ok(result),
        };
        EvaluationResult::new(result, dependencies)
    }
    fn create(value: Term, is_reduced: bool, is_optimized: bool) -> Self {
        let hash = value.hash();
        let capture_depth = value.capture_depth();
        let dynamic_dependencies = value.dynamic_dependencies();
        Self {
            value: Rc::new(value),
            hash,
            capture_depth,
            dynamic_dependencies,
            is_reduced,
            is_optimized,
        }
    }
    fn reduced(existing: &Expression) -> Self {
        Self {
            value: Rc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            is_reduced: true,
            is_optimized: existing.is_optimized,
        }
    }
    fn optimized(existing: &Expression) -> Self {
        Self {
            value: Rc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            is_reduced: existing.is_reduced,
            is_optimized: true,
        }
    }
}
impl Rewritable for Expression {
    fn capture_depth(&self) -> StackOffset {
        self.capture_depth
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::clone(&self.dynamic_dependencies)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        let can_skip = match substitutions {
            Substitutions::Static(substitutions) => substitutions.can_skip(self),
            Substitutions::Dynamic(_) => self.dynamic_dependencies.is_empty(),
        };
        if can_skip {
            None
        } else {
            self.value.substitute(substitutions)
        }
    }
    fn optimize(&self) -> Option<Expression> {
        if self.is_optimized {
            return None;
        }
        Some(
            self.value
                .optimize()
                .unwrap_or_else(|| Self::optimized(self)),
        )
    }
}
impl Reducible for Expression {
    fn reduce(&self) -> Option<Expression> {
        if self.is_reduced {
            return None;
        }
        Some(self.value.reduce().unwrap_or_else(|| Self::reduced(self)))
    }
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.value, f)
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}
fn evaluate_recursive(
    expression: Expression,
    state: &Substitutions,
    dependencies: DependencyList,
) -> (Expression, DependencyList) {
    let dependencies = dependencies.extend(expression.dynamic_dependencies());
    match expression.substitute(state) {
        Some(expression) => evaluate_recursive(expression, state, dependencies),
        None => match expression.reduce() {
            Some(expression) => evaluate_recursive(expression, state, dependencies),
            None => (expression, dependencies),
        },
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluationResult {
    result: Result<Expression, Vec<Signal>>,
    dependencies: DependencyList,
}
impl EvaluationResult {
    pub fn new(result: Result<Expression, Vec<Signal>>, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result,
            dependencies,
        }
    }
    pub fn result(&self) -> Result<&Expression, &Vec<Signal>> {
        self.result.as_ref()
    }
    pub fn dependencies(&self) -> &DependencyList {
        &self.dependencies
    }
    pub fn unwrap(self) -> Result<Expression, Vec<Signal>> {
        self.result
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum EvaluationResultType {
    Expression(Expression),
    Signal(Vec<Signal>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct DependencyList {
    dependencies: Option<DynamicDependencies>,
}
impl DependencyList {
    pub fn empty() -> Self {
        Self { dependencies: None }
    }
    pub fn of(dependencies: DynamicDependencies) -> Self {
        Self {
            dependencies: Some(dependencies),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.dependencies.is_none()
    }
    fn extend(self, other: Self) -> Self {
        match self.dependencies {
            None => self,
            Some(dependencies) => match other.dependencies {
                None => other,
                Some(other) => DependencyList::of(dependencies.union(&other)),
            },
        }
    }
}

pub type EnumIndex = u8;

#[derive(PartialEq, Clone, Debug)]
pub enum Term {
    Value(ValueTerm),
    Variable(VariableTerm),
    Lambda(LambdaTerm),
    Application(ApplicationTerm),
    Recursive(RecursiveTerm),
    Builtin(BuiltinTerm),
    DataStructure(DataStructureTerm),
    Matcher(MatcherTerm),
    Collection(CollectionTerm),
    Signal(SignalTerm),
}
impl Hashable for Term {
    fn hash(&self) -> HashId {
        match self {
            Self::Value(term) => prefix_hash(0, term.hash()),
            Self::Variable(term) => prefix_hash(1, term.hash()),
            Self::Lambda(term) => prefix_hash(2, term.hash()),
            Self::Application(term) => prefix_hash(3, term.hash()),
            Self::Recursive(term) => prefix_hash(4, term.hash()),
            Self::Builtin(term) => prefix_hash(5, term.hash()),
            Self::DataStructure(term) => prefix_hash(6, term.hash()),
            Self::Matcher(term) => prefix_hash(7, term.hash()),
            Self::Collection(term) => prefix_hash(8, term.hash()),
            Self::Signal(term) => prefix_hash(9, term.hash()),
        }
    }
}
impl Rewritable for Term {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Variable(term) => term.capture_depth(),
            Self::Lambda(term) => term.capture_depth(),
            Self::Application(term) => term.capture_depth(),
            Self::Recursive(term) => term.capture_depth(),
            Self::DataStructure(term) => term.capture_depth(),
            Self::Matcher(term) => term.capture_depth(),
            Self::Collection(term) => term.capture_depth(),
            _ => 0,
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Variable(term) => term.dynamic_dependencies(),
            Self::Lambda(term) => term.dynamic_dependencies(),
            Self::Application(term) => term.dynamic_dependencies(),
            Self::Recursive(term) => term.dynamic_dependencies(),
            Self::DataStructure(term) => term.dynamic_dependencies(),
            Self::Matcher(term) => term.dynamic_dependencies(),
            Self::Collection(term) => term.dynamic_dependencies(),
            _ => DependencyList::empty(),
        }
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.substitute(substitutions),
            Self::Lambda(term) => term.substitute(substitutions),
            Self::Application(term) => term.substitute(substitutions),
            Self::Recursive(term) => term.substitute(substitutions),
            Self::DataStructure(term) => term.substitute(substitutions),
            Self::Matcher(term) => term.substitute(substitutions),
            Self::Collection(term) => term.substitute(substitutions),
            _ => None,
        }
    }
    fn optimize(&self) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.optimize(),
            Self::Lambda(term) => term.optimize(),
            Self::Application(term) => term.optimize(),
            Self::Recursive(term) => term.optimize(),
            Self::DataStructure(term) => term.optimize(),
            Self::Matcher(term) => term.optimize(),
            Self::Collection(term) => term.optimize(),
            _ => None,
        }
    }
}
impl Term {
    fn is_reducible(&self) -> bool {
        match self {
            Self::Application(_) => true,
            Self::Recursive(_) => true,
            _ => false,
        }
    }
}
impl Reducible for Term {
    fn reduce(&self) -> Option<Expression> {
        match self {
            Self::Application(term) => term.reduce(),
            Self::Recursive(term) => term.reduce(),
            _ => None,
        }
    }
}
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(term) => fmt::Display::fmt(term, f),
            Self::Variable(term) => fmt::Display::fmt(term, f),
            Self::Lambda(term) => fmt::Display::fmt(term, f),
            Self::Application(term) => fmt::Display::fmt(term, f),
            Self::Recursive(term) => fmt::Display::fmt(term, f),
            Self::Builtin(term) => fmt::Display::fmt(term, f),
            Self::DataStructure(term) => fmt::Display::fmt(term, f),
            Self::Matcher(term) => fmt::Display::fmt(term, f),
            Self::Collection(term) => fmt::Display::fmt(term, f),
            Self::Signal(term) => fmt::Display::fmt(term, f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum VariableTerm {
    Static(StaticVariableTerm),
    Dynamic(DynamicVariableTerm),
}
impl Hashable for VariableTerm {
    fn hash(&self) -> HashId {
        match self {
            Self::Static(term) => prefix_hash(0, term.hash()),
            Self::Dynamic(term) => prefix_hash(1, term.hash()),
        }
    }
}
impl Rewritable for VariableTerm {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Static(term) => term.capture_depth(),
            Self::Dynamic(term) => term.capture_depth(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Static(term) => term.dynamic_dependencies(),
            Self::Dynamic(term) => term.dynamic_dependencies(),
        }
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match self {
            Self::Static(term) => term.substitute(substitutions),
            Self::Dynamic(term) => term.substitute(substitutions),
        }
    }
    fn optimize(&self) -> Option<Expression> {
        match self {
            Self::Static(term) => term.optimize(),
            Self::Dynamic(term) => term.optimize(),
        }
    }
}
impl fmt::Display for VariableTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Static(term) => fmt::Display::fmt(term, f),
            Self::Dynamic(term) => fmt::Display::fmt(term, f),
        }
    }
}

pub type StackOffset = u32;
fn hash_stack_offset(value: StackOffset) -> HashId {
    hash_u32(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct StaticVariableTerm {
    offset: StackOffset,
}
impl Hashable for StaticVariableTerm {
    fn hash(&self) -> HashId {
        hash_stack_offset(self.offset)
    }
}
impl StaticVariableTerm {
    pub fn new(offset: StackOffset) -> Self {
        Self { offset }
    }
    fn offset(&self) -> StackOffset {
        self.offset
    }
}
impl Rewritable for StaticVariableTerm {
    fn capture_depth(&self) -> StackOffset {
        self.offset + 1
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match substitutions {
            Substitutions::Static(substitutions) => substitutions.get(self.offset),
            _ => None,
        }
    }
    fn optimize(&self) -> Option<Expression> {
        None
    }
}
impl fmt::Display for StaticVariableTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<static:{}>", self.offset)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct DynamicVariableTerm {
    id: StateToken,
    fallback: Expression,
}
impl Hashable for DynamicVariableTerm {
    fn hash(&self) -> HashId {
        combine_hashes(hash_state_token(self.id), self.fallback.hash())
    }
}
impl Rewritable for DynamicVariableTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::of(DynamicDependencies::of(self.id))
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match substitutions {
            Substitutions::Dynamic(state) => Some(match state.get(self.id) {
                Some(value) => value,
                None => Expression::clone(&self.fallback),
            }),
            _ => None,
        }
    }
    fn optimize(&self) -> Option<Expression> {
        None
    }
}
impl fmt::Display for DynamicVariableTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dynamic:{}>", self.id)
    }
}

pub struct DynamicState {
    values: HashMap<StateToken, Expression>,
}
impl DynamicState {
    pub fn new() -> Self {
        DynamicState {
            values: HashMap::new(),
        }
    }
    pub fn get(&self, key: StateToken) -> Option<Expression> {
        let value = self.values.get(&key)?;
        Some(Expression::clone(value))
    }
    pub fn set(&mut self, key: StateToken, value: Expression) {
        self.values.insert(key, value);
    }
}

pub type StateToken = u32;
fn hash_state_token(value: StateToken) -> HashId {
    hash_u32(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct DynamicDependencies {
    values: Rc<HashSet<StateToken>>,
}
impl Hashable for DynamicDependencies {
    fn hash(&self) -> HashId {
        hash_unordered_sequence(self.values.iter().copied())
    }
}
impl DynamicDependencies {
    pub fn of(id: StateToken) -> Self {
        Self {
            values: Rc::new(once(id).collect()),
        }
    }
    fn union(&self, other: &DynamicDependencies) -> Self {
        Self {
            values: Rc::new(self.values.union(&other.values).copied().collect()),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct RecursiveTerm {
    factory: Expression,
}
impl Hashable for RecursiveTerm {
    fn hash(&self) -> HashId {
        self.factory.hash()
    }
}
impl RecursiveTerm {
    pub fn new(factory: Expression) -> Self {
        Self { factory }
    }
}
impl Rewritable for RecursiveTerm {
    fn capture_depth(&self) -> StackOffset {
        self.factory.capture_depth()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.factory.dynamic_dependencies()
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        self.factory
            .substitute(substitutions)
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
    fn optimize(&self) -> Option<Expression> {
        self.factory
            .optimize()
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
}
impl Reducible for RecursiveTerm {
    fn reduce(&self) -> Option<Expression> {
        // TODO: Implement recursion via cached circular reference cell rather than repeated factory invocation
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::clone(&self.factory),
            vec![Expression::new(Term::Recursive(Self {
                factory: Expression::clone(&self.factory),
            }))],
        )))
        .reduce()
    }
}
impl fmt::Display for RecursiveTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<recursive:{}>", format!("{}", self.factory))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LambdaTerm {
    arity: Arity,
    body: Expression,
}
impl Hashable for LambdaTerm {
    fn hash(&self) -> HashId {
        combine_hashes(self.arity.hash(), self.body.hash())
    }
}
impl LambdaTerm {
    pub fn new(arity: Arity, body: Expression) -> Self {
        Self { arity, body }
    }
    fn apply(&self, args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        // TODO: support variadic 'rest args' iterator in lambda functions
        let arity = self.arity.required();
        let substitutions = args
            .into_iter()
            .take(arity)
            .enumerate()
            .map(|(index, arg)| ((arity - index - 1) as StackOffset, arg))
            .collect::<Vec<_>>();
        self.body
            .substitute(&Substitutions::Static(StaticSubstitutions::new(
                &substitutions,
            )))
            .unwrap_or_else(|| Expression::clone(&self.body))
    }
}
impl Rewritable for LambdaTerm {
    fn capture_depth(&self) -> StackOffset {
        let arity = self.arity.required() as StackOffset;
        self.body.capture_depth().saturating_sub(arity)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.body.dynamic_dependencies()
    }
    fn substitute<'a>(&self, substitutions: &'a Substitutions) -> Option<Expression> {
        let arity = self.arity.required() as StackOffset;
        let body = match substitutions {
            Substitutions::Dynamic(_) => self.body.substitute(substitutions),
            Substitutions::Static(substitutions) => self
                .body
                .substitute(&Substitutions::Static(substitutions.offset(arity))),
        };
        body.map(|body| Expression::new(Term::Lambda(LambdaTerm::new(self.arity, body))))
    }
    fn optimize(&self) -> Option<Expression> {
        let arity = self.arity.required() as StackOffset;
        let reduced_body = self.body.reduce();
        let eta_reduced_body = reduced_body.as_ref().map_or_else(
            || apply_eta_reduction(&self.body, arity),
            |body| apply_eta_reduction(&body, arity),
        );
        eta_reduced_body
            .and_then(|body| body.reduce())
            .or(eta_reduced_body.map(Expression::clone))
            .or(reduced_body)
    }
}
impl fmt::Display for LambdaTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<function:{}{}>",
            self.arity.required(),
            if self.arity.variadic.is_some() {
                "+"
            } else {
                ""
            }
        )
    }
}
fn apply_eta_reduction(body: &Expression, arity: StackOffset) -> Option<&Expression> {
    match body.value() {
        Term::Application(ApplicationTerm { target, args })
            if target.capture_depth() == 0
                && args.len() as StackOffset <= arity
                && args
                    .iter()
                    .enumerate()
                    .all(|(index, arg)| match arg.value() {
                        Term::Variable(VariableTerm::Static(term)) => {
                            term.offset() == arity - (index as StackOffset) - 1
                        }
                        _ => false,
                    }) =>
        {
            Some(target)
        }
        _ => None,
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ApplicationTerm {
    target: Expression,
    args: Vec<Expression>,
}
impl fmt::Display for ApplicationTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<call:{}:{}>",
            self.target,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
impl Hashable for ApplicationTerm {
    fn hash(&self) -> HashId {
        combine_hashes(
            self.target.hash(),
            hash_sequence(self.args.iter().map(|arg| arg.hash())),
        )
    }
}
impl ApplicationTerm {
    pub fn new(target: Expression, args: Vec<Expression>) -> ApplicationTerm {
        Self { target, args }
    }
}
impl Rewritable for ApplicationTerm {
    fn capture_depth(&self) -> StackOffset {
        let target_depth = self.target.capture_depth();
        let arg_depth = capture_depth_multiple(&self.args);
        target_depth.max(arg_depth)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        let target_dependencies = self.target.dynamic_dependencies();
        let arity = match self.target.value() {
            Term::Lambda(target) => Some(target.arity),
            Term::Builtin(target) => Some(target.arity()),
            _ => None,
        };
        let eager_arity = arity.map(|arity| arity.eager()).unwrap_or(0);
        let eager_varargs = match arity {
            Some(Arity {
                variadic: Some(VarArgs::Eager),
                ..
            }) => true,
            _ => false,
        };
        let required_arity = arity.map(|arity| arity.required()).unwrap_or(0);
        let eager_args = self
            .args
            .iter()
            .enumerate()
            .filter(|(index, _)| {
                let index = *index;
                let is_eager = index < eager_arity || (eager_varargs && index >= required_arity);
                is_eager
            })
            .map(|(_, arg)| arg);
        let arg_dependencies = eager_args.fold(DependencyList::empty(), |acc, arg| {
            acc.extend(arg.dynamic_dependencies())
        });
        target_dependencies.extend(arg_dependencies)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        let target = self.target.substitute(substitutions);
        let args = substitute_multiple(&self.args, substitutions);
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| Expression::clone(&self.target));
        let args = args.unwrap_or_else(|| self.args.clone());
        Some(Expression::new(Term::Application(Self::new(target, args))))
    }
    fn optimize(&self) -> Option<Expression> {
        let optimized_args = optimize_multiple(&self.args);
        let optimized_expression = optimized_args.map(|args| {
            Expression::new(Term::Application(Self::new(
                Expression::clone(&self.target),
                args,
            )))
        });
        optimized_expression
            .as_ref()
            .map_or_else(|| self.reduce(), |expression| expression.reduce())
            .or(optimized_expression)
    }
}
impl Reducible for ApplicationTerm {
    fn reduce(&self) -> Option<Expression> {
        let reduced_target = self.target.reduce();
        let target = reduced_target
            .as_ref()
            .map_or_else(|| self.target.value(), |target| target.value());
        let result = match target {
            Term::Lambda(target) => Ok(reduce_lambda_application(target, self.args.iter())),
            Term::Builtin(target) => match reduce_builtin_application(target, self.args.iter()) {
                Ok(result) => Ok(result),
                Err(args) => Err(Some(args)),
            },
            _ => Err(None),
        };
        match result {
            Ok(result) => result.reduce().or_else(|| Some(result)),
            Err(args) => {
                if reduced_target.is_none() && args.is_none() {
                    None
                } else {
                    let target = reduced_target.unwrap_or_else(|| Expression::clone(&self.target));
                    let args = args.unwrap_or_else(|| self.args.clone());
                    Some(Expression::reduced(&Expression::new(Term::Application(
                        Self::new(target, args),
                    ))))
                }
            }
        }
    }
}

fn reduce_lambda_application<'a>(
    target: &LambdaTerm,
    args: impl IntoIterator<Item = &'a Expression> + ExactSizeIterator,
) -> Expression {
    let arity = target.arity;
    if args.len() < arity.required() {
        panic!(
            "Expected {} arguments, received {}",
            arity.required(),
            args.len()
        )
    }
    match reduce_eager_args(&arity, args) {
        Err(signal) => signal,
        Ok(args) => target.apply(args.into_iter()),
    }
}

fn reduce_builtin_application<'a>(
    target: &BuiltinTerm,
    args: impl IntoIterator<Item = &'a Expression> + ExactSizeIterator,
) -> Result<Expression, Vec<Expression>> {
    let arity = target.arity();
    if args.len() < arity.required() {
        panic!(
            "Expected {} arguments, received {}",
            arity.required(),
            args.len()
        );
    }
    match reduce_eager_args(&arity, args) {
        Err(signal) => Ok(signal),
        Ok(args) => {
            let num_eager_args = match arity.variadic {
                Some(VarArgs::Eager) => args.len(),
                _ => arity.eager(),
            };
            let has_unresolved_args = args
                .iter()
                .take(num_eager_args)
                .any(|arg| arg.capture_depth() > 0 || !arg.dynamic_dependencies().is_empty());
            if has_unresolved_args {
                Err(args)
            } else {
                Ok(target.apply(args.into_iter()))
            }
        }
    }
}

fn reduce_eager_args<'a>(
    arity: &Arity,
    args: impl IntoIterator<Item = &'a Expression> + ExactSizeIterator,
) -> Result<Vec<Expression>, Expression> {
    // TODO: avoid unnecessary cloning if no argument reductions are needed
    let eager_arity = arity.eager();
    let eager_varargs = match arity.variadic {
        Some(VarArgs::Eager) => true,
        _ => false,
    };
    if eager_arity == 0 && !eager_varargs {
        return Ok(args.into_iter().map(Expression::clone).collect());
    }
    let num_args = args.len();
    let required_arity = arity.required();
    enum EvaluatedArgs {
        Args(Vec<Expression>),
        Signal(Expression),
        CombinedSignal(CombinedSignal),
    }
    let result = args
        .into_iter()
        .enumerate()
        .map(|(index, arg)| {
            let is_eager = index < eager_arity || (eager_varargs && index >= required_arity);
            if is_eager {
                let arg = arg.reduce().unwrap_or_else(|| Expression::clone(arg));
                (arg, true)
            } else {
                (Expression::clone(arg), false)
            }
        })
        .fold(
            EvaluatedArgs::Args(Vec::with_capacity(num_args)),
            |result: EvaluatedArgs, (arg, is_eager)| match arg.value() {
                Term::Signal(signal) if is_eager => match result {
                    EvaluatedArgs::Args(_) => EvaluatedArgs::Signal(arg),
                    EvaluatedArgs::Signal(existing_result) => match existing_result.value() {
                        Term::Signal(existing) => match existing {
                            SignalTerm::Single(existing) => match signal {
                                SignalTerm::Single(signal) => {
                                    if signal.hash() == existing.hash() {
                                        EvaluatedArgs::Signal(existing_result)
                                    } else {
                                        EvaluatedArgs::CombinedSignal(CombinedSignal::join(
                                            Signal::clone(existing),
                                            Signal::clone(signal),
                                        ))
                                    }
                                }
                                SignalTerm::Combined(signal) => EvaluatedArgs::CombinedSignal(
                                    signal.append(Signal::clone(existing)),
                                ),
                            },
                            SignalTerm::Combined(existing) => match signal {
                                SignalTerm::Single(signal) => EvaluatedArgs::CombinedSignal(
                                    existing.append(Signal::clone(signal)),
                                ),
                                SignalTerm::Combined(signal) => {
                                    EvaluatedArgs::CombinedSignal(existing.union(signal))
                                }
                            },
                        },
                        _ => panic!(),
                    },
                    EvaluatedArgs::CombinedSignal(existing) => match signal {
                        SignalTerm::Single(signal) => {
                            EvaluatedArgs::CombinedSignal(existing.append(Signal::clone(signal)))
                        }
                        SignalTerm::Combined(signal) => {
                            EvaluatedArgs::CombinedSignal(existing.union(signal))
                        }
                    },
                },
                _ => match result {
                    EvaluatedArgs::Args(mut args) => {
                        args.push(arg);
                        EvaluatedArgs::Args(args)
                    }
                    _ => result,
                },
            },
        );
    match result {
        EvaluatedArgs::Signal(signal) => Err(signal),
        EvaluatedArgs::CombinedSignal(signal) => {
            Err(Expression::new(Term::Signal(SignalTerm::Combined(signal))))
        }
        EvaluatedArgs::Args(args) => Ok(args),
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum DataStructureTerm {
    Enum(EnumTerm),
    Struct(StructTerm),
}
impl Hashable for DataStructureTerm {
    fn hash(&self) -> HashId {
        match self {
            DataStructureTerm::Enum(term) => prefix_hash(0, term.hash()),
            DataStructureTerm::Struct(term) => prefix_hash(1, term.hash()),
        }
    }
}
impl Rewritable for DataStructureTerm {
    fn capture_depth(&self) -> StackOffset {
        match self {
            DataStructureTerm::Enum(term) => term.capture_depth(),
            DataStructureTerm::Struct(term) => term.capture_depth(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            DataStructureTerm::Enum(term) => term.dynamic_dependencies(),
            DataStructureTerm::Struct(term) => term.dynamic_dependencies(),
        }
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        match self {
            DataStructureTerm::Enum(term) => term.substitute(substitutions),
            DataStructureTerm::Struct(term) => term.substitute(substitutions),
        }
    }
    fn optimize(&self) -> Option<Expression> {
        match self {
            DataStructureTerm::Enum(term) => term.optimize(),
            DataStructureTerm::Struct(term) => term.optimize(),
        }
    }
}
impl fmt::Display for DataStructureTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataStructureTerm::Enum(term) => fmt::Display::fmt(term, f),
            DataStructureTerm::Struct(term) => fmt::Display::fmt(term, f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct EnumTerm {
    index: EnumIndex,
    args: Vec<Expression>,
}
impl Hashable for EnumTerm {
    fn hash(&self) -> HashId {
        prefix_hash(
            self.index,
            hash_sequence(self.args.iter().map(|arg| arg.hash())),
        )
    }
}
impl EnumTerm {
    pub fn new(index: EnumIndex, args: Vec<Expression>) -> Self {
        Self { index, args }
    }
    pub fn index(&self) -> EnumIndex {
        self.index
    }
    pub fn args(&self) -> &[Expression] {
        &self.args
    }
}
impl Rewritable for EnumTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.args)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.args)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        substitute_multiple(&self.args, substitutions).map(|args| {
            Expression::new(Term::DataStructure(DataStructureTerm::Enum(Self::new(
                self.index, args,
            ))))
        })
    }
    fn optimize(&self) -> Option<Expression> {
        optimize_multiple(&self.args).map(|args| {
            Expression::new(Term::DataStructure(DataStructureTerm::Enum(Self::new(
                self.index, args,
            ))))
        })
    }
}
impl fmt::Display for EnumTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<enum:{}:{}>",
            self.index,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

pub type StructFieldOffset = u32;
pub fn hash_struct_field_offset(value: StructFieldOffset) -> HashId {
    hash_u32(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructTerm {
    fields: Vec<Expression>,
}
impl Hashable for StructTerm {
    fn hash(&self) -> HashId {
        hash_sequence(self.fields.iter().map(|field| field.hash()))
    }
}
impl StructTerm {
    pub fn new(fields: Vec<Expression>) -> Self {
        Self { fields }
    }
    pub fn get(&self, field_offset: StructFieldOffset) -> Option<Expression> {
        self.fields
            .get(field_offset as usize)
            .map(Expression::clone)
    }
}
impl Rewritable for StructTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.fields)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.fields)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        substitute_multiple(&self.fields, substitutions).map(|fields| {
            Expression::new(Term::DataStructure(DataStructureTerm::Struct(Self::new(
                fields,
            ))))
        })
    }
    fn optimize(&self) -> Option<Expression> {
        optimize_multiple(&self.fields).map(|fields| {
            Expression::new(Term::DataStructure(DataStructureTerm::Struct(Self::new(
                fields,
            ))))
        })
    }
}
impl fmt::Display for StructTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<struct:{{{}}}>",
            self.fields
                .iter()
                .enumerate()
                .map(|(index, field)| format!("{}:{}", index, field))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct MatcherTerm {
    handlers: Vec<Expression>,
}
impl Hashable for MatcherTerm {
    fn hash(&self) -> HashId {
        hash_sequence(self.handlers.iter().map(|case| case.hash()))
    }
}
impl MatcherTerm {
    pub fn new(handlers: Vec<Expression>) -> Self {
        Self { handlers }
    }
    pub fn handlers(&self) -> &[Expression] {
        &self.handlers
    }
}
impl Rewritable for MatcherTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.handlers)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        dynamic_dependencies_multiple(&self.handlers)
    }
    fn substitute(&self, substitutions: &Substitutions) -> Option<Expression> {
        substitute_multiple(&self.handlers, substitutions)
            .map(|cases| Expression::new(Term::Matcher(Self::new(cases))))
    }
    fn optimize(&self) -> Option<Expression> {
        optimize_multiple(&self.handlers)
            .map(|cases| Expression::new(Term::Matcher(Self::new(cases))))
    }
}
impl fmt::Display for MatcherTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<matcher:{}>", self.handlers.len())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SignalTerm {
    Single(Signal),
    Combined(CombinedSignal),
}
impl Hashable for SignalTerm {
    fn hash(&self) -> HashId {
        match self {
            Self::Single(signal) => prefix_hash(0, signal.hash()),
            Self::Combined(signal) => prefix_hash(1, signal.hash()),
        }
    }
}
impl SignalTerm {
    pub fn new(signal: Signal) -> Self {
        Self::Single(signal)
    }
    pub fn combine(&self, other: &SignalTerm) -> Self {
        match self {
            Self::Single(existing) => match other {
                Self::Single(other) => {
                    if existing.hash() == other.hash() {
                        Self::Single(Signal::clone(existing))
                    } else {
                        Self::Combined(CombinedSignal::join(
                            Signal::clone(existing),
                            Signal::clone(other),
                        ))
                    }
                }
                Self::Combined(other) => Self::Combined(other.append(Signal::clone(existing))),
            },
            Self::Combined(existing) => match other {
                Self::Single(other) => Self::Combined(existing.append(Signal::clone(other))),
                Self::Combined(other) => Self::Combined(existing.union(&other)),
            },
        }
    }
    fn flatten(&self) -> Vec<Signal> {
        match self {
            Self::Single(signal) => vec![signal.clone()],
            Self::Combined(_) => vec![],
        }
    }
}
impl fmt::Display for SignalTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Single(signal) => fmt::Display::fmt(signal, f),
            Self::Combined(signal) => fmt::Display::fmt(signal, f),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Signal {
    hash: HashId,
    signal: SignalType,
    args: Option<Rc<Vec<ValueTerm>>>,
}
impl Hashable for Signal {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl Signal {
    pub fn new(signal: SignalType, args: impl IntoIterator<Item = ValueTerm>) -> Self {
        let args = args.into_iter().collect::<Vec<_>>();
        let hash = combine_hashes(
            signal.hash(),
            hash_sequence(args.iter().map(|arg| arg.hash())),
        );
        Self {
            hash,
            signal,
            args: if args.is_empty() {
                None
            } else {
                Some(Rc::new(args))
            },
        }
    }
    pub fn is_type(&self, signal: SignalType) -> bool {
        self.signal == signal
    }
    pub fn args(&self) -> Option<&Vec<ValueTerm>> {
        self.args.as_ref().map(|args| &**args)
    }
}
impl fmt::Display for Signal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<signal:{}{}>",
            self.signal,
            self.args
                .as_ref()
                .map(|args| format!(
                    ":{}",
                    args.iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(",")
                ))
                .unwrap_or_else(|| String::from(""))
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct CombinedSignal {
    signals: Rc<HashMap<HashId, Signal>>,
}
impl Hashable for CombinedSignal {
    fn hash(&self) -> HashId {
        hash_unordered_sequence(self.signals.iter().map(|(hash, _)| *hash))
    }
}
impl CombinedSignal {
    fn join(left: Signal, right: Signal) -> Self {
        Self {
            signals: Rc::new(
                once(left)
                    .chain(once(right))
                    .map(|signal| (signal.hash(), signal))
                    .collect::<HashMap<_, _>>(),
            ),
        }
    }
    fn append(&self, signal: Signal) -> Self {
        let hash = signal.hash();
        if self.signals.contains_key(&hash) {
            return Self {
                signals: Rc::clone(&self.signals),
            };
        }
        let entries = self
            .signals
            .iter()
            .map(|(hash, signal)| (*hash, Signal::clone(signal)))
            .chain(once((hash, signal)))
            .collect::<HashMap<_, _>>();
        Self {
            signals: Rc::new(entries),
        }
    }
    fn union(&self, other: &CombinedSignal) -> Self {
        let entries = other
            .signals
            .iter()
            .filter(|(hash, _)| !self.signals.contains_key(hash))
            .fold(None, |acc, (hash, signal)| match acc {
                None => Some(
                    self.signals
                        .iter()
                        .map(|(hash, signal)| (*hash, Signal::clone(signal)))
                        .chain(once((*hash, Signal::clone(signal))))
                        .collect::<HashMap<_, _>>(),
                ),
                Some(mut entries) => {
                    entries.insert(*hash, Signal::clone(signal));
                    Some(entries)
                }
            });
        match entries {
            Some(entries) => Self {
                signals: Rc::new(entries),
            },
            None => Self {
                signals: Rc::clone(&self.signals),
            },
        }
    }
}
impl fmt::Display for CombinedSignal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.signals
                .iter()
                .map(|(_, signal)| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

pub(crate) fn capture_depth_multiple(expressions: &[Expression]) -> StackOffset {
    expressions
        .iter()
        .map(|expression| expression.capture_depth())
        .fold(0 as StackOffset, |acc, depth| acc.max(depth))
}

pub(crate) fn dynamic_dependencies_multiple(expressions: &[Expression]) -> DependencyList {
    expressions
        .iter()
        .fold(DependencyList::empty(), |acc, arg| {
            acc.extend(arg.dynamic_dependencies())
        })
}

pub(crate) fn substitute_multiple(
    expressions: &[Expression],
    substitutions: &Substitutions,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |arg| arg.substitute(substitutions))
}

pub(crate) fn optimize_multiple<'a>(expressions: &[Expression]) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |arg| arg.reduce())
}

fn transform_expressions<'a>(
    expressions: &[Expression],
    transform: impl Fn(&Expression) -> Option<Expression>,
) -> Option<Vec<Expression>> {
    let expressions =
        expressions
            .into_iter()
            .enumerate()
            .fold(Vec::new(), |mut result, (index, expression)| {
                let replaced = transform(expression);
                match replaced {
                    Some(arg) => {
                        if result.is_empty() && index > 0 {
                            result.extend(
                                expressions
                                    .into_iter()
                                    .take(index)
                                    .map(Expression::clone)
                                    .chain(once(arg)),
                            )
                        } else {
                            result.push(arg)
                        }
                    }
                    None => {
                        if !result.is_empty() {
                            result.push(Expression::clone(expression))
                        }
                    }
                }
                result
            });
    if expressions.is_empty() {
        None
    } else {
        Some(expressions)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::sexpr::parse, stdlib::builtin::BuiltinTerm, stdlib::value::ValueTerm};

    use super::{DependencyList, DynamicState, EvaluationResult, Expression, Rewritable, Term};

    #[test]
    fn value_expressions() {
        let state = DynamicState::new();
        let expression = parse("3").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn lambda_optimizations() {
        let expression = parse("(lambda (foo) foo)").unwrap();
        let result = expression.value.optimize();
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(result, None);

        let expression = parse("(lambda (foo bar) (+ foo bar))").unwrap();
        let optimized = expression.value.optimize();
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(optimized, Some(parse("+").unwrap()));

        let expression = parse("(lambda (foo bar baz) (+ foo bar))").unwrap();
        let optimized = expression.value.optimize();
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(optimized, Some(parse("+").unwrap()));
    }

    #[test]
    fn lambda_application_expressions() {
        let state = DynamicState::new();
        let expression = parse("((lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );

        let expression = parse("((lambda (foo bar) (+ foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );

        let expression =
            parse("((lambda (foo bar) (((lambda (foo bar) (lambda (foo bar) (+ foo bar))) foo bar) foo bar)) 3 4)")
                .unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_expressions() {
        let state = DynamicState::new();
        let expression = parse("+").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Builtin(BuiltinTerm::Add))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_application_expressions() {
        let state = DynamicState::new();
        let expression = parse("(+ 1 2)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expressions() {
        let state = DynamicState::new();
        let expression = parse("(let () 3)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn letrec_expressions() {
        let state = DynamicState::new();
        let expression = parse("(letrec () 3)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );

        let expression = parse("(letrec ((foo 3) (bar foo)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo bar) (bar 3)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar (+ foo 1))) bar)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 1)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo (+ bar 1)) (bar 3)) foo)").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 1)))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))")
                .unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(
                    5 * 4 * 3 * 2 * 1
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-even? 3))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Boolean(false)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Boolean(true)))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((foo (cons 3 foo))) (car (cdr (cdr (cdr foo)))))").unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((foo (cons 1 bar)) (bar (cons 2 foo))) (car (cdr (cdr (cdr foo)))))")
                .unwrap();
        let result = expression.evaluate(&state);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(2)))),
                DependencyList::empty(),
            ),
        );
    }
}
