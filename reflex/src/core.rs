// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap},
    fmt,
    hash::{Hash, Hasher},
    iter::once,
    sync::Arc,
};

use im::OrdSet;

use crate::{
    cache::EvaluationCache,
    hash::{hash_object, HashId},
    stdlib::{
        builtin::BuiltinTerm, collection::CollectionTerm, signal::SignalType, value::ValueTerm,
    },
};

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum VarArgs {
    Eager,
    Lazy,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Arity {
    eager: usize,
    lazy: usize,
    variadic: Option<VarArgs>,
}
impl Arity {
    pub fn from(eager: usize, lazy: usize, variadic: Option<VarArgs>) -> Self {
        Self {
            eager,
            lazy,
            variadic,
        }
    }
    pub fn eager(&self) -> usize {
        self.eager
    }
    pub fn lazy(&self) -> usize {
        self.lazy
    }
    pub fn variadic(&self) -> Option<VarArgs> {
        self.variadic
    }
    pub fn required(&self) -> usize {
        self.eager + self.lazy
    }
}
impl fmt::Display for Arity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.eager, self.lazy)
    }
}

pub trait Rewritable {
    fn capture_depth(&self) -> StackOffset;
    fn dynamic_dependencies(&self) -> DependencyList;
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression>;
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression>;
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression>;
}

pub trait Reducible {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression>;
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Substitutions<'a> {
    entries: Option<&'a [(StackOffset, Expression)]>,
    scope_offset: Option<ScopeOffset>,
    min_depth: StackOffset,
    offset: StackOffset,
}
#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
pub enum ScopeOffset {
    Wrap(usize),
    Unwrap(usize),
}
impl<'a> Substitutions<'a> {
    pub fn named(
        entries: &'a [(StackOffset, Expression)],
        scope_offset: Option<ScopeOffset>,
    ) -> Self {
        Self {
            entries: Some(entries),
            scope_offset,
            min_depth: entries
                .iter()
                .map(|(offset, _)| offset)
                .fold(StackOffset::MAX, |acc, offset| acc.min(*offset)),
            offset: 0,
        }
    }
    pub fn wrap(depth: StackOffset) -> Self {
        Self {
            entries: None,
            scope_offset: Some(ScopeOffset::Wrap(depth)),
            min_depth: 0,
            offset: 0,
        }
    }
    pub fn unwrap(depth: StackOffset) -> Self {
        Self {
            entries: None,
            scope_offset: Some(ScopeOffset::Unwrap(depth)),
            min_depth: 0,
            offset: 0,
        }
    }
    fn offset(&self, offset: StackOffset) -> Self {
        Self {
            entries: self.entries.as_ref().copied(),
            scope_offset: self.scope_offset.as_ref().copied(),
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    fn can_skip(&self, expression: &Expression) -> bool {
        let capture_depth = expression.capture_depth();
        if capture_depth == 0 {
            true
        } else {
            expression.capture_depth() - 1 < self.min_depth + self.offset
        }
    }
    fn get(&self, offset: StackOffset, cache: &mut impl EvaluationCache) -> Option<Expression> {
        if offset < self.offset {
            return None;
        }
        let target_offset = offset - self.offset;
        let replacement = self.entries.and_then(|entries| {
            entries.iter().find_map(|(offset, replacement)| {
                if *offset == target_offset {
                    Some(replacement)
                } else {
                    None
                }
            })
        });
        match replacement {
            Some(replacement) => Some(
                replacement
                    .substitute_static(&Substitutions::wrap(self.offset), cache)
                    .unwrap_or_else(|| Expression::clone(replacement)),
            ),
            None => match &self.scope_offset {
                Some(scope_offset) => {
                    let target_offset = match scope_offset {
                        ScopeOffset::Wrap(scope_offset) => offset + scope_offset,
                        ScopeOffset::Unwrap(scope_offset) => offset - scope_offset,
                    };
                    if target_offset != offset {
                        Some(Expression::new(Term::Variable(VariableTerm::scoped(
                            target_offset,
                        ))))
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}

#[derive(Clone)]
pub struct Expression {
    value: Arc<Term>,
    hash: HashId,
    capture_depth: StackOffset,
    is_reduced: bool,
    is_normalized: bool,
    dynamic_dependencies: DependencyList,
}
impl Hash for Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl Ord for Expression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.cmp(&other.hash)
    }
}
impl PartialOrd for Expression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hash.partial_cmp(&other.hash)
    }
}
impl Eq for Expression {}
impl Expression {
    pub fn new(value: Term) -> Self {
        let is_reducible = value.is_reducible();
        let is_reduced = !is_reducible;
        let is_normalized = false;
        Self::create(value, is_reduced, is_normalized)
    }
    pub fn value(&self) -> &Term {
        &self.value
    }
    pub fn is_atomic(&self) -> bool {
        self.value.is_atomic()
    }
    pub fn is_static(&self) -> bool {
        self.value.is_static()
    }
    pub fn is_reducible(&self) -> bool {
        !self.is_reduced && self.value.is_reducible()
    }
    pub fn optimize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        self.normalize(cache)
    }
    pub fn evaluate(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> EvaluationResult {
        let (result, dependencies) = evaluate_recursive(
            Expression::clone(self),
            state,
            cache,
            DependencyList::empty(),
        );
        EvaluationResult::new(result, dependencies)
    }
    fn create(value: Term, is_reduced: bool, is_normalized: bool) -> Self {
        let hash = hash_object(&value);
        let capture_depth = value.capture_depth();
        let dynamic_dependencies = value.dynamic_dependencies();
        Self {
            value: Arc::new(value),
            hash,
            capture_depth,
            dynamic_dependencies,
            is_reduced,
            is_normalized,
        }
    }
    fn reduced(value: Term) -> Self {
        Self::create(value, true, false)
    }
    fn normalized(value: Term) -> Self {
        Self::create(value, true, true)
    }
    fn into_reduced(self) -> Self {
        Self {
            is_reduced: true,
            ..self
        }
    }
    fn into_normalized(self) -> Self {
        Self {
            is_reduced: true,
            is_normalized: true,
            ..self
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
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        if substitutions.can_skip(self) {
            return None;
        }
        match cache.retrieve_static_substitution(self, substitutions) {
            Some(result) => result,
            None => {
                let result = self.value.substitute_static(substitutions, cache);
                cache.store_static_substitution(
                    self,
                    substitutions,
                    result.as_ref().map(Expression::clone),
                );
                result
            }
        }
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        if self.dynamic_dependencies.is_empty() {
            return None;
        }
        match cache.retrieve_dynamic_substitution(self, state) {
            Some(result) => result,
            None => {
                let result = self.value.substitute_dynamic(state, cache);
                cache.store_dynamic_substitution(
                    self,
                    state,
                    result.as_ref().map(Expression::clone),
                );
                result
            }
        }
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        if self.is_normalized {
            return None;
        }
        self.value.normalize(cache)
    }
}
impl Reducible for Expression {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        if self.is_reduced {
            return None;
        }
        match cache.retrieve_reduction(self) {
            Some(result) => result,
            None => {
                let result = self.value.reduce(cache);
                cache.store_reduction(self, result.as_ref().map(Expression::clone));
                result
            }
        }
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
    state: &DynamicState,
    cache: &mut impl EvaluationCache,
    dependencies: DependencyList,
) -> (Expression, DependencyList) {
    let dependencies = dependencies.extend(expression.dynamic_dependencies());
    match expression.substitute_dynamic(state, cache) {
        Some(expression) => evaluate_recursive(expression, state, cache, dependencies),
        None => match expression.reduce(cache) {
            Some(expression) => evaluate_recursive(expression, state, cache, dependencies),
            None => (expression, dependencies),
        },
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvaluationResult {
    result: Expression,
    dependencies: DependencyList,
}
impl EvaluationResult {
    pub fn new(result: Expression, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result,
            dependencies,
        }
    }
    pub fn result(&self) -> &Expression {
        &self.result
    }
    pub fn dependencies(&self) -> &DependencyList {
        &self.dependencies
    }
    pub fn into_parts(self) -> (Expression, DependencyList) {
        (self.result, self.dependencies)
    }
}

pub type StateToken = HashId;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DependencyList {
    state_tokens: Option<OrdSet<StateToken>>,
}
impl DependencyList {
    pub fn empty() -> Self {
        Self { state_tokens: None }
    }
    pub fn of(state_token: StateToken) -> Self {
        Self {
            state_tokens: Some(OrdSet::unit(state_token)),
        }
    }
    pub fn from(state_tokens: impl IntoIterator<Item = StateToken>) -> Self {
        let dependencies = state_tokens.into_iter().collect::<OrdSet<_>>();
        Self {
            state_tokens: if dependencies.is_empty() {
                None
            } else {
                Some(dependencies)
            },
        }
    }
    pub fn len(&self) -> usize {
        match &self.state_tokens {
            None => 0,
            Some(dependencies) => dependencies.len(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.state_tokens.is_none()
    }
    pub fn extend(self, other: Self) -> Self {
        match self.state_tokens {
            None => other,
            Some(existing) => Self {
                state_tokens: Some(match other.state_tokens {
                    Some(other) => existing.union(other),
                    None => existing,
                }),
            },
        }
    }
    pub fn contains(&self, entries: &BTreeSet<StateToken>) -> bool {
        match &self.state_tokens {
            None => false,
            Some(dependencies) => dependencies
                .iter()
                .any(|dependency| entries.contains(dependency)),
        }
    }
}
impl<'a> IntoIterator for &'a DependencyList {
    type Item = StateToken;
    type IntoIter = DependencyListIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.state_tokens {
            Some(dependencies) => DependencyListIntoIter::Some(dependencies.iter()),
            None => DependencyListIntoIter::None,
        }
    }
}
pub enum DependencyListIntoIter<'a> {
    Some(im::ordset::Iter<'a, StateToken>),
    None,
}
impl<'a> Iterator for DependencyListIntoIter<'a> {
    type Item = StateToken;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Some(iter) => iter.next().copied(),
            Self::None => None,
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
}
impl<'a> ExactSizeIterator for DependencyListIntoIter<'a> {
    fn len(&self) -> usize {
        match self {
            Self::Some(items) => items.len(),
            Self::None => 0,
        }
    }
}

pub type EnumIndex = usize;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Term {
    Value(ValueTerm),
    Variable(VariableTerm),
    Lambda(LambdaTerm),
    Application(ApplicationTerm),
    Recursive(RecursiveTerm),
    Builtin(BuiltinTerm),
    Native(NativeFunction),
    Struct(StructTerm),
    Enum(EnumTerm),
    StructConstructor(VarArgs, StructPrototype),
    EnumConstructor(EnumVariantPrototype),
    Collection(CollectionTerm),
    Signal(SignalTerm),
    SignalHandler(SignalHandlerTerm),
}
impl Rewritable for Term {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Variable(term) => term.capture_depth(),
            Self::Lambda(term) => term.capture_depth(),
            Self::Application(term) => term.capture_depth(),
            Self::Recursive(term) => term.capture_depth(),
            Self::Struct(term) => term.capture_depth(),
            Self::Enum(term) => term.capture_depth(),
            Self::Collection(term) => term.capture_depth(),
            Self::SignalHandler(term) => term.capture_depth(),
            _ => 0,
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Variable(term) => term.dynamic_dependencies(),
            Self::Lambda(term) => term.dynamic_dependencies(),
            Self::Application(term) => term.dynamic_dependencies(),
            Self::Recursive(term) => term.dynamic_dependencies(),
            Self::Struct(term) => term.dynamic_dependencies(),
            Self::Enum(term) => term.dynamic_dependencies(),
            Self::Collection(term) => term.dynamic_dependencies(),
            Self::SignalHandler(term) => term.dynamic_dependencies(),
            _ => DependencyList::empty(),
        }
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.substitute_static(substitutions, cache),
            Self::Lambda(term) => term.substitute_static(substitutions, cache),
            Self::Application(term) => term.substitute_static(substitutions, cache),
            Self::Recursive(term) => term.substitute_static(substitutions, cache),
            Self::Struct(term) => term.substitute_static(substitutions, cache),
            Self::Enum(term) => term.substitute_static(substitutions, cache),
            Self::Collection(term) => term.substitute_static(substitutions, cache),
            Self::SignalHandler(term) => term.substitute_static(substitutions, cache),
            _ => None,
        }
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.substitute_dynamic(state, cache),
            Self::Lambda(term) => term.substitute_dynamic(state, cache),
            Self::Application(term) => term.substitute_dynamic(state, cache),
            Self::Recursive(term) => term.substitute_dynamic(state, cache),
            Self::Struct(term) => term.substitute_dynamic(state, cache),
            Self::Enum(term) => term.substitute_dynamic(state, cache),
            Self::Collection(term) => term.substitute_dynamic(state, cache),
            Self::SignalHandler(term) => term.substitute_dynamic(state, cache),
            _ => None,
        }
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.normalize(cache),
            Self::Lambda(term) => term.normalize(cache),
            Self::Application(term) => term.normalize(cache),
            Self::Recursive(term) => term.normalize(cache),
            Self::Struct(term) => term.normalize(cache),
            Self::Enum(term) => term.normalize(cache),
            Self::Collection(term) => term.normalize(cache),
            Self::SignalHandler(term) => term.normalize(cache),
            _ => None,
        }
    }
}
impl Term {
    fn is_atomic(&self) -> bool {
        // Term is fully evaluated and contains no sub-expressions
        match self {
            Self::Variable(_) => false,
            Self::Lambda(_) => false,
            Self::Application(_) => false,
            Self::Recursive(_) => false,
            Self::Struct(StructTerm { fields, .. }) if !fields.is_empty() => false,
            Self::Enum(EnumTerm { args, .. }) if !args.is_empty() => false,
            Self::Collection(collection) if !collection.is_empty() => false,
            Self::SignalHandler(_) => false,
            _ => true,
        }
    }
    fn is_static(&self) -> bool {
        // Weak head normal form - outer term is fully evaluated, sub-expressions may contain dynamic values
        match self {
            Self::Variable(_) => false,
            Self::Application(_) => false,
            Self::Recursive(_) => false,
            _ => true,
        }
    }
    fn is_reducible(&self) -> bool {
        match self {
            Self::Application(_) => true,
            Self::Recursive(_) => true,
            _ => false,
        }
    }
}
impl Reducible for Term {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        match self {
            Self::Application(term) => term.reduce(cache),
            Self::Recursive(term) => term.reduce(cache),
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
            Self::Native(term) => fmt::Display::fmt(term, f),
            Self::Struct(term) => fmt::Display::fmt(term, f),
            Self::Enum(term) => fmt::Display::fmt(term, f),
            Self::StructConstructor(_, term) => fmt::Display::fmt(term, f),
            Self::EnumConstructor(term) => fmt::Display::fmt(term, f),
            Self::Collection(term) => fmt::Display::fmt(term, f),
            Self::Signal(term) => fmt::Display::fmt(term, f),
            Self::SignalHandler(term) => fmt::Display::fmt(term, f),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum VariableTerm {
    Static(StaticVariableTerm),
    Dynamic(DynamicVariableTerm),
}
impl VariableTerm {
    pub fn scoped(offset: StackOffset) -> Self {
        Self::Static(StaticVariableTerm::new(offset))
    }
    pub fn dynamic(id: StateToken, fallback: Expression) -> Self {
        Self::Dynamic(DynamicVariableTerm::new(id, fallback))
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
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Static(term) => term.substitute_static(substitutions, cache),
            Self::Dynamic(term) => term.substitute_static(substitutions, cache),
        }
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Static(term) => term.substitute_dynamic(state, cache),
            Self::Dynamic(term) => term.substitute_dynamic(state, cache),
        }
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        match self {
            Self::Static(term) => term.normalize(cache),
            Self::Dynamic(term) => term.normalize(cache),
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

pub type StackOffset = usize;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct StaticVariableTerm {
    offset: StackOffset,
}
impl StaticVariableTerm {
    fn new(offset: StackOffset) -> Self {
        Self { offset }
    }
    pub fn offset(&self) -> StackOffset {
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
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitutions.get(self.offset, cache)
    }
    fn substitute_dynamic(
        &self,
        _state: &DynamicState,
        _cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        None
    }
    fn normalize(&self, _cache: &mut impl EvaluationCache) -> Option<Expression> {
        None
    }
}
impl fmt::Display for StaticVariableTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<static:{}>", self.offset)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DynamicVariableTerm {
    state_token: StateToken,
    fallback: Expression,
}
impl DynamicVariableTerm {
    fn new(state_token: StateToken, fallback: Expression) -> Self {
        Self {
            state_token,
            fallback,
        }
    }
    pub fn state_token(&self) -> StateToken {
        self.state_token
    }
    pub fn fallback(&self) -> &Expression {
        &self.fallback
    }
}
impl Rewritable for DynamicVariableTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::of(self.state_token)
    }
    fn substitute_static(
        &self,
        _substitutions: &Substitutions,
        _cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        None
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        _cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        Some(match state.get(self.state_token) {
            Some(value) => Expression::clone(value),
            None => Expression::clone(&self.fallback),
        })
    }
    fn normalize(&self, _cache: &mut impl EvaluationCache) -> Option<Expression> {
        None
    }
}
impl fmt::Display for DynamicVariableTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dynamic:{}>", self.state_token)
    }
}

pub struct DynamicState {
    hash: HashId,
    values: HashMap<StateToken, Expression>,
}
impl Hash for DynamicState {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl DynamicState {
    pub fn new() -> Self {
        DynamicState {
            hash: DefaultHasher::new().finish(),
            values: HashMap::new(),
        }
    }
    pub fn has(&self, key: StateToken) -> bool {
        self.values.contains_key(&key)
    }
    pub fn get(&self, key: StateToken) -> Option<&Expression> {
        self.values.get(&key)
    }
    pub fn set(&mut self, key: StateToken, value: Expression) {
        let value_hash = hash_object(&value);
        let previous = self.values.insert(key, value);
        let has_changes = match &previous {
            Some(previous_value) => value_hash != hash_object(previous_value),
            None => true,
        };
        if has_changes {
            self.hash = {
                let mut hasher = DefaultHasher::new();
                hasher.write_u64(self.hash);
                hasher.write_u64(key);
                hasher.write_u64(value_hash);
                hasher.finish()
            }
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct RecursiveTerm {
    factory: Expression,
}
impl RecursiveTerm {
    pub fn new(factory: Expression) -> Self {
        Self { factory }
    }
    pub fn factory(&self) -> &Expression {
        &self.factory
    }
}
impl Rewritable for RecursiveTerm {
    fn capture_depth(&self) -> StackOffset {
        self.factory.capture_depth()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.factory.dynamic_dependencies()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        self.factory
            .substitute_static(substitutions, cache)
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        self.factory
            .substitute_dynamic(state, cache)
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        self.factory
            .normalize(cache)
            .map(|factory| Expression::normalized(Term::Recursive(Self { factory })))
    }
}
impl Reducible for RecursiveTerm {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        // TODO: Implement recursion via cached circular reference cell rather than repeated factory invocation
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::clone(&self.factory),
            vec![Expression::new(Term::Recursive(Self {
                factory: Expression::clone(&self.factory),
            }))],
        )))
        .reduce(cache)
    }
}
impl fmt::Display for RecursiveTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<recursive:{}>", format!("{}", self.factory))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct LambdaTerm {
    arity: Arity,
    body: Expression,
}
impl LambdaTerm {
    pub fn new(arity: Arity, body: Expression) -> Self {
        Self { arity, body }
    }
    pub fn arity(&self) -> &Arity {
        &self.arity
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
        cache: &mut impl EvaluationCache,
    ) -> Expression {
        // TODO: support variadic 'rest args' iterator in lambda functions
        let arity = self.arity.required();
        if arity == 0 || self.body.capture_depth() == 0 {
            Expression::clone(&self.body)
        } else {
            let substitutions = args
                .into_iter()
                .take(arity)
                .enumerate()
                .map(|(index, arg)| ((arity - index - 1), arg))
                .collect::<Vec<_>>();
            let substitutions =
                Substitutions::named(&substitutions, Some(ScopeOffset::Unwrap(arity)));
            self.body
                .substitute_static(&substitutions, cache)
                .unwrap_or_else(|| Expression::clone(&self.body))
        }
    }
}
impl Rewritable for LambdaTerm {
    fn capture_depth(&self) -> StackOffset {
        let arity = self.arity.required();
        self.body.capture_depth().saturating_sub(arity)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let arity = self.arity.required();
        let body = self
            .body
            .substitute_static(&substitutions.offset(arity), cache);
        body.map(|body| Expression::new(Term::Lambda(LambdaTerm::new(self.arity, body))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let body = self.body.substitute_dynamic(state, cache);
        body.map(|body| Expression::new(Term::Lambda(LambdaTerm::new(self.arity, body))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        let arity = self.arity.required();
        let normalized_body = self.body.normalize(cache);
        let eta_reduced_body =
            apply_eta_reduction(normalized_body.as_ref().unwrap_or(&self.body), arity);
        eta_reduced_body
            .and_then(|eta_reduced_body| {
                eta_reduced_body
                    .normalize(cache)
                    .or_else(|| Some(Expression::clone(eta_reduced_body).into_normalized()))
            })
            .or(normalized_body.map(|body| {
                Expression::normalized(Term::Lambda(LambdaTerm::new(self.arity.clone(), body)))
            }))
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
                && args.len() <= arity
                && args
                    .iter()
                    .enumerate()
                    .all(|(index, arg)| match arg.value() {
                        Term::Variable(VariableTerm::Static(term)) => {
                            term.offset() == arity - index - 1
                        }
                        _ => false,
                    }) =>
        {
            Some(target)
        }
        _ => None,
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
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
impl ApplicationTerm {
    pub fn new(target: Expression, args: Vec<Expression>) -> ApplicationTerm {
        Self { target, args }
    }
    pub fn target(&self) -> &Expression {
        &self.target
    }
    pub fn args(&self) -> &[Expression] {
        &self.args
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
        let eager_args = get_function_arity(self.target.value()).map(|arity| {
            with_eagerness(self.args.iter(), &arity)
                .into_iter()
                .filter_map(|(arg, is_eager)| if is_eager { Some(arg) } else { None })
        });
        match eager_args {
            None => target_dependencies,
            Some(args) => args.fold(target_dependencies, |acc, arg| {
                acc.extend(arg.dynamic_dependencies())
            }),
        }
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let target = self.target.substitute_static(substitutions, cache);
        let args = substitute_static_multiple(&self.args, substitutions, cache);
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| Expression::clone(&self.target));
        let args = args.unwrap_or_else(|| self.args.clone());
        Some(Expression::new(Term::Application(Self::new(target, args))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        let target = self.target.substitute_dynamic(state, cache);
        let has_dynamic_args = self
            .args
            .iter()
            .any(|arg| !arg.dynamic_dependencies().is_empty());
        let args = if has_dynamic_args {
            get_function_arity(self.target.value()).map(|arity| {
                with_eagerness(self.args.iter(), &arity)
                    .into_iter()
                    .map(|(arg, is_eager)| {
                        if is_eager {
                            arg.substitute_dynamic(state, cache)
                                .unwrap_or(Expression::clone(arg))
                        } else {
                            Expression::clone(arg)
                        }
                    })
                    .collect::<Vec<_>>()
            })
        } else {
            None
        };
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| Expression::clone(&self.target));
        let args = args.unwrap_or_else(|| self.args.clone());
        Some(Expression::new(Term::Application(Self::new(target, args))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        let normalized_target = self.target.normalize(cache);
        let normalized_args = normalize_multiple(&self.args, cache);
        let substituted_expression = match normalized_args {
            Some(args) => Some(Expression::new(Term::Application(Self::new(
                normalized_target
                    .unwrap_or_else(|| Expression::clone(&self.target).into_normalized()),
                args,
            )))),
            None => normalized_target.map(|target| {
                Expression::new(Term::Application(Self::new(
                    target,
                    self.args
                        .iter()
                        .map(|arg| Expression::clone(arg).into_normalized())
                        .collect(),
                )))
            }),
        };
        let reduced_expression = match substituted_expression {
            Some(expression) => expression
                .reduce(cache)
                .or_else(|| Some(expression.into_normalized())),
            None => self.reduce(cache),
        };
        reduced_expression.map(|expression| {
            if expression.is_normalized {
                expression
            } else {
                expression
                    .normalize(cache)
                    .unwrap_or_else(|| expression.into_normalized())
            }
        })
    }
}
impl Reducible for ApplicationTerm {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        let reduced_target = self.target.reduce(cache);
        let target = reduced_target.as_ref().unwrap_or(&self.target);
        let target_term = target.value();
        let short_circuit_target = match target_term {
            Term::Signal(_) => true,
            _ => false,
        };
        if short_circuit_target {
            return reduced_target.or_else(|| Some(Expression::clone(&self.target)));
        }
        match get_function_arity(target_term) {
            None => None,
            Some(arity) => {
                if self.args.len() < arity.required() {
                    // TODO: Allow consumer-provided error factory
                    return Some(Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(format!(
                            "Expected {}, received {}",
                            match arity.required() {
                                1 => String::from("1 argument"),
                                arity => format!("{} arguments", arity),
                            },
                            self.args.len()
                        ))))],
                    )))));
                }
                let result = match evaluate_function_args(self.args.iter(), arity, cache) {
                    Err(args) => Err(args),
                    Ok((args, signals)) => match signals {
                        ShortCircuitArgs::None => {
                            apply_function(target_term, args.into_iter(), cache)
                        }
                        ShortCircuitArgs::Single(signal) => match target_term {
                            Term::SignalHandler(target) => Ok(target.apply(signal)),
                            _ => Ok(signal),
                        },
                        ShortCircuitArgs::Multiple(signals) => {
                            let signal = Expression::new(Term::Signal(SignalTerm::from(signals)));
                            match &target_term {
                                Term::SignalHandler(target) => Ok(target.apply(signal)),
                                _ => Ok(signal),
                            }
                        }
                    },
                };
                match result {
                    Ok(result) => result
                        .reduce(cache)
                        .or_else(|| Some(result))
                        .map(|result| result.into_reduced()),
                    Err(args) => {
                        if reduced_target.is_none()
                            && self
                                .args
                                .iter()
                                .zip(args.iter())
                                .all(|(arg, evaluated_arg)| arg.hash == evaluated_arg.hash)
                        {
                            None
                        } else {
                            let target =
                                reduced_target.unwrap_or_else(|| Expression::clone(&self.target));
                            Some(Expression::reduced(Term::Application(Self::new(
                                target, args,
                            ))))
                        }
                    }
                }
            }
        }
    }
}

fn evaluate_function_args<'a>(
    args: impl IntoIterator<Item = &'a Expression> + ExactSizeIterator,
    arity: Arity,
    cache: &mut impl EvaluationCache,
) -> Result<(Vec<Expression>, ShortCircuitArgs), Vec<Expression>> {
    let args = with_eagerness(args, &arity)
        .into_iter()
        .map(|(arg, is_eager)| {
            if is_eager {
                arg.reduce(cache).unwrap_or_else(|| Expression::clone(arg))
            } else {
                Expression::clone(arg)
            }
        })
        .collect::<Vec<_>>();
    let has_unresolved_args = with_eagerness(args.iter(), &arity)
        .into_iter()
        .any(|(arg, is_eager)| is_eager && is_unresolved_arg(arg));
    if has_unresolved_args {
        return Err(args);
    }
    let signals =
        ShortCircuitArgs::from_args(with_eagerness(args.iter(), &arity).into_iter().filter_map(
            |(arg, is_eager)| {
                if is_eager {
                    match arg.value() {
                        Term::Signal(_) => Some(Expression::clone(arg)),
                        _ => None,
                    }
                } else {
                    None
                }
            },
        ));
    Ok((args, signals))
}
fn is_unresolved_arg(arg: &Expression) -> bool {
    arg.capture_depth() > 0 || !arg.dynamic_dependencies.is_empty() || arg.is_reducible()
}
fn with_eagerness<T>(
    args: impl IntoIterator<Item = T>,
    arity: &Arity,
) -> impl IntoIterator<Item = (T, bool)> {
    let required_arity = arity.required();
    let eager_arity = arity.eager();
    let eager_varargs = match arity.variadic {
        Some(VarArgs::Eager) => true,
        _ => false,
    };
    args.into_iter().enumerate().map(move |(index, arg)| {
        let is_eager = index < eager_arity || (eager_varargs && index >= required_arity);
        (arg, is_eager)
    })
}
pub(crate) fn get_function_arity(target: &Term) -> Option<Arity> {
    match target {
        Term::Lambda(target) => Some(target.arity),
        Term::Builtin(target) => Some(target.arity()),
        Term::Native(target) => Some(target.arity),
        Term::StructConstructor(eager, target) => Some(target.arity(eager)),
        Term::EnumConstructor(target) => Some(Arity::from(0, target.arity, None)),
        Term::SignalHandler(_) => Some(Arity::from(1, 0, None)),
        _ => None,
    }
}
pub(crate) fn apply_function(
    target: &Term,
    args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
    cache: &mut impl EvaluationCache,
) -> Result<Expression, Vec<Expression>> {
    match target {
        Term::Lambda(target) => Ok(target.apply(args, cache)),
        Term::Builtin(target) => Ok(target.apply(args)),
        Term::Native(target) => Ok(target.apply(args)),
        Term::StructConstructor(_, target) => {
            let num_fields = args.len() / 2;
            let args = args.into_iter().collect::<Vec<_>>();
            let (keys, values) = args.split_at(num_fields);
            let keys = keys
                .into_iter()
                .map(|key| match key.value() {
                    Term::Value(key) => Some(key),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>();
            let values = values.into_iter().map(Expression::clone);
            let result = match keys {
                None => None,
                Some(keys) => target.apply(keys.into_iter().zip(values)),
            };
            match result {
                Some(result) => Ok(Expression::new(Term::Struct(result))),
                None => Err(args),
            }
        }
        Term::EnumConstructor(target) => Ok(target.apply(args)),
        Term::SignalHandler(_) => Ok(args.into_iter().next().unwrap()),
        _ => Err(args.into_iter().collect::<Vec<_>>()),
    }
}
enum ShortCircuitArgs {
    None,
    Single(Expression),
    Multiple(Vec<Signal>),
}
impl ShortCircuitArgs {
    fn from_args(args: impl IntoIterator<Item = Expression>) -> Self {
        args.into_iter()
            .fold(Self::None, |existing, arg| match arg.value() {
                Term::Signal(signal) => match existing {
                    Self::None => Self::Single(arg),
                    Self::Single(existing) => match existing.value() {
                        Term::Signal(existing) => Self::Multiple(
                            existing
                                .signals()
                                .into_iter()
                                .chain(signal.signals().into_iter())
                                .map(Signal::clone)
                                .collect(),
                        ),
                        _ => panic!("Invalid signal args"),
                    },
                    Self::Multiple(mut signals) => {
                        signals.extend(signal.signals().into_iter().map(Signal::clone));
                        Self::Multiple(signals)
                    }
                },
                _ => existing,
            })
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    uid: HashId,
    arity: Arity,
    body: Arc<dyn Fn(Vec<Expression>) -> Expression + Sync + Send>,
}
impl NativeFunction {
    pub fn new(
        uid: HashId,
        arity: Arity,
        body: impl Fn(Vec<Expression>) -> Expression + Sync + Send + 'static,
    ) -> Self {
        Self {
            uid,
            arity,
            body: Arc::new(body),
        }
    }
    pub fn uid(&self) -> HashId {
        self.uid
    }
    pub fn arity(&self) -> &Arity {
        &self.arity
    }
    fn apply(&self, args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        (self.body)(args.into_iter().collect())
    }
}
impl Hash for NativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.uid);
    }
}
impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.uid == other.uid
    }
}
impl Eq for NativeFunction {}
impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native:{}:{}>", self.arity.required(), self.uid)
    }
}
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub type StructFieldOffset = usize;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct StructTerm {
    prototype: Option<StructPrototype>,
    fields: Vec<Expression>,
}
impl StructTerm {
    pub fn new(prototype: Option<StructPrototype>, fields: Vec<Expression>) -> Self {
        Self { prototype, fields }
    }
    pub fn prototype(&self) -> Option<&StructPrototype> {
        self.prototype.as_ref()
    }
    pub fn fields(&self) -> &[Expression] {
        &self.fields
    }
    pub fn get(&self, field_offset: StructFieldOffset) -> Option<&Expression> {
        self.fields.get(field_offset)
    }
}
impl Rewritable for StructTerm {
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.fields)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_static_multiple(&self.fields, substitutions, cache)
            .map(|fields| Expression::new(Term::Struct(Self::new(self.prototype.clone(), fields))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_dynamic_multiple(&self.fields, state, cache)
            .map(|fields| Expression::new(Term::Struct(Self::new(self.prototype.clone(), fields))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        normalize_multiple(&self.fields, cache).map(|fields| {
            Expression::normalized(Term::Struct(Self::new(self.prototype.clone(), fields)))
        })
    }
}
impl fmt::Display for StructTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.prototype {
            Some(prototype) => match prototype.keys.len() {
                0 => write!(f, "{{}}"),
                _ => write!(
                    f,
                    "{{ {} }}",
                    prototype
                        .keys
                        .iter()
                        .zip(self.fields.iter())
                        .map(|(key, value)| format!("{}: {}", key, value))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
            },
            None => write!(
                f,
                "({})",
                self.fields
                    .iter()
                    .map(|value| format!("{}", value))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct StructPrototype {
    keys: Vec<ValueTerm>,
    key_hashes: Vec<HashId>,
}
impl Hash for StructPrototype {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key_hashes
            .iter()
            .for_each(|hash| state.write_u64(*hash));
    }
}
impl StructPrototype {
    pub fn new(keys: Vec<ValueTerm>) -> Self {
        let key_hashes = keys.iter().map(hash_object).collect();
        Self { keys, key_hashes }
    }
    pub fn keys(&self) -> &[ValueTerm] {
        &self.keys
    }
    pub fn field(&self, key: &ValueTerm) -> Option<StructFieldOffset> {
        let key_hash = hash_object(key);
        self.key_hashes
            .iter()
            .enumerate()
            .find_map(|(index, existing)| {
                if *existing == key_hash {
                    Some(index)
                } else {
                    None
                }
            })
    }
    fn arity(&self, eager: &VarArgs) -> Arity {
        let num_keys = self.keys.len();
        match eager {
            VarArgs::Lazy => Arity::from(num_keys, num_keys, None),
            VarArgs::Eager => Arity::from(num_keys * 2, 0, None),
        }
    }
    pub fn apply<'a>(
        &self,
        fields: impl IntoIterator<Item = (&'a ValueTerm, Expression)> + ExactSizeIterator,
    ) -> Option<StructTerm> {
        let fields = fields.into_iter().collect::<Vec<_>>();
        let has_correctly_ordered_keys = fields.len() >= self.keys.len()
            && fields
                .iter()
                .map(hash_object)
                .zip(self.key_hashes.iter())
                .all(|(key_hash, existing_hash)| key_hash == *existing_hash);
        let fields = if has_correctly_ordered_keys {
            Some(
                fields
                    .into_iter()
                    .take(self.keys.len())
                    .map(|(_, value)| value)
                    .collect::<Vec<_>>(),
            )
        } else {
            let mut remaining_fields = fields
                .into_iter()
                .map(|(key, value)| (hash_object(key), value))
                .collect::<Vec<_>>();
            self.key_hashes
                .iter()
                .map(|existing_hash| {
                    let index = remaining_fields
                        .iter()
                        .position(|(key_hash, _)| *key_hash == *existing_hash)?;
                    let (_, value) = remaining_fields.remove(index);
                    Some(value)
                })
                .collect::<Option<Vec<_>>>()
        }?;
        Some(StructTerm::new(Some(self.clone()), fields))
    }
}
impl fmt::Display for StructPrototype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<constructor:struct:{}>", self.keys.len())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct EnumTerm {
    index: EnumIndex,
    args: Vec<Expression>,
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
        DependencyList::empty()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_static_multiple(&self.args, substitutions, cache)
            .map(|args| Expression::new(Term::Enum(Self::new(self.index, args))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        substitute_dynamic_multiple(&self.args, state, cache)
            .map(|args| Expression::new(Term::Enum(Self::new(self.index, args))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        normalize_multiple(&self.args, cache)
            .map(|args| Expression::normalized(Term::Enum(Self::new(self.index, args))))
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

pub type EnumConstructorArity = usize;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct EnumVariantPrototype {
    index: EnumIndex,
    arity: EnumConstructorArity,
}
impl EnumVariantPrototype {
    pub fn new(index: EnumIndex, arity: EnumConstructorArity) -> Self {
        Self { index, arity }
    }
    pub fn index(&self) -> EnumIndex {
        self.index
    }
    pub fn arity(&self) -> EnumConstructorArity {
        self.arity
    }
    fn apply(&self, args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        Expression::new(Term::Enum(EnumTerm::new(
            self.index,
            args.into_iter().collect(),
        )))
    }
}
impl fmt::Display for EnumVariantPrototype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<constructor:enum:{}:{}>", self.index, self.arity)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SignalTerm {
    signals: BTreeSet<Signal>,
}
impl SignalTerm {
    pub fn new(signal: Signal) -> Self {
        Self::from(once(signal))
    }
    pub fn from(signals: impl IntoIterator<Item = Signal>) -> Self {
        Self {
            signals: signals.into_iter().collect(),
        }
    }
    pub fn signals(&self) -> impl IntoIterator<Item = &Signal> + ExactSizeIterator {
        self.signals.iter()
    }
}
impl fmt::Display for SignalTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Signal({})",
            self.signals
                .iter()
                .map(|signal| format!("{}", signal))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Signal {
    hash: HashId,
    signal_type: SignalType,
    args: Vec<Expression>,
}
impl Hash for Signal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl Ord for Signal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.cmp(&other.hash)
    }
}
impl PartialOrd for Signal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hash.partial_cmp(&other.hash)
    }
}
impl Signal {
    pub fn new(signal_type: SignalType, args: impl IntoIterator<Item = Expression>) -> Self {
        let args = args.into_iter().collect::<Vec<_>>();
        let hash = {
            let mut hasher = DefaultHasher::new();
            signal_type.hash(&mut hasher);
            for arg in args.iter() {
                arg.hash(&mut hasher);
            }
            hasher.finish()
        };
        Self {
            hash,
            signal_type,
            args,
        }
    }
    pub fn id(&self) -> StateToken {
        self.hash
    }
    pub fn signal_type(&self) -> &SignalType {
        &self.signal_type
    }
    pub fn args(&self) -> &[Expression] {
        &self.args
    }
    pub fn is_type(&self, signal: SignalType) -> bool {
        self.signal_type == signal
    }
}
impl fmt::Display for Signal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<signal:{}:{}>",
            self.signal_type,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct SignalHandlerTerm {
    handler: Expression,
}
impl SignalHandlerTerm {
    pub fn new(handler: Expression) -> Self {
        Self { handler }
    }
    pub fn handler(&self) -> &Expression {
        &self.handler
    }
    fn apply(&self, signal: Expression) -> Expression {
        Expression::new(Term::Application(ApplicationTerm::new(
            Expression::clone(&self.handler),
            vec![signal],
        )))
    }
}
impl Rewritable for SignalHandlerTerm {
    fn capture_depth(&self) -> StackOffset {
        self.handler.capture_depth()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.handler.dynamic_dependencies()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        self.handler
            .substitute_static(substitutions, cache)
            .map(|handler| Expression::new(Term::SignalHandler(Self::new(handler))))
    }
    fn substitute_dynamic(
        &self,
        state: &DynamicState,
        cache: &mut impl EvaluationCache,
    ) -> Option<Expression> {
        self.handler
            .substitute_dynamic(state, cache)
            .map(|handler| Expression::new(Term::SignalHandler(Self::new(handler))))
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        self.handler
            .normalize(cache)
            .map(|handler| Expression::normalized(Term::SignalHandler(Self::new(handler))))
    }
}
impl fmt::Display for SignalHandlerTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<handler:{}>", self.handler)
    }
}

pub(crate) fn capture_depth_multiple(expressions: &[Expression]) -> StackOffset {
    expressions
        .iter()
        .map(|expression| expression.capture_depth())
        .fold(0, |acc, depth| acc.max(depth))
}

pub(crate) fn dynamic_dependencies_multiple(expressions: &[Expression]) -> DependencyList {
    expressions
        .iter()
        .fold(DependencyList::empty(), |acc, arg| {
            acc.extend(arg.dynamic_dependencies())
        })
}

pub(crate) fn substitute_static_multiple(
    expressions: &[Expression],
    substitutions: &Substitutions,
    cache: &mut impl EvaluationCache,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |arg| {
        arg.substitute_static(substitutions, cache)
    })
}

pub(crate) fn substitute_dynamic_multiple(
    expressions: &[Expression],
    state: &DynamicState,
    cache: &mut impl EvaluationCache,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |arg| arg.substitute_dynamic(state, cache))
}

pub(crate) fn normalize_multiple(
    expressions: &[Expression],
    cache: &mut impl EvaluationCache,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |expression| expression.normalize(cache))
}

fn transform_expressions(
    expressions: &[Expression],
    mut transform: impl FnMut(&Expression) -> Option<Expression>,
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
    use crate::{
        cache::SubstitutionCache,
        core::{Signal, SignalTerm},
        parser::sexpr::parse,
        stdlib::builtin::BuiltinTerm,
        stdlib::{
            signal::SignalType,
            value::{StringValue, ValueTerm},
        },
    };

    use super::{
        ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
        NativeFunction, Rewritable, Term,
    };

    #[test]
    fn value_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("3").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            )
        );
    }

    #[test]
    fn lambda_optimizations() {
        let mut cache = SubstitutionCache::new();
        let expression = parse("(lambda (foo) foo)").unwrap();
        let result = expression.value.normalize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(result, None);

        let expression = parse("(lambda (foo bar) (+ foo bar))").unwrap();
        let normalized = expression.value.normalize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+").unwrap()));

        let expression = parse("(lambda (foo bar baz) (+ foo bar))").unwrap();
        let normalized = expression.value.normalize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(normalized, Some(parse("+").unwrap()));
    }

    #[test]
    fn lambda_application_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("((lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("((lambda (foo bar) (+ foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("((lambda (foo bar) (((lambda (foo bar) (lambda (foo bar) (+ foo bar))) foo bar) foo bar)) 3 4)")
                .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn invalid_function_applications() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("((lambda (foo) foo))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Expected 1 argument, received 0")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("((lambda (first second third) first) 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Expected 3 arguments, received 1")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("((lambda (first second third) first) 3 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Expected 3 arguments, received 2")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("+").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Builtin(BuiltinTerm::Add)),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn builtin_application_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("(+ 1 2)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn signal_short_circuiting() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("((/ 3 0) 4 5)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::from(vec![Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Division by zero: 3 / 0")
                    )))]
                ),]))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Division by zero: 3 / 0")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ 3 (/ 4 0))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Division by zero: 4 / 0")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 4 0))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::from(vec![
                    Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("Division by zero: 3 / 0")
                        )))]
                    ),
                    Signal::new(
                        SignalType::Error,
                        vec![Expression::new(Term::Value(ValueTerm::String(
                            StringValue::from("Division by zero: 4 / 0")
                        )))]
                    ),
                ]))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (/ 3 0) (/ 3 0))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::from(vec![Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Division by zero: 3 / 0")
                    )))]
                ),]))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(+ (+ (/ 3 0) 4) 5)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                    SignalType::Error,
                    vec![Expression::new(Term::Value(ValueTerm::String(
                        StringValue::from("Division by zero: 3 / 0")
                    )))]
                )))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn let_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("(let () 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn letrec_expressions() {
        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = parse("(letrec () 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4))),
                DependencyList::empty(),
            ),
        );

        let expression = parse("(letrec ((foo 3) (bar foo)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo bar) (bar 3)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 3))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar (+ foo 1))) bar)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 1))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo (+ bar 1)) (bar 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 1))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((fac (lambda (n) (if (= n 1) n (* n (fac (- n 1))))))) (fac 5))")
                .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(5 * 4 * 3 * 2 * 1))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-even? 3))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(false))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Boolean(true))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((foo (cons 3 foo))) (car (cdr (cdr (cdr foo)))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((foo (cons 1 bar)) (bar (cons 2 foo))) (car (cdr (cdr (cdr foo)))))")
                .unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(2))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn custom_builtins() {
        fn constant(_args: Vec<Expression>) -> Expression {
            Expression::new(Term::Value(ValueTerm::Int(3)))
        }
        fn add3(args: Vec<Expression>) -> Expression {
            let mut args = args.into_iter();
            let first = args.next().unwrap();
            let second = args.next().unwrap();
            let third = args.next().unwrap();
            match (first.value(), second.value(), third.value()) {
                (
                    Term::Value(ValueTerm::Int(first)),
                    Term::Value(ValueTerm::Int(second)),
                    Term::Value(ValueTerm::Int(third)),
                ) => Expression::new(Term::Value(ValueTerm::Int(first + second + third))),
                _ => panic!("Invalid arguments"),
            }
        }
        let constant = NativeFunction::new(0, Arity::from(0, 0, None), constant);
        let add3 = NativeFunction::new(1, Arity::from(3, 0, None), add3);

        let state = DynamicState::new();
        let mut cache = SubstitutionCache::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Native(constant)),
            vec![],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3))),
                DependencyList::empty(),
            ),
        );
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Native(add3)),
            vec![
                Expression::new(Term::Value(ValueTerm::Int(3))),
                Expression::new(Term::Value(ValueTerm::Int(4))),
                Expression::new(Term::Value(ValueTerm::Int(5))),
            ],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Expression::new(Term::Value(ValueTerm::Int(3 + 4 + 5))),
                DependencyList::empty(),
            ),
        );
    }

    #[test]
    fn partial_evaluation() {
        let expression =
            parse("((lambda (foo) ((lambda (bar) (foo 3)) #f)) (lambda (foo) #t))").unwrap();
        assert_eq!(
            expression.optimize(&mut SubstitutionCache::new()),
            Some(Expression::new(Term::Value(ValueTerm::Boolean(true))))
        );

        let expression = parse(
            "
            (let ((identity (lambda (value) value)))
                (let ((identity2 (lambda (value) (identity value))))
                    ((lambda (value) (identity2 (* value 2)))
                      3)))
        ",
        )
        .unwrap();
        assert_eq!(
            expression.optimize(&mut SubstitutionCache::new()),
            Some(Expression::new(Term::Value(ValueTerm::Int(6))))
        );
    }
}
