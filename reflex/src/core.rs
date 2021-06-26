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
    fn eager(&self) -> usize {
        self.eager
    }
    fn required(&self) -> usize {
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
    substitutions: Option<&'a [(StackOffset, Expression)]>,
    min_depth: StackOffset,
    offset: StackOffset,
}
impl<'a> Substitutions<'a> {
    pub fn named(substitutions: &'a [(StackOffset, Expression)]) -> Self {
        Self {
            substitutions: Some(substitutions),
            min_depth: substitutions
                .iter()
                .fold(StackOffset::MAX, |acc, (offset, _)| acc.min(*offset)),
            offset: 0,
        }
    }
    pub fn wildcard(offset: StackOffset) -> Self {
        Self {
            substitutions: None,
            min_depth: 0,
            offset,
        }
    }
    fn offset(&self, offset: StackOffset) -> Self {
        Self {
            substitutions: match self.substitutions {
                Some(entries) => Some(entries),
                None => None,
            },
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    fn can_skip(&self, expression: &Expression) -> bool {
        let capture_depth = expression.capture_depth();
        capture_depth == 0
            || self.substitutions.is_some() && (capture_depth - 1 < self.min_depth + self.offset)
    }
    fn get(&self, offset: StackOffset, cache: &mut impl EvaluationCache) -> Option<Expression> {
        match self.substitutions {
            None => Some(Expression::new(Term::Variable(VariableTerm::scoped(
                self.offset + offset,
            )))),
            Some(entries) => {
                if offset < self.offset {
                    return None;
                }
                let target_offset = offset - self.offset;
                entries
                    .iter()
                    .find(|(offset, _)| (*offset == target_offset))
                    .map(|(_, expression)| {
                        if expression.capture_depth() > 0 {
                            expression
                                .substitute_static(&Substitutions::wildcard(self.offset), cache)
                                .unwrap_or_else(|| Expression::clone(expression))
                        } else {
                            Expression::clone(expression)
                        }
                    })
            }
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
    pub fn compile(&self, cache: &mut impl EvaluationCache) -> Expression {
        self.normalize(cache)
            .unwrap_or_else(|| Expression::clone(self))
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
    fn reduced(existing: &Expression) -> Self {
        Self {
            value: Arc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            is_reduced: true,
            is_normalized: existing.is_normalized,
        }
    }
    fn normalized(existing: &Expression) -> Self {
        Self {
            value: Arc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            is_reduced: existing.is_reduced,
            is_normalized: true,
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
                cache.store_static_substitution(self, substitutions, result.as_ref());
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
                cache.store_dynamic_substitution(self, state, result.as_ref());
                result
            }
        }
    }
    fn normalize(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        if self.is_normalized {
            return None;
        }
        Some(
            self.value
                .normalize(cache)
                .unwrap_or_else(|| Self::normalized(self)),
        )
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
                let result = Some(
                    self.value
                        .reduce(cache)
                        .unwrap_or_else(|| Self::reduced(self)),
                );
                cache.store_reduction(self, result.as_ref());
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

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
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
    pub fn from(values: impl IntoIterator<Item = StateToken>) -> Self {
        let values = values.into_iter().collect::<Vec<_>>();
        if values.is_empty() {
            DependencyList::empty()
        } else {
            DependencyList::of(DynamicDependencies::from(values))
        }
    }
    pub fn is_empty(&self) -> bool {
        self.dependencies.is_none()
    }
    pub fn extend(self, other: Self) -> Self {
        match &self.dependencies {
            None => other,
            Some(dependencies) => match &other.dependencies {
                None => self,
                Some(other) => DependencyList::of(dependencies.union(other)),
            },
        }
    }
    pub fn contains(&self, entries: &BTreeSet<StateToken>) -> bool {
        match &self.dependencies {
            None => false,
            Some(dependencies) => dependencies.contains(entries),
        }
    }
}
impl<'a> IntoIterator for &'a DependencyList {
    type Item = StateToken;
    type IntoIter = DependencyListIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.dependencies {
            Some(dependencies) => DependencyListIntoIter::Some(dependencies.iter()),
            None => DependencyListIntoIter::None,
        }
    }
}
pub enum DependencyListIntoIter<'a> {
    Some(DynamicDependenciesIter<'a>),
    None,
}
impl<'a> Iterator for DependencyListIntoIter<'a> {
    type Item = StateToken;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Some(iter) => iter.next(),
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
    id: StateToken,
    fallback: Expression,
}
impl DynamicVariableTerm {
    fn new(id: StateToken, fallback: Expression) -> Self {
        Self { id, fallback }
    }
}
impl Rewritable for DynamicVariableTerm {
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::of(DynamicDependencies::of(self.id))
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
        Some(match state.get(self.id) {
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
        write!(f, "<dynamic:{}>", self.id)
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

pub type StateToken = HashId;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct DynamicDependencies {
    values: Arc<BTreeSet<StateToken>>,
}
impl DynamicDependencies {
    pub fn from(values: impl IntoIterator<Item = StateToken>) -> Self {
        Self {
            values: Arc::new(values.into_iter().collect()),
        }
    }
    pub fn of(id: StateToken) -> Self {
        Self::from(once(id))
    }
    fn union(&self, other: &DynamicDependencies) -> Self {
        Self::from(self.values.union(&other.values).copied())
    }
    fn contains(&self, entries: &BTreeSet<StateToken>) -> bool {
        !self.values.is_disjoint(entries)
    }
    fn iter(&self) -> DynamicDependenciesIter<'_> {
        self.values.iter().copied()
    }
}
type DynamicDependenciesIter<'a> = std::iter::Copied<std::collections::btree_set::Iter<'a, u64>>;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct RecursiveTerm {
    factory: Expression,
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
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
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
    fn apply(
        &self,
        args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
        cache: &mut impl EvaluationCache,
    ) -> Expression {
        // TODO: support variadic 'rest args' iterator in lambda functions
        let arity = self.arity.required();
        let substitutions = args
            .into_iter()
            .take(arity)
            .enumerate()
            .map(|(index, arg)| ((arity - index - 1), arg))
            .collect::<Vec<_>>();
        self.body
            .substitute_static(&Substitutions::named(&substitutions), cache)
            .unwrap_or_else(|| Expression::clone(&self.body))
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
        let normalized_body = self.body.reduce(cache);
        let eta_reduced_body = normalized_body.as_ref().map_or_else(
            || apply_eta_reduction(&self.body, arity),
            |body| apply_eta_reduction(&body, arity),
        );
        eta_reduced_body
            .and_then(|body| body.reduce(cache))
            .or(eta_reduced_body.map(Expression::clone))
            .or(normalized_body.map(|body| {
                Expression::new(Term::Lambda(LambdaTerm::new(self.arity.clone(), body)))
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
            Term::Native(target) => Some(target.arity),
            Term::StructConstructor(eager, prototype) => Some(prototype.arity(eager)),
            Term::EnumConstructor(target) => Some(Arity::from(0, target.arity, None)),
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
        let args = substitute_dynamic_multiple(&self.args, state, cache);
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
        let normalized_expression = match normalized_args {
            Some(args) => Some(Expression::new(Term::Application(Self::new(
                normalized_target.unwrap_or_else(|| Expression::clone(&self.target)),
                args,
            )))),
            None => normalized_target.map(|target| {
                Expression::new(Term::Application(Self::new(
                    target,
                    self.args.iter().map(Expression::clone).collect(),
                )))
            }),
        };
        match &normalized_expression {
            Some(expression) => expression.reduce(cache),
            None => self.reduce(cache),
        }
        .or(normalized_expression)
    }
}
impl Reducible for ApplicationTerm {
    fn reduce(&self, cache: &mut impl EvaluationCache) -> Option<Expression> {
        let reduced_target = self.target.reduce(cache);
        let short_circuit_target = match &reduced_target {
            Some(target) => match target.value() {
                Term::Signal(_) => true,
                _ => false,
            },
            _ => false,
        };
        if short_circuit_target {
            return reduced_target;
        }
        let target = reduced_target
            .as_ref()
            .map_or_else(|| self.target.value(), |target| target.value());
        let arity = match &target {
            Term::Lambda(target) => Some(target.arity),
            Term::Builtin(target) => Some(target.arity()),
            Term::Native(target) => Some(target.arity),
            Term::StructConstructor(eager, target) => Some(target.arity(eager)),
            Term::EnumConstructor(target) => Some(Arity::from(0, target.arity, None)),
            _ => None,
        };
        match arity {
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
                let result = match evaluate_args(self.args.iter(), arity, cache) {
                    Err(args) => Err(args),
                    Ok(args) => {
                        let num_signal_args =
                            args.iter().fold(0, |result, arg| match arg.value() {
                                Term::Signal(_) => result + 1,
                                _ => result,
                            });
                        match num_signal_args {
                            0 => match &target {
                                Term::Lambda(target) => Ok(target.apply(args.into_iter(), cache)),
                                Term::Builtin(target) => Ok(target.apply(args.into_iter())),
                                Term::Native(target) => Ok(target.apply(args.into_iter())),
                                Term::StructConstructor(_, target) => {
                                    let (keys, values) = args.split_at(args.len() / 2);
                                    let keys = keys
                                        .iter()
                                        .map(|key| match key.value() {
                                            Term::Value(key) => Some(key),
                                            _ => None,
                                        })
                                        .collect::<Option<Vec<_>>>();
                                    let result = keys.and_then(|keys| {
                                        target.apply(
                                            keys.into_iter()
                                                .zip(values.iter().map(Expression::clone))
                                                .map(|(key, value)| (key, value)),
                                        )
                                    });
                                    match result {
                                        Some(result) => Ok(Expression::new(Term::Struct(result))),
                                        None => Err(args),
                                    }
                                }
                                Term::EnumConstructor(target) => Ok(target.apply(args.into_iter())),
                                _ => Err(args),
                            },
                            1 => Ok(args
                                .into_iter()
                                .find(|arg| match arg.value() {
                                    Term::Signal(_) => true,
                                    _ => false,
                                })
                                .unwrap()),
                            _ => Ok(Expression::new(Term::Signal(SignalTerm::from(
                                args.iter()
                                    .filter_map(|arg| match arg.value() {
                                        Term::Signal(term) => Some(term),
                                        _ => None,
                                    })
                                    .flat_map(|term| {
                                        term.signals().into_iter().map(|signal| signal.clone())
                                    }),
                            )))),
                        }
                    }
                };
                match result {
                    Ok(result) => result.reduce(cache).or_else(|| Some(result)),
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
                            Some(Expression::new(Term::Application(Self::new(target, args))))
                        }
                    }
                }
            }
        }
    }
}

fn evaluate_args<'a>(
    args: impl IntoIterator<Item = &'a Expression> + ExactSizeIterator,
    arity: Arity,
    cache: &mut impl EvaluationCache,
) -> Result<Vec<Expression>, Vec<Expression>> {
    let eager_arity = arity.eager();
    let eager_varargs = match arity.variadic {
        Some(VarArgs::Eager) => true,
        _ => false,
    };
    if eager_arity == 0 && !eager_varargs {
        Ok(args.into_iter().map(Expression::clone).collect::<Vec<_>>())
    } else {
        let required_arity = arity.required();
        let mut has_unresolved_args = false;
        let args = args
            .into_iter()
            .enumerate()
            .map(|(index, arg)| {
                let is_eager = index < eager_arity || (eager_varargs && index >= required_arity);
                if is_eager {
                    let arg = arg.reduce(cache).unwrap_or_else(|| Expression::clone(arg));
                    has_unresolved_args = has_unresolved_args
                        || match arg.value() {
                            Term::Variable(_) => true,
                            term => term.is_reducible(),
                        };
                    arg
                } else {
                    Expression::clone(arg)
                }
            })
            .collect::<Vec<_>>();
        if has_unresolved_args {
            Err(args)
        } else {
            Ok(args)
        }
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
    pub fn get(&self, field_offset: StructFieldOffset) -> Option<&Expression> {
        self.fields.get(field_offset)
    }
    pub fn prototype(&self) -> Option<&StructPrototype> {
        self.prototype.as_ref()
    }
    pub fn fields(&self) -> &[Expression] {
        &self.fields
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
        normalize_multiple(&self.fields, cache)
            .map(|fields| Expression::new(Term::Struct(Self::new(self.prototype.clone(), fields))))
    }
}
impl fmt::Display for StructTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<struct:{}>",
            self.prototype.as_ref().map_or_else(
                || format!("{}", self.fields.len()),
                |prototype| format!("{}", prototype)
            ),
        )
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
    pub fn keys(&self) -> &[ValueTerm] {
        &self.keys
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
            .map(|args| Expression::new(Term::Enum(Self::new(self.index, args))))
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
    fn apply(&self, args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        Expression::new(Term::Enum(EnumTerm::new(
            self.index,
            args.into_iter().collect(),
        )))
    }
    pub fn index(&self) -> EnumIndex {
        self.index
    }
    pub fn arity(&self) -> EnumConstructorArity {
        self.arity
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
            "{{{}}}",
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
    signal: SignalType,
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
    pub fn new(signal: SignalType, args: impl IntoIterator<Item = Expression>) -> Self {
        let args = args.into_iter().collect::<Vec<_>>();
        let hash = {
            let mut hasher = DefaultHasher::new();
            signal.hash(&mut hasher);
            for arg in args.iter() {
                arg.hash(&mut hasher);
            }
            hasher.finish()
        };
        Self { hash, signal, args }
    }
    pub fn id(&self) -> StateToken {
        self.hash
    }
    pub fn get_type(&self) -> &SignalType {
        &self.signal
    }
    pub fn is_type(&self, signal: SignalType) -> bool {
        self.signal == signal
    }
    pub fn args(&self) -> &[Expression] {
        &self.args
    }
}
impl fmt::Display for Signal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<signal:{}:{}>",
            self.signal,
            self.args
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(",")
        )
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
        cache::GenerationalGc,
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
        let mut cache = GenerationalGc::new();
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
}
