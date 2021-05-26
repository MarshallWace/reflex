// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{HashMap, HashSet},
    fmt,
    iter::once,
    sync::Arc,
};

pub use crate::serialize::{SerializedListTerm, SerializedObjectTerm, SerializedTerm};
use crate::{
    cache::EvaluationCache,
    hash::{
        combine_hashes, hash_bytes, hash_option, hash_seed, hash_sequence, hash_u32, hash_u8,
        hash_unordered_sequence, prefix_hash, HashId, Hashable,
    },
    stdlib::{
        builtin::BuiltinTerm, collection::CollectionTerm, signal::SignalType, value::ValueTerm,
    },
};

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
    fn signals(&self) -> Vec<Signal>;
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression>;
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression>;
}

pub(crate) trait Reducible {
    fn reduce(&self, cache: &mut EvaluationCache) -> Option<Expression>;
}

pub struct Substitutions<'a> {
    hash: HashId,
    substitutions: SubstitutionType<'a>,
}
impl<'a> Hashable for Substitutions<'a> {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl<'a> Substitutions<'a> {
    pub fn from(substitutions: SubstitutionType<'a>) -> Self {
        Self {
            hash: substitutions.hash(),
            substitutions,
        }
    }
    pub fn new(substitutions: &'a [(StackOffset, Expression)]) -> Self {
        let substitutions = SubstitutionType::Static(StaticSubstitutions::new(substitutions));
        Self {
            hash: substitutions.hash(),
            substitutions,
        }
    }
    pub fn wildcard(offset: StackOffset) -> Self {
        let substitutions = SubstitutionType::Static(StaticSubstitutions::all(offset));
        Self {
            hash: substitutions.hash(),
            substitutions,
        }
    }
    pub fn dynamic(state: &'a DynamicState) -> Self {
        let substitutions = SubstitutionType::Dynamic(state);
        Self {
            hash: substitutions.hash(),
            substitutions: substitutions,
        }
    }
    pub fn kind(&self) -> &SubstitutionType<'a> {
        &self.substitutions
    }
}

pub enum SubstitutionType<'a> {
    Static(StaticSubstitutions<'a>),
    Dynamic(&'a DynamicState),
}
impl<'a> Hashable for SubstitutionType<'a> {
    fn hash(&self) -> HashId {
        match self {
            Self::Static(substitutions) => prefix_hash(0, substitutions.hash()),
            Self::Dynamic(substitutions) => prefix_hash(1, substitutions.hash()),
        }
    }
}
pub enum StaticSubstitutions<'a> {
    Some(TargetedStaticSubstitutions<'a>),
    All(WildcardStaticSubstitutions),
}
impl<'a> Hashable for StaticSubstitutions<'a> {
    fn hash(&self) -> HashId {
        match self {
            Self::Some(substitutions) => prefix_hash(0, substitutions.hash()),
            Self::All(substitutions) => prefix_hash(1, substitutions.hash()),
        }
    }
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
    fn get(&self, offset: StackOffset, cache: &mut EvaluationCache) -> Option<Expression> {
        match self {
            Self::Some(substitutions) => substitutions.get(offset, cache),
            Self::All(substitutions) => substitutions.get(offset),
        }
    }
}

pub struct TargetedStaticSubstitutions<'a> {
    substitutions: &'a [(StackOffset, Expression)],
    min_depth: StackOffset,
    offset: StackOffset,
}
impl<'a> Hashable for TargetedStaticSubstitutions<'a> {
    fn hash(&self) -> HashId {
        combine_hashes(
            hash_stack_offset(self.offset),
            hash_sequence(
                self.substitutions
                    .iter()
                    .map(|(key, value)| combine_hashes(hash_stack_offset(*key), value.hash())),
            ),
        )
    }
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
    fn get(&self, offset: StackOffset, cache: &mut EvaluationCache) -> Option<Expression> {
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
                        .substitute(&Substitutions::wildcard(self.offset), cache)
                        .unwrap_or_else(|| Expression::clone(expression))
                } else {
                    Expression::clone(expression)
                }
            })
    }
}

pub struct WildcardStaticSubstitutions {
    offset: StackOffset,
}
impl Hashable for WildcardStaticSubstitutions {
    fn hash(&self) -> HashId {
        hash_stack_offset(self.offset)
    }
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
    value: Arc<Term>,
    hash: HashId,
    capture_depth: StackOffset,
    is_reduced: bool,
    is_optimized: bool,
    dynamic_dependencies: DependencyList,
    // TODO: Investigate cheap cloning for signals tree
    signals: Vec<Signal>,
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
    pub fn has_free_variables(&self) -> bool {
        self.capture_depth > 0
    }
    pub fn is_reducible(&self) -> bool {
        !self.is_reduced && self.value.is_reducible()
    }
    pub fn compile(&self, cache: &mut EvaluationCache) -> Expression {
        self.optimize(cache)
            .unwrap_or_else(|| Expression::clone(self))
    }
    pub fn evaluate(&self, state: &DynamicState, cache: &mut EvaluationCache) -> EvaluationResult {
        let substitutions = Substitutions::dynamic(state);
        let (result, dependencies) = evaluate_recursive(
            Expression::clone(self),
            &substitutions,
            cache,
            DependencyList::empty(),
        );
        let signals = result.signals();
        let result = if signals.is_empty() {
            Ok(result)
        } else {
            Err(signals)
        };
        EvaluationResult::new(result, dependencies)
    }
    fn create(value: Term, is_reduced: bool, is_optimized: bool) -> Self {
        let hash = value.hash();
        let capture_depth = value.capture_depth();
        let dynamic_dependencies = value.dynamic_dependencies();
        let signals = value.signals();
        Self {
            value: Arc::new(value),
            hash,
            capture_depth,
            dynamic_dependencies,
            signals,
            is_reduced,
            is_optimized,
        }
    }
    fn reduced(existing: &Expression) -> Self {
        Self {
            value: Arc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            signals: existing.signals.clone(),
            is_reduced: true,
            is_optimized: existing.is_optimized,
        }
    }
    fn optimized(existing: &Expression) -> Self {
        Self {
            value: Arc::clone(&existing.value),
            hash: existing.hash,
            capture_depth: existing.capture_depth,
            dynamic_dependencies: DependencyList::clone(&existing.dynamic_dependencies),
            signals: existing.signals.clone(),
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
    fn signals(&self) -> Vec<Signal> {
        self.signals.clone()
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        let can_skip = match substitutions.kind() {
            SubstitutionType::Static(substitutions) => substitutions.can_skip(self),
            SubstitutionType::Dynamic(_) => self.dynamic_dependencies.is_empty(),
        };
        if can_skip {
            return None;
        }
        match cache.retrieve_substitution(self, substitutions) {
            Some(result) => result,
            None => {
                let result = self.value.substitute(substitutions, cache);
                cache.store_substitution(self, substitutions, result.as_ref());
                result
            }
        }
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        if self.is_optimized {
            return None;
        }
        Some(
            self.value
                .optimize(cache)
                .unwrap_or_else(|| Self::optimized(self)),
        )
    }
}
impl Reducible for Expression {
    fn reduce(&self, cache: &mut EvaluationCache) -> Option<Expression> {
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
    state: &Substitutions,
    cache: &mut EvaluationCache,
    dependencies: DependencyList,
) -> (Expression, DependencyList) {
    let dependencies = dependencies.extend(expression.dynamic_dependencies());
    match expression.substitute(state, cache) {
        Some(expression) => evaluate_recursive(expression, state, cache, dependencies),
        None => match expression.reduce(cache) {
            Some(expression) => evaluate_recursive(expression, state, cache, dependencies),
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
    pub fn unwrap(self) -> (Result<Expression, Vec<Signal>>, DependencyList) {
        (self.result, self.dependencies)
    }
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
    pub fn contains(&self, entries: &HashSet<StateToken>) -> bool {
        match &self.dependencies {
            None => false,
            Some(dependencies) => dependencies.contains(entries),
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
    Native(NativeFunction),
    Struct(StructTerm),
    Enum(EnumTerm),
    StructConstructor(VarArgs, StructPrototype),
    EnumConstructor(EnumVariantPrototype),
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
            Self::Native(term) => prefix_hash(6, term.hash()),
            Self::Struct(term) => prefix_hash(7, term.hash()),
            Self::Enum(term) => prefix_hash(8, term.hash()),
            Self::StructConstructor(eager, prototype) => prefix_hash(
                9,
                prefix_hash(
                    match eager {
                        VarArgs::Lazy => 0,
                        VarArgs::Eager => 1,
                    },
                    prototype.hash(),
                ),
            ),
            Self::EnumConstructor(term) => prefix_hash(10, term.hash()),
            Self::Collection(term) => prefix_hash(11, term.hash()),
            Self::Signal(term) => prefix_hash(12, term.hash()),
        }
    }
}
impl Rewritable for Term {
    fn signals(&self) -> Vec<Signal> {
        match self {
            Self::Variable(term) => term.signals(),
            Self::Lambda(term) => term.signals(),
            Self::Application(term) => term.signals(),
            Self::Recursive(term) => term.signals(),
            Self::Struct(term) => term.signals(),
            Self::Enum(term) => term.signals(),
            Self::Collection(term) => term.signals(),
            Self::Signal(term) => term.signals(),
            _ => Vec::new(),
        }
    }
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Variable(term) => term.capture_depth(),
            Self::Lambda(term) => term.capture_depth(),
            Self::Application(term) => term.capture_depth(),
            Self::Recursive(term) => term.capture_depth(),
            Self::Struct(term) => term.capture_depth(),
            Self::Enum(term) => term.capture_depth(),
            Self::Collection(term) => term.capture_depth(),
            Self::Signal(term) => term.capture_depth(),
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
            Self::Signal(term) => term.dynamic_dependencies(),
            _ => DependencyList::empty(),
        }
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.substitute(substitutions, cache),
            Self::Lambda(term) => term.substitute(substitutions, cache),
            Self::Application(term) => term.substitute(substitutions, cache),
            Self::Recursive(term) => term.substitute(substitutions, cache),
            Self::Struct(term) => term.substitute(substitutions, cache),
            Self::Enum(term) => term.substitute(substitutions, cache),
            Self::Collection(term) => term.substitute(substitutions, cache),
            Self::Signal(term) => term.substitute(substitutions, cache),
            _ => None,
        }
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        match self {
            Self::Variable(term) => term.optimize(cache),
            Self::Lambda(term) => term.optimize(cache),
            Self::Application(term) => term.optimize(cache),
            Self::Recursive(term) => term.optimize(cache),
            Self::Struct(term) => term.optimize(cache),
            Self::Enum(term) => term.optimize(cache),
            Self::Collection(term) => term.optimize(cache),
            Self::Signal(term) => term.optimize(cache),
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
    fn reduce(&self, cache: &mut EvaluationCache) -> Option<Expression> {
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
    fn signals(&self) -> Vec<Signal> {
        match self {
            Self::Static(term) => term.signals(),
            Self::Dynamic(term) => term.signals(),
        }
    }
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
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        match self {
            Self::Static(term) => term.substitute(substitutions, cache),
            Self::Dynamic(term) => term.substitute(substitutions, cache),
        }
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        match self {
            Self::Static(term) => term.optimize(cache),
            Self::Dynamic(term) => term.optimize(cache),
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
    fn signals(&self) -> Vec<Signal> {
        Vec::new()
    }
    fn capture_depth(&self) -> StackOffset {
        self.offset + 1
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        match substitutions.kind() {
            SubstitutionType::Static(substitutions) => substitutions.get(self.offset, cache),
            _ => None,
        }
    }
    fn optimize(&self, _cache: &mut EvaluationCache) -> Option<Expression> {
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
    fn signals(&self) -> Vec<Signal> {
        Vec::new()
    }
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::of(DynamicDependencies::of(self.id))
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        _cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        match substitutions.kind() {
            SubstitutionType::Dynamic(state) => Some(match state.get(self.id) {
                Some(value) => Expression::clone(value),
                None => Expression::clone(&self.fallback),
            }),
            _ => None,
        }
    }
    fn optimize(&self, _cache: &mut EvaluationCache) -> Option<Expression> {
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
impl Hashable for DynamicState {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl DynamicState {
    pub fn new() -> Self {
        DynamicState {
            hash: hash_seed(),
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
        let value_hash = value.hash();
        let previous = self.values.insert(key, value);
        let has_changes = match previous {
            Some(previous_value) => value_hash != previous_value.hash(),
            None => true,
        };
        if has_changes {
            let entry_hash = combine_hashes(hash_state_token(key), value_hash);
            self.hash = combine_hashes(self.hash, entry_hash);
        }
    }
}

pub type StateToken = u32;
fn hash_state_token(value: StateToken) -> HashId {
    hash_u32(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct DynamicDependencies {
    values: Arc<HashSet<StateToken>>,
}
impl Hashable for DynamicDependencies {
    fn hash(&self) -> HashId {
        hash_unordered_sequence(self.values.iter().copied())
    }
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
    fn contains(&self, entries: &HashSet<StateToken>) -> bool {
        !self.values.is_disjoint(entries)
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
    fn signals(&self) -> Vec<Signal> {
        self.factory.signals()
    }
    fn capture_depth(&self) -> StackOffset {
        self.factory.capture_depth()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.factory.dynamic_dependencies()
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        self.factory
            .substitute(substitutions, cache)
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        self.factory
            .optimize(cache)
            .map(|factory| Expression::new(Term::Recursive(Self { factory })))
    }
}
impl Reducible for RecursiveTerm {
    fn reduce(&self, cache: &mut EvaluationCache) -> Option<Expression> {
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
    fn apply(
        &self,
        args: impl IntoIterator<Item = Expression> + ExactSizeIterator,
        cache: &mut EvaluationCache,
    ) -> Expression {
        // TODO: support variadic 'rest args' iterator in lambda functions
        let arity = self.arity.required();
        let substitutions = args
            .into_iter()
            .take(arity)
            .enumerate()
            .map(|(index, arg)| ((arity - index - 1) as StackOffset, arg))
            .collect::<Vec<_>>();
        self.body
            .substitute(&Substitutions::new(&substitutions), cache)
            .unwrap_or_else(|| Expression::clone(&self.body))
    }
}
impl Rewritable for LambdaTerm {
    fn signals(&self) -> Vec<Signal> {
        Vec::new()
    }
    fn capture_depth(&self) -> StackOffset {
        let arity = self.arity.required() as StackOffset;
        self.body.capture_depth().saturating_sub(arity)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute<'a>(
        &self,
        substitutions: &'a Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        let arity = self.arity.required() as StackOffset;
        let body = match substitutions.kind() {
            SubstitutionType::Dynamic(_) => self.body.substitute(substitutions, cache),
            SubstitutionType::Static(substitutions) => self.body.substitute(
                &Substitutions::from(SubstitutionType::Static(substitutions.offset(arity))),
                cache,
            ),
        };
        body.map(|body| Expression::new(Term::Lambda(LambdaTerm::new(self.arity, body))))
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        let arity = self.arity.required() as StackOffset;
        let reduced_body = self.body.reduce(cache);
        let eta_reduced_body = reduced_body.as_ref().map_or_else(
            || apply_eta_reduction(&self.body, arity),
            |body| apply_eta_reduction(&body, arity),
        );
        eta_reduced_body
            .and_then(|body| body.reduce(cache))
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
    fn signals(&self) -> Vec<Signal> {
        let mut signals = self.target.signals();
        signals.extend(signals_multiple(&self.args));
        signals
    }
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
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        let target = self.target.substitute(substitutions, cache);
        let args = substitute_multiple(&self.args, substitutions, cache);
        if target.is_none() && args.is_none() {
            return None;
        }
        let target = target.unwrap_or_else(|| Expression::clone(&self.target));
        let args = args.unwrap_or_else(|| self.args.clone());
        Some(Expression::new(Term::Application(Self::new(target, args))))
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        let optimized_target = self.target.optimize(cache);
        let optimized_args = optimize_multiple(&self.args, cache);
        let optimized_expression = optimized_args.map(|args| {
            Expression::new(Term::Application(Self::new(
                optimized_target.unwrap_or_else(|| Expression::clone(&self.target)),
                args,
            )))
        });
        match &optimized_expression {
            Some(expression) => expression.reduce(cache),
            None => self.reduce(cache),
        }
        .or(optimized_expression)
    }
}
impl Reducible for ApplicationTerm {
    fn reduce(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        let reduced_target = self.target.reduce(cache);
        let target = reduced_target
            .as_ref()
            .map_or_else(|| self.target.value(), |target| target.value());
        let arity = match &target {
            Term::Lambda(target) => Some((target.arity, false)),
            Term::Builtin(target) => Some((target.arity(), true)),
            Term::Native(target) => Some((target.arity, true)),
            Term::StructConstructor(eager, target) => Some((target.arity(eager), false)),
            Term::EnumConstructor(target) => Some((Arity::from(0, target.arity, None), false)),
            _ => None,
        };
        match arity {
            None => None,
            Some((arity, strict)) => {
                let result = match evaluate_args(self.args.iter(), arity, strict, cache) {
                    Err(args) => Err(args),
                    Ok(args) => match &target {
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
                };
                match result {
                    Ok(result) => result.reduce(cache).or_else(|| Some(result)),
                    Err(args) => {
                        if reduced_target.is_none()
                            && self
                                .args
                                .iter()
                                .zip(args.iter())
                                .all(|(arg, evaluated_arg)| arg.hash() == evaluated_arg.hash())
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
    strict: bool,
    cache: &mut EvaluationCache,
) -> Result<Vec<Expression>, Vec<Expression>> {
    if args.len() < arity.required() {
        // TODO: Handle application arity errors gracefully
        panic!(
            "Expected {} arguments, received {}",
            arity.required(),
            args.len()
        );
    }
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
                            Term::Value(_) => false,
                            Term::Signal(_) => true,
                            _ => {
                                strict
                                    && (arg.capture_depth() > 0
                                        || !arg.dynamic_dependencies().is_empty()
                                        || !arg.signals().is_empty())
                            }
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
    hash: HashId,
    arity: Arity,
    body: Arc<dyn Fn(Vec<Expression>) -> Expression + Sync + Send>,
}
impl NativeFunction {
    pub fn new(
        hash: HashId,
        arity: Arity,
        body: impl Fn(Vec<Expression>) -> Expression + Sync + Send + 'static,
    ) -> Self {
        Self {
            hash,
            arity,
            body: Arc::new(body),
        }
    }
    fn apply(&self, args: impl IntoIterator<Item = Expression> + ExactSizeIterator) -> Expression {
        (self.body)(args.into_iter().collect())
    }
}
impl Hashable for NativeFunction {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}
impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native:{}:{}>", self.arity.required(), self.hash)
    }
}
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub type StructFieldOffset = u32;
pub fn hash_struct_field_offset(value: StructFieldOffset) -> HashId {
    hash_u32(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructTerm {
    prototype: Option<StructPrototype>,
    fields: Vec<Expression>,
}
impl Hashable for StructTerm {
    fn hash(&self) -> HashId {
        combine_hashes(
            hash_option(
                self.prototype
                    .as_ref()
                    .map(|constructor| constructor.hash()),
            ),
            hash_sequence(self.fields.iter().map(|field| field.hash())),
        )
    }
}
impl StructTerm {
    pub fn new(prototype: Option<StructPrototype>, fields: Vec<Expression>) -> Self {
        Self { prototype, fields }
    }
    pub fn get(&self, field_offset: StructFieldOffset) -> Option<&Expression> {
        self.fields.get(field_offset as usize)
    }
    pub fn prototype(&self) -> Option<&StructPrototype> {
        self.prototype.as_ref()
    }
    pub fn fields(&self) -> &[Expression] {
        &self.fields
    }
}
impl Rewritable for StructTerm {
    fn signals(&self) -> Vec<Signal> {
        Vec::new()
    }
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.fields)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        substitute_multiple(&self.fields, substitutions, cache)
            .map(|fields| Expression::new(Term::Struct(Self::new(self.prototype.clone(), fields))))
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        optimize_multiple(&self.fields, cache)
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

#[derive(PartialEq, Clone, Debug)]
pub struct StructPrototype {
    keys: Vec<ValueTerm>,
}
impl Hashable for StructPrototype {
    fn hash(&self) -> HashId {
        hash_sequence(self.keys.iter().map(|key| key.hash()))
    }
}
impl StructPrototype {
    pub fn new(keys: Vec<ValueTerm>) -> Self {
        Self { keys }
    }
    pub fn field(&self, key: &ValueTerm) -> Option<StructFieldOffset> {
        self.keys
            .iter()
            .enumerate()
            .find(|(_, existing)| existing.hash() == key.hash())
            .map(|(index, _)| index as StructFieldOffset)
    }
    pub fn keys(&self) -> &[ValueTerm] {
        &self.keys
    }
    fn arity(&self, eager: &VarArgs) -> Arity {
        let num_keys = self.keys.len() as u8;
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
                .zip(self.keys.iter())
                .all(|((id, _), key)| id.hash() == key.hash());
        let fields = if has_correctly_ordered_keys {
            Some(
                fields
                    .into_iter()
                    .take(self.keys.len())
                    .map(|(_, value)| value)
                    .collect::<Vec<_>>(),
            )
        } else {
            let mut remaining_fields = fields;
            self.keys
                .iter()
                .map(|key| {
                    let index = remaining_fields
                        .iter()
                        .position(|(id, _)| id.hash() == key.hash())?;
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
    fn signals(&self) -> Vec<Signal> {
        Vec::new()
    }
    fn capture_depth(&self) -> StackOffset {
        capture_depth_multiple(&self.args)
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        substitute_multiple(&self.args, substitutions, cache)
            .map(|args| Expression::new(Term::Enum(Self::new(self.index, args))))
    }
    fn optimize(&self, cache: &mut EvaluationCache) -> Option<Expression> {
        optimize_multiple(&self.args, cache)
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

pub type EnumConstructorArity = u8;
fn hash_enum_constructor_arity(value: EnumConstructorArity) -> HashId {
    hash_u8(value)
}

#[derive(PartialEq, Clone, Debug)]
pub struct EnumVariantPrototype {
    index: EnumIndex,
    arity: EnumConstructorArity,
}
impl Hashable for EnumVariantPrototype {
    fn hash(&self) -> HashId {
        prefix_hash(self.index, hash_enum_constructor_arity(self.arity))
    }
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

#[derive(PartialEq, Clone, Debug)]
pub struct SignalTerm {
    signal: Signal,
}
impl Hashable for SignalTerm {
    fn hash(&self) -> HashId {
        self.signal.hash
    }
}
impl Rewritable for SignalTerm {
    fn signals(&self) -> Vec<Signal> {
        vec![self.signal.clone()]
    }
    fn capture_depth(&self) -> StackOffset {
        0
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        let id = self.signal.hash;
        match self.signal.signal {
            SignalType::Error | SignalType::Pending => DependencyList::empty(),
            SignalType::Custom(_) => DependencyList::of(DynamicDependencies::of(id)),
        }
    }
    fn substitute(
        &self,
        substitutions: &Substitutions,
        _cache: &mut EvaluationCache,
    ) -> Option<Expression> {
        let id = self.signal.hash;
        match substitutions.substitutions {
            SubstitutionType::Static(_) => None,
            SubstitutionType::Dynamic(state) => state.get(id).map(Expression::clone),
        }
    }
    fn optimize(&self, _cache: &mut EvaluationCache) -> Option<Expression> {
        None
    }
}
impl SignalTerm {
    pub fn new(signal: Signal) -> Self {
        Self { signal }
    }
    pub fn get_type(&self) -> &SignalType {
        self.signal.get_type()
    }
    pub fn is_type(&self, signal: SignalType) -> bool {
        self.signal.is_type(signal)
    }
    pub fn args(&self) -> Option<&Vec<SerializedTerm>> {
        self.signal.args()
    }
}
impl fmt::Display for SignalTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.signal, f)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Signal {
    hash: HashId,
    signal: SignalType,
    args: Option<Vec<SerializedTerm>>,
}
impl Hashable for Signal {
    fn hash(&self) -> HashId {
        self.hash
    }
}
impl Signal {
    pub fn new(signal: SignalType, args: impl IntoIterator<Item = SerializedTerm>) -> Self {
        let args = args.into_iter().collect::<Vec<_>>();
        let hash = combine_hashes(
            signal.hash(),
            hash_sequence(args.iter().map(|arg| arg.hash())),
        );
        Self {
            hash,
            signal,
            args: if args.is_empty() { None } else { Some(args) },
        }
    }
    pub fn get_type(&self) -> &SignalType {
        &self.signal
    }
    pub fn is_type(&self, signal: SignalType) -> bool {
        self.signal == signal
    }
    pub fn args(&self) -> Option<&Vec<SerializedTerm>> {
        self.args.as_ref()
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

pub(crate) fn capture_depth_multiple(expressions: &[Expression]) -> StackOffset {
    expressions
        .iter()
        .map(|expression| expression.capture_depth())
        .fold(0 as StackOffset, |acc, depth| acc.max(depth))
}

pub(crate) fn signals_multiple(expressions: &[Expression]) -> Vec<Signal> {
    expressions
        .iter()
        .flat_map(|expression| expression.signals())
        .collect()
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
    cache: &mut EvaluationCache,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |arg| arg.substitute(substitutions, cache))
}

pub(crate) fn optimize_multiple<'a>(
    expressions: &[Expression],
    cache: &mut EvaluationCache,
) -> Option<Vec<Expression>> {
    transform_expressions(expressions, |expression| expression.optimize(cache))
}

fn transform_expressions<'a>(
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
        cache::EvaluationCache, parser::sexpr::parse, stdlib::builtin::BuiltinTerm,
        stdlib::value::ValueTerm,
    };

    use super::{
        ApplicationTerm, Arity, DependencyList, DynamicState, EvaluationResult, Expression,
        NativeFunction, Rewritable, Term,
    };

    #[test]
    fn value_expressions() {
        let state = DynamicState::new();
        let mut cache = EvaluationCache::new();
        let expression = parse("3").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let mut cache = EvaluationCache::new();
        let expression = parse("(lambda (foo) foo)").unwrap();
        let result = expression.value.optimize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(result, None);

        let expression = parse("(lambda (foo bar) (+ foo bar))").unwrap();
        let optimized = expression.value.optimize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(optimized, Some(parse("+").unwrap()));

        let expression = parse("(lambda (foo bar baz) (+ foo bar))").unwrap();
        let optimized = expression.value.optimize(&mut cache);
        assert_eq!(expression.capture_depth(), 0);
        assert_eq!(optimized, Some(parse("+").unwrap()));
    }

    #[test]
    fn lambda_application_expressions() {
        let state = DynamicState::new();
        let mut cache = EvaluationCache::new();
        let expression = parse("((lambda (foo) foo) 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );

        let expression = parse("((lambda (foo bar) (+ foo bar)) 3 4)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let result = expression.evaluate(&state, &mut cache);
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
        let mut cache = EvaluationCache::new();
        let expression = parse("+").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let mut cache = EvaluationCache::new();
        let expression = parse("(+ 1 2)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let mut cache = EvaluationCache::new();
        let expression = parse("(let () 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(let ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let mut cache = EvaluationCache::new();
        let expression = parse("(letrec () 3)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar 4)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) first))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((first 3)) (let ((second 4)) (let ((third first) (fourth second)) (+ third fourth))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4)))),
                DependencyList::empty(),
            ),
        );

        let expression = parse("(letrec ((foo 3) (bar foo)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo bar) (bar 3)) (+ foo bar))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 3)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo 3) (bar (+ foo 1))) bar)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 1)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((foo (+ bar 1)) (bar 3)) foo)").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let result = expression.evaluate(&state, &mut cache);
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
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Boolean(false)))),
                DependencyList::empty(),
            ),
        );
        let expression = parse("(letrec ((is-even? (lambda (x) (if (= x 0) #t (is-odd? (- x 1))))) (is-odd? (lambda (x) (if (= x 0) #f (is-even? (- x 1)))))) (is-odd? 3))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Boolean(true)))),
                DependencyList::empty(),
            ),
        );
        let expression =
            parse("(letrec ((foo (cons 3 foo))) (car (cdr (cdr (cdr foo)))))").unwrap();
        let result = expression.evaluate(&state, &mut cache);
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
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(2)))),
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
        let mut cache = EvaluationCache::new();
        let expression = Expression::new(Term::Application(ApplicationTerm::new(
            Expression::new(Term::Native(constant)),
            vec![],
        )));
        let result = expression.evaluate(&state, &mut cache);
        assert_eq!(
            result,
            EvaluationResult::new(
                Ok(Expression::new(Term::Value(ValueTerm::Int(3)))),
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
                Ok(Expression::new(Term::Value(ValueTerm::Int(3 + 4 + 5)))),
                DependencyList::empty(),
            ),
        );
    }
}
