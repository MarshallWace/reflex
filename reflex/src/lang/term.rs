// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
mod application;
use std::collections::HashSet;

pub use application::*;
mod builtin;
pub use builtin::*;
mod collection;
pub use collection::*;
mod compiled;
pub use compiled::*;
mod lambda;
pub use lambda::*;
mod r#let;
pub use r#let::*;
mod native;
pub use native::*;
mod partial;
pub use partial::*;
mod recursive;
pub use recursive::*;
mod signal;
pub use signal::*;
mod r#struct;
pub use r#struct::*;
mod tuple;
pub use tuple::*;
mod value;
pub use value::*;
mod variable;
pub use variable::*;

use crate::{
    compiler::{Compile, Compiler, Program},
    core::{
        Applicable, Arity, DependencyList, DynamicState, Evaluate, EvaluationCache,
        EvaluationResult, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SerializeJson, StackOffset, StringValue, Substitutions, VarArgs,
    },
    hash::HashId,
};

pub type StringPrimitive = String;
impl StringValue for StringPrimitive {
    fn as_str(&self) -> &str {
        self
    }
}

pub type CachedList<TValue> = ExpressionList<CachedTerm<TValue>>;

#[derive(Eq, Clone, Copy)]
pub struct CachedTerm<TValue: Expression> {
    hash: HashId,
    capture_depth: StackOffset,
    value: TValue,
}
impl<T: Expression> CachedTerm<T> {
    pub fn new(value: T) -> Self {
        Self {
            hash: value.id(),
            capture_depth: value.capture_depth(),
            value,
        }
    }
    pub fn value(&self) -> &T {
        &self.value
    }
}
impl<T: Expression> Expression for CachedTerm<T> {
    type String = StringPrimitive;
    fn id(&self) -> HashId {
        self.hash
    }
}
impl<T: Expression> std::hash::Hash for CachedTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
impl<T: Expression> PartialEq for CachedTerm<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl<T: Expression> GraphNode for CachedTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.capture_depth
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        self.value.dynamic_dependencies()
    }
    fn is_static(&self) -> bool {
        self.value.is_static()
    }
    fn is_atomic(&self) -> bool {
        self.value.is_atomic()
    }
}
impl<T: Expression + Rewritable<CachedTerm<T>>> Rewritable<CachedTerm<T>> for CachedTerm<T> {
    fn subexpressions(&self) -> Vec<&CachedTerm<T>> {
        self.value.subexpressions()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<CachedTerm<T>>,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Option<CachedTerm<T>> {
        if substitutions.can_skip(self) {
            return None;
        }
        match cache.retrieve_static_substitution(self, substitutions) {
            Some(result) => result,
            None => {
                let result = self
                    .value
                    .substitute_static(substitutions, factory, allocator, cache);
                cache.store_static_substitution(self, substitutions, result.as_ref().cloned());
                result
            }
        }
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<CachedTerm<T>>,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Option<CachedTerm<T>> {
        if self.dynamic_dependencies().is_empty() {
            return None;
        }
        match cache.retrieve_dynamic_substitution(self, state) {
            Some(result) => result,
            None => {
                let result = self
                    .value
                    .substitute_dynamic(state, factory, allocator, cache);
                cache.store_dynamic_substitution(self, state, result.as_ref().cloned());
                result
            }
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Option<CachedTerm<T>> {
        self.value.normalize(factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
    ) -> Option<CachedTerm<T>> {
        self.value.hoist_free_variables(factory, allocator)
    }
}
impl<T: Expression + Rewritable<CachedTerm<T>> + Reducible<CachedTerm<T>>> Reducible<CachedTerm<T>>
    for CachedTerm<T>
{
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Option<CachedTerm<T>> {
        match cache.retrieve_reduction(self) {
            Some(result) => result,
            None => {
                let result = self.value.reduce(factory, allocator, cache);
                cache.store_reduction(self, result.as_ref().cloned());
                result
            }
        }
    }
}
impl<T: Expression + Applicable<CachedTerm<T>>> Applicable<CachedTerm<T>> for CachedTerm<T> {
    fn arity(&self) -> Option<Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = CachedTerm<T>>,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Result<CachedTerm<T>, String> {
        // TODO: Memoize function applications based on arg list hash
        self.value.apply(args, factory, allocator, cache)
    }
}
impl<T: Expression + Rewritable<CachedTerm<T>> + Compile<CachedTerm<T>>> Compile<CachedTerm<T>>
    for CachedTerm<T>
{
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
impl<T: Expression + Rewritable<CachedTerm<T>> + Reducible<CachedTerm<T>>> Evaluate<CachedTerm<T>>
    for CachedTerm<T>
{
    fn evaluate(
        &self,
        state: &impl DynamicState<CachedTerm<T>>,
        factory: &impl ExpressionFactory<CachedTerm<T>>,
        allocator: &impl HeapAllocator<CachedTerm<T>>,
        cache: &mut impl EvaluationCache<CachedTerm<T>>,
    ) -> Option<EvaluationResult<CachedTerm<T>>> {
        fn evaluate_recursive<T: Expression + Rewritable<T> + Reducible<T>>(
            expression: &T,
            state: &impl DynamicState<T>,
            factory: &impl ExpressionFactory<T>,
            allocator: &impl HeapAllocator<T>,
            cache: &mut impl EvaluationCache<T>,
            dependencies: DependencyList,
        ) -> EvaluationResult<T> {
            let dependencies = dependencies.union(expression.dynamic_dependencies());
            match expression.substitute_dynamic(state, factory, allocator, cache) {
                Some(expression) => {
                    evaluate_recursive(&expression, state, factory, allocator, cache, dependencies)
                }
                None => match expression.reduce(factory, allocator, cache) {
                    Some(expression) => evaluate_recursive(
                        &expression,
                        state,
                        factory,
                        allocator,
                        cache,
                        dependencies,
                    ),
                    None => EvaluationResult::new(expression.clone(), dependencies),
                },
            }
        }

        let result = evaluate_recursive(
            self,
            state,
            factory,
            allocator,
            cache,
            DependencyList::empty(),
        );
        if result.result().id() == self.id() {
            None
        } else {
            Some(result)
        }
    }
}
impl<T: Expression> std::fmt::Display for CachedTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl<T: Expression> std::fmt::Debug for CachedTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}

impl<T: Expression> SerializeJson for CachedTerm<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        self.value().to_json()
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Term<T: Expression + Compile<T>> {
    Value(ValueTerm<StringPrimitive>),
    Variable(VariableTerm<T>),
    Let(LetTerm<T>),
    Lambda(LambdaTerm<T>),
    Application(ApplicationTerm<T>),
    PartialApplication(PartialApplicationTerm<T>),
    Recursive(RecursiveTerm<T>),
    Builtin(BuiltinTerm),
    Native(NativeFunctionTerm<T>),
    CompiledFunction(CompiledFunctionTerm),
    Tuple(TupleTerm<T>),
    Struct(StructTerm<T>),
    Constructor(ConstructorTerm),
    Collection(CollectionTerm<T>),
    Signal(SignalTerm<T>),
}
impl<T: Expression + Applicable<T> + Compile<T>> GraphNode for Term<T> {
    fn capture_depth(&self) -> StackOffset {
        match self {
            Self::Value(term) => term.capture_depth(),
            Self::Variable(term) => term.capture_depth(),
            Self::Let(term) => term.capture_depth(),
            Self::Lambda(term) => term.capture_depth(),
            Self::Application(term) => term.capture_depth(),
            Self::PartialApplication(term) => term.capture_depth(),
            Self::Recursive(term) => term.capture_depth(),
            Self::Builtin(term) => term.capture_depth(),
            Self::Native(term) => term.capture_depth(),
            Self::CompiledFunction(term) => term.capture_depth(),
            Self::Tuple(term) => term.capture_depth(),
            Self::Struct(term) => term.capture_depth(),
            Self::Constructor(term) => term.capture_depth(),
            Self::Collection(term) => term.capture_depth(),
            Self::Signal(term) => term.capture_depth(),
        }
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        match self {
            Self::Value(term) => term.free_variables(),
            Self::Variable(term) => term.free_variables(),
            Self::Let(term) => term.free_variables(),
            Self::Lambda(term) => term.free_variables(),
            Self::Application(term) => term.free_variables(),
            Self::PartialApplication(term) => term.free_variables(),
            Self::Recursive(term) => term.free_variables(),
            Self::Builtin(term) => term.free_variables(),
            Self::Native(term) => term.free_variables(),
            Self::CompiledFunction(term) => term.free_variables(),
            Self::Tuple(term) => term.free_variables(),
            Self::Struct(term) => term.free_variables(),
            Self::Constructor(term) => term.free_variables(),
            Self::Collection(term) => term.free_variables(),
            Self::Signal(term) => term.free_variables(),
        }
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        match self {
            Self::Value(term) => term.dynamic_dependencies(),
            Self::Variable(term) => term.dynamic_dependencies(),
            Self::Let(term) => term.dynamic_dependencies(),
            Self::Lambda(term) => term.dynamic_dependencies(),
            Self::Application(term) => term.dynamic_dependencies(),
            Self::PartialApplication(term) => term.dynamic_dependencies(),
            Self::Recursive(term) => term.dynamic_dependencies(),
            Self::Builtin(term) => term.dynamic_dependencies(),
            Self::Native(term) => term.dynamic_dependencies(),
            Self::CompiledFunction(term) => term.dynamic_dependencies(),
            Self::Tuple(term) => term.dynamic_dependencies(),
            Self::Struct(term) => term.dynamic_dependencies(),
            Self::Constructor(term) => term.dynamic_dependencies(),
            Self::Collection(term) => term.dynamic_dependencies(),
            Self::Signal(term) => term.dynamic_dependencies(),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Value(term) => term.is_static(),
            Self::Variable(term) => term.is_static(),
            Self::Let(term) => term.is_static(),
            Self::Lambda(term) => term.is_static(),
            Self::Application(term) => term.is_static(),
            Self::PartialApplication(term) => term.is_static(),
            Self::Recursive(term) => term.is_static(),
            Self::Builtin(term) => term.is_static(),
            Self::Native(term) => term.is_static(),
            Self::CompiledFunction(term) => term.is_static(),
            Self::Tuple(term) => term.is_static(),
            Self::Struct(term) => term.is_static(),
            Self::Constructor(term) => term.is_static(),
            Self::Collection(term) => term.is_static(),
            Self::Signal(term) => term.is_static(),
        }
    }
    fn is_atomic(&self) -> bool {
        match self {
            Self::Value(term) => term.is_atomic(),
            Self::Variable(term) => term.is_atomic(),
            Self::Let(term) => term.is_atomic(),
            Self::Lambda(term) => term.is_atomic(),
            Self::Application(term) => term.is_atomic(),
            Self::PartialApplication(term) => term.is_atomic(),
            Self::Recursive(term) => term.is_atomic(),
            Self::Builtin(term) => term.is_atomic(),
            Self::Native(term) => term.is_atomic(),
            Self::CompiledFunction(term) => term.is_atomic(),
            Self::Tuple(term) => term.is_atomic(),
            Self::Struct(term) => term.is_atomic(),
            Self::Constructor(term) => term.is_atomic(),
            Self::Collection(term) => term.is_atomic(),
            Self::Signal(term) => term.is_atomic(),
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T> + Compile<T>>
    Rewritable<T> for Term<T>
{
    fn subexpressions(&self) -> Vec<&T> {
        match self {
            Self::Variable(term) => term.subexpressions(),
            Self::Let(term) => term.subexpressions(),
            Self::Lambda(term) => term.subexpressions(),
            Self::Application(term) => term.subexpressions(),
            Self::PartialApplication(term) => term.subexpressions(),
            Self::Recursive(term) => term.subexpressions(),
            Self::Tuple(term) => term.subexpressions(),
            Self::Struct(term) => term.subexpressions(),
            Self::Collection(term) => term.subexpressions(),
            _ => Vec::new(),
        }
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Variable(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            Self::Let(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Lambda(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Application(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            Self::PartialApplication(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            Self::Recursive(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            Self::Tuple(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Struct(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Collection(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            _ => None,
        }
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Variable(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Let(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Lambda(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Application(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::PartialApplication(term) => {
                term.substitute_dynamic(state, factory, allocator, cache)
            }
            Self::Recursive(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Tuple(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Struct(term) => term.substitute_dynamic(state, factory, allocator, cache),
            Self::Collection(term) => term.substitute_dynamic(state, factory, allocator, cache),
            _ => None,
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Variable(term) => term.normalize(factory, allocator, cache),
            Self::Let(term) => term.normalize(factory, allocator, cache),
            Self::Lambda(term) => term.normalize(factory, allocator, cache),
            Self::Application(term) => term.normalize(factory, allocator, cache),
            Self::PartialApplication(term) => term.normalize(factory, allocator, cache),
            Self::Recursive(term) => term.normalize(factory, allocator, cache),
            Self::Tuple(term) => term.normalize(factory, allocator, cache),
            Self::Struct(term) => term.normalize(factory, allocator, cache),
            Self::Collection(term) => term.normalize(factory, allocator, cache),
            _ => None,
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        match self {
            Self::Variable(term) => term.hoist_free_variables(factory, allocator),
            Self::Let(term) => term.hoist_free_variables(factory, allocator),
            Self::Lambda(term) => term.hoist_free_variables(factory, allocator),
            Self::Application(term) => term.hoist_free_variables(factory, allocator),
            Self::PartialApplication(term) => term.hoist_free_variables(factory, allocator),
            Self::Recursive(term) => term.hoist_free_variables(factory, allocator),
            Self::Tuple(term) => term.hoist_free_variables(factory, allocator),
            Self::Struct(term) => term.hoist_free_variables(factory, allocator),
            Self::Collection(term) => term.hoist_free_variables(factory, allocator),
            _ => None,
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T> + Compile<T>>
    Reducible<T> for Term<T>
{
    fn is_reducible(&self) -> bool {
        match self {
            Self::Let(term) => term.is_reducible(),
            Self::Application(term) => term.is_reducible(),
            Self::Recursive(term) => term.is_reducible(),
            _ => false,
        }
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Let(term) => term.reduce(factory, allocator, cache),
            Self::Application(term) => term.reduce(factory, allocator, cache),
            Self::Recursive(term) => term.reduce(factory, allocator, cache),
            _ => None,
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Applicable<T>
    for Term<T>
{
    fn arity(&self) -> Option<Arity> {
        match self {
            Self::Lambda(term) => term.arity(),
            Self::PartialApplication(term) => term.arity(),
            Self::Builtin(term) => Applicable::<T>::arity(term),
            Self::Native(term) => Applicable::<T>::arity(term),
            Self::CompiledFunction(term) => Applicable::<T>::arity(term),
            Self::Constructor(term) => Applicable::<T>::arity(term),
            _ => None,
        }
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        match self {
            Self::Lambda(term) => term.apply(args, factory, allocator, cache),
            Self::PartialApplication(term) => term.apply(args, factory, allocator, cache),
            Self::Builtin(term) => Applicable::<T>::apply(term, args, factory, allocator, cache),
            Self::Native(term) => term.apply(args, factory, allocator, cache),
            Self::CompiledFunction(term) => term.apply(args, factory, allocator, cache),
            Self::Constructor(term) => term.apply(args, factory, allocator, cache),
            _ => Err(format!("Invalid function application target: {}", self)),
        }
    }
}
impl<
        T: Expression<String = StringPrimitive>
            + Rewritable<T>
            + Reducible<T>
            + Applicable<T>
            + Compile<T>,
    > Compile<T> for Term<T>
{
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        match self {
            Self::Value(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Variable(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Let(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Lambda(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Application(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::PartialApplication(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Recursive(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::CompiledFunction(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Builtin(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Native(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Tuple(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Struct(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Constructor(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Collection(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Signal(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
        }
    }
}
impl<T: Expression + Compile<T>> std::fmt::Display for Term<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(term) => std::fmt::Display::fmt(term, f),
            Self::Variable(term) => std::fmt::Display::fmt(term, f),
            Self::Let(term) => std::fmt::Display::fmt(term, f),
            Self::Lambda(term) => std::fmt::Display::fmt(term, f),
            Self::Application(term) => std::fmt::Display::fmt(term, f),
            Self::PartialApplication(term) => std::fmt::Display::fmt(term, f),
            Self::Recursive(term) => std::fmt::Display::fmt(term, f),
            Self::CompiledFunction(term) => std::fmt::Display::fmt(term, f),
            Self::Builtin(term) => std::fmt::Display::fmt(term, f),
            Self::Native(term) => std::fmt::Display::fmt(term, f),
            Self::Tuple(term) => std::fmt::Display::fmt(term, f),
            Self::Struct(term) => std::fmt::Display::fmt(term, f),
            Self::Constructor(term) => std::fmt::Display::fmt(term, f),
            Self::Collection(term) => std::fmt::Display::fmt(term, f),
            Self::Signal(term) => std::fmt::Display::fmt(term, f),
        }
    }
}

impl<'a, T: Expression + Compile<T>> SerializeJson for Term<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            Term::Value(term) => term.to_json(),
            Term::Variable(term) => term.to_json(),
            Term::Let(term) => term.to_json(),
            Term::Lambda(term) => term.to_json(),
            Term::Application(term) => term.to_json(),
            Term::PartialApplication(term) => term.to_json(),
            Term::Recursive(term) => term.to_json(),
            Term::CompiledFunction(term) => term.to_json(),
            Term::Builtin(term) => term.to_json(),
            Term::Native(term) => term.to_json(),
            Term::Tuple(term) => term.to_json(),
            Term::Struct(term) => term.to_json(),
            Term::Constructor(term) => term.to_json(),
            Term::Collection(term) => term.to_json(),
            Term::Signal(term) => term.to_json(),
        }
    }
}
