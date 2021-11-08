// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;

mod application;
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
        Applicable, Arity, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, GraphNode, HeapAllocator, Reducible, Rewritable, SerializeJson,
        StackOffset, Substitutions, VarArgs,
    },
    hash::{hash_object, HashId},
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Term<T: Expression> {
    Value(ValueTerm<T::String>),
    Variable(VariableTerm<T>),
    Let(LetTerm<T>),
    Lambda(LambdaTerm<T>),
    Application(ApplicationTerm<T>),
    PartialApplication(PartialApplicationTerm<T>),
    Recursive(RecursiveTerm<T>),
    Builtin(BuiltinTerm<T>),
    CompiledFunction(CompiledFunctionTerm),
    Tuple(TupleTerm<T>),
    Struct(StructTerm<T>),
    Constructor(ConstructorTerm),
    Collection(CollectionTerm<T>),
    Signal(SignalTerm<T>),
}
impl<T: Expression + Applicable<T>> Expression for Term<T> {
    type String = T::String;
    type Builtin = T::Builtin;
    fn id(&self) -> HashId {
        hash_object(self)
    }
}
impl<T: Expression + Applicable<T>> GraphNode for Term<T> {
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
            Self::CompiledFunction(term) => term.free_variables(),
            Self::Tuple(term) => term.free_variables(),
            Self::Struct(term) => term.free_variables(),
            Self::Constructor(term) => term.free_variables(),
            Self::Collection(term) => term.free_variables(),
            Self::Signal(term) => term.free_variables(),
        }
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        match self {
            Self::Value(term) => term.dynamic_dependencies(deep),
            Self::Variable(term) => term.dynamic_dependencies(deep),
            Self::Let(term) => term.dynamic_dependencies(deep),
            Self::Lambda(term) => term.dynamic_dependencies(deep),
            Self::Application(term) => term.dynamic_dependencies(deep),
            Self::PartialApplication(term) => term.dynamic_dependencies(deep),
            Self::Recursive(term) => term.dynamic_dependencies(deep),
            Self::Builtin(term) => term.dynamic_dependencies(deep),
            Self::CompiledFunction(term) => term.dynamic_dependencies(deep),
            Self::Tuple(term) => term.dynamic_dependencies(deep),
            Self::Struct(term) => term.dynamic_dependencies(deep),
            Self::Constructor(term) => term.dynamic_dependencies(deep),
            Self::Collection(term) => term.dynamic_dependencies(deep),
            Self::Signal(term) => term.dynamic_dependencies(deep),
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        match self {
            Self::Value(term) => term.has_dynamic_dependencies(deep),
            Self::Variable(term) => term.has_dynamic_dependencies(deep),
            Self::Let(term) => term.has_dynamic_dependencies(deep),
            Self::Lambda(term) => term.has_dynamic_dependencies(deep),
            Self::Application(term) => term.has_dynamic_dependencies(deep),
            Self::PartialApplication(term) => term.has_dynamic_dependencies(deep),
            Self::Recursive(term) => term.has_dynamic_dependencies(deep),
            Self::Builtin(term) => term.has_dynamic_dependencies(deep),
            Self::CompiledFunction(term) => term.has_dynamic_dependencies(deep),
            Self::Tuple(term) => term.has_dynamic_dependencies(deep),
            Self::Struct(term) => term.has_dynamic_dependencies(deep),
            Self::Constructor(term) => term.has_dynamic_dependencies(deep),
            Self::Collection(term) => term.has_dynamic_dependencies(deep),
            Self::Signal(term) => term.has_dynamic_dependencies(deep),
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
            Self::CompiledFunction(term) => term.is_atomic(),
            Self::Tuple(term) => term.is_atomic(),
            Self::Struct(term) => term.is_atomic(),
            Self::Constructor(term) => term.is_atomic(),
            Self::Collection(term) => term.is_atomic(),
            Self::Signal(term) => term.is_atomic(),
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Rewritable<T>
    for Term<T>
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
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::Variable(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Let(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Lambda(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Application(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
            Self::PartialApplication(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
            Self::Recursive(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
            Self::Tuple(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Struct(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Collection(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
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
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Reducible<T>
    for Term<T>
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
            Self::CompiledFunction(term) => term.apply(args, factory, allocator, cache),
            Self::Constructor(term) => term.apply(args, factory, allocator, cache),
            _ => Err(format!("Invalid function application target: {}", self)),
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>> Compile<T>
    for Term<T>
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
impl<T: Expression> std::fmt::Display for Term<T> {
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
            Self::Tuple(term) => std::fmt::Display::fmt(term, f),
            Self::Struct(term) => std::fmt::Display::fmt(term, f),
            Self::Constructor(term) => std::fmt::Display::fmt(term, f),
            Self::Collection(term) => std::fmt::Display::fmt(term, f),
            Self::Signal(term) => std::fmt::Display::fmt(term, f),
        }
    }
}
impl<T: Expression> SerializeJson for Term<T> {
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
            Term::Tuple(term) => term.to_json(),
            Term::Struct(term) => term.to_json(),
            Term::Constructor(term) => term.to_json(),
            Term::Collection(term) => term.to_json(),
            Term::Signal(term) => term.to_json(),
        }
    }
}
