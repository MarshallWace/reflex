// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::collections::HashSet;

use serde::{Deserialize, Serialize};

mod application;
pub use application::*;
mod boolean;
pub use boolean::*;
mod builtin;
pub use builtin::*;
mod constructor;
pub use constructor::*;
mod compiled;
pub use compiled::*;
mod float;
pub use float::*;
mod int;
pub use int::*;
mod lambda;
pub use lambda::*;
mod r#let;
pub use r#let::*;
mod nil;
pub use nil::*;
mod partial;
pub use partial::*;
mod recursive;
pub use recursive::*;
mod signal;
pub use signal::*;
mod string;
pub use string::*;
mod symbol;
pub use symbol::*;
mod record;
pub use record::*;
mod tuple;
pub use tuple::*;
mod variable;
pub use variable::*;

mod collection {
    pub mod hashmap;
    pub mod hashset;
    pub mod vector;
}
pub use collection::hashmap::*;
pub use collection::hashset::*;
pub use collection::vector::*;

use crate::core::{CompoundNode, Evaluate, EvaluationResult};
use crate::{
    compiler::{Compile, Compiler, Program},
    core::{
        Applicable, Arity, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, GraphNode, HeapAllocator, Reducible, Rewritable, SerializeJson,
        StackOffset, Substitutions, VarArgs,
    },
    hash::{hash_object, HashId},
};

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Term<T: Expression> {
    #[serde(bound(
        serialize = "<T as Expression>::String: Serialize",
        deserialize = "<T as Expression>::String: Deserialize<'de>"
    ))]
    Nil(NilTerm),
    Boolean(BooleanTerm),
    Int(IntTerm),
    Float(FloatTerm),
    String(StringTerm<T::String>),
    Symbol(SymbolTerm),
    StaticVariable(StaticVariableTerm),
    DynamicVariable(DynamicVariableTerm<T>),
    Let(LetTerm<T>),
    Lambda(LambdaTerm<T>),
    Application(ApplicationTerm<T>),
    PartialApplication(PartialApplicationTerm<T>),
    Recursive(RecursiveTerm<T>),
    Builtin(BuiltinTerm<T>),
    CompiledFunction(CompiledFunctionTerm),
    Tuple(TupleTerm<T>),
    Record(RecordTerm<T>),
    Constructor(ConstructorTerm),
    Vector(VectorTerm<T>),
    HashMap(HashMapTerm<T>),
    HashSet(HashSetTerm<T>),
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
            Self::Nil(term) => term.capture_depth(),
            Self::Boolean(term) => term.capture_depth(),
            Self::Int(term) => term.capture_depth(),
            Self::Float(term) => term.capture_depth(),
            Self::String(term) => term.capture_depth(),
            Self::Symbol(term) => term.capture_depth(),
            Self::StaticVariable(term) => term.capture_depth(),
            Self::DynamicVariable(term) => term.capture_depth(),
            Self::Let(term) => term.capture_depth(),
            Self::Lambda(term) => term.capture_depth(),
            Self::Application(term) => term.capture_depth(),
            Self::PartialApplication(term) => term.capture_depth(),
            Self::Recursive(term) => term.capture_depth(),
            Self::Builtin(term) => term.capture_depth(),
            Self::CompiledFunction(term) => term.capture_depth(),
            Self::Tuple(term) => term.capture_depth(),
            Self::Record(term) => term.capture_depth(),
            Self::Constructor(term) => term.capture_depth(),
            Self::Vector(term) => term.capture_depth(),
            Self::HashMap(term) => term.capture_depth(),
            Self::HashSet(term) => term.capture_depth(),
            Self::Signal(term) => term.capture_depth(),
        }
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        match self {
            Self::Nil(term) => term.free_variables(),
            Self::Boolean(term) => term.free_variables(),
            Self::Int(term) => term.free_variables(),
            Self::Float(term) => term.free_variables(),
            Self::String(term) => term.free_variables(),
            Self::Symbol(term) => term.free_variables(),
            Self::StaticVariable(term) => term.free_variables(),
            Self::DynamicVariable(term) => term.free_variables(),
            Self::Let(term) => term.free_variables(),
            Self::Lambda(term) => term.free_variables(),
            Self::Application(term) => term.free_variables(),
            Self::PartialApplication(term) => term.free_variables(),
            Self::Recursive(term) => term.free_variables(),
            Self::Builtin(term) => term.free_variables(),
            Self::CompiledFunction(term) => term.free_variables(),
            Self::Tuple(term) => term.free_variables(),
            Self::Record(term) => term.free_variables(),
            Self::Constructor(term) => term.free_variables(),
            Self::Vector(term) => term.free_variables(),
            Self::HashMap(term) => term.free_variables(),
            Self::HashSet(term) => term.free_variables(),
            Self::Signal(term) => term.free_variables(),
        }
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        match self {
            Self::Nil(term) => term.count_variable_usages(offset),
            Self::Boolean(term) => term.count_variable_usages(offset),
            Self::Int(term) => term.count_variable_usages(offset),
            Self::Float(term) => term.count_variable_usages(offset),
            Self::String(term) => term.count_variable_usages(offset),
            Self::Symbol(term) => term.count_variable_usages(offset),
            Self::StaticVariable(term) => term.count_variable_usages(offset),
            Self::DynamicVariable(term) => term.count_variable_usages(offset),
            Self::Let(term) => term.count_variable_usages(offset),
            Self::Lambda(term) => term.count_variable_usages(offset),
            Self::Application(term) => term.count_variable_usages(offset),
            Self::PartialApplication(term) => term.count_variable_usages(offset),
            Self::Recursive(term) => term.count_variable_usages(offset),
            Self::Builtin(term) => term.count_variable_usages(offset),
            Self::CompiledFunction(term) => term.count_variable_usages(offset),
            Self::Tuple(term) => term.count_variable_usages(offset),
            Self::Record(term) => term.count_variable_usages(offset),
            Self::Constructor(term) => term.count_variable_usages(offset),
            Self::Vector(term) => term.count_variable_usages(offset),
            Self::HashMap(term) => term.count_variable_usages(offset),
            Self::HashSet(term) => term.count_variable_usages(offset),
            Self::Signal(term) => term.count_variable_usages(offset),
        }
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        match self {
            Self::Nil(term) => term.dynamic_dependencies(deep),
            Self::Boolean(term) => term.dynamic_dependencies(deep),
            Self::Int(term) => term.dynamic_dependencies(deep),
            Self::Float(term) => term.dynamic_dependencies(deep),
            Self::String(term) => term.dynamic_dependencies(deep),
            Self::Symbol(term) => term.dynamic_dependencies(deep),
            Self::StaticVariable(term) => term.dynamic_dependencies(deep),
            Self::DynamicVariable(term) => term.dynamic_dependencies(deep),
            Self::Let(term) => term.dynamic_dependencies(deep),
            Self::Lambda(term) => term.dynamic_dependencies(deep),
            Self::Application(term) => term.dynamic_dependencies(deep),
            Self::PartialApplication(term) => term.dynamic_dependencies(deep),
            Self::Recursive(term) => term.dynamic_dependencies(deep),
            Self::Builtin(term) => term.dynamic_dependencies(deep),
            Self::CompiledFunction(term) => term.dynamic_dependencies(deep),
            Self::Tuple(term) => term.dynamic_dependencies(deep),
            Self::Record(term) => term.dynamic_dependencies(deep),
            Self::Constructor(term) => term.dynamic_dependencies(deep),
            Self::Vector(term) => term.dynamic_dependencies(deep),
            Self::HashMap(term) => term.dynamic_dependencies(deep),
            Self::HashSet(term) => term.dynamic_dependencies(deep),
            Self::Signal(term) => term.dynamic_dependencies(deep),
        }
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        match self {
            Self::Nil(term) => term.has_dynamic_dependencies(deep),
            Self::Boolean(term) => term.has_dynamic_dependencies(deep),
            Self::Int(term) => term.has_dynamic_dependencies(deep),
            Self::Float(term) => term.has_dynamic_dependencies(deep),
            Self::String(term) => term.has_dynamic_dependencies(deep),
            Self::Symbol(term) => term.has_dynamic_dependencies(deep),
            Self::StaticVariable(term) => term.has_dynamic_dependencies(deep),
            Self::DynamicVariable(term) => term.has_dynamic_dependencies(deep),
            Self::Let(term) => term.has_dynamic_dependencies(deep),
            Self::Lambda(term) => term.has_dynamic_dependencies(deep),
            Self::Application(term) => term.has_dynamic_dependencies(deep),
            Self::PartialApplication(term) => term.has_dynamic_dependencies(deep),
            Self::Recursive(term) => term.has_dynamic_dependencies(deep),
            Self::Builtin(term) => term.has_dynamic_dependencies(deep),
            Self::CompiledFunction(term) => term.has_dynamic_dependencies(deep),
            Self::Tuple(term) => term.has_dynamic_dependencies(deep),
            Self::Record(term) => term.has_dynamic_dependencies(deep),
            Self::Constructor(term) => term.has_dynamic_dependencies(deep),
            Self::Vector(term) => term.has_dynamic_dependencies(deep),
            Self::HashMap(term) => term.has_dynamic_dependencies(deep),
            Self::HashSet(term) => term.has_dynamic_dependencies(deep),
            Self::Signal(term) => term.has_dynamic_dependencies(deep),
        }
    }
    fn is_static(&self) -> bool {
        match self {
            Self::Nil(term) => term.is_static(),
            Self::Boolean(term) => term.is_static(),
            Self::Int(term) => term.is_static(),
            Self::Float(term) => term.is_static(),
            Self::String(term) => term.is_static(),
            Self::Symbol(term) => term.is_static(),
            Self::StaticVariable(term) => term.is_static(),
            Self::DynamicVariable(term) => term.is_static(),
            Self::Let(term) => term.is_static(),
            Self::Lambda(term) => term.is_static(),
            Self::Application(term) => term.is_static(),
            Self::PartialApplication(term) => term.is_static(),
            Self::Recursive(term) => term.is_static(),
            Self::Builtin(term) => term.is_static(),
            Self::CompiledFunction(term) => term.is_static(),
            Self::Tuple(term) => term.is_static(),
            Self::Record(term) => term.is_static(),
            Self::Constructor(term) => term.is_static(),
            Self::Vector(term) => term.is_static(),
            Self::HashMap(term) => term.is_static(),
            Self::HashSet(term) => term.is_static(),
            Self::Signal(term) => term.is_static(),
        }
    }
    fn is_atomic(&self) -> bool {
        match self {
            Self::Nil(term) => term.is_atomic(),
            Self::Boolean(term) => term.is_atomic(),
            Self::Int(term) => term.is_atomic(),
            Self::Float(term) => term.is_atomic(),
            Self::String(term) => term.is_atomic(),
            Self::Symbol(term) => term.is_atomic(),
            Self::StaticVariable(term) => term.is_atomic(),
            Self::DynamicVariable(term) => term.is_atomic(),
            Self::Let(term) => term.is_atomic(),
            Self::Lambda(term) => term.is_atomic(),
            Self::Application(term) => term.is_atomic(),
            Self::PartialApplication(term) => term.is_atomic(),
            Self::Recursive(term) => term.is_atomic(),
            Self::Builtin(term) => term.is_atomic(),
            Self::CompiledFunction(term) => term.is_atomic(),
            Self::Tuple(term) => term.is_atomic(),
            Self::Record(term) => term.is_atomic(),
            Self::Constructor(term) => term.is_atomic(),
            Self::Vector(term) => term.is_atomic(),
            Self::HashMap(term) => term.is_atomic(),
            Self::HashSet(term) => term.is_atomic(),
            Self::Signal(term) => term.is_atomic(),
        }
    }
    fn is_complex(&self) -> bool {
        match self {
            Self::Nil(term) => term.is_complex(),
            Self::Boolean(term) => term.is_complex(),
            Self::Int(term) => term.is_complex(),
            Self::Float(term) => term.is_complex(),
            Self::String(term) => term.is_complex(),
            Self::Symbol(term) => term.is_complex(),
            Self::StaticVariable(term) => term.is_complex(),
            Self::DynamicVariable(term) => term.is_complex(),
            Self::Let(term) => term.is_complex(),
            Self::Lambda(term) => term.is_complex(),
            Self::Application(term) => term.is_complex(),
            Self::PartialApplication(term) => term.is_complex(),
            Self::Recursive(term) => term.is_complex(),
            Self::Builtin(term) => term.is_complex(),
            Self::CompiledFunction(term) => term.is_complex(),
            Self::Tuple(term) => term.is_complex(),
            Self::Record(term) => term.is_complex(),
            Self::Constructor(term) => term.is_complex(),
            Self::Vector(term) => term.is_complex(),
            Self::HashMap(term) => term.is_complex(),
            Self::HashSet(term) => term.is_complex(),
            Self::Signal(term) => term.is_complex(),
        }
    }
}
pub enum TermChildren<'a, T: Expression> {
    DynamicVariable(DynamicVariableTermChildren<'a, T>),
    Let(LetTermChildren<'a, T>),
    Lambda(LambdaTermChildren<'a, T>),
    Application(ApplicationTermChildren<'a, T>),
    PartialApplication(PartialApplicationTermChildren<'a, T>),
    Recursive(RecursiveTermChildren<'a, T>),
    Tuple(TupleTermChildren<'a, T>),
    Record(RecordTermChildren<'a, T>),
    Vector(VectorTermChildren<'a, T>),
    HashMap(HashMapTermChildren<'a, T>),
    HashSet(HashSetTermChildren<'a, T>),
    Empty,
}
impl<'a, T: Expression> Iterator for TermChildren<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::DynamicVariable(iter) => iter.next(),
            Self::Let(iter) => iter.next(),
            Self::Lambda(iter) => iter.next(),
            Self::Application(iter) => iter.next(),
            Self::PartialApplication(iter) => iter.next(),
            Self::Recursive(iter) => iter.next(),
            Self::Tuple(iter) => iter.next(),
            Self::Record(iter) => iter.next(),
            Self::Vector(iter) => iter.next(),
            Self::HashMap(iter) => iter.next(),
            Self::HashSet(iter) => iter.next(),
            Self::Empty => None,
        }
    }
}
impl<'a, T: Expression + 'a> CompoundNode<'a, T> for Term<T> {
    type Children = TermChildren<'a, T>;
    fn children(&'a self) -> Self::Children {
        match self {
            Self::DynamicVariable(term) => TermChildren::DynamicVariable(term.children()),
            Self::Let(term) => TermChildren::Let(term.children()),
            Self::Lambda(term) => TermChildren::Lambda(term.children()),
            Self::Application(term) => TermChildren::Application(term.children()),
            Self::PartialApplication(term) => TermChildren::PartialApplication(term.children()),
            Self::Recursive(term) => TermChildren::Recursive(term.children()),
            Self::Tuple(term) => TermChildren::Tuple(term.children()),
            Self::Record(term) => TermChildren::Record(term.children()),
            Self::Vector(term) => TermChildren::Vector(term.children()),
            Self::HashMap(term) => TermChildren::HashMap(term.children()),
            Self::HashSet(term) => TermChildren::HashSet(term.children()),
            _ => TermChildren::Empty,
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>> Rewritable<T>
    for Term<T>
{
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        match self {
            Self::StaticVariable(term) => {
                term.substitute_static(substitutions, factory, allocator, cache)
            }
            Self::DynamicVariable(term) => {
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
            Self::Record(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::Vector(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::HashMap(term) => term.substitute_static(substitutions, factory, allocator, cache),
            Self::HashSet(term) => term.substitute_static(substitutions, factory, allocator, cache),
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
            Self::StaticVariable(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
            Self::DynamicVariable(term) => {
                term.substitute_dynamic(deep, state, factory, allocator, cache)
            }
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
            Self::Record(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::Vector(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::HashMap(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
            Self::HashSet(term) => term.substitute_dynamic(deep, state, factory, allocator, cache),
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
            Self::StaticVariable(term) => term.normalize(factory, allocator, cache),
            Self::DynamicVariable(term) => term.normalize(factory, allocator, cache),
            Self::Let(term) => term.normalize(factory, allocator, cache),
            Self::Lambda(term) => term.normalize(factory, allocator, cache),
            Self::Application(term) => term.normalize(factory, allocator, cache),
            Self::PartialApplication(term) => term.normalize(factory, allocator, cache),
            Self::Recursive(term) => term.normalize(factory, allocator, cache),
            Self::Tuple(term) => term.normalize(factory, allocator, cache),
            Self::Record(term) => term.normalize(factory, allocator, cache),
            Self::Vector(term) => term.normalize(factory, allocator, cache),
            Self::HashMap(term) => term.normalize(factory, allocator, cache),
            Self::HashSet(term) => term.normalize(factory, allocator, cache),
            _ => None,
        }
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        match self {
            Self::StaticVariable(term) => term.hoist_free_variables(factory, allocator),
            Self::DynamicVariable(term) => term.hoist_free_variables(factory, allocator),
            Self::Let(term) => term.hoist_free_variables(factory, allocator),
            Self::Lambda(term) => term.hoist_free_variables(factory, allocator),
            Self::Application(term) => term.hoist_free_variables(factory, allocator),
            Self::PartialApplication(term) => term.hoist_free_variables(factory, allocator),
            Self::Recursive(term) => term.hoist_free_variables(factory, allocator),
            Self::Tuple(term) => term.hoist_free_variables(factory, allocator),
            Self::Record(term) => term.hoist_free_variables(factory, allocator),
            Self::Vector(term) => term.hoist_free_variables(factory, allocator),
            Self::HashMap(term) => term.hoist_free_variables(factory, allocator),
            Self::HashSet(term) => term.hoist_free_variables(factory, allocator),
            _ => None,
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>> Reducible<T>
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
impl<T: Expression + Rewritable<T> + Applicable<T>> Applicable<T> for Term<T> {
    fn arity(&self) -> Option<Arity> {
        match self {
            Self::Lambda(term) => Applicable::<T>::arity(term),
            Self::PartialApplication(term) => Applicable::<T>::arity(term),
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
            Self::Builtin(term) => Applicable::apply(term, args, factory, allocator, cache),
            Self::CompiledFunction(term) => term.apply(args, factory, allocator, cache),
            Self::Constructor(term) => term.apply(args, factory, allocator, cache),
            _ => Err(format!("Invalid function application target: {}", self)),
        }
    }
    fn should_parallelize(&self, args: &[T]) -> bool {
        match self {
            Self::Lambda(term) => term.should_parallelize(args),
            Self::PartialApplication(term) => term.should_parallelize(args),
            Self::Builtin(term) => term.should_parallelize(args),
            Self::CompiledFunction(term) => term.should_parallelize(args),
            Self::Constructor(term) => term.should_parallelize(args),
            _ => false,
        }
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Applicable<T> + Evaluate<T>> Evaluate<T>
    for Term<T>
{
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>> {
        match self {
            Self::DynamicVariable(term) => term.evaluate(state, factory, allocator, cache),
            Self::Application(term) => term.evaluate(state, factory, allocator, cache),
            _ => {
                if self.is_reducible() {
                    self.reduce(factory, allocator, cache).map(|result| {
                        result
                            .evaluate(state, factory, allocator, cache)
                            .unwrap_or_else(|| {
                                EvaluationResult::new(result, DependencyList::empty())
                            })
                    })
                } else {
                    None
                }
            }
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
            Self::Nil(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Boolean(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Int(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Float(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::String(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Symbol(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::StaticVariable(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::DynamicVariable(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
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
            Self::Record(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Constructor(term) => {
                term.compile(eager, stack_offset, factory, allocator, compiler)
            }
            Self::Vector(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashMap(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::HashSet(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
            Self::Signal(term) => term.compile(eager, stack_offset, factory, allocator, compiler),
        }
    }
}
impl<T: Expression> std::fmt::Display for Term<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil(term) => std::fmt::Display::fmt(term, f),
            Self::Boolean(term) => std::fmt::Display::fmt(term, f),
            Self::Int(term) => std::fmt::Display::fmt(term, f),
            Self::Float(term) => std::fmt::Display::fmt(term, f),
            Self::String(term) => std::fmt::Display::fmt(term, f),
            Self::Symbol(term) => std::fmt::Display::fmt(term, f),
            Self::StaticVariable(term) => std::fmt::Display::fmt(term, f),
            Self::DynamicVariable(term) => std::fmt::Display::fmt(term, f),
            Self::Let(term) => std::fmt::Display::fmt(term, f),
            Self::Lambda(term) => std::fmt::Display::fmt(term, f),
            Self::Application(term) => std::fmt::Display::fmt(term, f),
            Self::PartialApplication(term) => std::fmt::Display::fmt(term, f),
            Self::Recursive(term) => std::fmt::Display::fmt(term, f),
            Self::CompiledFunction(term) => std::fmt::Display::fmt(term, f),
            Self::Builtin(term) => std::fmt::Display::fmt(term, f),
            Self::Tuple(term) => std::fmt::Display::fmt(term, f),
            Self::Record(term) => std::fmt::Display::fmt(term, f),
            Self::Constructor(term) => std::fmt::Display::fmt(term, f),
            Self::Vector(term) => std::fmt::Display::fmt(term, f),
            Self::HashMap(term) => std::fmt::Display::fmt(term, f),
            Self::HashSet(term) => std::fmt::Display::fmt(term, f),
            Self::Signal(term) => std::fmt::Display::fmt(term, f),
        }
    }
}
impl<T: Expression> SerializeJson for Term<T> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        match self {
            Self::Nil(term) => term.to_json(),
            Self::Boolean(term) => term.to_json(),
            Self::Int(term) => term.to_json(),
            Self::Float(term) => term.to_json(),
            Self::String(term) => term.to_json(),
            Self::Symbol(term) => term.to_json(),
            Self::StaticVariable(term) => term.to_json(),
            Self::DynamicVariable(term) => term.to_json(),
            Self::Let(term) => term.to_json(),
            Self::Lambda(term) => term.to_json(),
            Self::Application(term) => term.to_json(),
            Self::PartialApplication(term) => term.to_json(),
            Self::Recursive(term) => term.to_json(),
            Self::CompiledFunction(term) => term.to_json(),
            Self::Builtin(term) => term.to_json(),
            Self::Tuple(term) => term.to_json(),
            Self::Record(term) => term.to_json(),
            Self::Constructor(term) => term.to_json(),
            Self::Vector(term) => term.to_json(),
            Self::HashMap(term) => term.to_json(),
            Self::HashSet(term) => term.to_json(),
            Self::Signal(term) => term.to_json(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::allocator::DefaultAllocator;
    use crate::core::SignalType;
    use crate::lang::{CachedSharedTerm, SharedTermFactory};
    use crate::parser::sexpr::parse;
    use crate::stdlib::Stdlib;

    #[test]
    fn serialization() {
        let factory = SharedTermFactory::<Stdlib>::default();
        let allocator = DefaultAllocator::default();

        let input = factory.create_int_term(5);
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: CachedSharedTerm<Stdlib> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(input, deserialized);

        let input =
            factory.create_signal_term(allocator.create_signal_list([allocator.create_signal(
                SignalType::Custom(String::from("foo")),
                allocator.create_unit_list(factory.create_int_term(3)),
            )]));
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: CachedSharedTerm<Stdlib> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(input, deserialized);

        let input = parse("((lambda (foo) (* (+ 2 3) foo)) 2)", &factory, &allocator).unwrap();
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: CachedSharedTerm<Stdlib> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(input, deserialized);
    }
}
