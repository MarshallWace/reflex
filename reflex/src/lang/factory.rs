// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, marker::PhantomData};
use tracing::trace;

use crate::{
    compiler::{Compile, Compiler, InstructionPointer, Program},
    core::{
        Applicable, Arity, Builtin, DependencyList, DynamicState, Evaluate, EvaluationCache,
        EvaluationResult, Expression, ExpressionFactory, ExpressionList, GraphNode, HeapAllocator,
        Reducible, Rewritable, SerializeJson, SignalList, StackOffset, StateToken, StructPrototype,
        Substitutions, VarArgs,
    },
    hash::HashId,
};
use serde::{Deserialize, Serialize};

use super::{
    expression::{CachedExpression, SharedExpression},
    term::*,
};

#[derive(Clone)]
pub struct SharedTermFactory<TBuiltin: Builtin> {
    _builtin: PhantomData<TBuiltin>,
}
impl<TBuiltin: Builtin> Default for SharedTermFactory<TBuiltin> {
    fn default() -> Self {
        Self {
            _builtin: PhantomData,
        }
    }
}
impl<TBuiltin: Builtin> SharedTermFactory<TBuiltin> {
    fn create_expression(
        &self,
        value: Term<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        CachedSharedTerm {
            value: CachedExpression::new(SharedExpression::new(TermExpression::new(value))),
        }
    }
}
impl<TBuiltin: Builtin> ExpressionFactory<CachedSharedTerm<TBuiltin>>
    for SharedTermFactory<TBuiltin>
{
    fn create_value_term(&self, value: ValueTerm<String>) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "value_term");
        self.create_expression(Term::Value(value))
    }
    fn create_static_variable_term(&self, offset: StackOffset) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "static_variable");
        self.create_expression(Term::Variable(VariableTerm::Static(
            StaticVariableTerm::new(offset),
        )))
    }
    fn create_dynamic_variable_term(
        &self,
        state_token: StateToken,
        fallback: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "dynamic_variable");
        self.create_expression(Term::Variable(VariableTerm::Dynamic(
            DynamicVariableTerm::new(state_token, fallback),
        )))
    }
    fn create_let_term(
        &self,
        initializer: CachedSharedTerm<TBuiltin>,
        body: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "let");
        self.create_expression(Term::Let(LetTerm::new(initializer, body)))
    }
    fn create_lambda_term(
        &self,
        num_args: StackOffset,
        body: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "lambda");
        self.create_expression(Term::Lambda(LambdaTerm::new(num_args, body)))
    }
    fn create_application_term(
        &self,
        target: CachedSharedTerm<TBuiltin>,
        args: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "application");
        self.create_expression(Term::Application(ApplicationTerm::new(target, args)))
    }
    fn create_partial_application_term(
        &self,
        target: CachedSharedTerm<TBuiltin>,
        args: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "partial_application");
        self.create_expression(Term::PartialApplication(PartialApplicationTerm::new(
            target, args,
        )))
    }
    fn create_recursive_term(
        &self,
        factory: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "recursive");
        self.create_expression(Term::Recursive(RecursiveTerm::new(factory)))
    }
    fn create_builtin_term(&self, target: impl Into<TBuiltin>) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "builtin");
        self.create_expression(Term::Builtin(BuiltinTerm::new(target.into())))
    }
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "compiled_function");
        self.create_expression(Term::CompiledFunction(CompiledFunctionTerm::new(
            address,
            hash,
            required_args,
            optional_args,
        )))
    }
    fn create_tuple_term(
        &self,
        fields: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "tuple");
        self.create_expression(Term::Tuple(TupleTerm::new(fields)))
    }
    fn create_struct_term(
        &self,
        prototype: StructPrototype,
        fields: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "struct");
        self.create_expression(Term::Struct(StructTerm::new(prototype, fields)))
    }
    fn create_constructor_term(&self, prototype: StructPrototype) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "constructor");
        self.create_expression(Term::Constructor(ConstructorTerm::new(prototype)))
    }
    fn create_vector_term(
        &self,
        items: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "vector");
        self.create_expression(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            items,
        ))))
    }
    fn create_hashmap_term(
        &self,
        keys: ExpressionList<CachedSharedTerm<TBuiltin>>,
        values: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "hashmap");
        self.create_expression(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
            keys, values,
        ))))
    }
    fn create_hashset_term(
        &self,
        values: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "hashset");
        self.create_expression(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
            values,
        ))))
    }
    fn create_signal_term(
        &self,
        signals: SignalList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        trace!(factory_create = "signal");
        self.create_expression(Term::Signal(SignalTerm::new(signals)))
    }
    fn match_value_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a ValueTerm<String>> {
        match expression.inner_term() {
            Term::Value(term) => Some(term),
            _ => None,
        }
    }
    fn match_static_variable_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a StaticVariableTerm> {
        match expression.inner_term() {
            Term::Variable(VariableTerm::Static(term)) => Some(term),
            _ => None,
        }
    }
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a DynamicVariableTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Variable(VariableTerm::Dynamic(term)) => Some(term),
            _ => None,
        }
    }
    fn match_let_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a LetTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Let(term) => Some(term),
            _ => None,
        }
    }
    fn match_lambda_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a LambdaTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Lambda(term) => Some(term),
            _ => None,
        }
    }
    fn match_application_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a ApplicationTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Application(term) => Some(term),
            _ => None,
        }
    }
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a PartialApplicationTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::PartialApplication(term) => Some(term),
            _ => None,
        }
    }
    fn match_recursive_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a RecursiveTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Recursive(term) => Some(term),
            _ => None,
        }
    }
    fn match_builtin_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a BuiltinTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Builtin(term) => Some(term),
            _ => None,
        }
    }
    fn match_compiled_function_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a CompiledFunctionTerm> {
        match expression.inner_term() {
            Term::CompiledFunction(term) => Some(term),
            _ => None,
        }
    }
    fn match_tuple_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a TupleTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Tuple(term) => Some(term),
            _ => None,
        }
    }
    fn match_struct_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a StructTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Struct(term) => Some(term),
            _ => None,
        }
    }
    fn match_constructor_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a ConstructorTerm> {
        match expression.inner_term() {
            Term::Constructor(term) => Some(term),
            _ => None,
        }
    }
    fn match_vector_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a VectorTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Collection(CollectionTerm::Vector(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashmap_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a HashMapTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Collection(CollectionTerm::HashMap(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashset_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a HashSetTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Collection(CollectionTerm::HashSet(term)) => Some(term),
            _ => None,
        }
    }
    fn match_signal_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a SignalTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Signal(term) => Some(term),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct CachedSharedTerm<TBuiltin: Builtin> {
    value: CachedExpression<SharedExpression<TermExpression<Self>>>,
}
impl<TBuiltin: Builtin> CachedSharedTerm<TBuiltin> {
    pub(crate) fn inner_term(&self) -> &Term<Self> {
        self.value.value().value.value()
    }
    pub(crate) fn inner_expression(&self) -> &TermExpression<Self> {
        self.value.value().value.as_ref()
    }
    fn into_normalized(self) -> Self {
        Self {
            value: self.value.into_normalized(),
        }
    }
}
impl<TBuiltin: Builtin> Expression for CachedSharedTerm<TBuiltin> {
    type String = String;
    type Builtin = TBuiltin;
    fn id(&self) -> HashId {
        self.value.id()
    }
}
impl<TBuiltin: Builtin> GraphNode for CachedSharedTerm<TBuiltin> {
    fn capture_depth(&self) -> StackOffset {
        self.value.capture_depth()
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        self.value.free_variables()
    }
    fn count_variable_usages(&self, offset: StackOffset) -> usize {
        self.value.count_variable_usages(offset)
    }
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList {
        self.value.dynamic_dependencies(deep)
    }
    fn has_dynamic_dependencies(&self, deep: bool) -> bool {
        self.value.has_dynamic_dependencies(deep)
    }
    fn is_static(&self) -> bool {
        self.value.is_static()
    }
    fn is_atomic(&self) -> bool {
        self.value.is_atomic()
    }
}
impl<TBuiltin: Builtin> std::fmt::Debug for CachedSharedTerm<TBuiltin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}
impl<TBuiltin: Builtin> std::fmt::Display for CachedSharedTerm<TBuiltin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl<TBuiltin: Builtin> SerializeJson for CachedSharedTerm<TBuiltin> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        SerializeJson::to_json(&self.value)
    }
}
impl<TBuiltin: Builtin> Rewritable<Self> for CachedSharedTerm<TBuiltin> {
    fn children(&self) -> Vec<&Self> {
        self.inner_term().children()
    }
    fn count_subexpression_usages(
        &self,
        expression: &Self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
    ) -> usize {
        if self.id() == expression.id() {
            1
        } else {
            self.inner_expression()
                .count_subexpression_usages(expression, factory, allocator)
        }
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        // TODO: This should not all magically depend on people using CachedSharedTerm, instead we
        // should expose a method for doing a walk on the tree and perform transformations to the tree
        if substitutions.can_skip(&self) {
            return None;
        }
        match cache.retrieve_static_substitution(&self, substitutions) {
            Some(result) => result,
            None => {
                // TODO: This should not all magically depend on people using Cached term, instead we
                // should expose a method for doing a walk on the tree and perform transformations to the tree
                let result = substitutions
                    .substitute_term(self.id(), factory, allocator, cache)
                    .or_else(|| {
                        self.inner_expression().substitute_static(
                            substitutions,
                            factory,
                            allocator,
                            cache,
                        )
                    });
                cache.store_static_substitution(&self, substitutions, result.as_ref().cloned());
                result
            }
        }
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        self.inner_term()
            .substitute_dynamic(deep, state, factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
    ) -> Option<Self> {
        self.inner_term().hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        if self.value.normalized() {
            None
        } else {
            match cache.retrieve_normalization(&self) {
                Some(result) => result,
                None => {
                    let result = self
                        .inner_term()
                        .normalize(factory, allocator, cache)
                        .and_then(|result| {
                            if result.id() == self.id() {
                                None
                            } else {
                                Some(result.into_normalized())
                            }
                        });
                    cache.store_normalization(&self, result.as_ref().cloned());
                    result
                }
            }
        }
    }
}
impl<TBuiltin: Builtin> Reducible<Self> for CachedSharedTerm<TBuiltin> {
    fn is_reducible(&self) -> bool {
        self.inner_term().is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        match cache.retrieve_reduction(&self) {
            Some(result) => result,
            None => {
                let result = self
                    .inner_term()
                    .reduce(factory, allocator, cache)
                    .and_then(|result| {
                        if result.id() == self.id() {
                            None
                        } else {
                            Some(result)
                        }
                    });
                cache.store_reduction(&self, result.as_ref().cloned());
                result
            }
        }
    }
}
impl<TBuiltin: Builtin> Applicable<Self> for CachedSharedTerm<TBuiltin> {
    fn arity(&self) -> Option<Arity> {
        self.inner_term().arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Result<Self, String> {
        self.inner_term().apply(args, factory, allocator, cache)
    }
    fn should_parallelize(&self, args: &[Self]) -> bool {
        self.inner_expression().should_parallelize(args)
    }
}
impl<TBuiltin: Builtin> Compile<Self> for CachedSharedTerm<TBuiltin> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.inner_term()
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
impl<TBuiltin: Builtin> Evaluate<Self> for CachedSharedTerm<TBuiltin> {
    fn evaluate(
        &self,
        state: &impl DynamicState<Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<EvaluationResult<Self>> {
        evaluate_recursive(
            self.inner_expression(),
            state,
            factory,
            allocator,
            cache,
            None,
            DependencyList::empty(),
        )
    }
}

fn evaluate_recursive<
    T: Expression + Rewritable<T> + Reducible<T>,
    TValue: Expression + Rewritable<T> + Reducible<T>,
>(
    expression: &TValue,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
    result: Option<T>,
    dependencies: DependencyList,
) -> Option<EvaluationResult<T>> {
    let dependencies = dependencies.union(expression.dynamic_dependencies(false));
    match expression.substitute_dynamic(false, state, factory, allocator, cache) {
        Some(expression) => evaluate_recursive(
            &expression,
            state,
            factory,
            allocator,
            cache,
            Some(expression.clone()),
            dependencies,
        ),
        None => match expression.reduce(factory, allocator, cache) {
            Some(expression) => evaluate_recursive(
                &expression,
                state,
                factory,
                allocator,
                cache,
                Some(expression.clone()),
                dependencies,
            ),
            None => result.map(|result| EvaluationResult::new(result, dependencies)),
        },
    }
}
