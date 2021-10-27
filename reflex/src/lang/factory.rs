// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{collections::HashSet, sync::Arc};
use tracing::trace;

use crate::{
    compiler::{Compile, Compiler, InstructionPointer, Program},
    core::{
        Applicable, DependencyList, DynamicState, Evaluate, EvaluationCache, EvaluationResult,
        Expression, ExpressionFactory, GraphNode, HeapAllocator, Reducible, Rewritable,
        SerializeJson, SignalList, StackOffset, StateToken, StructPrototype, Substitutions,
        VarArgs,
    },
    hash::{hash_object, HashId},
};

use super::term::*;

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct SharedTerm {
    pub(crate) value: Arc<Term<CachedTerm<SharedTerm>>>,
}
impl SharedTerm {
    fn new(value: Term<CachedTerm<SharedTerm>>) -> Self {
        Self {
            value: Arc::new(value),
        }
    }
}
impl Expression for SharedTerm {
    type String = String;
    fn id(&self) -> HashId {
        hash_object(&*self.value)
    }
}
impl GraphNode for SharedTerm {
    fn capture_depth(&self) -> StackOffset {
        self.value.capture_depth()
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
impl Rewritable<CachedTerm<SharedTerm>> for SharedTerm {
    fn subexpressions(&self) -> Vec<&CachedTerm<SharedTerm>> {
        self.value.subexpressions()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<CachedTerm<SharedTerm>>,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Option<CachedTerm<SharedTerm>> {
        self.value
            .substitute_static(substitutions, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<CachedTerm<SharedTerm>>,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Option<CachedTerm<SharedTerm>> {
        self.value
            .substitute_dynamic(state, factory, allocator, cache)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Option<CachedTerm<SharedTerm>> {
        self.value.normalize(factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
    ) -> Option<CachedTerm<SharedTerm>> {
        self.value.hoist_free_variables(factory, allocator)
    }
}
impl Reducible<CachedTerm<SharedTerm>> for SharedTerm {
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Option<CachedTerm<SharedTerm>> {
        self.value.reduce(factory, allocator, cache)
    }
}
impl Applicable<CachedTerm<SharedTerm>> for SharedTerm {
    fn arity(&self) -> Option<crate::core::Arity> {
        self.value.arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = CachedTerm<SharedTerm>>,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Result<CachedTerm<SharedTerm>, String> {
        self.value.apply(args, factory, allocator, cache)
    }
}
impl Evaluate<CachedTerm<SharedTerm>> for SharedTerm {
    fn evaluate(
        &self,
        state: &impl DynamicState<CachedTerm<SharedTerm>>,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        cache: &mut impl EvaluationCache<CachedTerm<SharedTerm>>,
    ) -> Option<EvaluationResult<CachedTerm<SharedTerm>>> {
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
            let dependencies = dependencies.union(expression.dynamic_dependencies());
            match expression.substitute_dynamic(state, factory, allocator, cache) {
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

        evaluate_recursive(
            self,
            state,
            factory,
            allocator,
            cache,
            None,
            DependencyList::empty(),
        )
    }
}
impl Compile<CachedTerm<SharedTerm>> for SharedTerm {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<CachedTerm<SharedTerm>>,
        allocator: &impl HeapAllocator<CachedTerm<SharedTerm>>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
impl std::fmt::Display for SharedTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl std::fmt::Debug for SharedTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}

impl SerializeJson for SharedTerm {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        (*self.value).to_json()
    }
}

#[derive(Clone, Default)]
pub struct TermFactory {}
impl TermFactory {
    fn create_expression(&self, value: Term<CachedTerm<SharedTerm>>) -> CachedTerm<SharedTerm> {
        CachedTerm::new(SharedTerm::new(value))
    }
}
impl ExpressionFactory<CachedTerm<SharedTerm>> for TermFactory {
    fn create_value_term(&self, value: ValueTerm<StringPrimitive>) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "value_term");
        self.create_expression(Term::Value(value))
    }
    fn create_static_variable_term(&self, offset: StackOffset) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "static_variable");
        self.create_expression(Term::Variable(VariableTerm::Static(
            StaticVariableTerm::new(offset),
        )))
    }
    fn create_dynamic_variable_term(
        &self,
        state_token: StateToken,
        fallback: CachedTerm<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "dynamic_variable");
        self.create_expression(Term::Variable(VariableTerm::Dynamic(
            DynamicVariableTerm::new(state_token, fallback),
        )))
    }
    fn create_let_term(
        &self,
        initializer: CachedTerm<SharedTerm>,
        body: CachedTerm<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "let");
        self.create_expression(Term::Let(LetTerm::new(initializer, body)))
    }
    fn create_lambda_term(
        &self,
        num_args: StackOffset,
        body: CachedTerm<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "lambda");
        self.create_expression(Term::Lambda(LambdaTerm::new(num_args, body)))
    }
    fn create_application_term(
        &self,
        target: CachedTerm<SharedTerm>,
        args: CachedList<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "application");
        self.create_expression(Term::Application(ApplicationTerm::new(target, args)))
    }
    fn create_partial_application_term(
        &self,
        target: CachedTerm<SharedTerm>,
        args: CachedList<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "partial_application");
        self.create_expression(Term::PartialApplication(PartialApplicationTerm::new(
            target, args,
        )))
    }
    fn create_recursive_term(&self, factory: CachedTerm<SharedTerm>) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "recursive");
        self.create_expression(Term::Recursive(RecursiveTerm::new(factory)))
    }
    fn create_builtin_term(&self, target: BuiltinTerm) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "builtin");
        self.create_expression(Term::Builtin(target))
    }
    fn create_native_function_term(
        &self,
        target: NativeFunction<CachedTerm<SharedTerm>>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "native_function");
        self.create_expression(Term::Native(NativeFunctionTerm::new(target)))
    }
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "compiled_function");
        self.create_expression(Term::CompiledFunction(CompiledFunctionTerm::new(
            address,
            hash,
            required_args,
            optional_args,
        )))
    }
    fn create_tuple_term(&self, fields: CachedList<SharedTerm>) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "tuple");
        self.create_expression(Term::Tuple(TupleTerm::new(fields)))
    }
    fn create_struct_term(
        &self,
        prototype: StructPrototype,
        fields: CachedList<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "struct");
        self.create_expression(Term::Struct(StructTerm::new(prototype, fields)))
    }
    fn create_constructor_term(&self, prototype: StructPrototype) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "constructor");
        self.create_expression(Term::Constructor(ConstructorTerm::new(prototype)))
    }
    fn create_vector_term(&self, items: CachedList<SharedTerm>) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "vector");
        self.create_expression(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            items,
        ))))
    }
    fn create_hashmap_term(
        &self,
        keys: CachedList<SharedTerm>,
        values: CachedList<SharedTerm>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "hashmap");
        self.create_expression(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
            keys, values,
        ))))
    }
    fn create_hashset_term(&self, values: CachedList<SharedTerm>) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "hashset");
        self.create_expression(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
            values,
        ))))
    }
    fn create_signal_term(
        &self,
        signals: SignalList<CachedTerm<SharedTerm>>,
    ) -> CachedTerm<SharedTerm> {
        trace!(factory_create = "signal");
        self.create_expression(Term::Signal(SignalTerm::new(signals)))
    }

    fn match_value_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a ValueTerm<StringPrimitive>> {
        match expression.value().value.as_ref() {
            Term::Value(term) => Some(term),
            _ => None,
        }
    }
    fn match_static_variable_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a StaticVariableTerm> {
        match expression.value().value.as_ref() {
            Term::Variable(VariableTerm::Static(term)) => Some(term),
            _ => None,
        }
    }
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a DynamicVariableTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Variable(VariableTerm::Dynamic(term)) => Some(term),
            _ => None,
        }
    }
    fn match_let_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a LetTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Let(term) => Some(term),
            _ => None,
        }
    }
    fn match_lambda_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a LambdaTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Lambda(term) => Some(term),
            _ => None,
        }
    }
    fn match_application_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a ApplicationTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Application(term) => Some(term),
            _ => None,
        }
    }
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a PartialApplicationTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::PartialApplication(term) => Some(term),
            _ => None,
        }
    }
    fn match_recursive_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a RecursiveTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Recursive(term) => Some(term),
            _ => None,
        }
    }
    fn match_builtin_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a BuiltinTerm> {
        match expression.value().value.as_ref() {
            Term::Builtin(term) => Some(term),
            _ => None,
        }
    }
    fn match_native_function_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a NativeFunctionTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Native(term) => Some(term),
            _ => None,
        }
    }
    fn match_compiled_function_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a CompiledFunctionTerm> {
        match expression.value().value.as_ref() {
            Term::CompiledFunction(term) => Some(term),
            _ => None,
        }
    }
    fn match_tuple_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a TupleTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Tuple(term) => Some(term),
            _ => None,
        }
    }
    fn match_struct_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a StructTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Struct(term) => Some(term),
            _ => None,
        }
    }
    fn match_constructor_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a ConstructorTerm> {
        match expression.value().value.as_ref() {
            Term::Constructor(term) => Some(term),
            _ => None,
        }
    }
    fn match_vector_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a VectorTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Collection(CollectionTerm::Vector(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashmap_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a HashMapTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Collection(CollectionTerm::HashMap(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashset_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a HashSetTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Collection(CollectionTerm::HashSet(term)) => Some(term),
            _ => None,
        }
    }
    fn match_signal_term<'a>(
        &self,
        expression: &'a CachedTerm<SharedTerm>,
    ) -> Option<&'a SignalTerm<CachedTerm<SharedTerm>>> {
        match expression.value().value.as_ref() {
            Term::Signal(term) => Some(term),
            _ => None,
        }
    }
}
