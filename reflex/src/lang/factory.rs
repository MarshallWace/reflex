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

use super::{
    expression::{CachedExpression, SharedExpression},
    term::*,
};

type FactoryTermString = String;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct FactoryTerm<TBuiltin: Builtin> {
    value: CachedExpression<SharedExpression<Term<FactoryTerm<TBuiltin>>>>,
}
impl<TBuiltin: Builtin> Expression for FactoryTerm<TBuiltin> {
    type String = FactoryTermString;
    type Builtin = TBuiltin;
    fn id(&self) -> HashId {
        self.value.id()
    }
}
impl<TBuiltin: Builtin> GraphNode for FactoryTerm<TBuiltin> {
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
impl<TBuiltin: Builtin> std::fmt::Debug for FactoryTerm<TBuiltin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.value, f)
    }
}
impl<TBuiltin: Builtin> std::fmt::Display for FactoryTerm<TBuiltin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}
impl<TBuiltin: Builtin> SerializeJson for FactoryTerm<TBuiltin> {
    fn to_json(&self) -> Result<serde_json::Value, String> {
        SerializeJson::to_json(&self.value)
    }
}
impl<TBuiltin: Builtin> Rewritable<FactoryTerm<TBuiltin>> for FactoryTerm<TBuiltin> {
    fn subexpressions(&self) -> Vec<&FactoryTerm<TBuiltin>> {
        self.value.value().value().subexpressions()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<FactoryTerm<TBuiltin>>,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Option<FactoryTerm<TBuiltin>> {
        self.value
            .value()
            .value()
            .substitute_static(substitutions, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<FactoryTerm<TBuiltin>>,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Option<FactoryTerm<TBuiltin>> {
        self.value
            .value()
            .value()
            .substitute_dynamic(state, factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
    ) -> Option<FactoryTerm<TBuiltin>> {
        self.value
            .value()
            .value()
            .hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Option<FactoryTerm<TBuiltin>> {
        self.value
            .value()
            .value()
            .normalize(factory, allocator, cache)
    }
}
impl<TBuiltin: Builtin> Reducible<FactoryTerm<TBuiltin>> for FactoryTerm<TBuiltin> {
    fn is_reducible(&self) -> bool {
        self.value.value().value().is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Option<FactoryTerm<TBuiltin>> {
        self.value.value().value().reduce(factory, allocator, cache)
    }
}
impl<TBuiltin: Builtin> Applicable<FactoryTerm<TBuiltin>> for FactoryTerm<TBuiltin> {
    fn arity(&self) -> Option<Arity> {
        self.value.value().value().arity()
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = FactoryTerm<TBuiltin>>,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Result<FactoryTerm<TBuiltin>, String> {
        self.value
            .value()
            .value()
            .apply(args, factory, allocator, cache)
    }
}
impl<TBuiltin: Builtin> Compile<FactoryTerm<TBuiltin>> for FactoryTerm<TBuiltin> {
    fn compile(
        &self,
        eager: VarArgs,
        stack_offset: StackOffset,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        compiler: &mut Compiler,
    ) -> Result<Program, String> {
        self.value
            .value()
            .value()
            .compile(eager, stack_offset, factory, allocator, compiler)
    }
}
impl<TBuiltin: Builtin> Evaluate<FactoryTerm<TBuiltin>> for FactoryTerm<TBuiltin> {
    fn evaluate(
        &self,
        state: &impl DynamicState<FactoryTerm<TBuiltin>>,
        factory: &impl ExpressionFactory<FactoryTerm<TBuiltin>>,
        allocator: &impl HeapAllocator<FactoryTerm<TBuiltin>>,
        cache: &mut impl EvaluationCache<FactoryTerm<TBuiltin>>,
    ) -> Option<EvaluationResult<FactoryTerm<TBuiltin>>> {
        evaluate_recursive(
            self.value.value().value(),
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

#[derive(Clone)]
pub struct TermFactory<TBuiltin: Builtin> {
    _builtin: PhantomData<TBuiltin>,
}
impl<TBuiltin: Builtin> Default for TermFactory<TBuiltin> {
    fn default() -> Self {
        Self {
            _builtin: PhantomData,
        }
    }
}
impl<TBuiltin: Builtin> TermFactory<TBuiltin> {
    fn create_expression(&self, value: Term<FactoryTerm<TBuiltin>>) -> FactoryTerm<TBuiltin> {
        FactoryTerm {
            value: CachedExpression::new(SharedExpression::new(value)),
        }
    }
}
impl<TBuiltin: Builtin> ExpressionFactory<FactoryTerm<TBuiltin>> for TermFactory<TBuiltin> {
    fn create_value_term(&self, value: ValueTerm<StringPrimitive>) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "value_term");
        self.create_expression(Term::Value(value))
    }
    fn create_static_variable_term(&self, offset: StackOffset) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "static_variable");
        self.create_expression(Term::Variable(VariableTerm::Static(
            StaticVariableTerm::new(offset),
        )))
    }
    fn create_dynamic_variable_term(
        &self,
        state_token: StateToken,
        fallback: FactoryTerm<TBuiltin>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "dynamic_variable");
        self.create_expression(Term::Variable(VariableTerm::Dynamic(
            DynamicVariableTerm::new(state_token, fallback),
        )))
    }
    fn create_let_term(
        &self,
        initializer: FactoryTerm<TBuiltin>,
        body: FactoryTerm<TBuiltin>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "let");
        self.create_expression(Term::Let(LetTerm::new(initializer, body)))
    }
    fn create_lambda_term(
        &self,
        num_args: StackOffset,
        body: FactoryTerm<TBuiltin>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "lambda");
        self.create_expression(Term::Lambda(LambdaTerm::new(num_args, body)))
    }
    fn create_application_term(
        &self,
        target: FactoryTerm<TBuiltin>,
        args: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "application");
        self.create_expression(Term::Application(ApplicationTerm::new(target, args)))
    }
    fn create_partial_application_term(
        &self,
        target: FactoryTerm<TBuiltin>,
        args: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "partial_application");
        self.create_expression(Term::PartialApplication(PartialApplicationTerm::new(
            target, args,
        )))
    }
    fn create_recursive_term(&self, factory: FactoryTerm<TBuiltin>) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "recursive");
        self.create_expression(Term::Recursive(RecursiveTerm::new(factory)))
    }
    fn create_builtin_term(&self, target: impl Into<TBuiltin>) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "builtin");
        self.create_expression(Term::Builtin(BuiltinTerm::new(target.into())))
    }
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> FactoryTerm<TBuiltin> {
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
        fields: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "tuple");
        self.create_expression(Term::Tuple(TupleTerm::new(fields)))
    }
    fn create_struct_term(
        &self,
        prototype: StructPrototype,
        fields: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "struct");
        self.create_expression(Term::Struct(StructTerm::new(prototype, fields)))
    }
    fn create_constructor_term(&self, prototype: StructPrototype) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "constructor");
        self.create_expression(Term::Constructor(ConstructorTerm::new(prototype)))
    }
    fn create_vector_term(
        &self,
        items: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "vector");
        self.create_expression(Term::Collection(CollectionTerm::Vector(VectorTerm::new(
            items,
        ))))
    }
    fn create_hashmap_term(
        &self,
        keys: ExpressionList<FactoryTerm<TBuiltin>>,
        values: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "hashmap");
        self.create_expression(Term::Collection(CollectionTerm::HashMap(HashMapTerm::new(
            keys, values,
        ))))
    }
    fn create_hashset_term(
        &self,
        values: ExpressionList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "hashset");
        self.create_expression(Term::Collection(CollectionTerm::HashSet(HashSetTerm::new(
            values,
        ))))
    }
    fn create_signal_term(
        &self,
        signals: SignalList<FactoryTerm<TBuiltin>>,
    ) -> FactoryTerm<TBuiltin> {
        trace!(factory_create = "signal");
        self.create_expression(Term::Signal(SignalTerm::new(signals)))
    }

    fn match_value_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a ValueTerm<StringPrimitive>> {
        match expression.value.value().value() {
            Term::Value(term) => Some(term),
            _ => None,
        }
    }
    fn match_static_variable_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a StaticVariableTerm> {
        match expression.value.value().value() {
            Term::Variable(VariableTerm::Static(term)) => Some(term),
            _ => None,
        }
    }
    fn match_dynamic_variable_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a DynamicVariableTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Variable(VariableTerm::Dynamic(term)) => Some(term),
            _ => None,
        }
    }
    fn match_let_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a LetTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Let(term) => Some(term),
            _ => None,
        }
    }
    fn match_lambda_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a LambdaTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Lambda(term) => Some(term),
            _ => None,
        }
    }
    fn match_application_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a ApplicationTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Application(term) => Some(term),
            _ => None,
        }
    }
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a PartialApplicationTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::PartialApplication(term) => Some(term),
            _ => None,
        }
    }
    fn match_recursive_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a RecursiveTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Recursive(term) => Some(term),
            _ => None,
        }
    }
    fn match_builtin_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a BuiltinTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Builtin(term) => Some(term),
            _ => None,
        }
    }
    fn match_compiled_function_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a CompiledFunctionTerm> {
        match expression.value.value().value() {
            Term::CompiledFunction(term) => Some(term),
            _ => None,
        }
    }
    fn match_tuple_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a TupleTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Tuple(term) => Some(term),
            _ => None,
        }
    }
    fn match_struct_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a StructTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Struct(term) => Some(term),
            _ => None,
        }
    }
    fn match_constructor_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a ConstructorTerm> {
        match expression.value.value().value() {
            Term::Constructor(term) => Some(term),
            _ => None,
        }
    }
    fn match_vector_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a VectorTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Collection(CollectionTerm::Vector(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashmap_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a HashMapTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Collection(CollectionTerm::HashMap(term)) => Some(term),
            _ => None,
        }
    }
    fn match_hashset_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a HashSetTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Collection(CollectionTerm::HashSet(term)) => Some(term),
            _ => None,
        }
    }
    fn match_signal_term<'a>(
        &self,
        expression: &'a FactoryTerm<TBuiltin>,
    ) -> Option<&'a SignalTerm<FactoryTerm<TBuiltin>>> {
        match expression.value.value().value() {
            Term::Signal(term) => Some(term),
            _ => None,
        }
    }
}
