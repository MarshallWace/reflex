// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{collections::HashSet, marker::PhantomData};

use reflex::{
    core::{
        Applicable, Arity, Builtin, CompoundNode, DependencyList, DynamicState, Eagerness,
        Evaluate, EvaluationCache, EvaluationResult, Expression, ExpressionFactory, FloatValue,
        GraphNode, HeapAllocator, InstructionPointer, IntValue, Internable, NodeId, Reducible,
        Rewritable, SerializeJson, StackOffset, Substitutions, SymbolId,
    },
    hash::HashId,
};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::{ExpressionList, Signal, SignalList, StructPrototype};

use super::{
    expression::{CachedExpression, SharedExpression},
    term::*,
};

#[derive(Clone, Copy, Serialize, Deserialize)]
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
        CachedSharedTerm::new(value)
    }
}
impl<TBuiltin: Builtin> ExpressionFactory<CachedSharedTerm<TBuiltin>>
    for SharedTermFactory<TBuiltin>
{
    fn create_nil_term(&self) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Nil(NilTerm))
    }
    fn create_boolean_term(&self, value: bool) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Boolean(BooleanTerm::new(value)))
    }
    fn create_int_term(&self, value: IntValue) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Int(IntTerm::new(value)))
    }
    fn create_float_term(&self, value: FloatValue) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Float(FloatTerm::new(value)))
    }
    fn create_string_term(
        &self,
        value: <CachedSharedTerm<TBuiltin> as Expression>::String,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::String(StringTerm::new(value)))
    }
    fn create_symbol_term(&self, id: SymbolId) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Symbol(SymbolTerm::new(id)))
    }
    fn create_variable_term(&self, offset: StackOffset) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Variable(VariableTerm::new(offset)))
    }
    fn create_effect_term(
        &self,
        condition: Signal<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Effect(EffectTerm::new(condition)))
    }
    fn create_let_term(
        &self,
        initializer: CachedSharedTerm<TBuiltin>,
        body: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Let(LetTerm::new(initializer, body)))
    }
    fn create_lambda_term(
        &self,
        num_args: StackOffset,
        body: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Lambda(LambdaTerm::new(num_args, body)))
    }
    fn create_application_term(
        &self,
        target: CachedSharedTerm<TBuiltin>,
        args: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Application(ApplicationTerm::new(target, args)))
    }
    fn create_partial_application_term(
        &self,
        target: CachedSharedTerm<TBuiltin>,
        args: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::PartialApplication(PartialApplicationTerm::new(
            target, args,
        )))
    }
    fn create_recursive_term(
        &self,
        factory: CachedSharedTerm<TBuiltin>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Recursive(RecursiveTerm::new(factory)))
    }
    fn create_builtin_term(&self, target: impl Into<TBuiltin>) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Builtin(BuiltinTerm::new(target.into())))
    }
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::CompiledFunction(CompiledFunctionTerm::new(
            address,
            hash,
            required_args,
            optional_args,
        )))
    }
    fn create_record_term(
        &self,
        prototype: StructPrototype<CachedSharedTerm<TBuiltin>>,
        fields: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Record(RecordTerm::new(prototype, fields)))
    }
    fn create_constructor_term(
        &self,
        prototype: StructPrototype<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Constructor(ConstructorTerm::new(prototype)))
    }
    fn create_list_term(
        &self,
        items: ExpressionList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::List(ListTerm::new(items)))
    }
    fn create_hashmap_term(
        &self,
        entries: impl IntoIterator<
            Item = (CachedSharedTerm<TBuiltin>, CachedSharedTerm<TBuiltin>),
            IntoIter = impl ExactSizeIterator<
                Item = (CachedSharedTerm<TBuiltin>, CachedSharedTerm<TBuiltin>),
            >,
        >,
    ) -> CachedSharedTerm<TBuiltin> {
        let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
        self.create_expression(Term::HashMap(HashMapTerm::new(
            ExpressionList::new(keys),
            ExpressionList::new(values),
        )))
    }
    fn create_hashset_term(
        &self,
        values: impl IntoIterator<
            Item = CachedSharedTerm<TBuiltin>,
            IntoIter = impl ExactSizeIterator<Item = CachedSharedTerm<TBuiltin>>,
        >,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::HashSet(HashSetTerm::new(ExpressionList::new(values))))
    }
    fn create_signal_term(
        &self,
        signals: SignalList<CachedSharedTerm<TBuiltin>>,
    ) -> CachedSharedTerm<TBuiltin> {
        self.create_expression(Term::Signal(SignalTerm::new(signals)))
    }
    fn match_nil_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a NilTerm> {
        match expression.inner_term() {
            Term::Nil(term) => Some(term),
            _ => None,
        }
    }
    fn match_boolean_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a BooleanTerm> {
        match expression.inner_term() {
            Term::Boolean(term) => Some(term),
            _ => None,
        }
    }
    fn match_int_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a IntTerm> {
        match expression.inner_term() {
            Term::Int(term) => Some(term),
            _ => None,
        }
    }
    fn match_float_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a FloatTerm> {
        match expression.inner_term() {
            Term::Float(term) => Some(term),
            _ => None,
        }
    }
    fn match_string_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a StringTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::String(term) => Some(term),
            _ => None,
        }
    }
    fn match_symbol_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a SymbolTerm> {
        match expression.inner_term() {
            Term::Symbol(term) => Some(term),
            _ => None,
        }
    }
    fn match_variable_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a VariableTerm> {
        match expression.inner_term() {
            Term::Variable(term) => Some(term),
            _ => None,
        }
    }
    fn match_effect_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a EffectTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Effect(term) => Some(term),
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
    fn match_record_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a RecordTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Record(term) => Some(term),
            _ => None,
        }
    }
    fn match_constructor_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a ConstructorTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::Constructor(term) => Some(term),
            _ => None,
        }
    }
    fn match_list_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a ListTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::List(term) => Some(term),
            _ => None,
        }
    }
    fn match_hashmap_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a HashMapTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::HashMap(term) => Some(term),
            _ => None,
        }
    }
    fn match_hashset_term<'a>(
        &self,
        expression: &'a CachedSharedTerm<TBuiltin>,
    ) -> Option<&'a HashSetTerm<CachedSharedTerm<TBuiltin>>> {
        match expression.inner_term() {
            Term::HashSet(term) => Some(term),
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

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct CachedSharedTerm<TBuiltin: Builtin> {
    _stdlib: PhantomData<TBuiltin>,
    value: SharedExpression<CachedExpression<Term<Self>>>,
}
impl<TBuiltin: Builtin> CachedSharedTerm<TBuiltin> {
    pub fn new(value: Term<Self>) -> Self {
        Self {
            _stdlib: PhantomData,
            value: SharedExpression::new(CachedExpression::new(value)),
        }
    }
    pub fn value(&self) -> &SharedExpression<CachedExpression<Term<Self>>> {
        &self.value
    }
    fn inner_term(&self) -> &Term<Self> {
        self.value.value().value()
    }
}
impl<TBuiltin: Builtin> Expression for CachedSharedTerm<TBuiltin> {
    type String = String;
    type Builtin = TBuiltin;
    type Signal = Signal<Self>;
    type SignalList = SignalList<Self>;
    type StructPrototype = StructPrototype<Self>;
    type ExpressionList = ExpressionList<Self>;
    type NilTerm = NilTerm;
    type BooleanTerm = BooleanTerm;
    type IntTerm = IntTerm;
    type FloatTerm = FloatTerm;
    type StringTerm = StringTerm<Self>;
    type SymbolTerm = SymbolTerm;
    type VariableTerm = VariableTerm;
    type EffectTerm = EffectTerm<Self>;
    type LetTerm = LetTerm<Self>;
    type LambdaTerm = LambdaTerm<Self>;
    type ApplicationTerm = ApplicationTerm<Self>;
    type PartialApplicationTerm = PartialApplicationTerm<Self>;
    type RecursiveTerm = RecursiveTerm<Self>;
    type BuiltinTerm = BuiltinTerm<Self>;
    type CompiledFunctionTerm = CompiledFunctionTerm;
    type RecordTerm = RecordTerm<Self>;
    type ConstructorTerm = ConstructorTerm<Self>;
    type ListTerm = ListTerm<Self>;
    type HashmapTerm = HashMapTerm<Self>;
    type HashsetTerm = HashSetTerm<Self>;
    type SignalTerm = SignalTerm<Self>;

    type StringRef<'a> = &'a Self::String
    where
        Self::String: 'a,
        Self: 'a;

    type SignalRef<'a> = &'a <Self as Expression>::Signal
    where
        <Self as Expression>::Signal: 'a,
        Self: 'a;

    type StructPrototypeRef<'a> = &'a <Self as Expression>::StructPrototype
    where
        <Self as Expression>::StructPrototype: 'a,
        Self: 'a;

    type SignalListRef<'a> = &'a <Self as Expression>::SignalList
    where
        <Self as Expression>::SignalList: 'a,
        Self: 'a;

    type ExpressionListRef<'a> = &'a <Self as Expression>::ExpressionList
    where
        <Self as Expression>::ExpressionList: 'a,
        Self: 'a;

    type ExpressionRef<'a> = &'a Self
    where
        Self: 'a;
}
impl<TBuiltin: Builtin> NodeId for CachedSharedTerm<TBuiltin> {
    fn id(&self) -> HashId {
        self.value.id()
    }
}
impl<TBuiltin: Builtin> GraphNode for CachedSharedTerm<TBuiltin> {
    fn size(&self) -> usize {
        self.value.size()
    }
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
    fn is_complex(&self) -> bool {
        self.value.is_complex()
    }
}
impl<TStdlib: Builtin + 'static> CompoundNode<Self> for CachedSharedTerm<TStdlib> {
    type Children<'a> = TermChildren<'a, Self>
    where
        Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        Self: 'a,
    {
        self.value.children()
    }
}
impl<TBuiltin: Builtin> Rewritable<Self> for CachedSharedTerm<TBuiltin> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        self.value
            .substitute_static(substitutions, factory, allocator, cache)
    }
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        self.value
            .substitute_dynamic(deep, state, factory, allocator, cache)
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
    ) -> Option<Self> {
        self.value.hoist_free_variables(factory, allocator)
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        self.value.normalize(factory, allocator, cache)
    }
}
impl<TBuiltin: Builtin> Reducible<Self> for CachedSharedTerm<TBuiltin> {
    fn is_reducible(&self) -> bool {
        self.value.is_reducible()
    }
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Option<Self> {
        self.value.reduce(factory, allocator, cache)
    }
}
impl<TBuiltin: Builtin> Applicable<Self> for CachedSharedTerm<TBuiltin> {
    fn arity(&self) -> Option<Arity> {
        self.value.arity()
    }
    fn should_parallelize(&self, args: &[Self]) -> bool {
        self.value.should_parallelize(args)
    }
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = Self>,
        factory: &impl ExpressionFactory<Self>,
        allocator: &impl HeapAllocator<Self>,
        cache: &mut impl EvaluationCache<Self>,
    ) -> Result<Self, String> {
        self.value.apply(args, factory, allocator, cache)
    }
}

impl<TBuiltin: Builtin> Internable for CachedSharedTerm<TBuiltin> {
    fn should_intern(&self, eager: Eagerness) -> bool {
        self.value.should_intern(eager)
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
        self.value.evaluate(state, factory, allocator, cache)
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
    fn to_json(&self) -> Result<JsonValue, String> {
        SerializeJson::to_json(&self.value)
    }

    fn patch(&self, target: &Self) -> Result<Option<JsonValue>, String> {
        self.value.patch(&target.value)
    }
}
impl<TBuiltin: Builtin> serde::Serialize for CachedSharedTerm<TBuiltin> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}
impl<'de, TBuiltin: Builtin> serde::Deserialize<'de> for CachedSharedTerm<TBuiltin> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self {
            value: SharedExpression::<CachedExpression<Term<Self>>>::deserialize(deserializer)?,
            _stdlib: PhantomData,
        })
    }
}
