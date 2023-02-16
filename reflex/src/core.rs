// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::HashSet,
    convert::TryFrom,
    hash::{Hash, Hasher},
    iter::{once, repeat, FromIterator},
    marker::PhantomData,
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

use serde::{Deserialize, Serialize};
pub use uuid::{uuid, Uuid};

pub use crate::cache::EvaluationCache;
use crate::hash::{hash_object, FnvHasher, HashId, IntMap, IntSet};

pub type IntValue = i64;
pub type FloatValue = f64;
pub type SymbolId = u32;

pub fn is_integer(value: FloatValue) -> bool {
    as_integer(value).is_some()
}

pub fn as_integer(value: FloatValue) -> Option<IntValue> {
    let int_value = value as IntValue;
    if value == int_value as FloatValue {
        Some(int_value)
    } else {
        None
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Default, Clone, Copy, Serialize, Deserialize)]
pub struct InstructionPointer(pub usize);
impl InstructionPointer {
    pub fn new(address: usize) -> Self {
        Self(address)
    }
    pub fn get(&self) -> usize {
        self.0
    }
    pub fn advance(&self) -> Self {
        self.offset(1)
    }
    fn offset(&self, offset: usize) -> Self {
        Self(self.0 + offset)
    }
}
impl std::fmt::LowerHex for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}
impl std::fmt::UpperHex for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08X}", self.0)
    }
}
impl std::fmt::Debug for InstructionPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<address:{:x}>", self)
    }
}

pub trait NilTermType: Clone {}

pub trait BooleanTermType: Clone {
    fn value(&self) -> bool;
}

pub trait IntTermType: Clone {
    fn value(&self) -> IntValue;
}

pub trait FloatTermType: Clone {
    fn value(&self) -> FloatValue;
}

pub trait StringTermType<T: Expression>: Clone {
    fn value<'a>(&'a self) -> T::StringRef<'a>
    where
        T::String: 'a,
        T: 'a;
}

pub trait SymbolTermType: Clone {
    fn id(&self) -> SymbolId;
}

pub trait VariableTermType: Clone {
    fn offset(&self) -> StackOffset;
}

pub trait EffectTermType<T: Expression>: Clone {
    fn condition<'a>(&'a self) -> T::SignalRef<'a>
    where
        T::Signal: 'a,
        T: 'a;
}

pub trait LetTermType<T: Expression>: Clone {
    fn initializer<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
    fn body<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
}

pub trait LambdaTermType<T: Expression>: Clone {
    fn num_args(&self) -> StackOffset;
    fn body<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
}

pub trait ApplicationTermType<T: Expression>: Clone {
    fn target<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
    fn args<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T: 'a,
        T::ExpressionList: 'a;
}

pub trait PartialApplicationTermType<T: Expression>: Clone {
    fn target<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
    fn args<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T: 'a,
        T::ExpressionList: 'a;
}

pub trait RecursiveTermType<T: Expression>: Clone {
    fn factory<'a>(&'a self) -> T::ExpressionRef<'a>
    where
        T: 'a;
}

pub trait BuiltinTermType<T: Expression>: Clone {
    fn target<'a>(&'a self) -> T::Builtin
    where
        T: 'a,
        T::Builtin: 'a;
}

pub trait CompiledFunctionTermType: Clone {
    fn address(&self) -> InstructionPointer;
    fn hash(&self) -> HashId;
    fn required_args(&self) -> StackOffset;
    fn optional_args(&self) -> StackOffset;
}

pub trait RecordTermType<T: Expression>: Clone {
    fn prototype<'a>(&'a self) -> T::StructPrototypeRef<'a>
    where
        T::StructPrototype: 'a,
        T: 'a;
    fn values<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T::ExpressionList: 'a,
        T: 'a;
    fn get<'a>(&'a self, key: &T) -> Option<T::ExpressionRef<'a>>
    where
        T: 'a;
}

pub trait ConstructorTermType<T: Expression>: Clone {
    fn prototype<'a>(&'a self) -> T::StructPrototypeRef<'a>
    where
        T::StructPrototype: 'a,
        T: 'a;
}

pub trait ListTermType<T: Expression>: Clone {
    fn items<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T::ExpressionList: 'a,
        T: 'a;
}

pub trait HashmapTermType<T: Expression>: Clone {
    type KeysIterator<'a>: ExactSizeIterator<Item = T::ExpressionRef<'a>>
    where
        T: 'a,
        Self: 'a;
    type ValuesIterator<'a>: ExactSizeIterator<Item = T::ExpressionRef<'a>>
    where
        T: 'a,
        Self: 'a;
    fn get<'a>(&'a self, key: &T) -> Option<T::ExpressionRef<'a>>
    where
        T: 'a;
    fn keys<'a>(&'a self) -> Self::KeysIterator<'a>
    where
        T: 'a;
    fn values<'a>(&'a self) -> Self::ValuesIterator<'a>
    where
        T: 'a;
}

pub trait HashsetTermType<T: Expression>: Clone {
    type ValuesIterator<'a>: ExactSizeIterator<Item = T::ExpressionRef<'a>>
    where
        T: 'a,
        Self: 'a;
    fn contains(&self, value: &T) -> bool;
    fn values<'a>(&'a self) -> Self::ValuesIterator<'a>
    where
        T: 'a;
}

pub trait SignalTermType<T: Expression>: Clone {
    fn signals<'a>(&'a self) -> T::SignalListRef<'a>
    where
        T::SignalList: 'a,
        T: 'a;
}

pub trait ConditionType<T: Expression>:
    Clone + PartialEq + Eq + std::fmt::Display + std::fmt::Debug
{
    fn id(&self) -> StateToken;
    fn signal_type(&self) -> SignalType;
    fn payload<'a>(&'a self) -> T::ExpressionRef<'a>;
    fn token<'a>(&'a self) -> T::ExpressionRef<'a>;
}

pub type ExpressionListIter<'a, T> =
    <<T as Expression>::ExpressionList as ExpressionListType<T>>::Iterator<'a>;

pub trait ExpressionListType<T: Expression>:
    Sized + PartialEq + Eq + Clone + std::fmt::Display + std::fmt::Debug + GraphNode
{
    type Iterator<'a>: ExactSizeIterator<Item = T::ExpressionRef<'a>>
    where
        T: 'a,
        Self: 'a;
    fn id(&self) -> HashId;
    fn len(&self) -> usize;
    fn get<'a>(&'a self, index: usize) -> Option<T::ExpressionRef<'a>>
    where
        T: 'a;
    fn iter<'a>(&'a self) -> Self::Iterator<'a>
    where
        T: 'a;
}

pub trait ConditionListType<T: Expression>:
    Sized + PartialEq + Eq + Clone + std::fmt::Display + std::fmt::Debug
{
    type Iterator<'a>: ExactSizeIterator<Item = T::SignalRef<'a>>
    where
        T::Signal: 'a,
        T: 'a,
        Self: 'a;
    fn id(&self) -> HashId;
    fn len(&self) -> usize;
    fn iter<'a>(&'a self) -> Self::Iterator<'a>
    where
        T::Signal: 'a,
        T: 'a;
}
pub trait StructPrototypeType<T: Expression>:
    PartialEq + Eq + Clone + std::fmt::Display + std::fmt::Debug
{
    fn keys<'a>(&'a self) -> T::ExpressionListRef<'a>
    where
        T::ExpressionList: 'a,
        T: 'a;
}

pub fn parse_record_values<'a, T: Expression>(
    prototype: &T::StructPrototype,
    values: &T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<Option<T::ExpressionList>> {
    if let Some(input_values) = factory.match_record_term(values) {
        let input_prototype = input_values.prototype();
        let input_prototype = input_prototype.as_deref();
        if input_prototype.keys().as_deref().id() == prototype.keys().as_deref().id() {
            Some(None)
        } else if input_prototype.keys().as_deref().len()
            >= prototype.as_deref().keys().as_deref().len()
        {
            let values = prototype
                .as_deref()
                .keys()
                .as_deref()
                .iter()
                .map(|key| {
                    input_values
                        .get(key.as_deref())
                        .map(|item| item.as_deref().clone())
                })
                .collect::<Option<Vec<_>>>();
            match values {
                Some(field_values) => Some(Some(allocator.create_list(field_values))),
                None => None,
            }
        } else {
            None
        }
    } else {
        None
    }
}

pub trait RefType<T> {
    fn as_deref(&self) -> &T;
}

impl<'a, T> RefType<T> for &'a T {
    fn as_deref(&self) -> &T {
        self
    }
}

pub struct IntoRefTypeIterator<T, TRef, TIter> {
    inner: TIter,
    _ref: PhantomData<TRef>,
    _type: PhantomData<T>,
}
impl<'iter, T: 'iter, TRef, TIter> IntoRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = &'iter T>,
    TRef: RefType<T> + From<&'iter T>,
{
    pub fn new(inner: TIter) -> Self {
        Self {
            inner,
            _ref: PhantomData,
            _type: PhantomData,
        }
    }
}
impl<'iter, T: 'iter, TRef, TIter> Iterator for IntoRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = &'iter T> + ExactSizeIterator,
    TRef: RefType<T> + From<&'iter T>,
{
    type Item = TRef;
    fn next(&mut self) -> Option<Self::Item> {
        let Self { inner, .. } = self;
        inner.next().map(|item| item.into())
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let Self { inner, .. } = self;
        let len = inner.len();
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let Self { inner, .. } = self;
        inner.len()
    }
}
impl<'iter, T: 'iter, TRef, TIter> ExactSizeIterator for IntoRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = &'iter T> + ExactSizeIterator,
    TRef: RefType<T> + From<&'iter T>,
{
    fn len(&self) -> usize {
        let Self { inner, .. } = self;
        inner.len()
    }
}

pub struct FromRefTypeIterator<T, TRef, TIter> {
    inner: TIter,
    _ref: PhantomData<TRef>,
    _type: PhantomData<T>,
}
impl<T, TRef, TIter> FromRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = TRef>,
    TRef: RefType<T>,
    T: Clone,
{
    pub fn new(inner: TIter) -> Self {
        Self {
            inner,
            _ref: PhantomData,
            _type: PhantomData,
        }
    }
}
impl<T, TRef, TIter> Iterator for FromRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = TRef>,
    TRef: RefType<T>,
    T: Clone,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        let Self { inner, .. } = self;
        inner.next().map(|item| item.as_deref().clone())
    }
}
impl<T, TRef, TIter> ExactSizeIterator for FromRefTypeIterator<T, TRef, TIter>
where
    TIter: Iterator<Item = TRef> + ExactSizeIterator,
    TRef: RefType<T>,
    T: Clone,
{
    fn len(&self) -> usize {
        let Self { inner, .. } = self;
        inner.len()
    }
}

pub trait Expression:
    GraphNode
    + NodeId
    + SerializeJson
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::clone::Clone
    + std::fmt::Debug
    + std::fmt::Display
where
    Self: std::marker::Sized,
{
    type String: StringValue;
    type Builtin: Builtin;
    type Signal: ConditionType<Self>;
    type SignalList: ConditionListType<Self>;
    type StructPrototype: StructPrototypeType<Self>;
    type ExpressionList: ExpressionListType<Self>;
    type NilTerm: NilTermType;
    type BooleanTerm: BooleanTermType;
    type IntTerm: IntTermType;
    type FloatTerm: FloatTermType;
    type StringTerm: StringTermType<Self>;
    type SymbolTerm: SymbolTermType;
    type VariableTerm: VariableTermType;
    type EffectTerm: EffectTermType<Self>;
    type LetTerm: LetTermType<Self>;
    type LambdaTerm: LambdaTermType<Self>;
    type ApplicationTerm: ApplicationTermType<Self>;
    type PartialApplicationTerm: PartialApplicationTermType<Self>;
    type RecursiveTerm: RecursiveTermType<Self>;
    type BuiltinTerm: BuiltinTermType<Self>;
    type CompiledFunctionTerm: CompiledFunctionTermType;
    type RecordTerm: RecordTermType<Self>;
    type ConstructorTerm: ConstructorTermType<Self>;
    type ListTerm: ListTermType<Self>;
    type HashmapTerm: HashmapTermType<Self>;
    type HashsetTerm: HashsetTermType<Self>;
    type SignalTerm: SignalTermType<Self>;

    type StringRef<'a>: RefType<Self::String> + From<&'a Self::String> + Clone
    where
        Self::String: 'a,
        Self: 'a;

    type SignalRef<'a>: RefType<Self::Signal> + From<&'a Self::Signal> + Clone
    where
        Self::Signal: 'a,
        Self: 'a;

    type StructPrototypeRef<'a>: RefType<Self::StructPrototype>
        + From<&'a Self::StructPrototype>
        + Clone
    where
        Self::StructPrototype: 'a,
        Self: 'a;

    type SignalListRef<'a>: RefType<Self::SignalList> + From<&'a Self::SignalList> + Clone
    where
        Self::SignalList: 'a,
        Self: 'a;

    type ExpressionListRef<'a>: RefType<Self::ExpressionList>
        + From<&'a Self::ExpressionList>
        + Clone
    where
        Self::ExpressionList: 'a,
        Self: 'a;

    type ExpressionRef<'a>: RefType<Self> + From<&'a Self> + Clone
    where
        Self: 'a;
}

pub trait NodeId {
    fn id(&self) -> HashId;
}

pub trait Builtin:
    Uid
    + std::cmp::Eq
    + std::cmp::PartialEq
    + std::fmt::Debug
    + std::hash::Hash
    + std::marker::Copy
    + TryFrom<Uuid, Error = ()>
    + std::fmt::Display
{
    fn arity(&self) -> Arity;
    fn apply<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>;
    fn should_parallelize<T: Expression<Builtin = Self> + Applicable<T>>(&self, args: &[T])
        -> bool;
}

pub trait Uid {
    fn uid(&self) -> Uuid;
}

pub trait GraphNode {
    fn size(&self) -> usize;
    fn capture_depth(&self) -> StackOffset;
    fn free_variables(&self) -> HashSet<StackOffset>;
    fn count_variable_usages(&self, offset: StackOffset) -> usize;
    fn dynamic_dependencies(&self, deep: bool) -> DependencyList;
    fn has_dynamic_dependencies(&self, deep: bool) -> bool;
    /// Weak head normal form - outer term is fully evaluated, sub-expressions may contain dynamic values
    fn is_static(&self) -> bool;
    /// Term is fully evaluated and contains no dynamic sub-expressions
    fn is_atomic(&self) -> bool;
    /// Term contains sub-expressions
    fn is_complex(&self) -> bool;
}

pub trait CompoundNode<T: Expression> {
    type Children<'a>: Iterator<Item = T::ExpressionRef<'a>>
    where
        T: 'a,
        Self: 'a;
    fn children<'a>(&'a self) -> Self::Children<'a>
    where
        T: 'a;
}

pub trait Rewritable<T: Expression> {
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
    fn substitute_dynamic(
        &self,
        deep: bool,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T>;
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
}

pub trait Reducible<T: Expression> {
    fn is_reducible(&self) -> bool;
    fn reduce(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T>;
}

pub trait Applicable<T: Expression> {
    fn arity(&self) -> Option<Arity>;
    fn should_parallelize(&self, args: &[T]) -> bool;
    fn apply(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>;
}

pub trait Evaluate<T: Expression> {
    fn evaluate(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<EvaluationResult<T>>;
}

pub trait Internable {
    fn should_intern(&self, eager: Eagerness) -> bool;
}

pub trait SerializeJson {
    fn to_json(&self) -> Result<serde_json::Value, String>;
    /// Generate a JSON diff object that describes the changes needed for `self` to match `target` (both objects must be the same shape).
    fn patch(&self, target: &Self) -> Result<Option<serde_json::Value>, String>;
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum ArgType {
    /** Evaluate argument before function application, short-circuiting any signals encountered during evaluation */
    Strict,
    /** Evaluate argument before function application, passing signals into function body */
    Eager,
    /** Pass argument directly into function body without evaluating */
    Lazy,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct FunctionArity<const REQUIRED: usize, const OPTIONAL: usize> {
    pub required: [ArgType; REQUIRED],
    pub optional: [ArgType; OPTIONAL],
    pub variadic: Option<ArgType>,
}
impl<const REQUIRED: usize, const OPTIONAL: usize> FunctionArity<REQUIRED, OPTIONAL> {
    pub fn required(&self) -> &[ArgType] {
        &self.required
    }
    pub fn optional(&self) -> &[ArgType] {
        &self.optional
    }
    pub fn variadic(&self) -> Option<ArgType> {
        self.variadic
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Arity {
    /** Homogeneous arity arguments all have the same eagerness requirements */
    Homogeneous(HomogeneousArity),
    /** Heterogeneous arity arguments can specify a mixture of eagerness requirements */
    Heterogeneous(HeterogeneousArity),
}
impl<const R: usize, const O: usize> From<&'static FunctionArity<R, O>> for Arity {
    fn from(definition: &'static FunctionArity<R, O>) -> Self {
        Self::Heterogeneous(HeterogeneousArity::from(definition))
    }
}
impl Arity {
    pub fn strict(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Strict,
            required,
            optional,
            variadic,
        })
    }
    pub fn eager(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Eager,
            required,
            optional,
            variadic,
        })
    }
    pub fn lazy(required: usize, optional: usize, variadic: bool) -> Self {
        Self::Homogeneous(HomogeneousArity {
            arity_type: ArgType::Lazy,
            required,
            optional,
            variadic,
        })
    }
    pub fn required(&self) -> impl ExactSizeIterator<Item = ArgType> {
        match self {
            Self::Homogeneous(arity) => PositionalArityIterator::Homogeneous(arity.required()),
            Self::Heterogeneous(arity) => PositionalArityIterator::Heterogeneous(arity.required()),
        }
    }
    pub fn optional(&self) -> impl ExactSizeIterator<Item = ArgType> {
        match self {
            Self::Homogeneous(arity) => PositionalArityIterator::Homogeneous(arity.optional()),
            Self::Heterogeneous(arity) => PositionalArityIterator::Heterogeneous(arity.optional()),
        }
    }
    pub fn variadic(&self) -> Option<ArgType> {
        match self {
            Self::Homogeneous(arity) => arity.variadic(),
            Self::Heterogeneous(arity) => arity.variadic(),
        }
    }
    pub fn partial(&self, offset: usize) -> Self {
        match self {
            Self::Homogeneous(arity) => Self::Homogeneous(arity.partial(offset)),
            Self::Heterogeneous(arity) => Self::Heterogeneous(arity.partial(offset)),
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = ArgType> {
        self.required()
            .chain(self.optional())
            .chain(self.variadic().into_iter().flat_map(repeat))
    }
}
impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Homogeneous(arity) => std::fmt::Display::fmt(arity, f),
            Self::Heterogeneous(arity) => std::fmt::Display::fmt(arity, f),
        }
    }
}
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct HomogeneousArity {
    arity_type: ArgType,
    required: usize,
    optional: usize,
    variadic: bool,
}
impl HomogeneousArity {
    fn required(&self) -> HomogeneousArityIterator {
        HomogeneousArityIterator(self.required)
    }
    fn optional(&self) -> HomogeneousArityIterator {
        HomogeneousArityIterator(self.optional)
    }
    fn variadic(&self) -> Option<ArgType> {
        match self.variadic {
            true => Some(self.arity_type),
            false => None,
        }
    }
    fn partial(&self, offset: usize) -> Self {
        Self {
            arity_type: self.arity_type,
            required: self.required.saturating_sub(offset),
            optional: self
                .optional
                .saturating_sub(offset.saturating_sub(self.required)),
            variadic: self.variadic,
        }
    }
}
impl std::fmt::Display for HomogeneousArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.required + self.optional,
            if self.variadic {
                String::from("+")
            } else {
                String::from("")
            }
        )
    }
}
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct HeterogeneousArity {
    required: &'static [ArgType],
    optional: &'static [ArgType],
    variadic: Option<ArgType>,
}
impl<'a, const R: usize, const O: usize> From<&'static FunctionArity<R, O>> for HeterogeneousArity {
    fn from(definition: &'static FunctionArity<R, O>) -> Self {
        Self {
            required: definition.required(),
            optional: definition.optional(),
            variadic: definition.variadic(),
        }
    }
}
impl HeterogeneousArity {
    fn required(&self) -> HeterogeneousArityIterator {
        HeterogeneousArityIterator(self.required, 0)
    }
    fn optional(&self) -> HeterogeneousArityIterator {
        HeterogeneousArityIterator(self.optional, 0)
    }
    fn variadic(&self) -> Option<ArgType> {
        self.variadic
    }
    fn partial(&self, offset: usize) -> Self {
        Self {
            required: &self.required[offset.min(self.required.len())..],
            optional: &self.optional[offset
                .saturating_sub(self.required.len())
                .min(self.optional.len())..],
            variadic: self.variadic,
        }
    }
}
impl std::fmt::Display for HeterogeneousArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.required.len() + self.optional.len(),
            if self.variadic.is_some() {
                String::from("+")
            } else {
                String::from("")
            }
        )
    }
}
pub enum PositionalArityIterator {
    Homogeneous(HomogeneousArityIterator),
    Heterogeneous(HeterogeneousArityIterator),
}
impl Iterator for PositionalArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Homogeneous(iter) => iter.next(),
            Self::Heterogeneous(iter) => iter.next(),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Homogeneous(iter) => iter.size_hint(),
            Self::Heterogeneous(iter) => iter.size_hint(),
        }
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            Self::Homogeneous(iter) => iter.count(),
            Self::Heterogeneous(iter) => iter.count(),
        }
    }
}
impl ExactSizeIterator for PositionalArityIterator {
    fn len(&self) -> usize {
        match self {
            Self::Homogeneous(iter) => iter.len(),
            Self::Heterogeneous(iter) => iter.len(),
        }
    }
}
pub struct HomogeneousArityIterator(usize);
impl Iterator for HomogeneousArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        let remaining = self.0;
        if remaining == 0 {
            None
        } else {
            self.0 -= 1;
            Some(ArgType::Lazy)
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let len = ExactSizeIterator::len(&self);
        len
    }
}
impl ExactSizeIterator for HomogeneousArityIterator {
    fn len(&self) -> usize {
        let remaining = self.0;
        remaining
    }
}
pub struct HeterogeneousArityIterator(&'static [ArgType], usize);
impl Iterator for HeterogeneousArityIterator {
    type Item = ArgType;
    fn next(&mut self) -> Option<Self::Item> {
        let args = self.0;
        let index = self.1;
        let arg = args.get(index)?;
        self.1 += 1;
        Some(*arg)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = ExactSizeIterator::len(self);
        (len, Some(len))
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        let len = ExactSizeIterator::len(&self);
        len
    }
}
impl ExactSizeIterator for HeterogeneousArityIterator {
    fn len(&self) -> usize {
        let args = self.0;
        let index = self.1;
        let remaining = args.len() - index;
        remaining
    }
}

pub trait StringValue:
    std::cmp::Eq + std::cmp::PartialEq + std::clone::Clone + std::fmt::Debug + std::fmt::Display
where
    Self: std::marker::Sized,
{
    type StringRef<'a>: Deref<Target = str> + Into<String>
    where
        Self: 'a;
    fn id(&self) -> HashId;
    fn as_str<'a>(&'a self) -> Self::StringRef<'a>;
}
impl StringValue for String {
    type StringRef<'a> = &'a str
        where
            Self: 'a;
    fn id(&self) -> HashId {
        hash_object(self)
    }
    fn as_str<'a>(&'a self) -> Self::StringRef<'a> {
        self.as_str()
    }
}

pub type StateToken = HashId;

pub trait DynamicState<T> {
    fn id(&self) -> HashId;
    fn has(&self, key: &StateToken) -> bool;
    fn get(&self, key: &StateToken) -> Option<&T>;
}

pub struct StateCache<T: Expression> {
    hash: HashId,
    values: IntMap<StateToken, T>,
}
impl<T: Expression> Default for StateCache<T> {
    fn default() -> Self {
        Self {
            hash: FnvHasher::default().finish(),
            values: IntMap::default(),
        }
    }
}
impl<T: Expression> Hash for StateCache<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<T: Expression> DynamicState<T> for StateCache<T> {
    fn id(&self) -> HashId {
        self.hash
    }
    fn has(&self, key: &StateToken) -> bool {
        self.values.contains_key(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        self.values.get(key)
    }
}
impl<T: Expression> StateCache<T> {
    pub fn set(&mut self, key: StateToken, value: T) {
        let value_hash = value.id();
        let previous = self.values.insert(key, value);
        let has_changes = match &previous {
            Some(previous_value) => value_hash != previous_value.id(),
            None => true,
        };
        if has_changes {
            self.hash = {
                let mut hasher = FnvHasher::default();
                hasher.write_u64(self.hash);
                hasher.write_u64(key);
                hasher.write_u64(value_hash);
                hasher.finish()
            }
        }
    }
    pub fn remove(&mut self, key: &StateToken) -> Option<T> {
        let result = self.values.remove(key);
        if result.is_some() {
            let mut hasher = FnvHasher::default();
            hasher.write_u64(self.hash);
            hasher.write_u64(*key);
            hasher.write_u8(0);
            self.hash = hasher.finish();
        }
        result
    }
    pub fn extend(&mut self, entries: impl IntoIterator<Item = (StateToken, T)>) {
        let mut hasher = FnvHasher::default();
        hasher.write_u64(self.hash);
        for (key, value) in entries {
            hasher.write_u64(key);
            hasher.write_u64(value.id());
            self.set(key, value);
        }
        self.hash = hasher.finish();
    }
    pub fn gc(&mut self, retained_keys: &DependencyList) {
        let mut hasher = FnvHasher::default();
        hasher.write_u64(self.hash);
        self.values.retain(|key, _| {
            if retained_keys.contains(*key) {
                true
            } else {
                hasher.write_u64(*key);
                hasher.write_u8(0);
                false
            }
        });
        self.values.shrink_to_fit();
        self.hash = hasher.finish();
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
}
impl<T: Expression> FromIterator<(StateToken, T)> for StateCache<T> {
    fn from_iter<I: IntoIterator<Item = (StateToken, T)>>(iter: I) -> Self {
        let mut hasher = FnvHasher::default();
        let mut values = IntMap::default();
        for (key, value) in iter {
            hasher.write_u64(key);
            hasher.write_u64(value.id());
            values.insert(key, value);
        }
        Self {
            hash: hasher.finish(),
            values,
        }
    }
}
impl<T: Expression> DynamicState<T> for Rc<StateCache<T>> {
    fn id(&self) -> HashId {
        (&**self).id()
    }
    fn has(&self, key: &StateToken) -> bool {
        (&**self).has(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        (&**self).get(key)
    }
}
impl<T: Expression> DynamicState<T> for Arc<StateCache<T>> {
    fn id(&self) -> HashId {
        (&**self).id()
    }
    fn has(&self, key: &StateToken) -> bool {
        (&**self).has(key)
    }
    fn get(&self, key: &StateToken) -> Option<&T> {
        (&**self).get(key)
    }
}

pub fn hash_state_values<T: Expression>(
    state: &impl DynamicState<T>,
    state_tokens: impl IntoIterator<Item = StateToken>,
) -> HashId {
    let mut hasher = FnvHasher::default();
    for state_token in state_tokens {
        match state.get(&state_token) {
            None => hasher.write_u8(0),
            Some(value) => hasher.write_u64(value.id()),
        }
    }
    hasher.finish()
}

pub type StackOffset = usize;

pub trait ExpressionFactory<T: Expression> {
    fn create_nil_term(&self) -> T;
    fn create_boolean_term(&self, value: bool) -> T;
    fn create_int_term(&self, value: IntValue) -> T;
    fn create_float_term(&self, value: FloatValue) -> T;
    fn create_string_term(&self, value: T::String) -> T;
    fn create_symbol_term(&self, value: SymbolId) -> T;
    fn create_variable_term(&self, offset: StackOffset) -> T;
    fn create_effect_term(&self, condition: T::Signal) -> T;
    fn create_let_term(&self, initializer: T, body: T) -> T;
    fn create_lambda_term(&self, num_args: StackOffset, body: T) -> T;
    fn create_application_term(&self, target: T, args: T::ExpressionList) -> T;
    fn create_partial_application_term(&self, target: T, args: T::ExpressionList) -> T;
    fn create_recursive_term(&self, factory: T) -> T;
    fn create_builtin_term(&self, target: impl Into<T::Builtin>) -> T;
    fn create_compiled_function_term(
        &self,
        address: InstructionPointer,
        hash: HashId,
        required_args: StackOffset,
        optional_args: StackOffset,
    ) -> T;
    fn create_record_term(&self, prototype: T::StructPrototype, fields: T::ExpressionList) -> T;
    fn create_constructor_term(&self, prototype: T::StructPrototype) -> T;
    fn create_list_term(&self, items: T::ExpressionList) -> T;
    fn create_hashmap_term(
        &self,
        entries: impl IntoIterator<Item = (T, T), IntoIter = impl ExactSizeIterator<Item = (T, T)>>,
    ) -> T;
    fn create_hashset_term(
        &self,
        values: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T;
    fn create_signal_term(&self, signals: T::SignalList) -> T;

    fn match_nil_term<'a>(&self, expression: &'a T) -> Option<&'a T::NilTerm>;
    fn match_boolean_term<'a>(&self, expression: &'a T) -> Option<&'a T::BooleanTerm>;
    fn match_int_term<'a>(&self, expression: &'a T) -> Option<&'a T::IntTerm>;
    fn match_float_term<'a>(&self, expression: &'a T) -> Option<&'a T::FloatTerm>;
    fn match_string_term<'a>(&self, expression: &'a T) -> Option<&'a T::StringTerm>;
    fn match_symbol_term<'a>(&self, expression: &'a T) -> Option<&'a T::SymbolTerm>;
    fn match_variable_term<'a>(&self, expression: &'a T) -> Option<&'a T::VariableTerm>;
    fn match_effect_term<'a>(&self, expression: &'a T) -> Option<&'a T::EffectTerm>;
    fn match_let_term<'a>(&self, expression: &'a T) -> Option<&'a T::LetTerm>;
    fn match_lambda_term<'a>(&self, expression: &'a T) -> Option<&'a T::LambdaTerm>;
    fn match_application_term<'a>(&self, expression: &'a T) -> Option<&'a T::ApplicationTerm>;
    fn match_partial_application_term<'a>(
        &self,
        expression: &'a T,
    ) -> Option<&'a T::PartialApplicationTerm>;
    fn match_recursive_term<'a>(&self, expression: &'a T) -> Option<&'a T::RecursiveTerm>;
    fn match_builtin_term<'a>(&self, expression: &'a T) -> Option<&'a T::BuiltinTerm>;
    fn match_compiled_function_term<'a>(
        &self,
        target: &'a T,
    ) -> Option<&'a T::CompiledFunctionTerm>;
    fn match_record_term<'a>(&self, expression: &'a T) -> Option<&'a T::RecordTerm>;
    fn match_constructor_term<'a>(&self, expression: &'a T) -> Option<&'a T::ConstructorTerm>;
    fn match_list_term<'a>(&self, expression: &'a T) -> Option<&'a T::ListTerm>;
    fn match_hashmap_term<'a>(&self, expression: &'a T) -> Option<&'a T::HashmapTerm>;
    fn match_hashset_term<'a>(&self, expression: &'a T) -> Option<&'a T::HashsetTerm>;
    fn match_signal_term<'a>(&self, expression: &'a T) -> Option<&'a T::SignalTerm>;
}

pub trait HeapAllocator<T: Expression> {
    fn create_list(
        &self,
        expressions: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    ) -> T::ExpressionList;
    fn create_unsized_list(&self, expressions: impl IntoIterator<Item = T>) -> T::ExpressionList;
    fn create_sized_list(
        &self,
        size: usize,
        expressions: impl IntoIterator<Item = T>,
    ) -> T::ExpressionList;
    fn create_empty_list(&self) -> T::ExpressionList;
    fn create_unit_list(&self, value: T) -> T::ExpressionList;
    fn create_pair(&self, left: T, right: T) -> T::ExpressionList;
    fn create_triple(&self, first: T, second: T, third: T) -> T::ExpressionList;
    fn clone_list<'a>(&self, expressions: T::ExpressionListRef<'a>) -> T::ExpressionList
    where
        Self: 'a;
    fn create_signal_list(&self, signals: impl IntoIterator<Item = T::Signal>) -> T::SignalList;
    fn create_struct_prototype(&self, keys: T::ExpressionList) -> T::StructPrototype;
    fn clone_struct_prototype<'a>(
        &self,
        prototype: T::StructPrototypeRef<'a>,
    ) -> T::StructPrototype
    where
        Self: 'a;
    fn create_signal(&self, signal_type: SignalType, payload: T, token: T) -> T::Signal;
    fn clone_signal<'a>(&self, signal: T::SignalRef<'a>) -> T::Signal
    where
        Self: 'a;
    fn create_string(&self, value: impl Into<String>) -> T::String;
    fn create_static_string(&self, value: &'static str) -> T::String;
    fn clone_string<'a>(&self, value: T::StringRef<'a>) -> T::String
    where
        Self: 'a;
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize)]
pub enum Eagerness {
    Eager,
    Lazy,
}

pub type SignalId = HashId;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum SignalType {
    Error,
    Pending,
    Custom(String),
}
impl std::fmt::Display for SignalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum SubstitutionPatterns<'a, T: Expression> {
    Variable(&'a Vec<(StackOffset, T)>, Option<ScopeOffset>),
    #[allow(dead_code)]
    Offset(ScopeOffset),
}

impl<'a, T: Expression> Hash for SubstitutionPatterns<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            SubstitutionPatterns::Variable(vec, offset) => {
                offset.hash(state);
                for (i, var) in vec.iter() {
                    i.hash(state);
                    state.write_u64(var.id())
                }
            }
            SubstitutionPatterns::Offset(offset) => offset.hash(state),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Substitutions<'a, T: Expression> {
    entries: SubstitutionPatterns<'a, T>,
    min_depth: StackOffset,
    offset: StackOffset,
}

impl<'a, T: Expression> Hash for Substitutions<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.entries.hash(state);
        self.min_depth.hash(state);
        self.offset.hash(state);
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
pub enum ScopeOffset {
    Wrap(usize),
    Unwrap(usize),
}
impl<'a, T: Expression + Rewritable<T>> Substitutions<'a, T> {
    pub fn named(entries: &'a Vec<(StackOffset, T)>, scope_offset: Option<ScopeOffset>) -> Self {
        Self {
            entries: SubstitutionPatterns::Variable(entries, scope_offset),
            min_depth: entries
                .iter()
                .map(|(offset, _)| offset)
                .fold(StackOffset::MAX, |acc, offset| acc.min(*offset)),
            offset: 0,
        }
    }
    pub fn increase_scope_offset(depth: StackOffset, min_depth: StackOffset) -> Self {
        Self {
            entries: SubstitutionPatterns::Offset(ScopeOffset::Wrap(depth)),
            min_depth,
            offset: 0,
        }
    }
    pub fn decrease_scope_offset(depth: StackOffset, min_depth: StackOffset) -> Self {
        Self {
            entries: SubstitutionPatterns::Offset(ScopeOffset::Unwrap(depth)),
            min_depth,
            offset: 0,
        }
    }
    pub fn offset(&self, offset: StackOffset) -> Self {
        Self {
            entries: self.entries.clone(),
            min_depth: self.min_depth,
            offset: self.offset + offset,
        }
    }
    pub fn can_skip(&self, expression: &impl GraphNode) -> bool {
        let capture_depth = expression.capture_depth();
        if capture_depth == 0 {
            true
        } else {
            expression.capture_depth() - 1 < self.min_depth + self.offset
        }
    }
    pub fn substitute_variable(
        &self,
        offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        if offset < self.min_depth + self.offset {
            return None;
        }
        let (entries, scope_offset) = match self.entries {
            SubstitutionPatterns::Variable(entries, scope_offset) => {
                Some((Some(entries), scope_offset))
            }
            SubstitutionPatterns::Offset(scope_offset) => Some((None, Some(scope_offset))),
        }?;

        let target_offset = offset - self.offset;
        let replacement = entries.and_then(|entries| {
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
                    .substitute_static(
                        &Substitutions::increase_scope_offset(self.offset, 0),
                        factory,
                        allocator,
                        cache,
                    )
                    .unwrap_or_else(|| replacement.clone()),
            ),
            None => match scope_offset {
                Some(scope_offset) => {
                    let target_offset = match scope_offset {
                        ScopeOffset::Wrap(scope_offset) => offset + scope_offset,
                        ScopeOffset::Unwrap(scope_offset) => offset - scope_offset,
                    };
                    if target_offset != offset {
                        Some(factory.create_variable_term(target_offset))
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}

#[derive(Default, Eq, PartialEq, Clone, Debug)]
pub struct DependencyList {
    state_tokens: IntSet<StateToken>,
}
impl Extend<StateToken> for DependencyList {
    fn extend<T: IntoIterator<Item = StateToken>>(&mut self, state_tokens: T) {
        self.state_tokens.extend(state_tokens);
    }
}
impl Extend<DependencyList> for DependencyList {
    fn extend<T: IntoIterator<Item = DependencyList>>(&mut self, values: T) {
        self.extend(values.into_iter().flatten())
    }
}
impl DependencyList {
    pub fn empty() -> Self {
        Self::default()
    }
    pub fn of(state_token: StateToken) -> Self {
        Self {
            state_tokens: IntSet::from_iter(once(state_token)),
        }
    }
    pub fn len(&self) -> usize {
        self.state_tokens.len()
    }
    pub fn is_empty(&self) -> bool {
        self.state_tokens.is_empty()
    }
    pub fn contains(&self, state_token: StateToken) -> bool {
        self.state_tokens.contains(&state_token)
    }
    pub fn insert(&mut self, state_token: StateToken) {
        self.state_tokens.insert(state_token);
    }
    pub fn union(mut self, other: Self) -> Self {
        if self.is_empty() {
            other
        } else {
            self.extend(other);
            self
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = StateToken> + ExactSizeIterator + '_ {
        self.state_tokens.iter().copied()
    }
}
impl FromIterator<StateToken> for DependencyList {
    fn from_iter<T: IntoIterator<Item = StateToken>>(iter: T) -> Self {
        Self {
            state_tokens: iter.into_iter().collect(),
        }
    }
}
impl IntoIterator for DependencyList {
    type Item = StateToken;
    type IntoIter = std::collections::hash_set::IntoIter<StateToken>;
    fn into_iter(self) -> Self::IntoIter {
        self.state_tokens.into_iter()
    }
}
impl<'a> IntoIterator for &'a DependencyList {
    type Item = StateToken;
    type IntoIter = std::iter::Copied<std::collections::hash_set::Iter<'a, StateToken>>;
    fn into_iter(self) -> Self::IntoIter {
        self.state_tokens.iter().copied()
    }
}
impl serde::Serialize for DependencyList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedDependencyList::from(self).serialize(serializer)
    }
}
impl<'de> serde::Deserialize<'de> for DependencyList {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedDependencyList::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedDependencyList(Vec<StateToken>);
impl<'a> From<&'a DependencyList> for SerializedDependencyList {
    fn from(value: &'a DependencyList) -> Self {
        let DependencyList { state_tokens } = value;
        SerializedDependencyList(state_tokens.iter().cloned().collect())
    }
}
impl From<SerializedDependencyList> for DependencyList {
    fn from(value: SerializedDependencyList) -> Self {
        let SerializedDependencyList(state_tokens) = value;
        Self::from_iter(state_tokens)
    }
}

pub fn match_typed_expression_list<'a, T: Expression + 'a, V, E>(
    expressions: impl IntoIterator<Item = &'a T, IntoIter = impl ExactSizeIterator<Item = &'a T>>,
    matcher: impl Fn(&'a T) -> Option<V>,
    err: impl Fn(&'a T) -> E,
) -> Result<impl IntoIterator<Item = V, IntoIter = impl ExactSizeIterator<Item = V>>, E> {
    expressions
        .into_iter()
        .map(|item| matcher(item).ok_or_else(|| err(item)))
        // TODO: Avoid intermediate allocation
        .collect::<Result<Vec<_>, _>>()
}

pub fn transform_expression_list<T: Expression>(
    expressions: &T::ExpressionList,
    allocator: &impl HeapAllocator<T>,
    mut transform: impl FnMut(&T) -> Option<T>,
) -> Option<T::ExpressionList> {
    // Create a lazy iterator that collects transformed values along with their indices
    // Note that we don't pre-emptively collect the iterator because there might be no transformed expressions,
    // in which case we don't need to allocate a vector
    let mut iter = expressions
        .iter()
        .map(|item| transform(item.as_deref()))
        .enumerate()
        .filter_map(|(index, result)| result.map(|result| (index, result)));
    // Pull the first value from the iterator, returning if there were no transformed expressions
    let (index, replaced) = iter.next()?;
    let mut results = expressions
        .iter()
        .map(|item| item.as_deref().clone())
        .collect::<Vec<_>>();
    results[index] = replaced.clone();
    // Post-fill with the remaining transformed expressions
    for (index, replaced) in iter {
        results[index] = replaced;
    }
    Some(allocator.create_list(results))
}

pub fn evaluate<T: Expression + Rewritable<T> + Reducible<T> + Evaluate<T>>(
    expression: &T,
    state: &impl DynamicState<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> EvaluationResult<T> {
    expression
        .evaluate(state, factory, allocator, cache)
        .unwrap_or_else(|| EvaluationResult::new(expression.clone(), DependencyList::empty()))
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct EvaluationResult<T: Expression> {
    result: T,
    dependencies: DependencyList,
}
impl<T: Expression> EvaluationResult<T> {
    pub fn new(result: T, dependencies: DependencyList) -> Self {
        EvaluationResult {
            result,
            dependencies,
        }
    }
    pub fn result(&self) -> &T {
        &self.result
    }
    pub fn dependencies(&self) -> &DependencyList {
        &self.dependencies
    }
    pub fn into_parts(self) -> (T, DependencyList) {
        (self.result, self.dependencies)
    }
    pub fn with_dependencies(self, dependencies: DependencyList) -> Self {
        Self {
            result: self.result,
            dependencies: self.dependencies.union(dependencies),
        }
    }
}

pub fn validate_function_application_arity<T: Expression>(
    target: &T,
    arity: &Arity,
    num_args: usize,
) -> Result<usize, String> {
    let num_required_args = arity.required().len();
    let num_optional_args = arity.optional().len();
    if num_args < num_required_args {
        Err(format!(
            "{}: Expected {} {}, received {}",
            target,
            num_required_args,
            if num_optional_args > 0 || arity.variadic().is_some() {
                "or more arguments"
            } else if num_required_args != 1 {
                "arguments"
            } else {
                "argument"
            },
            num_args,
        ))
    } else {
        let num_args = match arity.variadic() {
            Some(_) => num_args,
            None => num_args.min(num_required_args + num_optional_args),
        };
        Ok(num_args)
    }
}

pub fn create_record<T: Expression>(
    properties: impl IntoIterator<Item = (T, T)>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    let (keys, values): (Vec<_>, Vec<_>) = properties.into_iter().unzip();
    factory.create_record_term(
        allocator.create_struct_prototype(allocator.create_list(keys)),
        allocator.create_list(values),
    )
}

pub fn apply_function<T: Expression + Applicable<T>>(
    target: &T,
    args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
    cache: &mut impl EvaluationCache<T>,
) -> T {
    target
        .apply(args.into_iter(), factory, allocator, cache)
        .unwrap_or_else(|error| {
            create_error_expression(
                factory.create_string_term(allocator.create_string(error)),
                factory,
                allocator,
            )
        })
}

pub fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Pending,
        factory.create_nil_term(),
        factory.create_nil_term(),
    ))))
}

pub fn create_error_expression<T: Expression>(
    payload: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        payload,
        factory.create_nil_term(),
    ))))
}

pub fn get_short_circuit_signal<T: Expression>(
    args: &[T],
    arity: &Arity,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    get_combined_short_circuit_signal(
        get_num_short_circuit_signals(args.iter().cloned(), arity, factory),
        args.iter().cloned(),
        arity,
        factory,
        allocator,
    )
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum ShortCircuitCount {
    None,
    Single,
    Multiple,
}
impl ShortCircuitCount {
    pub fn increment(&mut self) {
        *self = match self {
            Self::None => Self::Single,
            Self::Single => Self::Multiple,
            Self::Multiple => Self::Multiple,
        }
    }
}

pub fn get_num_short_circuit_signals<T: Expression>(
    args: impl IntoIterator<Item = T>,
    arity: &Arity,
    factory: &impl ExpressionFactory<T>,
) -> ShortCircuitCount {
    let short_circuit_args = args
        .into_iter()
        .zip(arity.iter())
        .filter(|(arg, arg_type)| match arg_type {
            ArgType::Strict => factory.match_signal_term(&arg).is_some(),
            _ => false,
        });
    let mut num_short_circuit_args = ShortCircuitCount::None;
    for _ in short_circuit_args {
        if let ShortCircuitCount::None = num_short_circuit_args {
            num_short_circuit_args = ShortCircuitCount::Single;
        } else {
            return ShortCircuitCount::Multiple;
        }
    }
    num_short_circuit_args
}

pub fn get_combined_short_circuit_signal<T: Expression>(
    num_short_circuit_args: ShortCircuitCount,
    args: impl IntoIterator<Item = T>,
    arity: &Arity,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Option<T> {
    match num_short_circuit_args.into() {
        ShortCircuitCount::None => None,
        ShortCircuitCount::Single => Some(
            filter_short_circuit_args(args, arity, factory)
                .into_iter()
                .map(|(arg, _)| arg)
                .next()
                .unwrap(),
        ),
        ShortCircuitCount::Multiple => {
            let combined_signal = factory.create_signal_term(allocator.create_signal_list(
                filter_short_circuit_args(args, arity, factory).fold(
                    // TODO: avoid unnecessary intermediate allocations
                    Vec::<T::Signal>::new(),
                    |mut results, (_, signal)| {
                        results.extend(
                            signal
                                .signals()
                                .as_deref()
                                .iter()
                                .map(|item| item.as_deref().clone()),
                        );
                        results
                    },
                ),
            ));
            Some(combined_signal)
        }
    }
}

fn filter_short_circuit_args<'a, T: Expression + 'a>(
    args: impl IntoIterator<Item = T, IntoIter = impl Iterator<Item = T> + 'a>,
    arity: &Arity,
    matcher: &'a impl ExpressionFactory<T>,
) -> impl Iterator<Item = (T, T::SignalTerm)> + 'a {
    args.into_iter()
        .zip(arity.iter())
        .filter_map(|(arg, arg_type)| match arg_type {
            ArgType::Strict => matcher
                .match_signal_term(&arg)
                .map(|value| value.clone())
                .map(|signal| (arg, signal)),
            ArgType::Eager | ArgType::Lazy => None,
        })
}

pub fn get_hashmap_entries<'a, T: Expression + 'a>(
    target: &'a T::HashmapTerm,
    factory: &'a impl ExpressionFactory<T>,
    allocator: &'a impl HeapAllocator<T>,
) -> impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T> + 'a> + 'a {
    target
        .keys()
        .map(|item| item.as_deref().clone())
        .zip(target.values().map(|item| item.as_deref().clone()))
        .map(move |(key, value)| factory.create_list_term(allocator.create_pair(key, value)))
}

pub fn deduplicate_hashmap_entries<T: Expression>(keys: &[T], values: &[T]) -> Option<Vec<(T, T)>> {
    let lookup = build_hashmap_lookup_table(keys.iter().cloned());
    if lookup.len() == keys.len() {
        None
    } else {
        let (entries, _) = keys.iter().fold(
            (Vec::with_capacity(lookup.len()), HashSet::new()),
            |results, key| {
                let (mut deduplicated_entries, mut processed_keys) = results;
                let key_hash = key.id();
                if !processed_keys.contains(&key_hash) {
                    let target_index = *lookup.get(&key_hash).unwrap();
                    let value = values.get(target_index).unwrap();
                    deduplicated_entries.push((key.clone(), value.clone()));
                    processed_keys.insert(key_hash);
                }
                (deduplicated_entries, processed_keys)
            },
        );
        Some(entries)
    }
}

pub fn build_hashmap_lookup_table<T: Expression>(
    keys: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
) -> IntMap<HashId, usize> {
    keys.into_iter()
        .enumerate()
        .map(|(index, key)| (key.id(), index))
        .collect()
}

pub fn deduplicate_hashset_entries<T: Expression>(values: &[T]) -> Option<Vec<T>> {
    let lookup = build_hashset_lookup_table(values.iter().cloned());
    if lookup.len() == values.len() {
        None
    } else {
        let (values, _) = values.iter().fold(
            (Vec::with_capacity(lookup.len()), HashSet::new()),
            |results, value| {
                let (mut deduplicated_values, mut processed_values) = results;
                let value_hash = value.id();
                if !processed_values.contains(&value_hash) {
                    deduplicated_values.push(value.clone());
                    processed_values.insert(value_hash);
                }
                (deduplicated_values, processed_values)
            },
        );
        Some(values)
    }
}

pub fn build_hashset_lookup_table<T: Expression>(
    values: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
) -> HashSet<HashId> {
    let values = values.into_iter();
    values.into_iter().map(|value| value.id()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arity() {
        trait MyApplicable {
            fn required_args(&self) -> &[ArgType];
            fn optional_args(&self) -> &[ArgType];
            fn variadic_args(&self) -> Option<ArgType>;
        }
        const FOO_ARITY: FunctionArity<3, 1> = FunctionArity {
            required: [ArgType::Strict, ArgType::Lazy, ArgType::Lazy],
            optional: [ArgType::Strict],
            variadic: None,
        };
        struct Foo {}
        impl MyApplicable for Foo {
            fn required_args(&self) -> &[ArgType] {
                &FOO_ARITY.required()
            }
            fn optional_args(&self) -> &[ArgType] {
                &FOO_ARITY.optional()
            }
            fn variadic_args(&self) -> Option<ArgType> {
                FOO_ARITY.variadic()
            }
        }
        let expression = Foo {};
        assert_eq!(expression.required_args().len(), 3);
        assert_eq!(expression.optional_args().len(), 1);
        assert_eq!(
            expression
                .required_args()
                .iter()
                .map(|arg| match arg {
                    ArgType::Strict => String::from("strict"),
                    ArgType::Eager => String::from("eager"),
                    ArgType::Lazy => String::from("lazy"),
                })
                .collect::<String>(),
            "strictlazylazy"
        );
        struct Bar {
            offset: usize,
        }
        impl MyApplicable for Bar {
            fn required_args(&self) -> &[ArgType] {
                &FOO_ARITY.required()[self.offset.min(FOO_ARITY.required().len())..]
            }
            fn optional_args(&self) -> &[ArgType] {
                &FOO_ARITY.optional()[self.offset.saturating_sub(FOO_ARITY.required().len())..]
            }
            fn variadic_args(&self) -> Option<ArgType> {
                FOO_ARITY.variadic()
            }
        }
        let expression = Bar { offset: 4 };
        assert_eq!(expression.required_args().len(), 0);
        assert_eq!(expression.optional_args().len(), 0);
        assert_eq!(
            expression
                .required_args()
                .iter()
                .map(|arg| match arg {
                    ArgType::Strict => String::from("strict"),
                    ArgType::Eager => String::from("eager"),
                    ArgType::Lazy => String::from("lazy"),
                })
                .collect::<String>(),
            ""
        );
    }
}
