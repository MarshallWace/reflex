// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};

pub use abs::*;
pub use add::*;
pub use and::*;
pub use append::*;
pub use apply::*;
pub use car::*;
pub use cdr::*;
pub use ceil::*;
pub use collect::*;
pub use concat::*;
pub use cons::*;
pub use construct::*;
pub use contains::*;
pub use divide::*;
pub use effect::*;
pub use ends_with::*;
pub use entries::*;
pub use eq::*;
pub use equal::*;
pub use filter::*;
pub use floor::*;
pub use get::*;
pub use gt::*;
pub use gte::*;
pub use if_error::*;
pub use if_pending::*;
pub use insert::*;
pub use keys::*;
pub use lt::*;
pub use lte::*;
pub use map::*;
pub use max::*;
pub use merge::*;
pub use min::*;
pub use multiply::*;
pub use not::*;
pub use or::*;
pub use pow::*;
pub use push::*;
pub use push_front::*;
pub use r#if::*;
pub use r#match::*;
pub use reduce::*;
pub use remainder::*;
pub use replace::*;
pub use resolve::*;
pub use resolve_deep::*;
pub use round::*;
pub use sequence::*;
pub use slice::*;
pub use split::*;
pub use starts_with::*;
pub use subtract::*;
pub use values::*;

mod abs;
mod add;
mod and;
mod append;
mod apply;
mod car;
mod cdr;
mod ceil;
mod collect;
mod concat;
mod cons;
mod construct;
mod contains;
mod divide;
mod effect;
mod ends_with;
mod entries;
mod eq;
mod equal;
mod filter;
mod floor;
mod get;
mod gt;
mod gte;
mod r#if;
mod if_error;
mod if_pending;
mod insert;
mod keys;
mod lt;
mod lte;
mod map;
mod r#match;
mod max;
mod merge;
mod min;
mod multiply;
mod not;
mod or;
mod pow;
mod push;
mod push_front;
mod reduce;
mod remainder;
mod replace;
mod resolve;
mod resolve_deep;
pub use resolve_deep::*;
mod resolve_shallow;
pub use resolve_shallow::*;
mod round;
mod sequence;
mod slice;
mod split;
mod starts_with;
mod subtract;
mod values;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "type")]
pub enum Stdlib {
    Abs,
    Add,
    And,
    Append,
    Apply,
    Car,
    Cdr,
    Ceil,
    Collect,
    CollectFilterResults,
    CollectHashMap,
    CollectHashSet,
    CollectStruct,
    CollectTuple,
    CollectVector,
    Concat,
    Cons,
    ConstructHashMap,
    ConstructHashSet,
    ConstructStruct,
    ConstructTuple,
    ConstructVector,
    Contains,
    Divide,
    Effect,
    EndsWith,
    Entries,
    Eq,
    Equal,
    Filter,
    Floor,
    Get,
    Gt,
    Gte,
    If,
    IfError,
    IfPending,
    Insert,
    Keys,
    Lt,
    Lte,
    Map,
    Match,
    Max,
    Merge,
    Min,
    Multiply,
    Not,
    Or,
    Pow,
    Push,
    PushFront,
    Reduce,
    Remainder,
    Replace,
    ResolveDeep,
    ResolveHashMap,
    ResolveHashSet,
    ResolveShallow,
    ResolveStruct,
    ResolveTuple,
    ResolveVector,
    Round,
    Sequence,
    Slice,
    Split,
    StartsWith,
    Subtract,
    Values,
}
impl Stdlib {
    pub fn entries() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::Abs => Uid::uid(&Abs {}),
            Self::Add => Uid::uid(&Add {}),
            Self::And => Uid::uid(&And {}),
            Self::Append => Uid::uid(&Append {}),
            Self::Apply => Uid::uid(&Apply {}),
            Self::Car => Uid::uid(&Car {}),
            Self::Cdr => Uid::uid(&Cdr {}),
            Self::Ceil => Uid::uid(&Ceil {}),
            Self::Collect => Uid::uid(&Collect {}),
            Self::CollectFilterResults => Uid::uid(&CollectFilterResults {}),
            Self::CollectHashMap => Uid::uid(&CollectHashMap {}),
            Self::CollectHashSet => Uid::uid(&CollectHashSet {}),
            Self::CollectStruct => Uid::uid(&CollectStruct {}),
            Self::CollectTuple => Uid::uid(&CollectTuple {}),
            Self::CollectVector => Uid::uid(&CollectVector {}),
            Self::Concat => Uid::uid(&Concat {}),
            Self::Cons => Uid::uid(&Cons {}),
            Self::ConstructHashMap => Uid::uid(&ConstructHashMap {}),
            Self::ConstructHashSet => Uid::uid(&ConstructHashSet {}),
            Self::ConstructStruct => Uid::uid(&ConstructStruct {}),
            Self::ConstructTuple => Uid::uid(&ConstructTuple {}),
            Self::ConstructVector => Uid::uid(&ConstructVector {}),
            Self::Contains => Uid::uid(&Contains {}),
            Self::Divide => Uid::uid(&Divide {}),
            Self::Effect => Uid::uid(&Effect {}),
            Self::EndsWith => Uid::uid(&EndsWith {}),
            Self::Entries => Uid::uid(&Entries {}),
            Self::Eq => Uid::uid(&Eq {}),
            Self::Equal => Uid::uid(&Equal {}),
            Self::Filter => Uid::uid(&Filter {}),
            Self::Floor => Uid::uid(&Floor {}),
            Self::Get => Uid::uid(&Get {}),
            Self::Gt => Uid::uid(&Gt {}),
            Self::Gte => Uid::uid(&Gte {}),
            Self::If => Uid::uid(&If {}),
            Self::IfError => Uid::uid(&IfError {}),
            Self::IfPending => Uid::uid(&IfPending {}),
            Self::Insert => Uid::uid(&Insert {}),
            Self::Keys => Uid::uid(&Keys {}),
            Self::Lt => Uid::uid(&Lt {}),
            Self::Lte => Uid::uid(&Lte {}),
            Self::Map => Uid::uid(&Map {}),
            Self::Match => Uid::uid(&Match {}),
            Self::Max => Uid::uid(&Max {}),
            Self::Merge => Uid::uid(&Merge {}),
            Self::Min => Uid::uid(&Min {}),
            Self::Multiply => Uid::uid(&Multiply {}),
            Self::Not => Uid::uid(&Not {}),
            Self::Or => Uid::uid(&Or {}),
            Self::Pow => Uid::uid(&Pow {}),
            Self::Push => Uid::uid(&Push {}),
            Self::PushFront => Uid::uid(&PushFront {}),
            Self::Reduce => Uid::uid(&Reduce {}),
            Self::Remainder => Uid::uid(&Remainder {}),
            Self::Replace => Uid::uid(&Replace {}),
            Self::ResolveDeep => Uid::uid(&ResolveDeep {}),
            Self::ResolveHashMap => Uid::uid(&ResolveHashMap {}),
            Self::ResolveHashSet => Uid::uid(&ResolveHashSet {}),
            Self::ResolveShallow => Uid::uid(&ResolveShallow {}),
            Self::ResolveStruct => Uid::uid(&ResolveStruct {}),
            Self::ResolveTuple => Uid::uid(&ResolveTuple {}),
            Self::ResolveVector => Uid::uid(&ResolveVector {}),
            Self::Round => Uid::uid(&Round {}),
            Self::Sequence => Uid::uid(&Sequence {}),
            Self::Slice => Uid::uid(&Slice {}),
            Self::Split => Uid::uid(&Split {}),
            Self::StartsWith => Uid::uid(&StartsWith {}),
            Self::Subtract => Uid::uid(&Subtract {}),
            Self::Values => Uid::uid(&Values {}),
        }
    }
}
impl Stdlib {
    pub fn arity<T: Expression + Applicable<T>>(&self) -> Option<Arity>
    where
        T::Builtin: From<Self>,
    {
        match self {
            Self::Abs => Applicable::<T>::arity(&Abs {}),
            Self::Add => Applicable::<T>::arity(&Add {}),
            Self::And => Applicable::<T>::arity(&And {}),
            Self::Append => Applicable::<T>::arity(&Append {}),
            Self::Apply => Applicable::<T>::arity(&Apply {}),
            Self::Car => Applicable::<T>::arity(&Car {}),
            Self::Cdr => Applicable::<T>::arity(&Cdr {}),
            Self::Ceil => Applicable::<T>::arity(&Ceil {}),
            Self::Collect => Applicable::<T>::arity(&Collect {}),
            Self::CollectFilterResults => Applicable::<T>::arity(&CollectFilterResults {}),
            Self::CollectHashMap => Applicable::<T>::arity(&CollectHashMap {}),
            Self::CollectHashSet => Applicable::<T>::arity(&CollectHashSet {}),
            Self::CollectStruct => Applicable::<T>::arity(&CollectStruct {}),
            Self::CollectTuple => Applicable::<T>::arity(&CollectTuple {}),
            Self::CollectVector => Applicable::<T>::arity(&CollectVector {}),
            Self::Concat => Applicable::<T>::arity(&Concat {}),
            Self::Cons => Applicable::<T>::arity(&Cons {}),
            Self::ConstructHashMap => Applicable::<T>::arity(&ConstructHashMap {}),
            Self::ConstructHashSet => Applicable::<T>::arity(&ConstructHashSet {}),
            Self::ConstructStruct => Applicable::<T>::arity(&ConstructStruct {}),
            Self::ConstructTuple => Applicable::<T>::arity(&ConstructTuple {}),
            Self::ConstructVector => Applicable::<T>::arity(&ConstructVector {}),
            Self::Contains => Applicable::<T>::arity(&Contains {}),
            Self::Divide => Applicable::<T>::arity(&Divide {}),
            Self::Effect => Applicable::<T>::arity(&Effect {}),
            Self::EndsWith => Applicable::<T>::arity(&EndsWith {}),
            Self::Entries => Applicable::<T>::arity(&Entries {}),
            Self::Eq => Applicable::<T>::arity(&Eq {}),
            Self::Equal => Applicable::<T>::arity(&Equal {}),
            Self::Filter => Applicable::<T>::arity(&Filter {}),
            Self::Floor => Applicable::<T>::arity(&Floor {}),
            Self::Get => Applicable::<T>::arity(&Get {}),
            Self::Gt => Applicable::<T>::arity(&Gt {}),
            Self::Gte => Applicable::<T>::arity(&Gte {}),
            Self::If => Applicable::<T>::arity(&If {}),
            Self::IfError => Applicable::<T>::arity(&IfError {}),
            Self::IfPending => Applicable::<T>::arity(&IfPending {}),
            Self::Insert => Applicable::<T>::arity(&Insert {}),
            Self::Keys => Applicable::<T>::arity(&Keys {}),
            Self::Lt => Applicable::<T>::arity(&Lt {}),
            Self::Lte => Applicable::<T>::arity(&Lte {}),
            Self::Map => Applicable::<T>::arity(&Map {}),
            Self::Match => Applicable::<T>::arity(&Match {}),
            Self::Max => Applicable::<T>::arity(&Max {}),
            Self::Merge => Applicable::<T>::arity(&Merge {}),
            Self::Min => Applicable::<T>::arity(&Min {}),
            Self::Multiply => Applicable::<T>::arity(&Multiply {}),
            Self::Not => Applicable::<T>::arity(&Not {}),
            Self::Or => Applicable::<T>::arity(&Or {}),
            Self::Pow => Applicable::<T>::arity(&Pow {}),
            Self::Push => Applicable::<T>::arity(&Push {}),
            Self::PushFront => Applicable::<T>::arity(&PushFront {}),
            Self::Reduce => Applicable::<T>::arity(&Reduce {}),
            Self::Remainder => Applicable::<T>::arity(&Remainder {}),
            Self::Replace => Applicable::<T>::arity(&Replace {}),
            Self::ResolveDeep => Applicable::<T>::arity(&ResolveDeep {}),
            Self::ResolveHashMap => Applicable::<T>::arity(&ResolveHashMap {}),
            Self::ResolveHashSet => Applicable::<T>::arity(&ResolveHashSet {}),
            Self::ResolveShallow => Applicable::<T>::arity(&ResolveShallow {}),
            Self::ResolveStruct => Applicable::<T>::arity(&ResolveStruct {}),
            Self::ResolveTuple => Applicable::<T>::arity(&ResolveTuple {}),
            Self::ResolveVector => Applicable::<T>::arity(&ResolveVector {}),
            Self::Round => Applicable::<T>::arity(&Round {}),
            Self::Sequence => Applicable::<T>::arity(&Sequence {}),
            Self::Slice => Applicable::<T>::arity(&Slice {}),
            Self::Split => Applicable::<T>::arity(&Split {}),
            Self::StartsWith => Applicable::<T>::arity(&StartsWith {}),
            Self::Subtract => Applicable::<T>::arity(&Subtract {}),
            Self::Values => Applicable::<T>::arity(&Values {}),
        }
    }
    pub fn apply<T: Expression + Applicable<T>>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>
    where
        T::Builtin: From<Self>,
    {
        match self {
            Self::Abs => Applicable::<T>::apply(&Abs {}, args, factory, allocator, cache),
            Self::Add => Applicable::<T>::apply(&Add {}, args, factory, allocator, cache),
            Self::And => Applicable::<T>::apply(&And {}, args, factory, allocator, cache),
            Self::Append => Applicable::<T>::apply(&Append {}, args, factory, allocator, cache),
            Self::Apply => Applicable::<T>::apply(&Apply {}, args, factory, allocator, cache),
            Self::Car => Applicable::<T>::apply(&Car {}, args, factory, allocator, cache),
            Self::Cdr => Applicable::<T>::apply(&Cdr {}, args, factory, allocator, cache),
            Self::Ceil => Applicable::<T>::apply(&Ceil {}, args, factory, allocator, cache),
            Self::Collect => Applicable::<T>::apply(&Collect {}, args, factory, allocator, cache),
            Self::CollectFilterResults => {
                Applicable::<T>::apply(&CollectFilterResults {}, args, factory, allocator, cache)
            }
            Self::CollectHashMap => {
                Applicable::<T>::apply(&CollectHashMap {}, args, factory, allocator, cache)
            }
            Self::CollectHashSet => {
                Applicable::<T>::apply(&CollectHashSet {}, args, factory, allocator, cache)
            }
            Self::CollectStruct => {
                Applicable::<T>::apply(&CollectStruct {}, args, factory, allocator, cache)
            }
            Self::CollectTuple => {
                Applicable::<T>::apply(&CollectTuple {}, args, factory, allocator, cache)
            }
            Self::CollectVector => {
                Applicable::<T>::apply(&CollectVector {}, args, factory, allocator, cache)
            }
            Self::Concat => Applicable::<T>::apply(&Concat {}, args, factory, allocator, cache),
            Self::Cons => Applicable::<T>::apply(&Cons {}, args, factory, allocator, cache),
            Self::ConstructHashMap => {
                Applicable::<T>::apply(&ConstructHashMap {}, args, factory, allocator, cache)
            }
            Self::ConstructHashSet => {
                Applicable::<T>::apply(&ConstructHashSet {}, args, factory, allocator, cache)
            }
            Self::ConstructStruct => {
                Applicable::<T>::apply(&ConstructStruct {}, args, factory, allocator, cache)
            }
            Self::ConstructTuple => {
                Applicable::<T>::apply(&ConstructTuple {}, args, factory, allocator, cache)
            }
            Self::ConstructVector => {
                Applicable::<T>::apply(&ConstructVector {}, args, factory, allocator, cache)
            }
            Self::Contains => Applicable::<T>::apply(&Contains {}, args, factory, allocator, cache),
            Self::Divide => Applicable::<T>::apply(&Divide {}, args, factory, allocator, cache),
            Self::Effect => Applicable::<T>::apply(&Effect {}, args, factory, allocator, cache),
            Self::EndsWith => Applicable::<T>::apply(&EndsWith {}, args, factory, allocator, cache),
            Self::Entries => Applicable::<T>::apply(&Entries {}, args, factory, allocator, cache),
            Self::Eq => Applicable::<T>::apply(&Eq {}, args, factory, allocator, cache),
            Self::Equal => Applicable::<T>::apply(&Equal {}, args, factory, allocator, cache),
            Self::Filter => Applicable::<T>::apply(&Filter {}, args, factory, allocator, cache),
            Self::Floor => Applicable::<T>::apply(&Floor {}, args, factory, allocator, cache),
            Self::Get => Applicable::<T>::apply(&Get {}, args, factory, allocator, cache),
            Self::Gt => Applicable::<T>::apply(&Gt {}, args, factory, allocator, cache),
            Self::Gte => Applicable::<T>::apply(&Gte {}, args, factory, allocator, cache),
            Self::If => Applicable::<T>::apply(&If {}, args, factory, allocator, cache),
            Self::IfError => Applicable::<T>::apply(&IfError {}, args, factory, allocator, cache),
            Self::IfPending => {
                Applicable::<T>::apply(&IfPending {}, args, factory, allocator, cache)
            }
            Self::Insert => Applicable::<T>::apply(&Insert {}, args, factory, allocator, cache),
            Self::Keys => Applicable::<T>::apply(&Keys {}, args, factory, allocator, cache),
            Self::Lt => Applicable::<T>::apply(&Lt {}, args, factory, allocator, cache),
            Self::Lte => Applicable::<T>::apply(&Lte {}, args, factory, allocator, cache),
            Self::Map => Applicable::<T>::apply(&Map {}, args, factory, allocator, cache),
            Self::Match => Applicable::<T>::apply(&Match {}, args, factory, allocator, cache),
            Self::Max => Applicable::<T>::apply(&Max {}, args, factory, allocator, cache),
            Self::Merge => Applicable::<T>::apply(&Merge {}, args, factory, allocator, cache),
            Self::Min => Applicable::<T>::apply(&Min {}, args, factory, allocator, cache),
            Self::Multiply => Applicable::<T>::apply(&Multiply {}, args, factory, allocator, cache),
            Self::Not => Applicable::<T>::apply(&Not {}, args, factory, allocator, cache),
            Self::Or => Applicable::<T>::apply(&Or {}, args, factory, allocator, cache),
            Self::Pow => Applicable::<T>::apply(&Pow {}, args, factory, allocator, cache),
            Self::Push => Applicable::<T>::apply(&Push {}, args, factory, allocator, cache),
            Self::PushFront => {
                Applicable::<T>::apply(&PushFront {}, args, factory, allocator, cache)
            }
            Self::Reduce => Applicable::<T>::apply(&Reduce {}, args, factory, allocator, cache),
            Self::Remainder => {
                Applicable::<T>::apply(&Remainder {}, args, factory, allocator, cache)
            }
            Self::Replace => Applicable::<T>::apply(&Replace {}, args, factory, allocator, cache),
            Self::ResolveDeep => {
                Applicable::<T>::apply(&ResolveDeep {}, args, factory, allocator, cache)
            }
            Self::ResolveHashMap => {
                Applicable::<T>::apply(&ResolveHashMap {}, args, factory, allocator, cache)
            }
            Self::ResolveHashSet => {
                Applicable::<T>::apply(&ResolveHashSet {}, args, factory, allocator, cache)
            }
            Self::ResolveShallow => {
                Applicable::<T>::apply(&ResolveShallow {}, args, factory, allocator, cache)
            }
            Self::ResolveStruct => {
                Applicable::<T>::apply(&ResolveStruct {}, args, factory, allocator, cache)
            }
            Self::ResolveTuple => {
                Applicable::<T>::apply(&ResolveTuple {}, args, factory, allocator, cache)
            }
            Self::ResolveVector => {
                Applicable::<T>::apply(&ResolveVector {}, args, factory, allocator, cache)
            }
            Self::Round => Applicable::<T>::apply(&Round {}, args, factory, allocator, cache),
            Self::Sequence => Applicable::<T>::apply(&Sequence {}, args, factory, allocator, cache),
            Self::Slice => Applicable::<T>::apply(&Slice {}, args, factory, allocator, cache),
            Self::Split => Applicable::<T>::apply(&Split {}, args, factory, allocator, cache),
            Self::StartsWith => {
                Applicable::<T>::apply(&StartsWith {}, args, factory, allocator, cache)
            }
            Self::Subtract => Applicable::<T>::apply(&Subtract {}, args, factory, allocator, cache),
            Self::Values => Applicable::<T>::apply(&Values {}, args, factory, allocator, cache),
        }
    }
}
impl Builtin for Stdlib {
    fn arity<T: Expression<Builtin = Self> + Applicable<T>>(&self) -> Option<Arity> {
        self.arity::<T>()
    }
    fn apply<T: Expression<Builtin = Self> + Applicable<T>>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        self.apply(args, factory, allocator, cache)
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<stdlib:{:?}>", self)
    }
}