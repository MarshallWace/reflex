// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::convert::TryFrom;

use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};

pub use abs::*;
pub use add::*;
pub use and::*;
pub use apply::*;
pub use car::*;
pub use cdr::*;
pub use ceil::*;
pub use chain::*;
pub use collect::*;
pub use concat::*;
pub use cons::*;
pub use construct::*;
pub use contains::*;
pub use divide::*;
pub use effect::*;
pub use ends_with::*;
pub use eq::*;
pub use equal::*;
pub use filter::*;
pub use flatten::*;
pub use floor::*;
pub use get::*;
pub use gt::*;
pub use gte::*;
pub use hash::*;
pub use if_error::*;
pub use if_pending::*;
pub use insert::*;
pub use keys::*;
pub use length::*;
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
pub use reduce::*;
pub use remainder::*;
pub use replace::*;
pub use resolve::*;
pub use resolve_args::*;
pub use resolve_deep::*;
pub use resolve_shallow::*;
pub use round::*;
pub use sequence::*;
pub use slice::*;
pub use split::*;
pub use starts_with::*;
pub use subtract::*;
pub use unzip::*;
pub use values::*;
pub use zip::*;

mod abs;
mod add;
mod and;
mod apply;
mod car;
mod cdr;
mod ceil;
mod chain;
mod collect;
mod concat;
mod cons;
mod construct;
mod contains;
mod divide;
mod effect;
mod ends_with;
mod eq;
mod equal;
mod filter;
mod flatten;
mod floor;
mod get;
mod gt;
mod gte;
mod hash;
mod r#if;
mod if_error;
mod if_pending;
mod insert;
mod keys;
mod length;
mod lt;
mod lte;
mod map;
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
mod resolve_args;
mod resolve_deep;
mod resolve_shallow;
mod round;
mod sequence;
mod slice;
mod split;
mod starts_with;
mod subtract;
mod unzip;
mod values;
mod zip;

pub trait StdlibBuiltin:
    Builtin
    + From<Apply>
    + From<CollectHashMap>
    + From<CollectHashSet>
    + From<CollectList>
    + From<ConstructHashMap>
    + From<Flatten>
    + From<Get>
    + From<If>
    + From<Map>
    + From<ResolveDeep>
    + From<ResolveList>
    + From<Sequence>
{
}
impl<T> StdlibBuiltin for T where
    T: Builtin
        + From<Apply>
        + From<CollectHashMap>
        + From<CollectHashSet>
        + From<CollectList>
        + From<ConstructHashMap>
        + From<Flatten>
        + From<Get>
        + From<If>
        + From<Map>
        + From<ResolveDeep>
        + From<ResolveList>
        + From<Sequence>
{
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "uid")]
pub enum Stdlib {
    Abs,
    Add,
    And,
    Apply,
    Car,
    Cdr,
    Ceil,
    Chain,
    CollectHashMap,
    CollectHashSet,
    CollectList,
    Concat,
    Cons,
    ConstructHashMap,
    ConstructHashSet,
    ConstructRecord,
    ConstructList,
    Contains,
    Divide,
    Effect,
    EndsWith,
    Eq,
    Equal,
    Filter,
    Flatten,
    Floor,
    Get,
    Gt,
    Gte,
    Hash,
    If,
    IfError,
    IfPending,
    Insert,
    Keys,
    Length,
    Lt,
    Lte,
    Map,
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
    ResolveArgs,
    ResolveDeep,
    ResolveHashMap,
    ResolveHashSet,
    ResolveShallow,
    ResolveRecord,
    ResolveList,
    Round,
    Sequence,
    Slice,
    Split,
    StartsWith,
    Subtract,
    Unzip,
    Values,
    Zip,
}
impl Stdlib {
    pub fn entries() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}
impl TryFrom<Uuid> for Stdlib {
    type Error = ();
    fn try_from(uuid: Uuid) -> Result<Self, Self::Error> {
        match uuid {
            Abs::UUID => Ok(Self::Abs),
            Add::UUID => Ok(Self::Add),
            And::UUID => Ok(Self::And),
            Apply::UUID => Ok(Self::Apply),
            Car::UUID => Ok(Self::Car),
            Cdr::UUID => Ok(Self::Cdr),
            Ceil::UUID => Ok(Self::Ceil),
            Chain::UUID => Ok(Self::Chain),
            CollectHashMap::UUID => Ok(Self::CollectHashMap),
            CollectHashSet::UUID => Ok(Self::CollectHashSet),
            CollectList::UUID => Ok(Self::CollectList),
            Concat::UUID => Ok(Self::Concat),
            Cons::UUID => Ok(Self::Cons),
            ConstructHashMap::UUID => Ok(Self::ConstructHashMap),
            ConstructHashSet::UUID => Ok(Self::ConstructHashSet),
            ConstructRecord::UUID => Ok(Self::ConstructRecord),
            ConstructList::UUID => Ok(Self::ConstructList),
            Contains::UUID => Ok(Self::Contains),
            Divide::UUID => Ok(Self::Divide),
            Effect::UUID => Ok(Self::Effect),
            EndsWith::UUID => Ok(Self::EndsWith),
            Eq::UUID => Ok(Self::Eq),
            Equal::UUID => Ok(Self::Equal),
            Filter::UUID => Ok(Self::Filter),
            Flatten::UUID => Ok(Self::Flatten),
            Floor::UUID => Ok(Self::Floor),
            Get::UUID => Ok(Self::Get),
            Gt::UUID => Ok(Self::Gt),
            Gte::UUID => Ok(Self::Gte),
            Hash::UUID => Ok(Self::Hash),
            If::UUID => Ok(Self::If),
            IfError::UUID => Ok(Self::IfError),
            IfPending::UUID => Ok(Self::IfPending),
            Insert::UUID => Ok(Self::Insert),
            Keys::UUID => Ok(Self::Keys),
            Length::UUID => Ok(Self::Length),
            Lt::UUID => Ok(Self::Lt),
            Lte::UUID => Ok(Self::Lte),
            Map::UUID => Ok(Self::Map),
            Max::UUID => Ok(Self::Max),
            Merge::UUID => Ok(Self::Merge),
            Min::UUID => Ok(Self::Min),
            Multiply::UUID => Ok(Self::Multiply),
            Not::UUID => Ok(Self::Not),
            Or::UUID => Ok(Self::Or),
            Pow::UUID => Ok(Self::Pow),
            Push::UUID => Ok(Self::Push),
            PushFront::UUID => Ok(Self::PushFront),
            Reduce::UUID => Ok(Self::Reduce),
            Remainder::UUID => Ok(Self::Remainder),
            Replace::UUID => Ok(Self::Replace),
            ResolveArgs::UUID => Ok(Self::ResolveArgs),
            ResolveDeep::UUID => Ok(Self::ResolveDeep),
            ResolveHashMap::UUID => Ok(Self::ResolveHashMap),
            ResolveHashSet::UUID => Ok(Self::ResolveHashSet),
            ResolveShallow::UUID => Ok(Self::ResolveShallow),
            ResolveRecord::UUID => Ok(Self::ResolveRecord),
            ResolveList::UUID => Ok(Self::ResolveList),
            Round::UUID => Ok(Self::Round),
            Sequence::UUID => Ok(Self::Sequence),
            Slice::UUID => Ok(Self::Slice),
            Split::UUID => Ok(Self::Split),
            StartsWith::UUID => Ok(Self::StartsWith),
            Subtract::UUID => Ok(Self::Subtract),
            Unzip::UUID => Ok(Self::Unzip),
            Values::UUID => Ok(Self::Values),
            Zip::UUID => Ok(Self::Zip),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::Abs => Uid::uid(&Abs {}),
            Self::Add => Uid::uid(&Add {}),
            Self::And => Uid::uid(&And {}),
            Self::Apply => Uid::uid(&Apply {}),
            Self::Car => Uid::uid(&Car {}),
            Self::Cdr => Uid::uid(&Cdr {}),
            Self::Ceil => Uid::uid(&Ceil {}),
            Self::Chain => Uid::uid(&Chain {}),
            Self::CollectHashMap => Uid::uid(&CollectHashMap {}),
            Self::CollectHashSet => Uid::uid(&CollectHashSet {}),
            Self::CollectList => Uid::uid(&CollectList {}),
            Self::Concat => Uid::uid(&Concat {}),
            Self::Cons => Uid::uid(&Cons {}),
            Self::ConstructHashMap => Uid::uid(&ConstructHashMap {}),
            Self::ConstructHashSet => Uid::uid(&ConstructHashSet {}),
            Self::ConstructRecord => Uid::uid(&ConstructRecord {}),
            Self::ConstructList => Uid::uid(&ConstructList {}),
            Self::Contains => Uid::uid(&Contains {}),
            Self::Divide => Uid::uid(&Divide {}),
            Self::Effect => Uid::uid(&Effect {}),
            Self::EndsWith => Uid::uid(&EndsWith {}),
            Self::Eq => Uid::uid(&Eq {}),
            Self::Equal => Uid::uid(&Equal {}),
            Self::Filter => Uid::uid(&Filter {}),
            Self::Flatten => Uid::uid(&Flatten {}),
            Self::Floor => Uid::uid(&Floor {}),
            Self::Get => Uid::uid(&Get {}),
            Self::Gt => Uid::uid(&Gt {}),
            Self::Gte => Uid::uid(&Gte {}),
            Self::Hash => Uid::uid(&Hash {}),
            Self::If => Uid::uid(&If {}),
            Self::IfError => Uid::uid(&IfError {}),
            Self::IfPending => Uid::uid(&IfPending {}),
            Self::Insert => Uid::uid(&Insert {}),
            Self::Keys => Uid::uid(&Keys {}),
            Self::Length => Uid::uid(&Length {}),
            Self::Lt => Uid::uid(&Lt {}),
            Self::Lte => Uid::uid(&Lte {}),
            Self::Map => Uid::uid(&Map {}),
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
            Self::ResolveArgs => Uid::uid(&ResolveArgs {}),
            Self::ResolveDeep => Uid::uid(&ResolveDeep {}),
            Self::ResolveHashMap => Uid::uid(&ResolveHashMap {}),
            Self::ResolveHashSet => Uid::uid(&ResolveHashSet {}),
            Self::ResolveShallow => Uid::uid(&ResolveShallow {}),
            Self::ResolveRecord => Uid::uid(&ResolveRecord {}),
            Self::ResolveList => Uid::uid(&ResolveList {}),
            Self::Round => Uid::uid(&Round {}),
            Self::Sequence => Uid::uid(&Sequence {}),
            Self::Slice => Uid::uid(&Slice {}),
            Self::Split => Uid::uid(&Split {}),
            Self::StartsWith => Uid::uid(&StartsWith {}),
            Self::Subtract => Uid::uid(&Subtract {}),
            Self::Unzip => Uid::uid(&Unzip {}),
            Self::Values => Uid::uid(&Values {}),
            Self::Zip => Uid::uid(&Zip {}),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Abs => Abs::arity(),
            Self::Add => Add::arity(),
            Self::And => And::arity(),
            Self::Apply => Apply::arity(),
            Self::Car => Car::arity(),
            Self::Cdr => Cdr::arity(),
            Self::Ceil => Ceil::arity(),
            Self::Chain => Chain::arity(),
            Self::CollectHashMap => CollectHashMap::arity(),
            Self::CollectHashSet => CollectHashSet::arity(),
            Self::CollectList => CollectList::arity(),
            Self::Concat => Concat::arity(),
            Self::Cons => Cons::arity(),
            Self::ConstructHashMap => ConstructHashMap::arity(),
            Self::ConstructHashSet => ConstructHashSet::arity(),
            Self::ConstructRecord => ConstructRecord::arity(),
            Self::ConstructList => ConstructList::arity(),
            Self::Contains => Contains::arity(),
            Self::Divide => Divide::arity(),
            Self::Effect => Effect::arity(),
            Self::EndsWith => EndsWith::arity(),
            Self::Eq => Eq::arity(),
            Self::Equal => Equal::arity(),
            Self::Filter => Filter::arity(),
            Self::Flatten => Flatten::arity(),
            Self::Floor => Floor::arity(),
            Self::Get => Get::arity(),
            Self::Gt => Gt::arity(),
            Self::Gte => Gte::arity(),
            Self::Hash => Hash::arity(),
            Self::If => If::arity(),
            Self::IfError => IfError::arity(),
            Self::IfPending => IfPending::arity(),
            Self::Insert => Insert::arity(),
            Self::Keys => Keys::arity(),
            Self::Length => Length::arity(),
            Self::Lt => Lt::arity(),
            Self::Lte => Lte::arity(),
            Self::Map => Map::arity(),
            Self::Max => Max::arity(),
            Self::Merge => Merge::arity(),
            Self::Min => Min::arity(),
            Self::Multiply => Multiply::arity(),
            Self::Not => Not::arity(),
            Self::Or => Or::arity(),
            Self::Pow => Pow::arity(),
            Self::Push => Push::arity(),
            Self::PushFront => PushFront::arity(),
            Self::Reduce => Reduce::arity(),
            Self::Remainder => Remainder::arity(),
            Self::Replace => Replace::arity(),
            Self::ResolveArgs => ResolveArgs::arity(),
            Self::ResolveDeep => ResolveDeep::arity(),
            Self::ResolveHashMap => ResolveHashMap::arity(),
            Self::ResolveHashSet => ResolveHashSet::arity(),
            Self::ResolveShallow => ResolveShallow::arity(),
            Self::ResolveRecord => ResolveRecord::arity(),
            Self::ResolveList => ResolveList::arity(),
            Self::Round => Round::arity(),
            Self::Sequence => Sequence::arity(),
            Self::Slice => Slice::arity(),
            Self::Split => Split::arity(),
            Self::StartsWith => StartsWith::arity(),
            Self::Subtract => Subtract::arity(),
            Self::Unzip => Unzip::arity(),
            Self::Values => Values::arity(),
            Self::Zip => Zip::arity(),
        }
    }
    pub fn apply<T: Expression>(
        &self,
        args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String>
    where
        T::Builtin: StdlibBuiltin,
    {
        match self {
            Self::Abs => Applicable::<T>::apply(&Abs, args, factory, allocator, cache),
            Self::Add => Applicable::<T>::apply(&Add, args, factory, allocator, cache),
            Self::And => Applicable::<T>::apply(&And, args, factory, allocator, cache),
            Self::Apply => Applicable::<T>::apply(&Apply, args, factory, allocator, cache),
            Self::Car => Applicable::<T>::apply(&Car, args, factory, allocator, cache),
            Self::Cdr => Applicable::<T>::apply(&Cdr, args, factory, allocator, cache),
            Self::Ceil => Applicable::<T>::apply(&Ceil, args, factory, allocator, cache),
            Self::Chain => Applicable::<T>::apply(&Chain, args, factory, allocator, cache),
            Self::CollectHashMap => {
                Applicable::<T>::apply(&CollectHashMap, args, factory, allocator, cache)
            }
            Self::CollectHashSet => {
                Applicable::<T>::apply(&CollectHashSet, args, factory, allocator, cache)
            }
            Self::CollectList => {
                Applicable::<T>::apply(&CollectList, args, factory, allocator, cache)
            }
            Self::Concat => Applicable::<T>::apply(&Concat, args, factory, allocator, cache),
            Self::Cons => Applicable::<T>::apply(&Cons, args, factory, allocator, cache),
            Self::ConstructHashMap => {
                Applicable::<T>::apply(&ConstructHashMap, args, factory, allocator, cache)
            }
            Self::ConstructHashSet => {
                Applicable::<T>::apply(&ConstructHashSet, args, factory, allocator, cache)
            }
            Self::ConstructRecord => {
                Applicable::<T>::apply(&ConstructRecord, args, factory, allocator, cache)
            }
            Self::ConstructList => {
                Applicable::<T>::apply(&ConstructList, args, factory, allocator, cache)
            }
            Self::Contains => Applicable::<T>::apply(&Contains, args, factory, allocator, cache),
            Self::Divide => Applicable::<T>::apply(&Divide, args, factory, allocator, cache),
            Self::Effect => Applicable::<T>::apply(&Effect, args, factory, allocator, cache),
            Self::EndsWith => Applicable::<T>::apply(&EndsWith, args, factory, allocator, cache),
            Self::Eq => Applicable::<T>::apply(&Eq, args, factory, allocator, cache),
            Self::Equal => Applicable::<T>::apply(&Equal, args, factory, allocator, cache),
            Self::Filter => Applicable::<T>::apply(&Filter, args, factory, allocator, cache),
            Self::Flatten => Applicable::<T>::apply(&Flatten, args, factory, allocator, cache),
            Self::Floor => Applicable::<T>::apply(&Floor, args, factory, allocator, cache),
            Self::Get => Applicable::<T>::apply(&Get, args, factory, allocator, cache),
            Self::Gt => Applicable::<T>::apply(&Gt, args, factory, allocator, cache),
            Self::Gte => Applicable::<T>::apply(&Gte, args, factory, allocator, cache),
            Self::Hash => Applicable::<T>::apply(&Hash, args, factory, allocator, cache),
            Self::If => Applicable::<T>::apply(&If, args, factory, allocator, cache),
            Self::IfError => Applicable::<T>::apply(&IfError, args, factory, allocator, cache),
            Self::IfPending => Applicable::<T>::apply(&IfPending, args, factory, allocator, cache),
            Self::Insert => Applicable::<T>::apply(&Insert, args, factory, allocator, cache),
            Self::Keys => Applicable::<T>::apply(&Keys, args, factory, allocator, cache),
            Self::Length => Applicable::<T>::apply(&Length, args, factory, allocator, cache),
            Self::Lt => Applicable::<T>::apply(&Lt, args, factory, allocator, cache),
            Self::Lte => Applicable::<T>::apply(&Lte, args, factory, allocator, cache),
            Self::Map => Applicable::<T>::apply(&Map, args, factory, allocator, cache),
            Self::Max => Applicable::<T>::apply(&Max, args, factory, allocator, cache),
            Self::Merge => Applicable::<T>::apply(&Merge, args, factory, allocator, cache),
            Self::Min => Applicable::<T>::apply(&Min, args, factory, allocator, cache),
            Self::Multiply => Applicable::<T>::apply(&Multiply, args, factory, allocator, cache),
            Self::Not => Applicable::<T>::apply(&Not, args, factory, allocator, cache),
            Self::Or => Applicable::<T>::apply(&Or, args, factory, allocator, cache),
            Self::Pow => Applicable::<T>::apply(&Pow, args, factory, allocator, cache),
            Self::Push => Applicable::<T>::apply(&Push, args, factory, allocator, cache),
            Self::PushFront => Applicable::<T>::apply(&PushFront, args, factory, allocator, cache),
            Self::Reduce => Applicable::<T>::apply(&Reduce, args, factory, allocator, cache),
            Self::Remainder => Applicable::<T>::apply(&Remainder, args, factory, allocator, cache),
            Self::Replace => Applicable::<T>::apply(&Replace, args, factory, allocator, cache),
            Self::ResolveArgs => {
                Applicable::<T>::apply(&ResolveArgs, args, factory, allocator, cache)
            }
            Self::ResolveDeep => {
                Applicable::<T>::apply(&ResolveDeep, args, factory, allocator, cache)
            }
            Self::ResolveHashMap => {
                Applicable::<T>::apply(&ResolveHashMap, args, factory, allocator, cache)
            }
            Self::ResolveHashSet => {
                Applicable::<T>::apply(&ResolveHashSet, args, factory, allocator, cache)
            }
            Self::ResolveShallow => {
                Applicable::<T>::apply(&ResolveShallow, args, factory, allocator, cache)
            }
            Self::ResolveRecord => {
                Applicable::<T>::apply(&ResolveRecord, args, factory, allocator, cache)
            }
            Self::ResolveList => {
                Applicable::<T>::apply(&ResolveList, args, factory, allocator, cache)
            }
            Self::Round => Applicable::<T>::apply(&Round, args, factory, allocator, cache),
            Self::Sequence => Applicable::<T>::apply(&Sequence, args, factory, allocator, cache),
            Self::Slice => Applicable::<T>::apply(&Slice, args, factory, allocator, cache),
            Self::Split => Applicable::<T>::apply(&Split, args, factory, allocator, cache),
            Self::StartsWith => {
                Applicable::<T>::apply(&StartsWith, args, factory, allocator, cache)
            }
            Self::Subtract => Applicable::<T>::apply(&Subtract, args, factory, allocator, cache),
            Self::Unzip => Applicable::<T>::apply(&Unzip, args, factory, allocator, cache),
            Self::Values => Applicable::<T>::apply(&Values, args, factory, allocator, cache),
            Self::Zip => Applicable::<T>::apply(&Zip, args, factory, allocator, cache),
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: StdlibBuiltin,
    {
        match self {
            Self::Abs => Applicable::<T>::should_parallelize(&Abs, args),
            Self::Add => Applicable::<T>::should_parallelize(&Add, args),
            Self::And => Applicable::<T>::should_parallelize(&And, args),
            Self::Apply => Applicable::<T>::should_parallelize(&Apply, args),
            Self::Car => Applicable::<T>::should_parallelize(&Car, args),
            Self::Cdr => Applicable::<T>::should_parallelize(&Cdr, args),
            Self::Ceil => Applicable::<T>::should_parallelize(&Ceil, args),
            Self::Chain => Applicable::<T>::should_parallelize(&Chain, args),
            Self::CollectHashMap => Applicable::<T>::should_parallelize(&CollectHashMap, args),
            Self::CollectHashSet => Applicable::<T>::should_parallelize(&CollectHashSet, args),
            Self::CollectList => Applicable::<T>::should_parallelize(&CollectList, args),
            Self::Concat => Applicable::<T>::should_parallelize(&Concat, args),
            Self::Cons => Applicable::<T>::should_parallelize(&Cons, args),
            Self::ConstructHashMap => Applicable::<T>::should_parallelize(&ConstructHashMap, args),
            Self::ConstructHashSet => Applicable::<T>::should_parallelize(&ConstructHashSet, args),
            Self::ConstructRecord => Applicable::<T>::should_parallelize(&ConstructRecord, args),
            Self::ConstructList => Applicable::<T>::should_parallelize(&ConstructList, args),
            Self::Contains => Applicable::<T>::should_parallelize(&Contains, args),
            Self::Divide => Applicable::<T>::should_parallelize(&Divide, args),
            Self::Effect => Applicable::<T>::should_parallelize(&Effect, args),
            Self::EndsWith => Applicable::<T>::should_parallelize(&EndsWith, args),
            Self::Eq => Applicable::<T>::should_parallelize(&Eq, args),
            Self::Equal => Applicable::<T>::should_parallelize(&Equal, args),
            Self::Filter => Applicable::<T>::should_parallelize(&Filter, args),
            Self::Flatten => Applicable::<T>::should_parallelize(&Flatten, args),
            Self::Floor => Applicable::<T>::should_parallelize(&Floor, args),
            Self::Get => Applicable::<T>::should_parallelize(&Get, args),
            Self::Gt => Applicable::<T>::should_parallelize(&Gt, args),
            Self::Gte => Applicable::<T>::should_parallelize(&Gte, args),
            Self::Hash => Applicable::<T>::should_parallelize(&Hash, args),
            Self::If => Applicable::<T>::should_parallelize(&If, args),
            Self::IfError => Applicable::<T>::should_parallelize(&IfError, args),
            Self::IfPending => Applicable::<T>::should_parallelize(&IfPending, args),
            Self::Insert => Applicable::<T>::should_parallelize(&Insert, args),
            Self::Keys => Applicable::<T>::should_parallelize(&Keys, args),
            Self::Length => Applicable::<T>::should_parallelize(&Length, args),
            Self::Lt => Applicable::<T>::should_parallelize(&Lt, args),
            Self::Lte => Applicable::<T>::should_parallelize(&Lte, args),
            Self::Map => Applicable::<T>::should_parallelize(&Map, args),
            Self::Max => Applicable::<T>::should_parallelize(&Max, args),
            Self::Merge => Applicable::<T>::should_parallelize(&Merge, args),
            Self::Min => Applicable::<T>::should_parallelize(&Min, args),
            Self::Multiply => Applicable::<T>::should_parallelize(&Multiply, args),
            Self::Not => Applicable::<T>::should_parallelize(&Not, args),
            Self::Or => Applicable::<T>::should_parallelize(&Or, args),
            Self::Pow => Applicable::<T>::should_parallelize(&Pow, args),
            Self::Push => Applicable::<T>::should_parallelize(&Push, args),
            Self::PushFront => Applicable::<T>::should_parallelize(&PushFront, args),
            Self::Reduce => Applicable::<T>::should_parallelize(&Reduce, args),
            Self::Remainder => Applicable::<T>::should_parallelize(&Remainder, args),
            Self::Replace => Applicable::<T>::should_parallelize(&Replace, args),
            Self::ResolveArgs => Applicable::<T>::should_parallelize(&ResolveArgs, args),
            Self::ResolveDeep => Applicable::<T>::should_parallelize(&ResolveDeep, args),
            Self::ResolveHashMap => Applicable::<T>::should_parallelize(&ResolveHashMap, args),
            Self::ResolveHashSet => Applicable::<T>::should_parallelize(&ResolveHashSet, args),
            Self::ResolveShallow => Applicable::<T>::should_parallelize(&ResolveShallow, args),
            Self::ResolveRecord => Applicable::<T>::should_parallelize(&ResolveRecord, args),
            Self::ResolveList => Applicable::<T>::should_parallelize(&ResolveList, args),
            Self::Round => Applicable::<T>::should_parallelize(&Round, args),
            Self::Sequence => Applicable::<T>::should_parallelize(&Sequence, args),
            Self::Slice => Applicable::<T>::should_parallelize(&Slice, args),
            Self::Split => Applicable::<T>::should_parallelize(&Split, args),
            Self::StartsWith => Applicable::<T>::should_parallelize(&StartsWith, args),
            Self::Subtract => Applicable::<T>::should_parallelize(&Subtract, args),
            Self::Unzip => Applicable::<T>::should_parallelize(&Unzip, args),
            Self::Values => Applicable::<T>::should_parallelize(&Values, args),
            Self::Zip => Applicable::<T>::should_parallelize(&Zip, args),
        }
    }
}
impl Builtin for Stdlib {
    fn arity(&self) -> Arity {
        self.arity()
    }
    fn should_parallelize<T: Expression<Builtin = Self>>(&self, args: &[T]) -> bool {
        self.should_parallelize(args)
    }
    fn apply<T: Expression<Builtin = Self>>(
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

impl From<Abs> for Stdlib {
    fn from(_value: Abs) -> Self {
        Self::Abs
    }
}
impl From<Add> for Stdlib {
    fn from(_value: Add) -> Self {
        Self::Add
    }
}
impl From<And> for Stdlib {
    fn from(_value: And) -> Self {
        Self::And
    }
}
impl From<Apply> for Stdlib {
    fn from(_value: Apply) -> Self {
        Self::Apply
    }
}
impl From<Car> for Stdlib {
    fn from(_value: Car) -> Self {
        Self::Car
    }
}
impl From<Cdr> for Stdlib {
    fn from(_value: Cdr) -> Self {
        Self::Cdr
    }
}
impl From<Ceil> for Stdlib {
    fn from(_value: Ceil) -> Self {
        Self::Ceil
    }
}
impl From<Chain> for Stdlib {
    fn from(_value: Chain) -> Self {
        Self::Chain
    }
}
impl From<CollectHashMap> for Stdlib {
    fn from(_value: CollectHashMap) -> Self {
        Self::CollectHashMap
    }
}
impl From<CollectHashSet> for Stdlib {
    fn from(_value: CollectHashSet) -> Self {
        Self::CollectHashSet
    }
}
impl From<CollectList> for Stdlib {
    fn from(_value: CollectList) -> Self {
        Self::CollectList
    }
}
impl From<Concat> for Stdlib {
    fn from(_value: Concat) -> Self {
        Self::Concat
    }
}
impl From<Cons> for Stdlib {
    fn from(_value: Cons) -> Self {
        Self::Cons
    }
}
impl From<ConstructHashMap> for Stdlib {
    fn from(_value: ConstructHashMap) -> Self {
        Self::ConstructHashMap
    }
}
impl From<ConstructHashSet> for Stdlib {
    fn from(_value: ConstructHashSet) -> Self {
        Self::ConstructHashSet
    }
}
impl From<ConstructRecord> for Stdlib {
    fn from(_value: ConstructRecord) -> Self {
        Self::ConstructRecord
    }
}
impl From<ConstructList> for Stdlib {
    fn from(_value: ConstructList) -> Self {
        Self::ConstructList
    }
}
impl From<Contains> for Stdlib {
    fn from(_value: Contains) -> Self {
        Self::Contains
    }
}
impl From<Divide> for Stdlib {
    fn from(_value: Divide) -> Self {
        Self::Divide
    }
}
impl From<Effect> for Stdlib {
    fn from(_value: Effect) -> Self {
        Self::Effect
    }
}
impl From<EndsWith> for Stdlib {
    fn from(_value: EndsWith) -> Self {
        Self::EndsWith
    }
}
impl From<Eq> for Stdlib {
    fn from(_value: Eq) -> Self {
        Self::Eq
    }
}
impl From<Equal> for Stdlib {
    fn from(_value: Equal) -> Self {
        Self::Equal
    }
}
impl From<Filter> for Stdlib {
    fn from(_value: Filter) -> Self {
        Self::Filter
    }
}
impl From<Flatten> for Stdlib {
    fn from(_value: Flatten) -> Self {
        Self::Flatten
    }
}
impl From<Floor> for Stdlib {
    fn from(_value: Floor) -> Self {
        Self::Floor
    }
}
impl From<Get> for Stdlib {
    fn from(_value: Get) -> Self {
        Self::Get
    }
}
impl From<Gt> for Stdlib {
    fn from(_value: Gt) -> Self {
        Self::Gt
    }
}
impl From<Gte> for Stdlib {
    fn from(_value: Gte) -> Self {
        Self::Gte
    }
}
impl From<Hash> for Stdlib {
    fn from(_value: Hash) -> Self {
        Self::Hash
    }
}
impl From<If> for Stdlib {
    fn from(_value: If) -> Self {
        Self::If
    }
}
impl From<IfError> for Stdlib {
    fn from(_value: IfError) -> Self {
        Self::IfError
    }
}
impl From<IfPending> for Stdlib {
    fn from(_value: IfPending) -> Self {
        Self::IfPending
    }
}
impl From<Insert> for Stdlib {
    fn from(_value: Insert) -> Self {
        Self::Insert
    }
}
impl From<Keys> for Stdlib {
    fn from(_value: Keys) -> Self {
        Self::Keys
    }
}
impl From<Length> for Stdlib {
    fn from(_value: Length) -> Self {
        Self::Length
    }
}
impl From<Lt> for Stdlib {
    fn from(_value: Lt) -> Self {
        Self::Lt
    }
}
impl From<Lte> for Stdlib {
    fn from(_value: Lte) -> Self {
        Self::Lte
    }
}
impl From<Map> for Stdlib {
    fn from(_value: Map) -> Self {
        Self::Map
    }
}
impl From<Max> for Stdlib {
    fn from(_value: Max) -> Self {
        Self::Max
    }
}
impl From<Merge> for Stdlib {
    fn from(_value: Merge) -> Self {
        Self::Merge
    }
}
impl From<Min> for Stdlib {
    fn from(_value: Min) -> Self {
        Self::Min
    }
}
impl From<Multiply> for Stdlib {
    fn from(_value: Multiply) -> Self {
        Self::Multiply
    }
}
impl From<Not> for Stdlib {
    fn from(_value: Not) -> Self {
        Self::Not
    }
}
impl From<Or> for Stdlib {
    fn from(_value: Or) -> Self {
        Self::Or
    }
}
impl From<Pow> for Stdlib {
    fn from(_value: Pow) -> Self {
        Self::Pow
    }
}
impl From<Push> for Stdlib {
    fn from(_value: Push) -> Self {
        Self::Push
    }
}
impl From<PushFront> for Stdlib {
    fn from(_value: PushFront) -> Self {
        Self::PushFront
    }
}
impl From<Reduce> for Stdlib {
    fn from(_value: Reduce) -> Self {
        Self::Reduce
    }
}
impl From<Remainder> for Stdlib {
    fn from(_value: Remainder) -> Self {
        Self::Remainder
    }
}
impl From<Replace> for Stdlib {
    fn from(_value: Replace) -> Self {
        Self::Replace
    }
}
impl From<ResolveArgs> for Stdlib {
    fn from(_value: ResolveArgs) -> Self {
        Self::ResolveArgs
    }
}
impl From<ResolveDeep> for Stdlib {
    fn from(_value: ResolveDeep) -> Self {
        Self::ResolveDeep
    }
}
impl From<ResolveHashMap> for Stdlib {
    fn from(_value: ResolveHashMap) -> Self {
        Self::ResolveHashMap
    }
}
impl From<ResolveHashSet> for Stdlib {
    fn from(_value: ResolveHashSet) -> Self {
        Self::ResolveHashSet
    }
}
impl From<ResolveShallow> for Stdlib {
    fn from(_value: ResolveShallow) -> Self {
        Self::ResolveShallow
    }
}
impl From<ResolveRecord> for Stdlib {
    fn from(_value: ResolveRecord) -> Self {
        Self::ResolveRecord
    }
}
impl From<ResolveList> for Stdlib {
    fn from(_value: ResolveList) -> Self {
        Self::ResolveList
    }
}
impl From<Round> for Stdlib {
    fn from(_value: Round) -> Self {
        Self::Round
    }
}
impl From<Sequence> for Stdlib {
    fn from(_value: Sequence) -> Self {
        Self::Sequence
    }
}
impl From<Slice> for Stdlib {
    fn from(_value: Slice) -> Self {
        Self::Slice
    }
}
impl From<Split> for Stdlib {
    fn from(_value: Split) -> Self {
        Self::Split
    }
}
impl From<StartsWith> for Stdlib {
    fn from(_value: StartsWith) -> Self {
        Self::StartsWith
    }
}
impl From<Subtract> for Stdlib {
    fn from(_value: Subtract) -> Self {
        Self::Subtract
    }
}
impl From<Unzip> for Stdlib {
    fn from(_value: Unzip) -> Self {
        Self::Unzip
    }
}
impl From<Values> for Stdlib {
    fn from(_value: Values) -> Self {
        Self::Values
    }
}
impl From<Zip> for Stdlib {
    fn from(_value: Zip) -> Self {
        Self::Zip
    }
}
