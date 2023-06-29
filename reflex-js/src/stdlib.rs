// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use reflex_stdlib::{
    CollectHashSet, CollectList, ConstructHashMap, ConstructRecord, Contains, Entries, Filter,
    Flatten, Get, Insert, Keys, Map, Push, PushFront, Reduce, Replace, ResolveDeep, ResolveShallow,
    Sequence, Slice, Split, Values,
};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use construct::*;
pub use date_constructor::*;
pub use dispatch::*;
pub use encode_uri_component::*;
pub use format_error_message::*;
pub use from_entries::*;
pub use is_finite::*;
pub use log::*;
pub use map_constructor::*;
pub use parse_float::*;
pub use parse_int::*;
pub use set_constructor::*;
pub use struct_type_factory::*;
pub use throw::*;
pub use to_string::*;

mod construct;
mod date_constructor;
mod dispatch;
mod encode_uri_component;
mod format_error_message;
mod from_entries;
mod is_finite;
mod log;
mod map_constructor;
mod parse_float;
mod parse_int;
mod set_constructor;
mod struct_type_factory;
mod throw;
mod to_string;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    Construct,
    DateConstructor,
    Dispatch,
    EncodeUriComponent,
    FormatErrorMessage,
    FromEntries,
    IsFinite,
    Log,
    LogArgs,
    MapConstructor,
    ParseFloat,
    ParseInt,
    SetConstructor,
    StructTypeFactory,
    Throw,
    ToString,
}
impl Stdlib {
    pub fn entries() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::Construct => Uid::uid(&Construct {}),
            Self::DateConstructor => Uid::uid(&DateConstructor {}),
            Self::Dispatch => Uid::uid(&Dispatch {}),
            Self::EncodeUriComponent => Uid::uid(&EncodeUriComponent {}),
            Self::FormatErrorMessage => Uid::uid(&FormatErrorMessage {}),
            Self::FromEntries => Uid::uid(&FromEntries {}),
            Self::IsFinite => Uid::uid(&IsFinite {}),
            Self::Log => Uid::uid(&Log {}),
            Self::LogArgs => Uid::uid(&LogArgs {}),
            Self::MapConstructor => Uid::uid(&MapConstructor {}),
            Self::ParseFloat => Uid::uid(&ParseFloat {}),
            Self::ParseInt => Uid::uid(&ParseInt {}),
            Self::SetConstructor => Uid::uid(&SetConstructor {}),
            Self::StructTypeFactory => Uid::uid(&StructTypeFactory {}),
            Self::Throw => Uid::uid(&Throw {}),
            Self::ToString => Uid::uid(&ToString {}),
        }
    }
}
impl TryFrom<Uuid> for Stdlib {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        match value {
            Construct::UUID => Ok(Self::Construct),
            DateConstructor::UUID => Ok(Self::DateConstructor),
            Dispatch::UUID => Ok(Self::Dispatch),
            EncodeUriComponent::UUID => Ok(Self::EncodeUriComponent),
            FormatErrorMessage::UUID => Ok(Self::FormatErrorMessage),
            FromEntries::UUID => Ok(Self::FromEntries),
            IsFinite::UUID => Ok(Self::IsFinite),
            Log::UUID => Ok(Self::Log),
            LogArgs::UUID => Ok(Self::LogArgs),
            MapConstructor::UUID => Ok(Self::MapConstructor),
            ParseInt::UUID => Ok(Self::ParseInt),
            ParseFloat::UUID => Ok(Self::ParseFloat),
            SetConstructor::UUID => Ok(Self::SetConstructor),
            StructTypeFactory::UUID => Ok(Self::StructTypeFactory),
            Throw::UUID => Ok(Self::Throw),
            ToString::UUID => Ok(Self::ToString),
            _ => Err(()),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Construct => Construct::arity(),
            Self::DateConstructor => DateConstructor::arity(),
            Self::Dispatch => Dispatch::arity(),
            Self::EncodeUriComponent => EncodeUriComponent::arity(),
            Self::FormatErrorMessage => FormatErrorMessage::arity(),
            Self::FromEntries => FromEntries::arity(),
            Self::IsFinite => IsFinite::arity(),
            Self::Log => Log::arity(),
            Self::LogArgs => LogArgs::arity(),
            Self::MapConstructor => MapConstructor::arity(),
            Self::ParseFloat => ParseFloat::arity(),
            Self::ParseInt => ParseInt::arity(),
            Self::SetConstructor => SetConstructor::arity(),
            Self::StructTypeFactory => StructTypeFactory::arity(),
            Self::Throw => Throw::arity(),
            Self::ToString => ToString::arity(),
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: Builtin
            + From<CollectHashSet>
            + From<CollectList>
            + From<ConstructHashMap>
            + From<ConstructRecord>
            + From<Contains>
            + From<Entries>
            + From<Filter>
            + From<Flatten>
            + From<Get>
            + From<Insert>
            + From<Keys>
            + From<LogArgs>
            + From<Map>
            + From<Push>
            + From<PushFront>
            + From<Reduce>
            + From<Replace>
            + From<ResolveDeep>
            + From<ResolveShallow>
            + From<Sequence>
            + From<Slice>
            + From<Split>
            + From<Values>,
    {
        match self {
            Self::Construct => Applicable::<T>::should_parallelize(&Construct, args),
            Self::DateConstructor => Applicable::<T>::should_parallelize(&DateConstructor, args),
            Self::Dispatch => Applicable::<T>::should_parallelize(&Dispatch, args),
            Self::EncodeUriComponent => {
                Applicable::<T>::should_parallelize(&EncodeUriComponent, args)
            }
            Self::FormatErrorMessage => {
                Applicable::<T>::should_parallelize(&FormatErrorMessage, args)
            }
            Self::FromEntries => Applicable::<T>::should_parallelize(&FromEntries, args),
            Self::IsFinite => Applicable::<T>::should_parallelize(&IsFinite, args),
            Self::Log => Applicable::<T>::should_parallelize(&Log, args),
            Self::LogArgs => Applicable::<T>::should_parallelize(&LogArgs, args),
            Self::MapConstructor => Applicable::<T>::should_parallelize(&MapConstructor, args),
            Self::ParseFloat => Applicable::<T>::should_parallelize(&ParseFloat, args),
            Self::ParseInt => Applicable::<T>::should_parallelize(&ParseInt, args),
            Self::SetConstructor => Applicable::<T>::should_parallelize(&SetConstructor, args),
            Self::StructTypeFactory => {
                Applicable::<T>::should_parallelize(&StructTypeFactory, args)
            }
            Self::Throw => Applicable::<T>::should_parallelize(&Throw, args),
            Self::ToString => Applicable::<T>::should_parallelize(&ToString, args),
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
        T::Builtin: Builtin
            + From<CollectHashSet>
            + From<CollectList>
            + From<ConstructHashMap>
            + From<ConstructRecord>
            + From<Contains>
            + From<Entries>
            + From<Filter>
            + From<Flatten>
            + From<Get>
            + From<Insert>
            + From<Keys>
            + From<LogArgs>
            + From<Map>
            + From<Push>
            + From<PushFront>
            + From<Reduce>
            + From<Replace>
            + From<ResolveDeep>
            + From<ResolveShallow>
            + From<Sequence>
            + From<Slice>
            + From<Split>
            + From<Values>,
    {
        match self {
            Self::Construct => Applicable::<T>::apply(&Construct, args, factory, allocator, cache),
            Self::DateConstructor => {
                Applicable::<T>::apply(&DateConstructor, args, factory, allocator, cache)
            }
            Self::Dispatch => Applicable::<T>::apply(&Dispatch, args, factory, allocator, cache),
            Self::EncodeUriComponent => {
                Applicable::<T>::apply(&EncodeUriComponent, args, factory, allocator, cache)
            }
            Self::FormatErrorMessage => {
                Applicable::<T>::apply(&FormatErrorMessage, args, factory, allocator, cache)
            }
            Self::FromEntries => {
                Applicable::<T>::apply(&FromEntries, args, factory, allocator, cache)
            }
            Self::IsFinite => Applicable::<T>::apply(&IsFinite, args, factory, allocator, cache),
            Self::Log => Applicable::<T>::apply(&Log, args, factory, allocator, cache),
            Self::LogArgs => Applicable::<T>::apply(&LogArgs, args, factory, allocator, cache),
            Self::MapConstructor => {
                Applicable::<T>::apply(&MapConstructor, args, factory, allocator, cache)
            }
            Self::ParseFloat => {
                Applicable::<T>::apply(&ParseFloat, args, factory, allocator, cache)
            }
            Self::ParseInt => Applicable::<T>::apply(&ParseInt, args, factory, allocator, cache),
            Self::SetConstructor => {
                Applicable::<T>::apply(&SetConstructor, args, factory, allocator, cache)
            }
            Self::StructTypeFactory => {
                Applicable::<T>::apply(&StructTypeFactory, args, factory, allocator, cache)
            }
            Self::Throw => Applicable::<T>::apply(&Throw, args, factory, allocator, cache),
            Self::ToString => Applicable::<T>::apply(&ToString, args, factory, allocator, cache),
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<js:{:?}>", self)
    }
}

impl From<Construct> for Stdlib {
    fn from(_value: Construct) -> Self {
        Self::Construct
    }
}
impl From<DateConstructor> for Stdlib {
    fn from(_value: DateConstructor) -> Self {
        Self::DateConstructor
    }
}
impl From<Dispatch> for Stdlib {
    fn from(_value: Dispatch) -> Self {
        Self::Dispatch
    }
}
impl From<EncodeUriComponent> for Stdlib {
    fn from(_value: EncodeUriComponent) -> Self {
        Self::EncodeUriComponent
    }
}
impl From<FormatErrorMessage> for Stdlib {
    fn from(_value: FormatErrorMessage) -> Self {
        Self::FormatErrorMessage
    }
}
impl From<FromEntries> for Stdlib {
    fn from(_value: FromEntries) -> Self {
        Self::FromEntries
    }
}
impl From<IsFinite> for Stdlib {
    fn from(_value: IsFinite) -> Self {
        Self::IsFinite
    }
}
impl From<Log> for Stdlib {
    fn from(_value: Log) -> Self {
        Self::Log
    }
}
impl From<LogArgs> for Stdlib {
    fn from(_value: LogArgs) -> Self {
        Self::LogArgs
    }
}
impl From<MapConstructor> for Stdlib {
    fn from(_value: MapConstructor) -> Self {
        Self::MapConstructor
    }
}
impl From<ParseFloat> for Stdlib {
    fn from(_value: ParseFloat) -> Self {
        Self::ParseFloat
    }
}
impl From<ParseInt> for Stdlib {
    fn from(_value: ParseInt) -> Self {
        Self::ParseInt
    }
}
impl From<SetConstructor> for Stdlib {
    fn from(_value: SetConstructor) -> Self {
        Self::SetConstructor
    }
}
impl From<StructTypeFactory> for Stdlib {
    fn from(_value: StructTypeFactory) -> Self {
        Self::StructTypeFactory
    }
}
impl From<Throw> for Stdlib {
    fn from(_value: Throw) -> Self {
        Self::Throw
    }
}
impl From<ToString> for Stdlib {
    fn from(_value: ToString) -> Self {
        Self::ToString
    }
}
