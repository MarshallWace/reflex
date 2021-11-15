// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid, Uuid,
    },
    stdlib::Stdlib as BuiltinStdlib,
};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use construct::*;
pub use dispatch::*;
pub use encode_uri_component::*;
pub use float::*;
pub use format_error_message::*;
pub use from_entries::*;
pub use getter::*;
pub use hash::*;
pub use int::*;
pub use json_parse::*;
pub use json_stringify::*;
pub use log::*;
pub use map_constructor::*;
pub use scan::*;
pub use set_constructor::*;
pub use setter::*;
pub use struct_type_factory::*;
pub use throw::*;
pub use to_request::*;
pub use to_string::*;
pub use variable::*;

mod construct;
mod dispatch;
mod encode_uri_component;
mod float;
mod format_error_message;
mod from_entries;
mod getter;
mod hash;
mod int;
mod json_parse;
mod json_stringify;
mod log;
mod map_constructor;
mod scan;
mod set_constructor;
mod setter;
mod struct_type_factory;
mod throw;
mod to_request;
mod to_string;
mod variable;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "type")]
pub enum Stdlib {
    Construct,
    Dispatch,
    EncodeUriComponent,
    Float,
    FormatErrorMessage,
    FromEntries,
    Getter,
    Hash,
    Int,
    JsonParse,
    JsonStringify,
    Log,
    LogArgs,
    MapConstructor,
    Scan,
    SetConstructor,
    Setter,
    StructTypeFactory,
    Throw,
    ToRequest,
    ToString,
    Variable,
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
            Self::Dispatch => Uid::uid(&Dispatch {}),
            Self::EncodeUriComponent => Uid::uid(&EncodeUriComponent {}),
            Self::Float => Uid::uid(&Float {}),
            Self::FormatErrorMessage => Uid::uid(&FormatErrorMessage {}),
            Self::FromEntries => Uid::uid(&FromEntries {}),
            Self::Getter => Uid::uid(&Getter {}),
            Self::Hash => Uid::uid(&Hash {}),
            Self::Int => Uid::uid(&Int {}),
            Self::JsonParse => Uid::uid(&JsonParse {}),
            Self::JsonStringify => Uid::uid(&JsonStringify {}),
            Self::Log => Uid::uid(&Log {}),
            Self::LogArgs => Uid::uid(&LogArgs {}),
            Self::MapConstructor => Uid::uid(&MapConstructor {}),
            Self::Scan => Uid::uid(&Scan {}),
            Self::SetConstructor => Uid::uid(&SetConstructor {}),
            Self::Setter => Uid::uid(&Setter {}),
            Self::StructTypeFactory => Uid::uid(&StructTypeFactory {}),
            Self::Throw => Uid::uid(&Throw {}),
            Self::ToRequest => Uid::uid(&ToRequest {}),
            Self::ToString => Uid::uid(&ToString {}),
            Self::Variable => Uid::uid(&Variable {}),
        }
    }
}
impl TryFrom<Uuid> for Stdlib {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        match value {
            Construct::UUID => Ok(Self::Construct),
            Dispatch::UUID => Ok(Self::Dispatch),
            EncodeUriComponent::UUID => Ok(Self::EncodeUriComponent),
            Float::UUID => Ok(Self::Float),
            FormatErrorMessage::UUID => Ok(Self::FormatErrorMessage),
            FromEntries::UUID => Ok(Self::FromEntries),
            Getter::UUID => Ok(Self::Getter),
            Hash::UUID => Ok(Self::Hash),
            Int::UUID => Ok(Self::Int),
            JsonParse::UUID => Ok(Self::JsonParse),
            JsonStringify::UUID => Ok(Self::JsonStringify),
            Log::UUID => Ok(Self::Log),
            LogArgs::UUID => Ok(Self::LogArgs),
            MapConstructor::UUID => Ok(Self::MapConstructor),
            Scan::UUID => Ok(Self::Scan),
            SetConstructor::UUID => Ok(Self::SetConstructor),
            Setter::UUID => Ok(Self::Setter),
            StructTypeFactory::UUID => Ok(Self::StructTypeFactory),
            Throw::UUID => Ok(Self::Throw),
            ToRequest::UUID => Ok(Self::ToRequest),
            ToString::UUID => Ok(Self::ToString),
            Variable::UUID => Ok(Self::Variable),
            _ => Err(()),
        }
    }
}
impl Stdlib {
    pub fn arity<T: Expression + Applicable<T>>(&self) -> Option<Arity>
    where
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::Construct => Applicable::<T>::arity(&Construct {}),
            Self::Dispatch => Applicable::<T>::arity(&Dispatch {}),
            Self::EncodeUriComponent => Applicable::<T>::arity(&EncodeUriComponent {}),
            Self::Float => Applicable::<T>::arity(&Float {}),
            Self::FormatErrorMessage => Applicable::<T>::arity(&FormatErrorMessage {}),
            Self::FromEntries => Applicable::<T>::arity(&FromEntries {}),
            Self::Getter => Applicable::<T>::arity(&Getter {}),
            Self::Hash => Applicable::<T>::arity(&Hash {}),
            Self::Int => Applicable::<T>::arity(&Int {}),
            Self::JsonParse => Applicable::<T>::arity(&JsonParse {}),
            Self::JsonStringify => Applicable::<T>::arity(&JsonStringify {}),
            Self::Log => Applicable::<T>::arity(&Log {}),
            Self::LogArgs => Applicable::<T>::arity(&LogArgs {}),
            Self::MapConstructor => Applicable::<T>::arity(&MapConstructor {}),
            Self::Scan => Applicable::<T>::arity(&Scan {}),
            Self::SetConstructor => Applicable::<T>::arity(&SetConstructor {}),
            Self::Setter => Applicable::<T>::arity(&Setter {}),
            Self::StructTypeFactory => Applicable::<T>::arity(&StructTypeFactory {}),
            Self::Throw => Applicable::<T>::arity(&Throw {}),
            Self::ToRequest => Applicable::<T>::arity(&ToRequest {}),
            Self::ToString => Applicable::<T>::arity(&ToString {}),
            Self::Variable => Applicable::<T>::arity(&Variable {}),
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
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::Construct => {
                Applicable::<T>::apply(&Construct {}, args, factory, allocator, cache)
            }
            Self::Dispatch => Applicable::<T>::apply(&Dispatch {}, args, factory, allocator, cache),
            Self::EncodeUriComponent => {
                Applicable::<T>::apply(&EncodeUriComponent {}, args, factory, allocator, cache)
            }
            Self::Float => Applicable::<T>::apply(&Float {}, args, factory, allocator, cache),
            Self::FormatErrorMessage => {
                Applicable::<T>::apply(&FormatErrorMessage {}, args, factory, allocator, cache)
            }
            Self::FromEntries => {
                Applicable::<T>::apply(&FromEntries {}, args, factory, allocator, cache)
            }
            Self::Getter => Applicable::<T>::apply(&Getter {}, args, factory, allocator, cache),
            Self::Hash => Applicable::<T>::apply(&Hash {}, args, factory, allocator, cache),
            Self::Int => Applicable::<T>::apply(&Int {}, args, factory, allocator, cache),
            Self::JsonParse => {
                Applicable::<T>::apply(&JsonParse {}, args, factory, allocator, cache)
            }
            Self::JsonStringify => {
                Applicable::<T>::apply(&JsonStringify {}, args, factory, allocator, cache)
            }
            Self::Log => Applicable::<T>::apply(&Log {}, args, factory, allocator, cache),
            Self::LogArgs => Applicable::<T>::apply(&LogArgs {}, args, factory, allocator, cache),
            Self::MapConstructor => {
                Applicable::<T>::apply(&MapConstructor {}, args, factory, allocator, cache)
            }
            Self::Scan => Applicable::<T>::apply(&Scan {}, args, factory, allocator, cache),
            Self::SetConstructor => {
                Applicable::<T>::apply(&SetConstructor {}, args, factory, allocator, cache)
            }
            Self::Setter => Applicable::<T>::apply(&Setter {}, args, factory, allocator, cache),
            Self::StructTypeFactory => {
                Applicable::<T>::apply(&StructTypeFactory {}, args, factory, allocator, cache)
            }
            Self::Throw => Applicable::<T>::apply(&Throw {}, args, factory, allocator, cache),
            Self::ToRequest => {
                Applicable::<T>::apply(&ToRequest {}, args, factory, allocator, cache)
            }
            Self::ToString => Applicable::<T>::apply(&ToString {}, args, factory, allocator, cache),
            Self::Variable => Applicable::<T>::apply(&Variable {}, args, factory, allocator, cache),
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<js:{:?}>", self)
    }
}
