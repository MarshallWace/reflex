// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{
        Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid, Uuid,
    },
    stdlib::Stdlib as BuiltinStdlib,
};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use construct::*;
pub use dispatch::*;
pub use encode_uri_component::*;
pub use format_error_message::*;
pub use from_entries::*;
pub use json_parse::*;
pub use json_stringify::*;
pub use map_constructor::*;
pub use set_constructor::*;
pub use struct_type_factory::*;
pub use throw::*;
pub use to_request::*;
pub use to_string::*;

mod construct;
mod dispatch;
mod encode_uri_component;
mod format_error_message;
mod from_entries;
mod json_parse;
mod json_stringify;
mod map_constructor;
mod set_constructor;
mod struct_type_factory;
mod throw;
mod to_request;
mod to_string;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "type")]
pub enum Stdlib {
    Construct,
    Dispatch,
    EncodeUriComponent,
    FormatErrorMessage,
    FromEntries,
    JsonParse,
    JsonStringify,
    MapConstructor,
    SetConstructor,
    StructTypeFactory,
    Throw,
    ToRequest,
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
            Self::Dispatch => Uid::uid(&Dispatch {}),
            Self::EncodeUriComponent => Uid::uid(&EncodeUriComponent {}),
            Self::FormatErrorMessage => Uid::uid(&FormatErrorMessage {}),
            Self::FromEntries => Uid::uid(&FromEntries {}),
            Self::JsonParse => Uid::uid(&JsonParse {}),
            Self::JsonStringify => Uid::uid(&JsonStringify {}),
            Self::MapConstructor => Uid::uid(&MapConstructor {}),
            Self::SetConstructor => Uid::uid(&SetConstructor {}),
            Self::StructTypeFactory => Uid::uid(&StructTypeFactory {}),
            Self::Throw => Uid::uid(&Throw {}),
            Self::ToRequest => Uid::uid(&ToRequest {}),
            Self::ToString => Uid::uid(&ToString {}),
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
            Self::FormatErrorMessage => Applicable::<T>::arity(&FormatErrorMessage {}),
            Self::FromEntries => Applicable::<T>::arity(&FromEntries {}),
            Self::JsonParse => Applicable::<T>::arity(&JsonParse {}),
            Self::JsonStringify => Applicable::<T>::arity(&JsonStringify {}),
            Self::MapConstructor => Applicable::<T>::arity(&MapConstructor {}),
            Self::SetConstructor => Applicable::<T>::arity(&SetConstructor {}),
            Self::StructTypeFactory => Applicable::<T>::arity(&StructTypeFactory {}),
            Self::Throw => Applicable::<T>::arity(&Throw {}),
            Self::ToRequest => Applicable::<T>::arity(&ToRequest {}),
            Self::ToString => Applicable::<T>::arity(&ToString {}),
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
            Self::FormatErrorMessage => {
                Applicable::<T>::apply(&FormatErrorMessage {}, args, factory, allocator, cache)
            }
            Self::FromEntries => {
                Applicable::<T>::apply(&FromEntries {}, args, factory, allocator, cache)
            }
            Self::JsonParse => {
                Applicable::<T>::apply(&JsonParse {}, args, factory, allocator, cache)
            }
            Self::JsonStringify => {
                Applicable::<T>::apply(&JsonStringify {}, args, factory, allocator, cache)
            }
            Self::MapConstructor => {
                Applicable::<T>::apply(&MapConstructor {}, args, factory, allocator, cache)
            }
            Self::SetConstructor => {
                Applicable::<T>::apply(&SetConstructor {}, args, factory, allocator, cache)
            }
            Self::StructTypeFactory => {
                Applicable::<T>::apply(&StructTypeFactory {}, args, factory, allocator, cache)
            }
            Self::Throw => Applicable::<T>::apply(&Throw {}, args, factory, allocator, cache),
            Self::ToRequest => {
                Applicable::<T>::apply(&ToRequest {}, args, factory, allocator, cache)
            }
            Self::ToString => Applicable::<T>::apply(&ToString {}, args, factory, allocator, cache),
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<js:{:?}>", self)
    }
}
