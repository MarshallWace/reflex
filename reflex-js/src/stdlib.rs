// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use accessor::*;
pub use construct::*;
pub use date_constructor::*;
pub use encode_uri_component::*;
pub use format_error_message::*;
pub use is_finite::*;
pub use log::*;
pub use parse_float::*;
pub use parse_int::*;
pub use throw::*;
pub use to_string::*;

mod accessor;
mod construct;
mod date_constructor;
mod encode_uri_component;
mod format_error_message;
mod is_finite;
mod log;
mod parse_float;
mod parse_int;
mod throw;
mod to_string;

pub trait JsStdlibBuiltin: Builtin + AccessorBuiltin + LogBuiltin {}
impl<T> JsStdlibBuiltin for T where T: Builtin + AccessorBuiltin + LogBuiltin {}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    Accessor,
    Construct,
    DateConstructor,
    EncodeUriComponent,
    FormatErrorMessage,
    IsFinite,
    Log,
    LogArgs,
    ParseFloat,
    ParseInt,
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
            Self::Accessor => Uid::uid(&Accessor {}),
            Self::Construct => Uid::uid(&Construct {}),
            Self::DateConstructor => Uid::uid(&DateConstructor {}),
            Self::EncodeUriComponent => Uid::uid(&EncodeUriComponent {}),
            Self::FormatErrorMessage => Uid::uid(&FormatErrorMessage {}),
            Self::IsFinite => Uid::uid(&IsFinite {}),
            Self::Log => Uid::uid(&Log {}),
            Self::LogArgs => Uid::uid(&LogArgs {}),
            Self::ParseFloat => Uid::uid(&ParseFloat {}),
            Self::ParseInt => Uid::uid(&ParseInt {}),
            Self::Throw => Uid::uid(&Throw {}),
            Self::ToString => Uid::uid(&ToString {}),
        }
    }
}
impl TryFrom<Uuid> for Stdlib {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        match value {
            Accessor::UUID => Ok(Self::Accessor),
            Construct::UUID => Ok(Self::Construct),
            DateConstructor::UUID => Ok(Self::DateConstructor),
            EncodeUriComponent::UUID => Ok(Self::EncodeUriComponent),
            FormatErrorMessage::UUID => Ok(Self::FormatErrorMessage),
            IsFinite::UUID => Ok(Self::IsFinite),
            Log::UUID => Ok(Self::Log),
            LogArgs::UUID => Ok(Self::LogArgs),
            ParseInt::UUID => Ok(Self::ParseInt),
            ParseFloat::UUID => Ok(Self::ParseFloat),
            Throw::UUID => Ok(Self::Throw),
            ToString::UUID => Ok(Self::ToString),
            _ => Err(()),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Accessor => Accessor::arity(),
            Self::Construct => Construct::arity(),
            Self::DateConstructor => DateConstructor::arity(),
            Self::EncodeUriComponent => EncodeUriComponent::arity(),
            Self::FormatErrorMessage => FormatErrorMessage::arity(),
            Self::IsFinite => IsFinite::arity(),
            Self::Log => Log::arity(),
            Self::LogArgs => LogArgs::arity(),
            Self::ParseFloat => ParseFloat::arity(),
            Self::ParseInt => ParseInt::arity(),
            Self::Throw => Throw::arity(),
            Self::ToString => ToString::arity(),
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: JsStdlibBuiltin,
    {
        match self {
            Self::Accessor => Applicable::<T>::should_parallelize(&Accessor, args),
            Self::Construct => Applicable::<T>::should_parallelize(&Construct, args),
            Self::DateConstructor => Applicable::<T>::should_parallelize(&DateConstructor, args),
            Self::EncodeUriComponent => {
                Applicable::<T>::should_parallelize(&EncodeUriComponent, args)
            }
            Self::FormatErrorMessage => {
                Applicable::<T>::should_parallelize(&FormatErrorMessage, args)
            }
            Self::IsFinite => Applicable::<T>::should_parallelize(&IsFinite, args),
            Self::Log => Applicable::<T>::should_parallelize(&Log, args),
            Self::LogArgs => Applicable::<T>::should_parallelize(&LogArgs, args),
            Self::ParseFloat => Applicable::<T>::should_parallelize(&ParseFloat, args),
            Self::ParseInt => Applicable::<T>::should_parallelize(&ParseInt, args),
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
        T::Builtin: JsStdlibBuiltin,
    {
        match self {
            Self::Accessor => Applicable::<T>::apply(&Accessor, args, factory, allocator, cache),
            Self::Construct => Applicable::<T>::apply(&Construct, args, factory, allocator, cache),
            Self::DateConstructor => {
                Applicable::<T>::apply(&DateConstructor, args, factory, allocator, cache)
            }
            Self::EncodeUriComponent => {
                Applicable::<T>::apply(&EncodeUriComponent, args, factory, allocator, cache)
            }
            Self::FormatErrorMessage => {
                Applicable::<T>::apply(&FormatErrorMessage, args, factory, allocator, cache)
            }
            Self::IsFinite => Applicable::<T>::apply(&IsFinite, args, factory, allocator, cache),
            Self::Log => Applicable::<T>::apply(&Log, args, factory, allocator, cache),
            Self::LogArgs => Applicable::<T>::apply(&LogArgs, args, factory, allocator, cache),
            Self::ParseFloat => {
                Applicable::<T>::apply(&ParseFloat, args, factory, allocator, cache)
            }
            Self::ParseInt => Applicable::<T>::apply(&ParseInt, args, factory, allocator, cache),
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

impl From<Accessor> for Stdlib {
    fn from(_value: Accessor) -> Self {
        Self::Accessor
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
