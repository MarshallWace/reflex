// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};

pub use json_deserialize::*;
pub use json_serialize::*;

mod json_deserialize;
mod json_serialize;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    JsonDeserialize,
    JsonSerialize,
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
            JsonDeserialize::UUID => Ok(Self::JsonDeserialize),
            JsonSerialize::UUID => Ok(Self::JsonSerialize),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::JsonDeserialize => Uid::uid(&JsonDeserialize {}),
            Self::JsonSerialize => Uid::uid(&JsonSerialize {}),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::JsonDeserialize => JsonDeserialize::arity(),
            Self::JsonSerialize => JsonSerialize::arity(),
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
        T::Builtin: From<Self>,
    {
        match self {
            Self::JsonDeserialize => {
                Applicable::<T>::apply(&JsonDeserialize {}, args, factory, allocator, cache)
            }
            Self::JsonSerialize => {
                Applicable::<T>::apply(&JsonSerialize {}, args, factory, allocator, cache)
            }
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: From<Self>,
    {
        match self {
            Self::JsonDeserialize => Applicable::<T>::should_parallelize(&JsonDeserialize {}, args),
            Self::JsonSerialize => Applicable::<T>::should_parallelize(&JsonSerialize {}, args),
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
        write!(f, "<json:{:?}>", self)
    }
}

impl From<JsonDeserialize> for Stdlib {
    fn from(_value: JsonDeserialize) -> Self {
        Self::JsonDeserialize
    }
}
impl From<JsonSerialize> for Stdlib {
    fn from(_value: JsonSerialize) -> Self {
        Self::JsonSerialize
    }
}
