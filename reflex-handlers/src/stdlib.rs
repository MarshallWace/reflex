// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use reflex::core::{
    Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid, Uuid,
};

pub(crate) mod getter;
pub(crate) mod scan;
pub(crate) mod setter;
pub(crate) mod to_request;
pub(crate) mod variable;

pub use getter::*;
pub use scan::*;
pub use setter::*;
pub use to_request::*;
pub use variable::*;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "type")]
pub enum Stdlib {
    Getter,
    Scan,
    Setter,
    ToRequest,
    Variable,
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
            Getter::UUID => Ok(Self::Getter),
            Scan::UUID => Ok(Self::Scan),
            Setter::UUID => Ok(Self::Setter),
            ToRequest::UUID => Ok(Self::ToRequest),
            Variable::UUID => Ok(Self::Variable),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::Getter => Uid::uid(&Getter {}),
            Self::Scan => Uid::uid(&Scan {}),
            Self::Setter => Uid::uid(&Setter {}),
            Self::ToRequest => Uid::uid(&ToRequest {}),
            Self::Variable => Uid::uid(&Variable {}),
        }
    }
}
impl Stdlib {
    pub fn arity<T: Expression>(&self) -> Option<Arity>
    where
        T::Builtin: From<Self> + From<reflex::stdlib::Stdlib>,
    {
        match self {
            Self::Getter => Applicable::<T>::arity(&Getter {}),
            Self::Scan => Applicable::<T>::arity(&Scan {}),
            Self::Setter => Applicable::<T>::arity(&Setter {}),
            Self::ToRequest => Applicable::<T>::arity(&ToRequest {}),
            Self::Variable => Applicable::<T>::arity(&Variable {}),
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
        T::Builtin: From<Self> + From<reflex::stdlib::Stdlib>,
    {
        match self {
            Self::Getter => Applicable::<T>::apply(&Getter {}, args, factory, allocator, cache),
            Self::Scan => Applicable::<T>::apply(&Scan {}, args, factory, allocator, cache),
            Self::Setter => Applicable::<T>::apply(&Setter {}, args, factory, allocator, cache),
            Self::ToRequest => {
                Applicable::<T>::apply(&ToRequest {}, args, factory, allocator, cache)
            }
            Self::Variable => Applicable::<T>::apply(&Variable {}, args, factory, allocator, cache),
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: From<Self> + From<reflex::stdlib::Stdlib>,
    {
        match self {
            Self::Getter => Applicable::<T>::should_parallelize(&Getter {}, args),
            Self::Scan => Applicable::<T>::should_parallelize(&Scan {}, args),
            Self::Setter => Applicable::<T>::should_parallelize(&Setter {}, args),
            Self::ToRequest => Applicable::<T>::should_parallelize(&ToRequest {}, args),
            Self::Variable => Applicable::<T>::should_parallelize(&Variable {}, args),
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<json:{:?}>", self)
    }
}
