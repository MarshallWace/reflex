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

pub(crate) mod scan;
pub(crate) mod to_request;
pub(crate) mod variable;

pub use scan::*;
pub use to_request::*;
pub use variable::*;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    Scan,
    ToRequest,
    GetVariable,
    SetVariable,
    IncrementVariable,
    DecrementVariable,
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
            Scan::UUID => Ok(Self::Scan),
            ToRequest::UUID => Ok(Self::ToRequest),
            GetVariable::UUID => Ok(Self::GetVariable),
            SetVariable::UUID => Ok(Self::SetVariable),
            IncrementVariable::UUID => Ok(Self::IncrementVariable),
            DecrementVariable::UUID => Ok(Self::DecrementVariable),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::Scan => Uid::uid(&Scan {}),
            Self::ToRequest => Uid::uid(&ToRequest {}),
            Self::GetVariable => Uid::uid(&GetVariable {}),
            Self::SetVariable => Uid::uid(&SetVariable {}),
            Self::IncrementVariable => Uid::uid(&IncrementVariable {}),
            Self::DecrementVariable => Uid::uid(&DecrementVariable {}),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::Scan => Scan::arity(),
            Self::ToRequest => ToRequest::arity(),
            Self::GetVariable => GetVariable::arity(),
            Self::SetVariable => SetVariable::arity(),
            Self::IncrementVariable => IncrementVariable::arity(),
            Self::DecrementVariable => DecrementVariable::arity(),
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
        T::Builtin: From<Self> + From<reflex_stdlib::Stdlib>,
    {
        match self {
            Self::Scan => Applicable::<T>::apply(&Scan {}, args, factory, allocator, cache),
            Self::ToRequest => {
                Applicable::<T>::apply(&ToRequest {}, args, factory, allocator, cache)
            }
            Self::GetVariable => {
                Applicable::<T>::apply(&GetVariable {}, args, factory, allocator, cache)
            }
            Self::SetVariable => {
                Applicable::<T>::apply(&SetVariable {}, args, factory, allocator, cache)
            }
            Self::IncrementVariable => {
                Applicable::<T>::apply(&IncrementVariable {}, args, factory, allocator, cache)
            }
            Self::DecrementVariable => {
                Applicable::<T>::apply(&DecrementVariable {}, args, factory, allocator, cache)
            }
        }
    }
    pub fn should_parallelize<T: Expression>(&self, args: &[T]) -> bool
    where
        T::Builtin: From<Self> + From<reflex_stdlib::Stdlib>,
    {
        match self {
            Self::Scan => Applicable::<T>::should_parallelize(&Scan {}, args),
            Self::ToRequest => Applicable::<T>::should_parallelize(&ToRequest {}, args),
            Self::GetVariable => Applicable::<T>::should_parallelize(&GetVariable {}, args),
            Self::SetVariable => Applicable::<T>::should_parallelize(&SetVariable {}, args),
            Self::IncrementVariable => {
                Applicable::<T>::should_parallelize(&IncrementVariable {}, args)
            }
            Self::DecrementVariable => {
                Applicable::<T>::should_parallelize(&IncrementVariable {}, args)
            }
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<handlers:{:?}>", self)
    }
}

impl From<Scan> for Stdlib {
    fn from(_value: Scan) -> Self {
        Self::Scan
    }
}
impl From<ToRequest> for Stdlib {
    fn from(_value: ToRequest) -> Self {
        Self::ToRequest
    }
}
impl From<GetVariable> for Stdlib {
    fn from(_value: GetVariable) -> Self {
        Self::GetVariable
    }
}
impl From<SetVariable> for Stdlib {
    fn from(_value: SetVariable) -> Self {
        Self::SetVariable
    }
}
impl From<IncrementVariable> for Stdlib {
    fn from(_value: IncrementVariable) -> Self {
        Self::IncrementVariable
    }
}
impl From<DecrementVariable> for Stdlib {
    fn from(_value: DecrementVariable) -> Self {
        Self::DecrementVariable
    }
}
