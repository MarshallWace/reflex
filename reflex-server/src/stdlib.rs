// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    Applicable, Arity, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid, Uuid,
};
use reflex_stdlib::Stdlib as BuiltinStdlib;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use graphql_resolver::*;

mod graphql_resolver;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    GraphQlResolver,
}
impl Stdlib {
    pub fn entries() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}
impl TryFrom<Uuid> for Stdlib {
    type Error = ();
    fn try_from(value: Uuid) -> Result<Self, Self::Error> {
        match value {
            GraphQlResolver::UUID => Ok(Self::GraphQlResolver),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::GraphQlResolver => Uid::uid(&GraphQlResolver {}),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::GraphQlResolver => GraphQlResolver::arity(),
        }
    }
    pub fn should_parallelize<T: Expression + Applicable<T>>(&self, args: &[T]) -> bool
    where
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::GraphQlResolver => Applicable::<T>::should_parallelize(&GraphQlResolver {}, args),
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
            Self::GraphQlResolver => {
                Applicable::<T>::apply(&GraphQlResolver {}, args, factory, allocator, cache)
            }
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<server:{:?}>", self)
    }
}
