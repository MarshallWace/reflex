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

pub use collect_query_list_items::*;
pub use dynamic_query_branch::*;
pub use flatten_deep::*;

mod collect_query_list_items;
mod dynamic_query_branch;
mod flatten_deep;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    CollectQueryListItems,
    DynamicQueryBranch,
    FlattenDeep,
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
            CollectQueryListItems::UUID => Ok(Self::CollectQueryListItems),
            DynamicQueryBranch::UUID => Ok(Self::DynamicQueryBranch),
            FlattenDeep::UUID => Ok(Self::FlattenDeep),
            _ => Err(()),
        }
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::CollectQueryListItems => Uid::uid(&CollectQueryListItems {}),
            Self::DynamicQueryBranch => Uid::uid(&DynamicQueryBranch {}),
            Self::FlattenDeep => Uid::uid(&FlattenDeep {}),
        }
    }
}
impl Stdlib {
    pub fn arity<T: Expression>(&self) -> Arity
    where
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::CollectQueryListItems => CollectQueryListItems::arity(),
            Self::DynamicQueryBranch => DynamicQueryBranch::arity(),
            Self::FlattenDeep => FlattenDeep::arity(),
        }
    }
    pub fn should_parallelize<T: Expression + Applicable<T>>(&self, args: &[T]) -> bool
    where
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::CollectQueryListItems => {
                Applicable::<T>::should_parallelize(&CollectQueryListItems {}, args)
            }
            Self::DynamicQueryBranch => {
                Applicable::<T>::should_parallelize(&DynamicQueryBranch {}, args)
            }
            Self::FlattenDeep => Applicable::<T>::should_parallelize(&FlattenDeep {}, args),
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
            Self::CollectQueryListItems => {
                Applicable::<T>::apply(&CollectQueryListItems {}, args, factory, allocator, cache)
            }
            Self::DynamicQueryBranch => {
                Applicable::<T>::apply(&DynamicQueryBranch {}, args, factory, allocator, cache)
            }
            Self::FlattenDeep => {
                Applicable::<T>::apply(&FlattenDeep {}, args, factory, allocator, cache)
            }
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<graphql:{:?}>", self)
    }
}
