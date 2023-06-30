// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{
    Applicable, Arity, Builtin, EvaluationCache, Expression, ExpressionFactory, HeapAllocator, Uid,
    Uuid,
};
use reflex_stdlib::CollectList;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub use collect_query_list_items::*;
pub use dynamic_query_branch::*;
pub use flatten_deep::*;
pub use graphql_resolver::*;

mod collect_query_list_items;
mod dynamic_query_branch;
mod flatten_deep;
mod graphql_resolver;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
pub enum Stdlib {
    CollectQueryListItems,
    DynamicQueryBranch,
    FlattenDeep,
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
            CollectQueryListItems::UUID => Ok(Self::CollectQueryListItems),
            DynamicQueryBranch::UUID => Ok(Self::DynamicQueryBranch),
            FlattenDeep::UUID => Ok(Self::FlattenDeep),
            GraphQlResolver::UUID => Ok(Self::GraphQlResolver),
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
            Self::GraphQlResolver => Uid::uid(&GraphQlResolver {}),
        }
    }
}
impl Stdlib {
    pub fn arity(&self) -> Arity {
        match self {
            Self::CollectQueryListItems => CollectQueryListItems::arity(),
            Self::DynamicQueryBranch => DynamicQueryBranch::arity(),
            Self::FlattenDeep => FlattenDeep::arity(),
            Self::GraphQlResolver => GraphQlResolver::arity(),
        }
    }
    pub fn should_parallelize<T: Expression + Applicable<T>>(&self, args: &[T]) -> bool
    where
        T::Builtin: Builtin
            + From<CollectList>
            + From<CollectQueryListItems>
            + From<DynamicQueryBranch>
            + From<FlattenDeep>
            + From<GraphQlResolver>,
    {
        match self {
            Self::CollectQueryListItems => {
                Applicable::<T>::should_parallelize(&CollectQueryListItems, args)
            }
            Self::DynamicQueryBranch => {
                Applicable::<T>::should_parallelize(&DynamicQueryBranch, args)
            }
            Self::FlattenDeep => Applicable::<T>::should_parallelize(&FlattenDeep, args),
            Self::GraphQlResolver => Applicable::<T>::should_parallelize(&GraphQlResolver, args),
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
        T::Builtin: Builtin
            + From<CollectList>
            + From<CollectQueryListItems>
            + From<DynamicQueryBranch>
            + From<FlattenDeep>
            + From<GraphQlResolver>,
    {
        match self {
            Self::CollectQueryListItems => {
                Applicable::<T>::apply(&CollectQueryListItems, args, factory, allocator, cache)
            }
            Self::DynamicQueryBranch => {
                Applicable::<T>::apply(&DynamicQueryBranch, args, factory, allocator, cache)
            }
            Self::FlattenDeep => {
                Applicable::<T>::apply(&FlattenDeep, args, factory, allocator, cache)
            }
            Self::GraphQlResolver => {
                Applicable::<T>::apply(&GraphQlResolver, args, factory, allocator, cache)
            }
        }
    }
}
impl std::fmt::Display for Stdlib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<graphql:{:?}>", self)
    }
}

impl From<CollectQueryListItems> for Stdlib {
    fn from(_value: CollectQueryListItems) -> Self {
        Self::CollectQueryListItems
    }
}
impl From<DynamicQueryBranch> for Stdlib {
    fn from(_value: DynamicQueryBranch) -> Self {
        Self::DynamicQueryBranch
    }
}
impl From<FlattenDeep> for Stdlib {
    fn from(_value: FlattenDeep) -> Self {
        Self::FlattenDeep
    }
}
impl From<GraphQlResolver> for Stdlib {
    fn from(_value: GraphQlResolver) -> Self {
        Self::GraphQlResolver
    }
}
