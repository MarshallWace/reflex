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

pub use dynamic_query_branch::*;
pub use flatten_deep::*;

mod dynamic_query_branch;
mod flatten_deep;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize, EnumIter)]
#[serde(tag = "type")]
pub enum Stdlib {
    DynamicQueryBranch,
    FlattenDeep,
}
impl Stdlib {
    pub fn entries() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}
impl Uid for Stdlib {
    fn uid(&self) -> Uuid {
        match self {
            Self::DynamicQueryBranch => Uid::uid(&DynamicQueryBranch {}),
            Self::FlattenDeep => Uid::uid(&FlattenDeep {}),
        }
    }
}
impl Stdlib {
    pub fn arity<T: Expression + Applicable<T>>(&self) -> Option<Arity>
    where
        T::Builtin: From<Self> + From<BuiltinStdlib>,
    {
        match self {
            Self::DynamicQueryBranch => Applicable::<T>::arity(&DynamicQueryBranch {}),
            Self::FlattenDeep => Applicable::<T>::arity(&FlattenDeep {}),
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