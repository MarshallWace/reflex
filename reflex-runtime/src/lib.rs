// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::sync::Arc;

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, StringValue},
    lang::ValueTerm,
};

pub mod action;
pub mod actor;
pub(crate) mod worker;

pub trait AsyncExpression: Expression + Send + 'static {}
impl<T> AsyncExpression for T
where
    T: Expression + Send + 'static,
    T::String: StringValue + Send,
{
}
pub trait AsyncExpressionFactory<T: AsyncExpression>:
    ExpressionFactory<T> + Send + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncExpressionFactory<E> for T
where
    T: ExpressionFactory<E> + Send + Clone + 'static,
    E::String: StringValue + Send,
{
}
pub trait AsyncHeapAllocator<T: AsyncExpression>:
    HeapAllocator<T> + Send + Clone + 'static
{
}
impl<T, E: AsyncExpression> AsyncHeapAllocator<E> for T
where
    T: HeapAllocator<E> + Send + Clone + 'static,
    E::String: StringValue + Send,
{
}

#[derive(Clone)]
pub enum StateUpdate<T: Expression> {
    Value(T),
    Patch(Arc<dyn (Fn(Option<&T>) -> T) + Send + Sync + 'static>),
}
impl<T: Expression> StateUpdate<T> {
    pub fn value(value: T) -> Self {
        Self::Value(value)
    }
    pub fn patch(updater: impl Fn(Option<&T>) -> T + Send + Sync + 'static) -> Self {
        Self::Patch(Arc::new(updater))
    }
}
impl<T: Expression> std::fmt::Display for StateUpdate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{}", value),
            Self::Patch(_) => write!(f, "<updater>"),
        }
    }
}
impl<T: Expression> std::fmt::Debug for StateUpdate<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(value) => f.debug_tuple("Value").field(value).finish(),
            Self::Patch(_) => f.debug_tuple("Patch").field(&"<function>").finish(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum QueryEvaluationMode {
    /// Evaluate the expression as a query function to be applied to the graph root
    Query,
    /// Evaluate the expression as a standalone expression unrelated to the graph root
    Standalone,
}
impl QueryEvaluationMode {
    pub(crate) fn serialize<T: Expression>(&self, factory: &impl ExpressionFactory<T>) -> T {
        factory.create_value_term(ValueTerm::Boolean(match self {
            Self::Query => false,
            Self::Standalone => true,
        }))
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_value_term(value)
            .and_then(|value| value.match_boolean())
            .map(|value| match value {
                false => Self::Query,
                true => Self::Standalone,
            })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum QueryInvalidationStrategy {
    /// Group sequential async state updates into combined batches and process simultaneously (better performance)
    CombineUpdateBatches,
    /// Evaluate a new result for every individual state update batch (useful when watching incremental queries)
    Exact,
}
impl Default for QueryInvalidationStrategy {
    fn default() -> Self {
        Self::CombineUpdateBatches
    }
}
impl QueryInvalidationStrategy {
    pub(crate) fn serialize<T: Expression>(&self, factory: &impl ExpressionFactory<T>) -> T {
        factory.create_value_term(ValueTerm::Boolean(match self {
            Self::CombineUpdateBatches => false,
            Self::Exact => true,
        }))
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_value_term(value)
            .and_then(|value| value.match_boolean())
            .map(|value| match value {
                false => Self::CombineUpdateBatches,
                true => Self::Exact,
            })
    }
}
