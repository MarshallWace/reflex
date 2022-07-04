// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use reflex::core::{Expression, ExpressionFactory, HeapAllocator, StringValue};
use serde::{Deserialize, Serialize};

pub mod action;
pub mod actor;
pub mod worker;

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

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum QueryEvaluationMode {
    /// Evaluate the expression as a query function to be applied to the graph root
    Query,
    /// Evaluate the expression as a standalone expression unrelated to the graph root
    Standalone,
}
impl QueryEvaluationMode {
    pub(crate) fn serialize<T: Expression>(&self, factory: &impl ExpressionFactory<T>) -> T {
        factory.create_boolean_term(match self {
            Self::Query => false,
            Self::Standalone => true,
        })
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_boolean_term(value)
            .map(|term| match term.value {
                false => Self::Query,
                true => Self::Standalone,
            })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
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
        factory.create_boolean_term(match self {
            Self::CombineUpdateBatches => false,
            Self::Exact => true,
        })
    }
    pub(crate) fn deserialize<T: Expression>(
        value: &T,
        factory: &impl ExpressionFactory<T>,
    ) -> Option<Self> {
        factory
            .match_boolean_term(value)
            .map(|term| match term.value {
                false => Self::CombineUpdateBatches,
                true => Self::Exact,
            })
    }
}
