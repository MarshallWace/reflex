// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::time::Duration;

use actor::{
    evaluate_handler::EvaluateHandler, query_manager::QueryManager, RuntimeActor,
    RuntimeMetricNames,
};
use reflex::core::{BooleanTermType, Expression, ExpressionFactory, HeapAllocator};
use reflex_dispatcher::ProcessId;
use reflex_macros::blanket_trait;
use serde::{Deserialize, Serialize};

pub mod action;
pub mod actor;
pub mod task;
pub mod utils;

blanket_trait!(
    pub trait AsyncExpression: Expression + Send + 'static {}
);
blanket_trait!(
    pub trait AsyncExpressionFactory<T: AsyncExpression>:
        ExpressionFactory<T> + Send + Clone + 'static
    {
    }
);
blanket_trait!(
    pub trait AsyncHeapAllocator<T: AsyncExpression>:
        HeapAllocator<T> + Send + Clone + 'static
    {
    }
);

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
            .map(|term| match term.value() {
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
            .map(|term| match term.value() {
                false => Self::CombineUpdateBatches,
                true => Self::Exact,
            })
    }
}

pub fn runtime_actors<T, TFactory, TAllocator>(
    factory: TFactory,
    allocator: TAllocator,
    effect_throttle: Option<Duration>,
    metric_names: RuntimeMetricNames,
    main_pid: ProcessId,
) -> impl IntoIterator<Item = RuntimeActor<T, TFactory, TAllocator>>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
{
    [
        RuntimeActor::QueryManager(QueryManager::new(
            factory.clone(),
            allocator.clone(),
            metric_names.query_manager,
            main_pid,
        )),
        RuntimeActor::EvaluateHandler(EvaluateHandler::new(
            factory,
            allocator,
            effect_throttle,
            metric_names.evaluate_handler,
            main_pid,
        )),
    ]
}
