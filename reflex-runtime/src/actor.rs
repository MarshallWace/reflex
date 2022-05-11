// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::core::{Expression, ExpressionFactory, HeapAllocator};
use reflex_dispatcher::{
    compose_actors, Action, Actor, ChainedActor, HandlerContext, MessageData, StateTransition,
};

pub mod bytecode_interpreter;
pub mod evaluate_handler;
pub mod query_manager;

use self::evaluate_handler::*;
use self::query_manager::*;

#[derive(Default, Clone, Copy, Debug)]
pub struct RuntimeMetricNames {
    pub query_manager: QueryManagerMetricNames,
    pub evaluate_handler: EvaluateHandlerMetricNames,
}

pub trait RuntimeAction<T: Expression>:
    Action + QueryManagerAction<T> + EvaluateHandlerAction<T>
{
}
impl<T: Expression, TAction> RuntimeAction<T> for TAction where
    Self: Action + QueryManagerAction<T> + EvaluateHandlerAction<T>
{
}

pub struct RuntimeActor<
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TAction: RuntimeAction<T>,
> {
    inner: ChainedActor<
        TAction,
        QueryManager<T, TFactory, TAllocator>,
        EvaluateHandler<T, TFactory, TAllocator>,
    >,
}
impl<T, TFactory, TAllocator, TAction> RuntimeActor<T, TFactory, TAllocator, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TAction: RuntimeAction<T>,
{
    pub fn new(factory: TFactory, allocator: TAllocator, metric_names: RuntimeMetricNames) -> Self {
        Self {
            inner: compose_actors(
                QueryManager::new(
                    factory.clone(),
                    allocator.clone(),
                    metric_names.query_manager,
                ),
                EvaluateHandler::new(factory, allocator, metric_names.evaluate_handler),
            ),
        }
    }
}
impl<T, TFactory, TAllocator, TAction> Actor<TAction>
    for RuntimeActor<T, TFactory, TAllocator, TAction>
where
    T: Expression,
    TFactory: ExpressionFactory<T> + Clone,
    TAllocator: HeapAllocator<T> + Clone,
    TAction: RuntimeAction<T>,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        self.inner.handle(action, metadata, context)
    }
}
