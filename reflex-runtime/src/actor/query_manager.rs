// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    marker::PhantomData,
    sync::Once,
};

use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::core::{
    EvaluationResult, Expression, ExpressionFactory, HeapAllocator, Signal, StateToken,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OutboundAction, StateOperation,
    StateTransition,
};

use crate::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        query::{QueryEmitAction, QuerySubscribeAction, QueryUnsubscribeAction},
    },
    actor::evaluate_handler::{
        create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
    },
    QueryEvaluationMode, QueryInvalidationStrategy, StateUpdate,
};

pub const METRIC_ACTIVE_QUERY_COUNT: &'static str = "active_query_count";

static INIT_METRICS: Once = Once::new();

fn init_metrics() {
    INIT_METRICS.call_once(|| {
        describe_gauge!(METRIC_ACTIVE_QUERY_COUNT, Unit::Count, "Active query count");
    });
}

pub trait QueryManagerAction<T: Expression>:
    Action
    + InboundAction<QuerySubscribeAction<T>>
    + InboundAction<QueryUnsubscribeAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + OutboundAction<QueryEmitAction<T>>
    + OutboundAction<EffectSubscribeAction<T>>
    + OutboundAction<EffectUnsubscribeAction<T>>
{
}
impl<T: Expression, TAction> QueryManagerAction<T> for TAction where
    Self: Action
        + InboundAction<QuerySubscribeAction<T>>
        + InboundAction<QueryUnsubscribeAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + OutboundAction<QueryEmitAction<T>>
        + OutboundAction<EffectSubscribeAction<T>>
        + OutboundAction<EffectUnsubscribeAction<T>>
{
}

pub(crate) struct QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    state: QueryManagerState<T>,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub(crate) fn new(factory: TFactory, allocator: TAllocator) -> Self {
        init_metrics();
        Self {
            factory,
            allocator,
            state: Default::default(),
            _expression: Default::default(),
        }
    }
}

struct QueryManagerState<T: Expression> {
    subscriptions: HashMap<StateToken, QuerySubscription<T>>,
}
impl<T: Expression> Default for QueryManagerState<T> {
    fn default() -> Self {
        Self {
            subscriptions: Default::default(),
        }
    }
}
struct QuerySubscription<T: Expression> {
    subscription_count: usize,
    query: T,
    effect: Signal<T>,
    result: Option<EvaluationResult<T>>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: QueryManagerAction<T>,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_query_subscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_query_unsubscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(action, metadata, context)
        } else {
            StateTransition::new(None)
        }
    }
}
impl<T, TFactory, TAllocator> QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_query_subscribe<TAction>(
        &mut self,
        action: &QuerySubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction:
            Action + OutboundAction<EffectSubscribeAction<T>> + OutboundAction<QueryEmitAction<T>>,
    {
        let QuerySubscribeAction { query } = action;
        let query_effect = create_evaluate_effect(
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        match self.state.subscriptions.entry(query_effect.id()) {
            // For any queries that are already actively subscribed, emit the latest result if one exists
            // (this is necessary because the caller that triggered this action might be expecting a result)
            Entry::Occupied(mut entry) => {
                entry.get_mut().subscription_count += 1;
                let emit_existing_result_action = entry.get().result.as_ref().map(|result| {
                    StateOperation::Send(
                        context.pid(),
                        QueryEmitAction {
                            query: query.clone(),
                            result: result.clone(),
                        }
                        .into(),
                    )
                });
                StateTransition::new(emit_existing_result_action)
            }
            // For any queries that are not yet actively subscribed, create a new subscription
            Entry::Vacant(entry) => {
                entry.insert(QuerySubscription {
                    query: query.clone(),
                    effect: query_effect.clone(),
                    result: None,
                    subscription_count: 1,
                });
                increment_gauge!(METRIC_ACTIVE_QUERY_COUNT, 1.0);
                StateTransition::new(Some(StateOperation::Send(
                    context.pid(),
                    EffectSubscribeAction {
                        effect_type: String::from(EFFECT_TYPE_EVALUATE),
                        effects: vec![query_effect],
                    }
                    .into(),
                )))
            }
        }
    }
    fn handle_query_unsubscribe<TAction>(
        &mut self,
        action: &QueryUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        let QueryUnsubscribeAction { query } = action;
        let query_effect = create_evaluate_effect(
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        match self.state.subscriptions.entry(query_effect.id()) {
            Entry::Vacant(_) => StateTransition::new(None),
            Entry::Occupied(mut entry) => {
                entry.get_mut().subscription_count -= 1;
                if entry.get().subscription_count == 0 {
                    let subscription = entry.remove();
                    decrement_gauge!(METRIC_ACTIVE_QUERY_COUNT, 1.0);
                    StateTransition::new(Some(StateOperation::Send(
                        context.pid(),
                        EffectUnsubscribeAction {
                            effect_type: String::from(EFFECT_TYPE_EVALUATE),
                            effects: vec![subscription.effect],
                        }
                        .into(),
                    )))
                } else {
                    StateTransition::new(None)
                }
            }
        }
    }
    fn handle_effect_emit<TAction>(
        &mut self,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + OutboundAction<QueryEmitAction<T>>,
    {
        let EffectEmitAction { updates } = action;
        let updated_queries = {
            updates.iter().filter_map(|(state_token, update)| {
                let subscription = self.state.subscriptions.get_mut(state_token)?;
                let effect_result = match update {
                    StateUpdate::Value(value) => value.clone(),
                    StateUpdate::Patch(updater) => {
                        updater(subscription.result.as_ref().map(|result| result.result()))
                    }
                };
                let result = parse_evaluate_effect_result(&effect_result, &self.factory)?;
                subscription.result.replace(result.clone());
                Some((subscription.query.clone(), result))
            })
        };
        let actions = updated_queries
            .map(|(query, result)| {
                StateOperation::Send(context.pid(), QueryEmitAction { query, result }.into())
            })
            .collect::<Vec<_>>();
        StateTransition::new(actions)
    }
}
