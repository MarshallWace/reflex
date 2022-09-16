// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
};

use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::core::{
    ConditionType, EvaluationResult, Expression, ExpressionFactory, HeapAllocator, StateToken,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};

use crate::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        query::{QueryEmitAction, QuerySubscribeAction, QueryUnsubscribeAction},
    },
    actor::evaluate_handler::{
        create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
    },
    QueryEvaluationMode, QueryInvalidationStrategy,
};

#[derive(Clone, Copy, Debug)]
pub struct QueryManagerMetricNames {
    pub active_query_count: &'static str,
}
impl QueryManagerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(self.active_query_count, Unit::Count, "Active query count");
        self
    }
}
impl Default for QueryManagerMetricNames {
    fn default() -> Self {
        Self {
            active_query_count: "active_query_count",
        }
    }
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

pub struct QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: QueryManagerMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub(crate) fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: QueryManagerMetricNames,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

pub struct QueryManagerState<T: Expression> {
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
    effect: T::Signal<T>,
    result: Option<EvaluationResult<T>>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TAction: QueryManagerAction<T>,
{
    type State = QueryManagerState<T>;
    fn init(&self) -> Self::State {
        Default::default()
    }
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction> {
        // FIXME: remove QueryManager in favour of EvaluateHandler
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_query_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_query_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator> QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_query_subscribe<TAction>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &QuerySubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction:
            Action + OutboundAction<EffectSubscribeAction<T>> + OutboundAction<QueryEmitAction<T>>,
    {
        let QuerySubscribeAction { query, label } = action;
        let query_effect = create_evaluate_effect(
            label.clone(),
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        match state.subscriptions.entry(query_effect.id()) {
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
                Some(StateTransition::new(emit_existing_result_action))
            }
            // For any queries that are not yet actively subscribed, create a new subscription
            Entry::Vacant(entry) => {
                entry.insert(QuerySubscription {
                    query: query.clone(),
                    effect: query_effect.clone(),
                    result: None,
                    subscription_count: 1,
                });
                increment_gauge!(self.metric_names.active_query_count, 1.0);
                let subscribe_action = StateOperation::Send(
                    context.pid(),
                    EffectSubscribeAction {
                        effect_type: String::from(EFFECT_TYPE_EVALUATE),
                        effects: vec![query_effect],
                    }
                    .into(),
                );
                Some(StateTransition::new(once(subscribe_action)))
            }
        }
    }
    fn handle_query_unsubscribe<TAction>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &QueryUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        let QueryUnsubscribeAction { query, label } = action;
        let query_effect = create_evaluate_effect(
            label.clone(),
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        let mut entry = match state.subscriptions.entry(query_effect.id()) {
            Entry::Vacant(_) => None,
            Entry::Occupied(entry) => Some(entry),
        }?;
        entry.get_mut().subscription_count -= 1;
        if entry.get().subscription_count != 0 {
            return None;
        }
        let subscription = entry.remove();
        decrement_gauge!(self.metric_names.active_query_count, 1.0);
        let unsubscribe_action = StateOperation::Send(
            context.pid(),
            EffectUnsubscribeAction {
                effect_type: String::from(EFFECT_TYPE_EVALUATE),
                effects: vec![subscription.effect],
            }
            .into(),
        );
        Some(StateTransition::new(once(unsubscribe_action)))
    }
    fn handle_effect_emit<TAction>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<QueryEmitAction<T>>,
    {
        let EffectEmitAction { updates } = action;
        let updated_queries = {
            updates.iter().filter_map(|(state_token, update)| {
                let subscription = state.subscriptions.get_mut(state_token)?;
                let result = parse_evaluate_effect_result(update, &self.factory)?;
                subscription.result.replace(result.clone());
                Some((subscription.query.clone(), result))
            })
        };
        let emit_actions = updated_queries
            .map(|(query, result)| {
                StateOperation::Send(context.pid(), QueryEmitAction { query, result }.into())
            })
            .collect::<Vec<_>>();
        Some(StateTransition::new(emit_actions))
    }
}
