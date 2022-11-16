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
    Action, ActorInitContext, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};

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

// TODO: Remove QueryManager in favour of interacting with EvaluateHandler directly
#[derive(Named, Clone)]
pub struct QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: QueryManagerMetricNames,
    main_pid: ProcessId,
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
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct QueryManagerState<T: Expression> {
    // TODO: Use newtypes for state hashmap keys
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

dispatcher!({
    pub enum QueryManagerAction<T: Expression> {
        Inbox(QuerySubscribeAction<T>),
        Inbox(QueryUnsubscribeAction<T>),
        Inbox(EffectEmitAction<T>),

        Outbox(QueryEmitAction<T>),
        Outbox(EffectSubscribeAction<T>),
        Outbox(EffectUnsubscribeAction<T>),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for QueryManager<T, TFactory, TAllocator>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = QueryManagerState<T>;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (Default::default(), inbox, Default::default())
        }

        fn accept(&self, _action: &QuerySubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &QuerySubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &QuerySubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_query_subscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &QueryUnsubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &QueryUnsubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &QueryUnsubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_query_unsubscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &EffectEmitAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectEmitAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectEmitAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_emit(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator> QueryManager<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    fn handle_query_subscribe<TAction, TTask>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &QuerySubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectSubscribeAction<T>> + From<QueryEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QuerySubscribeAction { query, label } = action;
        let query_effect = create_query_evaluate_effect(
            label.clone(),
            query.clone(),
            &self.factory,
            &self.allocator,
        );
        match state.subscriptions.entry(query_effect.id()) {
            // For any queries that are already actively subscribed, emit the latest result if one exists
            // (this is necessary because the caller that triggered this action might be expecting a result)
            Entry::Occupied(mut entry) => {
                entry.get_mut().subscription_count += 1;
                let emit_existing_result_action = entry.get().result.as_ref().map(|result| {
                    SchedulerCommand::Send(
                        self.main_pid,
                        QueryEmitAction {
                            query: query.clone(),
                            result: result.clone(),
                        }
                        .into(),
                    )
                });
                Some(SchedulerTransition::new(emit_existing_result_action))
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
                let subscribe_action = SchedulerCommand::Send(
                    self.main_pid,
                    EffectSubscribeAction {
                        effect_type: String::from(EFFECT_TYPE_EVALUATE),
                        effects: vec![query_effect],
                    }
                    .into(),
                );
                Some(SchedulerTransition::new(once(subscribe_action)))
            }
        }
    }
    fn handle_query_unsubscribe<TAction, TTask>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &QueryUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let QueryUnsubscribeAction { query, label } = action;
        let query_effect = create_query_evaluate_effect(
            label.clone(),
            query.clone(),
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
        let unsubscribe_action = SchedulerCommand::Send(
            self.main_pid,
            EffectUnsubscribeAction {
                effect_type: String::from(EFFECT_TYPE_EVALUATE),
                effects: vec![subscription.effect],
            }
            .into(),
        );
        Some(SchedulerTransition::new(once(unsubscribe_action)))
    }
    fn handle_effect_emit<TAction, TTask>(
        &self,
        state: &mut QueryManagerState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<QueryEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectEmitAction {
            effect_types: updates,
        } = action;
        let updated_queries = {
            updates
                .iter()
                .filter(|batch| &batch.effect_type == EFFECT_TYPE_EVALUATE)
                .flat_map(|batch| batch.updates.iter())
                .filter_map(|(effect_id, update)| {
                    let subscription = state.subscriptions.get_mut(effect_id)?;
                    let result = parse_evaluate_effect_result(update, &self.factory)?;
                    subscription.result.replace(result.clone());
                    Some((subscription.query.clone(), result))
                })
        };
        let emit_actions = updated_queries
            .map(|(query, result)| {
                SchedulerCommand::Send(self.main_pid, QueryEmitAction { query, result }.into())
            })
            .collect::<Vec<_>>();
        Some(SchedulerTransition::new(emit_actions))
    }
}

pub fn create_query_evaluate_effect<T: Expression>(
    label: String,
    query: T,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T::Signal<T> {
    create_evaluate_effect(
        label,
        query,
        QueryEvaluationMode::Query,
        QueryInvalidationStrategy::default(),
        factory,
        allocator,
    )
}
