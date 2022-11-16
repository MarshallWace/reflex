// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
};

use reflex::{
    core::{
        ConditionListType, ConditionType, DependencyList, EvaluationResult, Expression,
        ExpressionFactory, ExpressionListType, HeapAllocator, RefType, SignalTermType, SignalType,
        StateToken,
    },
    hash::HashId,
};
use reflex_dispatcher::{
    Action, ActorInitContext, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_graphql::GraphQlOperation;
use reflex_json::JsonValue;
use reflex_macros::{dispatcher, Named};
use reflex_runtime::{
    action::{
        effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
        evaluate::{EvaluateResultAction, EvaluateStartAction, EvaluateStopAction},
    },
    actor::evaluate_handler::{create_evaluate_effect, parse_evaluate_effect_query},
    QueryEvaluationMode, QueryInvalidationStrategy,
};
use reflex_utils::partition_results;
use uuid::Uuid;

use crate::{
    server::action::{
        graphql_server::{
            GraphQlServerParseSuccessAction, GraphQlServerSubscribeAction,
            GraphQlServerUnsubscribeAction,
        },
        telemetry::{
            TelemetryMiddlewareTransactionEndAction, TelemetryMiddlewareTransactionStartAction,
            TelemetryTransaction,
        },
    },
    utils::{
        sanitize::sanitize_json_value,
        traceparent::{parse_traceparent, Traceparent},
    },
};

#[derive(Named, Clone)]
pub struct TelemetryMiddleware<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TTransactionLabels: Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
{
    factory: TFactory,
    allocator: TAllocator,
    get_graphql_query_label: TQueryLabel,
    get_operation_transaction_labels: TTransactionLabels,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
    TelemetryMiddleware<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TTransactionLabels: Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        get_graphql_query_label: TQueryLabel,
        get_operation_transaction_labels: TTransactionLabels,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory,
            allocator,
            get_graphql_query_label,
            get_operation_transaction_labels,
            main_pid,
            _expression: Default::default(),
        }
    }
}

pub struct TelemetryMiddlewareState<T: Expression> {
    active_queries: HashMap<Uuid, TelemetryMiddlewareQueryState>,
    active_workers: HashMap<StateToken, T::Signal<T>>,
    effect_mappings: HashMap<HashId, TelemetryMiddlewareEffectState<T>>,
}
impl<T: Expression> Default for TelemetryMiddlewareState<T> {
    fn default() -> Self {
        Self {
            active_queries: Default::default(),
            active_workers: Default::default(),
            effect_mappings: Default::default(),
        }
    }
}
struct TelemetryMiddlewareEffectState<T: Expression> {
    effect: T::Signal<T>,
    transaction_id: Option<Traceparent>,
    parent_transactions: HashSet<Traceparent>,
    subscription_count: usize,
    latest_value: Option<T>,
    query_result: Option<Option<EvaluationResult<T>>>,
}
impl<T: Expression> TelemetryMiddlewareEffectState<T> {
    fn end_transaction(&mut self) -> Option<Traceparent> {
        if let Some(transaction_id) = self.transaction_id.take() {
            self.parent_transactions.clear();
            Some(transaction_id)
        } else {
            None
        }
    }
}
struct TelemetryMiddlewareQueryState {
    label: String,
    transaction_id: Traceparent,
    effect_id: Option<HashId>,
}
impl TelemetryMiddlewareQueryState {
    fn generate_transaction_id(&self) -> Traceparent {
        self.transaction_id.generate_child()
    }
}

// TODO: support async update transactions
const EMIT_ASYNC_ASYNC_TRANSACTIONS: bool = false;

dispatcher!({
    pub enum TelemetryMiddlewareAction<T: Expression> {
        Inbox(GraphQlServerSubscribeAction<T>),
        Inbox(GraphQlServerUnsubscribeAction<T>),
        Inbox(GraphQlServerParseSuccessAction<T>),
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(EffectEmitAction<T>),
        Inbox(EvaluateStartAction<T>),
        Inbox(EvaluateResultAction<T>),
        Inbox(EvaluateStopAction),

        Outbox(TelemetryMiddlewareTransactionStartAction),
        Outbox(TelemetryMiddlewareTransactionEndAction),
    }

    impl<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels, TAction, TTask>
        Dispatcher<TAction, TTask>
        for TelemetryMiddleware<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
    where
        T: Expression,
        TFactory: ExpressionFactory<T>,
        TAllocator: HeapAllocator<T>,
        TQueryLabel: Fn(&GraphQlOperation) -> String,
        TTransactionLabels: Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = TelemetryMiddlewareState<T>;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
            context: &impl ActorInitContext,
        ) -> (Self::State, Self::Events<TInbox>, Self::Dispose) {
            (Default::default(), inbox, Default::default())
        }

        fn accept(&self, _action: &GraphQlServerSubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerSubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerSubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_subscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerUnsubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerUnsubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerUnsubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_unsubscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &GraphQlServerParseSuccessAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &GraphQlServerParseSuccessAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &GraphQlServerParseSuccessAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_graphql_parse(state, action, metadata, context)
        }

        fn accept(&self, _action: &EffectSubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectSubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectSubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_subscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &EffectUnsubscribeAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EffectUnsubscribeAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EffectUnsubscribeAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_effect_unsubscribe(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateStartAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateStartAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateStartAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_start(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateStopAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateStopAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateStopAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_stop(state, action, metadata, context)
        }

        fn accept(&self, _action: &EvaluateResultAction<T>) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &EvaluateResultAction<T>,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &EvaluateResultAction<T>,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_evaluate_result(state, action, metadata, context)
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

impl<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
    TelemetryMiddleware<T, TFactory, TAllocator, TQueryLabel, TTransactionLabels>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
    TQueryLabel: Fn(&GraphQlOperation) -> String,
    TTransactionLabels: Fn(&GraphQlOperation) -> (String, Vec<(String, String)>),
{
    fn handle_graphql_subscribe<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TelemetryMiddlewareTransactionStartAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerSubscribeAction {
            subscription_id,
            operation,
            ..
        } = action;
        let entry = match state.active_queries.entry(*subscription_id) {
            Entry::Vacant(entry) => Some(entry),
            Entry::Occupied(_) => None,
        }?;
        let traceparent = parse_graphql_operation_traceparent_extensions(operation);
        let transaction_id = traceparent
            .map(|transaction_id| transaction_id.generate_child())
            .unwrap_or_else(|| Traceparent::generate());
        let (transaction_name, transaction_attributes) =
            (self.get_operation_transaction_labels)(operation);
        entry.insert(TelemetryMiddlewareQueryState {
            transaction_id,
            label: (self.get_graphql_query_label)(operation),
            effect_id: None,
        });
        let transaction_start_action = SchedulerCommand::Send(
            self.main_pid,
            TelemetryMiddlewareTransactionStartAction {
                transactions: vec![TelemetryTransaction {
                    transaction_id,
                    parent_ids: traceparent.into_iter().collect(),
                    name: transaction_name,
                    attributes: transaction_attributes,
                }],
            }
            .into(),
        );
        Some(SchedulerTransition::new(once(transaction_start_action)))
    }
    fn handle_graphql_parse<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerParseSuccessAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerParseSuccessAction {
            subscription_id,
            query,
        } = action;
        let query_state = state.active_queries.get_mut(subscription_id)?;
        let evaluate_effect = create_evaluate_effect(
            query_state.label.clone(),
            query.clone(),
            QueryEvaluationMode::Query,
            QueryInvalidationStrategy::default(),
            &self.factory,
            &self.allocator,
        );
        let effect_id = evaluate_effect.id();
        let previous_effect_id = query_state.effect_id.replace(effect_id);
        let is_unchanged = previous_effect_id
            .map(|previous_effect_id| previous_effect_id == effect_id)
            .unwrap_or(false);
        if is_unchanged {
            return None;
        }
        if let Some(effect_id) = previous_effect_id {
            if let Some(effect_state) = state.effect_mappings.get_mut(&effect_id) {
                effect_state
                    .parent_transactions
                    .remove(&query_state.transaction_id);
            }
        }
        match state.effect_mappings.entry(effect_id) {
            Entry::Vacant(entry) => {
                entry.insert(TelemetryMiddlewareEffectState {
                    effect: evaluate_effect,
                    transaction_id: Some(query_state.generate_transaction_id()),
                    query_result: Some(None),
                    parent_transactions: once(query_state.transaction_id).collect(),
                    subscription_count: 0,
                    latest_value: None,
                });
            }
            Entry::Occupied(mut entry) => {
                entry
                    .get_mut()
                    .parent_transactions
                    .insert(query_state.transaction_id);
            }
        }
        None
    }
    fn handle_graphql_unsubscribe<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &GraphQlServerUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TelemetryMiddlewareTransactionEndAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let GraphQlServerUnsubscribeAction {
            subscription_id, ..
        } = action;
        let query_state = state.active_queries.remove(subscription_id)?;
        let TelemetryMiddlewareQueryState {
            transaction_id,
            effect_id,
            ..
        } = query_state;
        if let Some(effect_id) = effect_id {
            if let Some(effect_state) = state.effect_mappings.get_mut(&effect_id) {
                effect_state.parent_transactions.remove(&transaction_id);
            }
        }
        let transaction_end_action = SchedulerCommand::Send(
            self.main_pid,
            TelemetryMiddlewareTransactionEndAction {
                transaction_ids: vec![transaction_id],
            }
            .into(),
        );
        Some(SchedulerTransition::new(once(transaction_end_action)))
    }
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<TelemetryMiddlewareTransactionStartAction>
            + From<TelemetryMiddlewareTransactionEndAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let subscribed_transactions = effects
            .iter()
            .filter_map(|effect| match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(entry) => {
                    let transaction_id = Traceparent::generate();
                    let effect_name = format_effect_label(effect);
                    let effect_attributes = format_effect_attributes(effect);
                    entry.insert(TelemetryMiddlewareEffectState {
                        effect: effect.clone(),
                        transaction_id: Some(transaction_id),
                        parent_transactions: Default::default(),
                        query_result: parse_evaluate_effect_query(effect, &self.factory)
                            .map(|_| None),
                        subscription_count: 1,
                        latest_value: None,
                    });
                    Some(TelemetryTransaction {
                        transaction_id,
                        parent_ids: Default::default(),
                        name: effect_name,
                        attributes: effect_attributes,
                    })
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().subscription_count += 1;
                    let effect_state = entry.get();
                    let is_first_subscription = effect_state.subscription_count == 1;
                    if is_first_subscription {
                        match effect_state.transaction_id {
                            Some(transaction_id) => Some(TelemetryTransaction {
                                transaction_id,
                                parent_ids: effect_state
                                    .parent_transactions
                                    .iter()
                                    .cloned()
                                    .collect(),
                                name: format_effect_label(effect),
                                attributes: format_effect_attributes(effect),
                            }),
                            None => None,
                        }
                    } else {
                        None
                    }
                }
            })
            .collect::<Vec<_>>();
        let transaction_start_action = if subscribed_transactions.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                TelemetryMiddlewareTransactionStartAction {
                    transactions: subscribed_transactions,
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(transaction_start_action))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<TelemetryMiddlewareTransactionEndAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let unsubscribed_transaction_ids = effects
            .iter()
            .filter_map(|effect| match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(_) => None,
                Entry::Occupied(mut entry) => {
                    entry.get_mut().subscription_count -= 1;
                    if entry.get().subscription_count == 0 {
                        let effect_state = entry.remove();
                        effect_state.transaction_id
                    } else {
                        None
                    }
                }
            })
            .collect::<Vec<_>>();
        let transaction_end_action = if unsubscribed_transaction_ids.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: unsubscribed_transaction_ids,
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(transaction_end_action))
    }
    fn handle_evaluate_start<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EvaluateStartAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateStartAction {
            cache_id,
            query,
            label,
            evaluation_mode,
            invalidation_strategy,
        } = action;
        match state.active_workers.entry(*cache_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                entry.insert(create_evaluate_effect(
                    label.clone(),
                    query.clone(),
                    *evaluation_mode,
                    *invalidation_strategy,
                    &self.factory,
                    &self.allocator,
                ));
                None
            }
        }
    }
    fn handle_evaluate_stop<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EvaluateStopAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateStopAction { cache_id } = action;
        state.active_workers.remove(cache_id);
        None
    }
    fn handle_evaluate_result<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EvaluateResultAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EvaluateResultAction {
            cache_id, result, ..
        } = action;
        let evaluate_effect = state.active_workers.get(cache_id)?;
        let (query_transaction_id, previous_result) = state
            .effect_mappings
            .get_mut(&evaluate_effect.id())
            .map(|query_effect_state| {
                let previous_result = query_effect_state
                    .query_result
                    .replace(Some(result.clone()));
                (query_effect_state.transaction_id, previous_result)
            })?;
        for effect in get_query_result_effects(result.result(), &self.factory) {
            match state.effect_mappings.entry(effect.id()) {
                Entry::Vacant(entry) => {
                    let effect_transaction_id = query_transaction_id
                        .map(|transaction_id| transaction_id.generate_child())
                        .unwrap_or_else(|| Traceparent::generate());
                    entry.insert(TelemetryMiddlewareEffectState {
                        effect: effect.clone(),
                        transaction_id: Some(effect_transaction_id),
                        parent_transactions: query_transaction_id.into_iter().collect(),
                        query_result: parse_evaluate_effect_query(effect, &self.factory)
                            .map(|_| None),
                        subscription_count: 0,
                        latest_value: None,
                    });
                }
                Entry::Occupied(mut entry) => {
                    let effect_state = entry.get_mut();
                    if let Some(query_transaction_id) = query_transaction_id {
                        effect_state
                            .parent_transactions
                            .insert(query_transaction_id);
                    }
                }
            }
        }
        let transaction_id = query_transaction_id?;
        let previous_dependencies = previous_result.and_then(|result| {
            result.map(|result| {
                let (_, dependencies) = result.into_parts();
                dependencies
            })
        });
        let Diff {
            added: added_dependencies,
            removed: removed_dependencies,
        } = get_dependency_diff(Some(result.dependencies()), previous_dependencies.as_ref());
        for state_token in removed_dependencies {
            if let Some(effect_state) = state.effect_mappings.get_mut(&state_token) {
                effect_state.parent_transactions.remove(&transaction_id);
            }
        }
        for state_token in added_dependencies {
            if let Some(effect_state) = state.effect_mappings.get_mut(&state_token) {
                effect_state.parent_transactions.insert(transaction_id);
            }
        }
        None
    }
    fn handle_effect_emit<TAction, TTask>(
        &self,
        state: &mut TelemetryMiddlewareState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action
            + From<TelemetryMiddlewareTransactionStartAction>
            + From<TelemetryMiddlewareTransactionEndAction>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectEmitAction {
            effect_types: updates,
        } = action;
        let (completed_transaction_ids, reemitted_effects): (Vec<_>, Vec<_>) = partition_results(
            updates
                .iter()
                .flat_map(|batch| batch.updates.iter())
                .filter_map(|(effect_id, update)| {
                    let update = state
                        .effect_mappings
                        .get_mut(effect_id)
                        .map(|effect_state| {
                            effect_state.latest_value.replace(update.clone());
                            if effect_state.query_result.is_some() {
                                let completed_transaction_id = effect_state.end_transaction();
                                Ok(completed_transaction_id)
                            } else {
                                match effect_state.end_transaction() {
                                    Some(completed_transaction_id) => {
                                        Ok(Some(completed_transaction_id))
                                    }
                                    None => Err(effect_state.effect.clone()),
                                }
                            }
                        })?;
                    match update {
                        Err(effect) => Some(Err(effect)),
                        Ok(completed_transaction_id) => match completed_transaction_id {
                            None => None,
                            Some(completed_transaction_id) => {
                                for effect_state in state.effect_mappings.values_mut() {
                                    effect_state
                                        .parent_transactions
                                        .remove(&completed_transaction_id);
                                }
                                Some(Ok(completed_transaction_id))
                            }
                        },
                    }
                }),
        );
        let completed_transactions_end_action = if completed_transaction_ids.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: completed_transaction_ids,
                }
                .into(),
            ))
        };
        let (effect_start_transactions, effect_end_transaction_ids) = if reemitted_effects
            .is_empty()
            || !EMIT_ASYNC_ASYNC_TRANSACTIONS
        {
            (Vec::new(), Vec::new())
        } else {
            let update_transaction_id = Traceparent::generate();
            let update_transaction = TelemetryTransaction {
                transaction_id: update_transaction_id,
                parent_ids: Default::default(),
                name: format_async_update_batch_label(reemitted_effects.len()),
                attributes: Default::default(),
            };
            let (updated_effect_transaction_ids, effect_transactions): (Vec<_>, Vec<_>) =
                reemitted_effects
                    .into_iter()
                    .map(move |effect| {
                        let effect_transaction_id = update_transaction_id.generate_child();
                        (
                            (effect.id(), effect_transaction_id),
                            TelemetryTransaction {
                                transaction_id: effect_transaction_id,
                                parent_ids: once(update_transaction_id).collect(),
                                name: format_effect_label(&effect),
                                attributes: format_effect_attributes(&effect),
                            },
                        )
                    })
                    .unzip();
            let dependent_query_transactions =
                state.active_queries.values().filter_map(|query_state| {
                    let query_effect_id = query_state.effect_id?;
                    let query_effect_state = state.effect_mappings.get_mut(&query_effect_id)?;
                    let latest_query_result = query_effect_state.query_result.as_ref()?.as_ref()?;
                    let query_dependencies = latest_query_result.dependencies();
                    if query_dependencies.is_empty() {
                        return None;
                    }
                    // TODO: [perf] Investigate less brute-force approaches for determining affected queries
                    let updated_dependency_ids = updated_effect_transaction_ids
                        .iter()
                        .filter_map(|(effect_id, transaction_id)| {
                            if query_dependencies.contains(*effect_id) {
                                Some(*transaction_id)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    query_effect_state
                        .parent_transactions
                        .extend(updated_dependency_ids.iter().copied());
                    if query_effect_state.transaction_id.is_none() {
                        let query_transaction_id = updated_dependency_ids.first()?.generate_child();
                        query_effect_state.transaction_id = Some(query_transaction_id);
                        Some(TelemetryTransaction {
                            transaction_id: query_transaction_id,
                            parent_ids: updated_dependency_ids,
                            name: format_effect_label(&query_effect_state.effect),
                            attributes: format_effect_attributes(&query_effect_state.effect),
                        })
                    } else {
                        return None;
                    }
                });
            let end_transaction_ids = effect_transactions
                .iter()
                .chain(once(&update_transaction))
                .map(|transaction| transaction.transaction_id)
                .collect::<Vec<_>>();
            let start_transactions = once(update_transaction)
                .chain(effect_transactions)
                .chain(dependent_query_transactions)
                .collect::<Vec<_>>();
            (start_transactions, end_transaction_ids)
        };
        let effect_transaction_start_action = if effect_start_transactions.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                TelemetryMiddlewareTransactionStartAction {
                    transactions: effect_start_transactions,
                }
                .into(),
            ))
        };
        // TODO: Defer completion of effect update transaction until all dependent queries have either emitted a result or removed current effect from dependencies
        let effect_transaction_end_action = if effect_end_transaction_ids.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                TelemetryMiddlewareTransactionEndAction {
                    transaction_ids: effect_end_transaction_ids,
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            completed_transactions_end_action
                .into_iter()
                .chain(effect_transaction_start_action)
                .chain(effect_transaction_end_action),
        ))
    }
}

fn get_query_result_effects<'a, T: Expression>(
    result: &'a T,
    factory: &impl ExpressionFactory<T>,
) -> impl Iterator<Item = &'a T::Signal<T>> + 'a {
    factory
        .match_signal_term(result)
        .map(|term| {
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .filter(|signal| matches!(signal.signal_type(), SignalType::Custom(_)))
        })
        .into_iter()
        .flatten()
}

struct Diff<T1, T2> {
    added: T1,
    removed: T2,
}
fn get_dependency_diff<'a>(
    current_dependencies: Option<&'a DependencyList>,
    previous_dependencies: Option<&'a DependencyList>,
) -> Diff<impl IntoIterator<Item = StateToken> + 'a, impl IntoIterator<Item = StateToken> + 'a> {
    let added_dependencies = {
        let (left, right) = match (previous_dependencies, current_dependencies) {
            (None, Some(current_dependencies)) => (Some(current_dependencies), None),
            (Some(previous_dependencies), Some(current_dependencies)) => (
                None,
                Some(
                    current_dependencies
                        .iter()
                        .filter(|state_token| !previous_dependencies.contains(*state_token)),
                ),
            ),
            _ => (None, None),
        };
        left.into_iter()
            .flatten()
            .chain(right.into_iter().flatten())
    };
    let removed_dependencies = {
        let (left, right) = match (previous_dependencies, current_dependencies) {
            (Some(previous_dependencies), None) => (Some(previous_dependencies), None),
            (Some(previous_dependencies), Some(current_dependencies)) => (
                None,
                Some(
                    previous_dependencies
                        .iter()
                        .filter(|state_token| !current_dependencies.contains(*state_token)),
                ),
            ),
            _ => (None, None),
        };
        left.into_iter()
            .flatten()
            .chain(right.into_iter().flatten())
    };
    Diff {
        added: added_dependencies,
        removed: removed_dependencies,
    }
}

fn parse_graphql_operation_traceparent_extensions(
    operation: &GraphQlOperation,
) -> Option<Traceparent> {
    match operation.extension("traceparent") {
        Some(JsonValue::String(value)) => parse_traceparent(value.as_str()),
        _ => None,
    }
}

fn format_effect_label<T: Expression<Signal<T> = V>, V: ConditionType<T>>(effect: &V) -> String {
    match effect.signal_type() {
        SignalType::Custom(signal_type) => format!("{}", signal_type),
        signal_type => format!("{}", signal_type),
    }
}

fn format_async_update_batch_label(batch_size: usize) -> String {
    if batch_size > 1 {
        format!("Async update x{}", batch_size)
    } else {
        String::from("Async update")
    }
}

fn format_effect_attributes<T: Expression<Signal<T> = V>, V: ConditionType<T>>(
    effect: &V,
) -> Vec<(String, String)> {
    vec![
        (String::from("effect.id"), format!("{}", effect.id())),
        (String::from("effect.type"), format_effect_label(effect)),
        (
            String::from("effect.args"),
            sanitize_json_value(JsonValue::Array(
                effect
                    .args()
                    .as_deref()
                    .iter()
                    .map(|item| item.as_deref())
                    .map(|arg| {
                        reflex_json::sanitize(arg).unwrap_or_else(|_| {
                            JsonValue::String(format!("<expression:{}>", arg.id()))
                        })
                    })
                    .collect(),
            ))
            .to_string(),
        ),
    ]
}
