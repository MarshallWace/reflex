// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
};

use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::{
    core::{
        Applicable, Arity, ConditionListType, ConditionType, Expression, ExpressionFactory,
        ExpressionListType, HashmapTermType, HeapAllocator, ListTermType, RefType, SignalTermType,
        SignalType, StateToken, StringTermType, StringValue,
    },
    hash::HashId,
};
use reflex_dispatcher::{
    Action, ActorEvents, HandlerContext, MessageData, NoopDisposeCallback, ProcessId,
    SchedulerCommand, SchedulerMode, SchedulerTransition, TaskFactory, TaskInbox,
};
use reflex_macros::{dispatcher, Named};
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction, EffectUpdateBatch,
    },
    actor::evaluate_handler::{
        create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy,
};

pub const EFFECT_TYPE_LOADER: &'static str = "reflex::loader";

#[derive(Clone, Copy, Debug)]
pub struct LoaderHandlerMetricNames {
    pub loader_effect_entity_count: &'static str,
}
impl LoaderHandlerMetricNames {
    fn init(self) -> Self {
        describe_gauge!(
            self.loader_effect_entity_count,
            Unit::Count,
            "Active loader entity count"
        );

        self
    }
}
impl Default for LoaderHandlerMetricNames {
    fn default() -> Self {
        Self {
            loader_effect_entity_count: "loader_effect_entity_count",
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
struct LoaderFactoryHash(HashId);
impl LoaderFactoryHash {
    fn new<T: Expression>(keys: &T) -> Self {
        Self(keys.id())
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
struct LoaderKeyHash(HashId);
impl LoaderKeyHash {
    fn new<T: Expression>(key: &T) -> Self {
        Self(key.id())
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
struct LoaderBatchHash(HashId);
impl LoaderBatchHash {
    fn new<T: Expression<ExpressionList<T> = TList>, TList: ExpressionListType<T>>(
        keys: &TList,
    ) -> Self {
        Self(keys.id())
    }
}

#[derive(Named, Clone)]
pub struct LoaderHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: LoaderHandlerMetricNames,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> LoaderHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: LoaderHandlerMetricNames,
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

pub struct LoaderHandlerState<T: Expression> {
    loaders: HashMap<LoaderFactoryHash, LoaderState<T>>,
    /// Maps the combined loader batch evaluate effect ID to the loader expression (used as the key to the loaders hashmap)
    loader_effect_mappings: HashMap<StateToken, T>,
}

struct LoaderState<T: Expression> {
    name: String,
    active_keys: HashSet<LoaderKeyHash>,
    /// Maps keylists to the corresponding loader batch
    active_batches: HashMap<LoaderBatchHash, LoaderBatch<T>>,
    /// Maps the individual entity load effect IDs to the keylist of the subscribed batch
    entity_effect_mappings: HashMap<StateToken, T::ExpressionList<T>>,
    /// Maps the combined loader batch evaluate effect ID to the keylist of the subscribed batch
    batch_effect_mappings: HashMap<StateToken, T::ExpressionList<T>>,
}

struct LoaderBatch<T: Expression> {
    /// Evaluate effect used to load the batch
    effect: T::Signal<T>,
    /// List of entries for all the individual entities contained within this batch
    subscriptions: Vec<LoaderEntitySubscription<T>>,
    /// Maintain a list of which keys are actively subscribed - when this becomes empty the batch is unsubscribed
    active_keys: HashSet<LoaderKeyHash>,
    latest_result: Option<T>,
}

struct LoaderEntitySubscription<T: Expression> {
    key: T,
    effect: T::Signal<T>,
}

impl<T: Expression> Default for LoaderHandlerState<T> {
    fn default() -> Self {
        Self {
            loaders: Default::default(),
            loader_effect_mappings: Default::default(),
        }
    }
}
impl<T: Expression> LoaderState<T> {
    fn new(name: String) -> Self {
        Self {
            name,
            active_batches: Default::default(),
            active_keys: Default::default(),
            entity_effect_mappings: Default::default(),
            batch_effect_mappings: Default::default(),
        }
    }
}

impl<T: Expression> LoaderHandlerState<T> {
    fn subscribe(
        &mut self,
        name: String,
        loader: T,
        keys: impl IntoIterator<Item = LoaderEntitySubscription<T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        metric_names: LoaderHandlerMetricNames,
    ) -> impl IntoIterator<Item = T::Signal<T>> {
        let loader_state = match self.loaders.entry(LoaderFactoryHash::new(&loader)) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(LoaderState::new(name.clone())),
        };
        let (keys, effects): (Vec<_>, Vec<_>) = keys
            .into_iter()
            .filter_map(|subscription| {
                let LoaderEntitySubscription { key, effect } = subscription;
                if loader_state.active_keys.contains(&LoaderKeyHash::new(&key)) {
                    None
                } else {
                    Some((key, effect))
                }
            })
            .unzip();
        let keys = allocator.create_list(keys);
        let combined_effect = create_load_batch_effect(
            name.clone(),
            loader.clone(),
            keys.clone(),
            factory,
            allocator,
        );
        let loader_batch = LoaderBatch {
            effect: combined_effect.clone(),
            subscriptions: keys
                .iter()
                .map(|item| item.as_deref())
                .enumerate()
                .filter_map(|(index, key)| {
                    effects
                        .get(index)
                        .map(|effect| (key.clone(), effect.clone()))
                })
                .map(|(key, effect)| LoaderEntitySubscription { key, effect })
                .collect(),
            active_keys: keys
                .iter()
                .map(|item| item.as_deref())
                .map(LoaderKeyHash::new)
                .collect(),
            latest_result: None,
        };
        let num_previous_keys = loader_state.active_keys.len();
        loader_state.active_keys.extend(
            keys.iter()
                .map(|item| item.as_deref())
                .map(LoaderKeyHash::new),
        );
        let num_added_keys = loader_state.active_keys.len() - num_previous_keys;
        let metric_labels = [("loader_name", name)];
        increment_gauge!(
            metric_names.loader_effect_entity_count,
            num_added_keys as f64,
            &metric_labels
        );
        loader_state
            .active_batches
            .insert(LoaderBatchHash::new(&keys), loader_batch);
        loader_state
            .entity_effect_mappings
            .extend(effects.iter().map(|effect| (effect.id(), keys.clone())));
        self.loader_effect_mappings
            .insert(combined_effect.id(), loader);
        loader_state
            .batch_effect_mappings
            .insert(combined_effect.id(), keys);
        Some(combined_effect)
    }
    fn unsubscribe<'a>(
        &mut self,
        name: String,
        loader: T,
        subscriptions: impl IntoIterator<
            Item = LoaderEntitySubscription<T>,
            IntoIter = impl Iterator<Item = LoaderEntitySubscription<T>> + 'a,
        >,
        metric_names: LoaderHandlerMetricNames,
    ) -> impl IntoIterator<Item = T::Signal<T>> {
        let (num_previous_keys, unsubscribed_effects) =
            match self.loaders.get_mut(&LoaderFactoryHash::new(&loader)) {
                None => (None, None),
                Some(loader_state) => {
                    let num_previous_keys = loader_state.active_keys.len();
                    let unsubscribed_effects =
                        subscriptions.into_iter().filter_map(|subscription| {
                            let LoaderEntitySubscription { key, effect } = subscription;
                            let loader_batch_keys =
                                loader_state.entity_effect_mappings.remove(&effect.id())?;
                            loader_state.active_keys.remove(&LoaderKeyHash::new(&key));
                            let loader_batch = loader_state
                                .active_batches
                                .get_mut(&LoaderBatchHash::new(&loader_batch_keys))?;
                            loader_batch.active_keys.remove(&LoaderKeyHash::new(&key));
                            if loader_batch.active_keys.is_empty() {
                                let loader_batch = loader_state
                                    .active_batches
                                    .remove(&LoaderBatchHash::new(&loader_batch_keys))?;
                                let combined_effect = loader_batch.effect;
                                loader_state
                                    .batch_effect_mappings
                                    .remove(&combined_effect.id());
                                self.loader_effect_mappings.remove(&combined_effect.id());
                                Some(combined_effect)
                            } else {
                                None
                            }
                        });
                    (Some(num_previous_keys), Some(unsubscribed_effects))
                }
            };
        let unsubscribed_effects = unsubscribed_effects
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let num_removed_keys = num_previous_keys.and_then(|num_previous_keys| {
            self.loaders
                .get(&LoaderFactoryHash::new(&loader))
                .map(|loader_state| num_previous_keys - loader_state.active_keys.len())
        });
        if let Some(num_removed_keys) = num_removed_keys {
            decrement_gauge!(metric_names.loader_effect_entity_count, num_removed_keys as f64, "loader_name" => name);
        }
        unsubscribed_effects
    }
}

fn create_load_batch_effect<T: Expression>(
    label: String,
    loader: T,
    keys: T::ExpressionList<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T::Signal<T> {
    create_evaluate_effect(
        label,
        factory.create_application_term(
            loader,
            allocator.create_unit_list(factory.create_list_term(keys)),
        ),
        QueryEvaluationMode::Standalone,
        QueryInvalidationStrategy::Exact,
        factory,
        allocator,
    )
}

dispatcher!({
    pub enum LoaderHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(EffectEmitAction<T>),

        Outbox(EffectSubscribeAction<T>),
        Outbox(EffectUnsubscribeAction<T>),
        Outbox(EffectEmitAction<T>),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for LoaderHandler<T, TFactory, TAllocator>
    where
        T: AsyncExpression + Applicable<T>,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = LoaderHandlerState<T>;
        type Events<TInbox: TaskInbox<TAction>> = TInbox;
        type Dispose = NoopDisposeCallback;

        fn init(&self) -> Self::State {
            Default::default()
        }
        fn events<TInbox: TaskInbox<TAction>>(
            &self,
            inbox: TInbox,
        ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
            ActorEvents::Sync(inbox)
        }

        fn accept(&self, action: &EffectSubscribeAction<T>) -> bool {
            action.effect_type.as_str() == EFFECT_TYPE_LOADER
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

        fn accept(&self, action: &EffectUnsubscribeAction<T>) -> bool {
            action.effect_type.as_str() == EFFECT_TYPE_LOADER
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

        fn accept(&self, action: &EffectEmitAction<T>) -> bool {
            action
                .effect_types
                .iter()
                .any(|batch| &batch.effect_type == EFFECT_TYPE_EVALUATE)
        }
        fn schedule(
            &self,
            action: &EffectEmitAction<T>,
            state: &Self::State,
        ) -> Option<SchedulerMode> {
            if state.loaders.is_empty() {
                return None;
            }
            let has_relevant_updates = action
                .effect_types
                .iter()
                .filter(|batch| &batch.effect_type == EFFECT_TYPE_EVALUATE)
                .flat_map(|batch| batch.updates.iter())
                .any(|(state_token, _update)| {
                    state.loader_effect_mappings.contains_key(state_token)
                });
            if !has_relevant_updates {
                return None;
            }
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

impl<T, TFactory, TAllocator> LoaderHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression + Applicable<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut LoaderHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectSubscribeAction<T>> + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_LOADER {
            return None;
        }
        let (initial_values, effects_by_loader) = effects.iter().fold(
            (
                Vec::<(StateToken, T)>::with_capacity(effects.len()),
                HashMap::<LoaderFactoryHash, (T, String, Vec<LoaderEntitySubscription<T>>)>::default(),
            ),
            |(mut initial_values, mut results), effect| {
                match parse_loader_effect_args(effect, &self.factory) {
                    Ok(args) => {
                        let LoaderEffectArgs { name, loader, key } = args;
                        let subscription = LoaderEntitySubscription {
                            key,
                            effect: effect.clone(),
                        };
                        match results.entry(LoaderFactoryHash::new(&loader)) {
                            Entry::Occupied(mut entry) => {
                                let loader_subscriptions = entry.get_mut();
                                let (_loader, _loader_name, subscriptions) = loader_subscriptions;
                                subscriptions.push(subscription);
                            }
                            Entry::Vacant(entry) => {
                                entry.insert((loader, name, vec![subscription]));
                            }
                        }
                        initial_values.push((
                            effect.id(),
                            create_pending_expression(&self.factory, &self.allocator),
                        ))
                    }
                    Err(message) => {
                        initial_values.push((
                            effect.id(),
                            create_error_expression(message, &self.factory, &self.allocator),
                        ));
                    }
                }
                (initial_values, results)
            },
        );
        let load_effects = effects_by_loader
            .into_values()
            .flat_map(|(loader, name, subscriptions)| {
                state.subscribe(
                    name,
                    loader,
                    subscriptions,
                    &self.factory,
                    &self.allocator,
                    self.metric_names,
                )
            })
            .collect::<Vec<_>>();
        let initial_values_action = if initial_values.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_LOADER.into(),
                        updates: initial_values,
                    }],
                }
                .into(),
            ))
        };
        let load_action = if load_effects.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectSubscribeAction {
                    effect_type: EFFECT_TYPE_EVALUATE.into(),
                    effects: load_effects,
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            initial_values_action.into_iter().chain(load_action),
        ))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut LoaderHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectUnsubscribeAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_LOADER {
            return None;
        }
        let effects_by_loader = effects.iter().fold(
            HashMap::<LoaderFactoryHash, (T, String, Vec<LoaderEntitySubscription<T>>)>::default(),
            |mut results, effect| {
                if let Ok(args) = parse_loader_effect_args(effect, &self.factory) {
                    let LoaderEffectArgs { name, loader, key } = args;
                    let subscription = LoaderEntitySubscription {
                        key,
                        effect: effect.clone(),
                    };
                    match results.entry(LoaderFactoryHash::new(&loader)) {
                        Entry::Occupied(mut entry) => {
                            let (_loader, _name, subscriptions) = entry.get_mut();
                            subscriptions.push(subscription);
                        }
                        Entry::Vacant(entry) => {
                            entry.insert((loader, name, vec![subscription]));
                        }
                    }
                }
                results
            },
        );
        let unsubscribe_effects = effects_by_loader
            .into_values()
            .flat_map(|(loader, name, subscriptions)| {
                state.unsubscribe(name, loader, subscriptions, self.metric_names)
            })
            .collect::<Vec<_>>();
        let unsubscribe_action = if unsubscribe_effects.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectUnsubscribeAction {
                    effect_type: EFFECT_TYPE_EVALUATE.into(),
                    effects: unsubscribe_effects,
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(unsubscribe_action))
    }
    fn handle_effect_emit<TAction, TTask>(
        &self,
        state: &mut LoaderHandlerState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectEmitAction { effect_types } = action;
        if state.loaders.is_empty() {
            return None;
        }
        let updates = effect_types
            .iter()
            .filter(|batch| &batch.effect_type == EFFECT_TYPE_EVALUATE)
            .flat_map(|batch| batch.updates.iter())
            .filter_map(|(state_token, update)| {
                let loader = state.loader_effect_mappings.get(state_token)?;
                let loader_state = state.loaders.get_mut(&LoaderFactoryHash::new(loader))?;
                let batch_keys = loader_state.batch_effect_mappings.get(state_token)?;
                let batch = loader_state
                    .active_batches
                    .get_mut(&LoaderBatchHash::new(batch_keys))?;
                let (value, _) = parse_evaluate_effect_result(update, &self.factory)?.into_parts();
                batch.latest_result.replace(value.clone());
                let mut results = if let Some(value) = self.factory.match_list_term(&value) {
                    let items = value.items().as_deref();
                    if items.len() != batch.subscriptions.len() {
                        Err(create_error_expression(
                            format!(
                                "Invalid {} loader result: Expected {} values, received {}",
                                loader_state.name,
                                batch.subscriptions.len(),
                                items.len()
                            ),
                            &self.factory,
                            &self.allocator,
                        ))
                    } else {
                        Ok(batch
                            .subscriptions
                            .iter()
                            .enumerate()
                            .filter_map(|(index, subscription)| {
                                items
                                    .get(index)
                                    .map(|value| value.as_deref())
                                    .cloned()
                                    .map(|value| (LoaderKeyHash::new(&subscription.key), value))
                            })
                            .collect::<HashMap<_, _>>())
                    }
                } else if let Some(value) = self.factory.match_hashmap_term(&value) {
                    let results = batch
                        .subscriptions
                        .iter()
                        .filter_map(|subscription| {
                            value
                                .get(&subscription.key)
                                .map(|item| item.as_deref())
                                .cloned()
                                .map(|value| (LoaderKeyHash::new(&subscription.key), value))
                        })
                        .collect::<HashMap<_, _>>();
                    if results.len() < batch.subscriptions.len() {
                        Err(create_error_expression(
                            format!(
                                "Invalid {} loader result: missing values for {}",
                                loader_state.name,
                                batch
                                    .subscriptions
                                    .iter()
                                    .filter_map(|subscription| match value.get(&subscription.key) {
                                        Some(_) => None,
                                        None => Some(format!("{}", subscription.key)),
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            &self.factory,
                            &self.allocator,
                        ))
                    } else {
                        Ok(results)
                    }
                } else if let Some(term) = self.factory.match_signal_term(&value) {
                    Err(if has_error_message_effects(term, &self.factory) {
                        prefix_error_message_effects(
                            &format!("{} loader error: ", loader_state.name.as_str()),
                            term,
                            &self.factory,
                            &self.allocator,
                        )
                    } else {
                        value.clone()
                    })
                } else {
                    Err(create_error_expression(
                        format!(
                            "Invalid {} loader result: Expected List or HashMap, received {}",
                            loader_state.name, value
                        ),
                        &self.factory,
                        &self.allocator,
                    ))
                };
                Some(
                    batch
                        .subscriptions
                        .iter()
                        .filter(|subscription| {
                            batch
                                .active_keys
                                .contains(&LoaderKeyHash::new(&subscription.key))
                        })
                        .filter_map(|subscription| {
                            let result = match &mut results {
                                Err(err) => Some(err.clone()),
                                Ok(results) => {
                                    results.remove(&LoaderKeyHash::new(&subscription.key))
                                }
                            };
                            match result {
                                Some(result) => Some((subscription.effect.id(), result)),
                                None => None,
                            }
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .flatten()
            .collect::<Vec<_>>();
        let update_action = if updates.is_empty() {
            None
        } else {
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_LOADER.into(),
                        updates,
                    }],
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(update_action))
    }
}

fn has_error_message_effects<T: Expression>(
    term: &T::SignalTerm<T>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    term.signals()
        .as_deref()
        .iter()
        .map(|item| item.as_deref())
        .any(|effect| as_error_message_effect(effect, factory).is_some())
}

fn prefix_error_message_effects<T: Expression>(
    prefix: &str,
    term: &T::SignalTerm<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(
            term.signals()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .map(|signal| {
                    if let Some(message) = as_error_message_effect(signal, factory) {
                        allocator.create_signal(
                            signal.signal_type().clone(),
                            factory.create_string_term(allocator.create_string(format!(
                                "{}{}",
                                prefix,
                                message.as_str()
                            ))),
                            signal.token().as_deref().clone(),
                        )
                    } else {
                        signal.clone()
                    }
                }),
        ),
    )
}

fn as_error_message_effect<'a, T: Expression + 'a>(
    effect: &'a T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T::String> {
    if !matches!(effect.signal_type(), SignalType::Error) {
        return None;
    }
    let payload = effect.payload().as_deref();
    factory
        .match_string_term(payload)
        .map(|value| value.as_deref())
        .map(|term| term.value().as_deref())
}

struct LoaderEffectArgs<T: Expression> {
    name: String,
    loader: T,
    key: T,
}

fn parse_loader_effect_args<T: Expression + Applicable<T>>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<LoaderEffectArgs<T>, String> {
    let payload = effect.payload().as_deref();
    let args = factory
        .match_list_term(payload)
        .map(|term| term.items().as_deref())
        .filter(|args| args.len() == 3)
        .ok_or_else(|| {
            format!(
                "Invalid loader signal: Expected 3 arguments, received {}",
                payload
            )
        })?;
    let mut args = args.iter().map(|iter| iter.as_deref());
    let name = args.next().unwrap();
    let loader = args.next().unwrap();
    let key = args.next().unwrap();
    let name = factory
        .match_string_term(name)
        .map(|term| term.value().as_deref().as_str())
        .ok_or(name);
    match (name, loader.arity()) {
        (Ok(name), Some(arity)) if is_valid_loader_signature(&arity) => Ok(LoaderEffectArgs {
            name: String::from(name),
            loader: loader.clone(),
            key: key.clone(),
        }),
        _ => Err(format!(
            "Invalid loader factory: Expected <function:1>, received {}",
            loader
        )),
    }
}

fn is_valid_loader_signature(arity: &Arity) -> bool {
    match arity.required().len() {
        1 => true,
        0 => arity.optional().len() > 0 || arity.variadic().is_some(),
        _ => false,
    }
}

fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Pending,
        factory.create_nil_term(),
        factory.create_nil_term(),
    ))))
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        factory.create_string_term(allocator.create_string(message)),
        factory.create_nil_term(),
    ))))
}
