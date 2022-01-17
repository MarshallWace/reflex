// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
    sync::Once,
};

use metrics::{decrement_gauge, describe_gauge, increment_gauge, Unit};
use reflex::{
    core::{
        Applicable, Arity, Expression, ExpressionFactory, ExpressionList, HeapAllocator, Signal,
        SignalType, StateToken, StringValue,
    },
    lang::{term::SignalTerm, ValueTerm},
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OutboundAction, StateOperation,
    StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    actor::evaluate_handler::{
        create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy, StateUpdate,
};

pub const EFFECT_TYPE_LOADER: &'static str = "reflex::loader";

pub const METRIC_LOADER_ENTITY_COUNT: &'static str = "loader_effect_entity_count";

static INIT_METRICS: Once = Once::new();

fn init_metrics() {
    INIT_METRICS.call_once(|| {
        describe_gauge!(
            METRIC_LOADER_ENTITY_COUNT,
            Unit::Count,
            "Active loader entity count"
        );
    });
}

pub trait LoaderHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectSubscribeAction<T>>
    + OutboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> LoaderHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectSubscribeAction<T>>
        + OutboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

pub struct LoaderHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    state: LoaderHandlerState<T>,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> LoaderHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub fn new(factory: TFactory, allocator: TAllocator) -> Self {
        Self {
            factory,
            allocator,
            state: Default::default(),
            _expression: Default::default(),
        }
    }
}

struct LoaderHandlerState<T: Expression> {
    loaders: HashMap<T, LoaderState<T>>,
    /// Maps the combined loader batch evaluate effect ID to the loader expression (used as the key to the loaders hashmap)
    loader_effect_mappings: HashMap<StateToken, T>,
}

struct LoaderState<T: Expression> {
    name: String,
    active_keys: HashSet<T>,
    /// Maps keylists to the corresponding loader batch
    active_batches: HashMap<ExpressionList<T>, LoaderBatch<T>>,
    /// Maps the individual entity load effect IDs to the keylist of the subscribed batch
    entity_effect_mappings: HashMap<StateToken, ExpressionList<T>>,
    /// Maps the combined loader batch evaluate effect ID to the keylist of the subscribed batch
    batch_effect_mappings: HashMap<StateToken, ExpressionList<T>>,
}

struct LoaderBatch<T: Expression> {
    /// Evaluate effect used to load the batch
    effect: Signal<T>,
    /// List of entries for all the individual entities contained within this batch
    subscriptions: Vec<LoaderEntitySubscription<T>>,
    /// Maintain a list of which keys are actively subscribed - when this becomes empty the batch is unsubscribed
    active_keys: HashSet<T>,
    latest_result: Option<T>,
}

struct LoaderEntitySubscription<T: Expression> {
    key: T,
    effect: Signal<T>,
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
        init_metrics();
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
    ) -> impl IntoIterator<Item = Signal<T>> {
        let loader_state = match self.loaders.entry(loader.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(LoaderState::new(name.clone())),
        };
        let (keys, effects): (Vec<_>, Vec<_>) = keys
            .into_iter()
            .filter_map(|subscription| {
                let LoaderEntitySubscription { key, effect } = subscription;
                if loader_state.active_keys.contains(&key) {
                    None
                } else {
                    Some((key, effect))
                }
            })
            .unzip();
        let keys = allocator.create_list(keys);
        let combined_effect =
            create_load_batch_effect(loader.clone(), keys.clone(), factory, allocator);
        let loader_batch = LoaderBatch {
            effect: combined_effect.clone(),
            subscriptions: keys
                .iter()
                .enumerate()
                .filter_map(|(index, key)| {
                    effects
                        .get(index)
                        .map(|effect| (key.clone(), effect.clone()))
                })
                .map(|(key, effect)| LoaderEntitySubscription { key, effect })
                .collect(),
            active_keys: keys.iter().cloned().collect(),
            latest_result: None,
        };
        let num_previous_keys = loader_state.active_keys.len();
        loader_state.active_keys.extend(keys.iter().cloned());
        let num_added_keys = loader_state.active_keys.len() - num_previous_keys;
        let metric_labels = [("loader_name", name)];
        increment_gauge!(
            METRIC_LOADER_ENTITY_COUNT,
            num_added_keys as f64,
            &metric_labels
        );
        loader_state
            .entity_effect_mappings
            .extend(effects.iter().map(|effect| (effect.id(), keys.clone())));
        self.loader_effect_mappings
            .insert(combined_effect.id(), loader);
        loader_state
            .batch_effect_mappings
            .insert(combined_effect.id(), keys.clone());
        loader_state.active_batches.insert(keys, loader_batch);
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
    ) -> impl IntoIterator<Item = Signal<T>> {
        let (num_previous_keys, unsubscribed_effects) = match self.loaders.get_mut(&loader) {
            None => (None, None),
            Some(loader_state) => {
                let num_previous_keys = loader_state.active_keys.len();
                let unsubscribed_effects = subscriptions.into_iter().filter_map(|subscription| {
                    let LoaderEntitySubscription { key, effect } = subscription;
                    let loader_batch_keys =
                        loader_state.entity_effect_mappings.remove(&effect.id())?;
                    loader_state.active_keys.remove(&key);
                    let loader_batch = loader_state.active_batches.get_mut(&loader_batch_keys)?;
                    loader_batch.active_keys.remove(&key);
                    if loader_batch.active_keys.is_empty() {
                        let loader_batch =
                            loader_state.active_batches.remove(&loader_batch_keys)?;
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
                .get(&loader)
                .map(|loader_state| num_previous_keys - loader_state.active_keys.len())
        });
        if let Some(num_removed_keys) = num_removed_keys {
            decrement_gauge!(METRIC_LOADER_ENTITY_COUNT, num_removed_keys as f64, "loader_name" => name);
        }
        unsubscribed_effects
    }
}

fn create_load_batch_effect<T: Expression>(
    loader: T,
    keys: ExpressionList<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Signal<T> {
    create_evaluate_effect(
        factory.create_application_term(
            loader,
            allocator.create_unit_list(factory.create_vector_term(keys)),
        ),
        QueryEvaluationMode::Standalone,
        QueryInvalidationStrategy::Exact,
        factory,
        allocator,
    )
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for LoaderHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression + Applicable<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: LoaderHandlerAction<T> + 'static,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(action, metadata, context)
        } else {
            StateTransition::new(None)
        }
    }
}
impl<T, TFactory, TAllocator> LoaderHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression + Applicable<T>,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &mut self,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action
            + 'static
            + OutboundAction<EffectSubscribeAction<T>>
            + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_LOADER {
            return StateTransition::new(None);
        }
        let current_pid = context.pid();
        let (initial_values, effects_by_loader) = effects.iter().fold(
            (
                Vec::<(StateToken, StateUpdate<T>)>::with_capacity(effects.len()),
                HashMap::<T, (String, Vec<LoaderEntitySubscription<T>>)>::default(),
            ),
            |(mut initial_values, mut results), effect| {
                match parse_loader_effect_args(&effect, &self.factory) {
                    Ok(args) => {
                        let LoaderEffectArgs { name, loader, key } = args;
                        let subscription = LoaderEntitySubscription {
                            key,
                            effect: effect.clone(),
                        };
                        match results.entry(loader) {
                            Entry::Occupied(mut entry) => {
                                let loader_subscriptions = entry.get_mut();
                                let (_loader_name, subscriptions) = loader_subscriptions;
                                subscriptions.push(subscription);
                            }
                            Entry::Vacant(entry) => {
                                entry.insert((name, vec![subscription]));
                            }
                        }
                        initial_values.push((
                            effect.id(),
                            StateUpdate::Value(create_pending_expression(
                                &self.factory,
                                &self.allocator,
                            )),
                        ))
                    }
                    Err(message) => {
                        initial_values.push((
                            effect.id(),
                            StateUpdate::Value(create_error_expression(
                                message,
                                &self.factory,
                                &self.allocator,
                            )),
                        ));
                    }
                }
                (initial_values, results)
            },
        );
        let load_effects = effects_by_loader
            .into_iter()
            .flat_map(|(loader, (name, subscriptions))| {
                self.state
                    .subscribe(name, loader, subscriptions, &self.factory, &self.allocator)
            })
            .collect::<Vec<_>>();
        let initial_values_action = if initial_values.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                current_pid,
                EffectEmitAction {
                    updates: initial_values,
                }
                .into(),
            ))
        };
        let load_action = if load_effects.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                current_pid,
                EffectSubscribeAction {
                    effect_type: String::from(EFFECT_TYPE_EVALUATE),
                    effects: load_effects,
                }
                .into(),
            ))
        };
        StateTransition::new(initial_values_action.into_iter().chain(load_action))
    }
    fn handle_effect_unsubscribe<TAction>(
        &mut self,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + 'static + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_LOADER {
            return StateTransition::new(None);
        }
        let current_pid = context.pid();
        let effects_by_loader = effects.iter().fold(
            HashMap::<T, (String, Vec<LoaderEntitySubscription<T>>)>::default(),
            |mut results, effect| {
                if let Ok(args) = parse_loader_effect_args(&effect, &self.factory) {
                    let LoaderEffectArgs { name, loader, key } = args;
                    let subscription = LoaderEntitySubscription {
                        key,
                        effect: effect.clone(),
                    };
                    match results.entry(loader) {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().1.push(subscription);
                        }
                        Entry::Vacant(entry) => {
                            entry.insert((name, vec![subscription]));
                        }
                    }
                }
                results
            },
        );
        let unsubscribe_effects = effects_by_loader
            .into_iter()
            .flat_map(|(loader, (name, subscriptions))| {
                self.state.unsubscribe(name, loader, subscriptions)
            })
            .collect::<Vec<_>>();
        let unsubscribe_action = if unsubscribe_effects.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                current_pid,
                EffectUnsubscribeAction {
                    effect_type: String::from(EFFECT_TYPE_EVALUATE),
                    effects: unsubscribe_effects,
                }
                .into(),
            ))
        };
        StateTransition::new(unsubscribe_action)
    }
    fn handle_effect_emit<TAction>(
        &mut self,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectEmitAction { updates } = action;
        if self.state.loaders.is_empty() {
            return StateTransition::new(None);
        }
        let updates = updates
            .iter()
            .filter_map(|(updated_state_token, update)| {
                let loader = self.state.loader_effect_mappings.get(updated_state_token)?;
                let loader_state = self.state.loaders.get_mut(loader)?;
                let batch_keys = loader_state
                    .batch_effect_mappings
                    .get(updated_state_token)?;
                let batch = loader_state.active_batches.get_mut(batch_keys)?;
                let (value, _) = match update {
                    StateUpdate::Value(value) => parse_evaluate_effect_result(value, &self.factory),
                    StateUpdate::Patch(updater) => parse_evaluate_effect_result(
                        &updater(batch.latest_result.as_ref()),
                        &self.factory,
                    ),
                }?
                .into_parts();
                batch.latest_result.replace(value.clone());
                let mut results = if let Some(value) = self.factory.match_vector_term(&value) {
                    Ok(batch
                        .subscriptions
                        .iter()
                        .enumerate()
                        .filter_map(|(index, subscription)| {
                            value
                                .items()
                                .get(index)
                                .cloned()
                                .map(|value| (subscription.key.clone(), value))
                        })
                        .collect::<HashMap<_, _>>())
                } else if let Some(value) = self.factory.match_hashmap_term(&value) {
                    Ok(batch
                        .subscriptions
                        .iter()
                        .filter_map(|subscription| {
                            value
                                .get(&subscription.key)
                                .cloned()
                                .map(|value| (subscription.key.clone(), value))
                        })
                        .collect::<HashMap<_, _>>())
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
                            "Invalid {} loader result: Expected Vector or HashMap, received {}",
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
                        .filter(|subscription| batch.active_keys.contains(&subscription.key))
                        .filter_map(|subscription| {
                            let result = match &mut results {
                                Err(err) => Some(err.clone()),
                                Ok(results) => results.remove(&subscription.key),
                            };
                            match result {
                                Some(result) => {
                                    Some((subscription.effect.id(), StateUpdate::Value(result)))
                                }
                                None => None,
                            }
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .flatten()
            .collect::<Vec<_>>();
        if updates.is_empty() {
            StateTransition::new(None)
        } else {
            StateTransition::new(Some(StateOperation::Send(
                context.pid(),
                EffectEmitAction { updates }.into(),
            )))
        }
    }
}

fn has_error_message_effects<T: Expression>(
    term: &SignalTerm<T>,
    factory: &impl ExpressionFactory<T>,
) -> bool {
    term.signals()
        .iter()
        .any(|effect| as_error_message_effect(effect, factory).is_some())
}

fn prefix_error_message_effects<T: Expression>(
    prefix: &str,
    term: &SignalTerm<T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(term.signals().iter().map(|signal| {
            if let Some(message) = as_error_message_effect(signal, factory) {
                allocator.create_signal(
                    signal.signal_type().clone(),
                    allocator.create_list(signal.args().iter().enumerate().map(|(index, arg)| {
                        if index == 0 {
                            factory.create_value_term(ValueTerm::String(
                                format!("{}{}", prefix, message.as_str()).into(),
                            ))
                        } else {
                            arg.clone()
                        }
                    })),
                )
            } else {
                signal.clone()
            }
        })),
    )
}

fn as_error_message_effect<'a, T: Expression>(
    effect: &'a Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Option<&'a T::String> {
    if !matches!(effect.signal_type(), SignalType::Error) {
        return None;
    }
    effect.args().iter().next().and_then(|arg| {
        factory
            .match_value_term(arg)
            .and_then(|value| value.match_string())
    })
}

struct LoaderEffectArgs<T: Expression> {
    name: String,
    loader: T,
    key: T,
}

fn parse_loader_effect_args<T: Expression + Applicable<T>>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<LoaderEffectArgs<T>, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 3 {
        return Err(format!(
            "Invalid loader signal: Expected 3 arguments, received {}",
            args.len()
        ));
    }
    let name = args.next().unwrap();
    let loader = args.next().unwrap();
    let key = args.next().unwrap();
    let name = factory
        .match_value_term(name)
        .and_then(|value| value.match_string().map(|value| value.as_str()))
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
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(factory.create_value_term(ValueTerm::String(message.into()))),
    ))))
}
