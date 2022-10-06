// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
};

use metrics::{describe_counter, describe_gauge, gauge, increment_gauge, Unit};
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator, RefType,
    SignalType, StateToken, StringTermType, StringValue,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    actor::evaluate_handler::{
        create_evaluate_effect, parse_evaluate_effect_result, EFFECT_TYPE_EVALUATE,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, QueryEvaluationMode,
    QueryInvalidationStrategy,
};

pub const EFFECT_TYPE_SCAN: &'static str = "reflex::scan";

#[derive(Clone, Copy, Debug)]
pub struct ScanHandlerMetricNames {
    pub scan_effect_iteration_count: &'static str,
    pub scan_effect_result_count: &'static str,
    pub scan_effect_state_size: &'static str,
}
impl ScanHandlerMetricNames {
    fn init(self) -> Self {
        describe_counter!(
            self.scan_effect_iteration_count,
            Unit::Count,
            "Scan effect iteration count"
        );
        describe_counter!(
            self.scan_effect_result_count,
            Unit::Count,
            "Scan effect result count"
        );
        describe_gauge!(
            self.scan_effect_state_size,
            Unit::Count,
            "Scan effect accumulated state size"
        );
        self
    }
}
impl Default for ScanHandlerMetricNames {
    fn default() -> Self {
        Self {
            scan_effect_iteration_count: "scan_effect_iteration_count",
            scan_effect_result_count: "scan_effect_result_count",
            scan_effect_state_size: "scan_effect_state_size",
        }
    }
}

const EVENT_TYPE_SCAN_SOURCE: &'static str = "reflex::scan::source";
const EVENT_TYPE_SCAN_STATE: &'static str = "reflex::scan::state";

pub trait ScanHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectEmitAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectSubscribeAction<T>>
    + OutboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> ScanHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectEmitAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectSubscribeAction<T>>
        + OutboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

#[derive(Clone)]
pub struct ScanHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    metric_names: ScanHandlerMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> ScanHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(
        factory: TFactory,
        allocator: TAllocator,
        metric_names: ScanHandlerMetricNames,
    ) -> Self {
        Self {
            factory,
            allocator,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

pub struct ScanHandlerState<T: Expression> {
    effect_state: HashMap<StateToken, ScanHandlerReducerState<T>>,
    /// Maps the child evaluate effect ID to the parent state effect ID
    effect_mappings: HashMap<StateToken, StateToken>,
}
impl<T: Expression> Default for ScanHandlerState<T> {
    fn default() -> Self {
        Self {
            effect_state: Default::default(),
            effect_mappings: Default::default(),
        }
    }
}

struct ScanHandlerReducerState<T: Expression> {
    metric_labels: Vec<(String, String)>,
    source_effect: T::Signal<T>,
    source_value_effect: T::Signal<T>,
    source_value: Option<T>,
    state_value_effect: T::Signal<T>,
    state_value: T,
    result_effect: T::Signal<T>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for ScanHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: ScanHandlerAction<T> + 'static,
{
    type State = ScanHandlerState<T>;
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
        let mut state = state;
        let actions = if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_unsubscribe(&mut state, action, metadata, context)
        } else if let Some(action) = action.match_type() {
            self.handle_effect_emit(&mut state, action, metadata, context)
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator> ScanHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut ScanHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
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
        if effect_type.as_str() != EFFECT_TYPE_SCAN {
            return None;
        }
        let current_pid = context.pid();
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(
                |effect| match parse_scan_effect_args(effect, &self.factory) {
                    Ok(args) => {
                        if let Some(action) = self.subscribe_scan_effect(state, effect, args) {
                            Some((
                                (
                                    effect.id(),
                                    create_pending_expression(&self.factory, &self.allocator),
                                ),
                                Some(StateOperation::Send(current_pid, action)),
                            ))
                        } else {
                            None
                        }
                    }
                    Err(err) => Some((
                        (
                            effect.id(),
                            create_error_expression(err, &self.factory, &self.allocator),
                        ),
                        None,
                    )),
                },
            )
            .unzip();
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
        Some(StateTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().flatten()),
        ))
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut ScanHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_SCAN {
            return None;
        }
        let current_pid = context.pid();
        let unsubscribe_actions = effects.iter().filter_map(|effect| {
            if let Some(operation) = self.unsubscribe_scan_effect(state, effect) {
                Some(StateOperation::Send(current_pid, operation))
            } else {
                None
            }
        });
        Some(StateTransition::new(unsubscribe_actions))
    }
    fn handle_effect_emit<TAction>(
        &self,
        state: &mut ScanHandlerState<T>,
        action: &EffectEmitAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectEmitAction { updates } = action;
        if state.effect_state.is_empty() {
            return None;
        }
        let updates = updates
            .iter()
            .filter_map(|(updated_state_token, update)| {
                let scan_effect_id = state.effect_mappings.get(updated_state_token)?;
                let reducer_state = state.effect_state.get_mut(scan_effect_id)?;
                let updated_state_token = *updated_state_token;
                if updated_state_token == reducer_state.source_effect.id() {
                    // The source input has emitted, so trigger the next reducer iteration
                    {
                        increment_gauge!(
                            self.metric_names.scan_effect_iteration_count,
                            1.0,
                            &reducer_state.metric_labels
                        );
                    }
                    let (value, _) =
                        parse_evaluate_effect_result(update, &self.factory)?.into_parts();
                    reducer_state.source_value.replace(value.clone());
                    // Assign values for both reducer arguments (this will trigger the reducer query to be re-evaluated)
                    Some([
                        (reducer_state.source_value_effect.id(), value),
                        (
                            reducer_state.state_value_effect.id(),
                            reducer_state.state_value.clone(),
                        ),
                    ])
                } else if updated_state_token == reducer_state.result_effect.id() {
                    // The reducer has emitted a result, so emit a new result and reset the reducer to a pending state
                    // while we wait for the next input value to arrive
                    let (value, _) =
                        parse_evaluate_effect_result(update, &self.factory)?.into_parts();
                    reducer_state.state_value = value.clone();
                    increment_gauge!(
                        self.metric_names.scan_effect_result_count,
                        1.0,
                        &reducer_state.metric_labels
                    );
                    gauge!(
                        self.metric_names.scan_effect_state_size,
                        value.size() as f64,
                        &reducer_state.metric_labels
                    );
                    // Emit a result for the overall scan effect, resetting the reducer state to a pending value
                    // (this effectively blocks the reducer query from being prematurely re-evaluated with a stale state
                    // when the next source value arrives)
                    Some([
                        (
                            reducer_state.state_value_effect.id(),
                            create_pending_expression(&self.factory, &self.allocator),
                        ),
                        (*scan_effect_id, value),
                    ])
                } else {
                    None
                }
            })
            .flatten()
            .collect::<Vec<_>>();
        let update_action = if updates.is_empty() {
            None
        } else {
            Some(StateOperation::Send(
                context.pid(),
                EffectEmitAction { updates }.into(),
            ))
        };
        Some(StateTransition::new(update_action))
    }
    fn subscribe_scan_effect<TAction>(
        &self,
        state: &mut ScanHandlerState<T>,
        effect: &T::Signal<T>,
        args: ScanEffectArgs<T>,
    ) -> Option<TAction>
    where
        TAction: Action + OutboundAction<EffectSubscribeAction<T>>,
    {
        let (source_effect, result_effect) = match state.effect_state.entry(effect.id()) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                let ScanEffectArgs {
                    label,
                    target,
                    seed,
                    iteratee,
                } = args;
                let source_value_effect = self.allocator.create_signal(
                    SignalType::Custom(String::from(EVENT_TYPE_SCAN_SOURCE)),
                    self.allocator
                        .create_triple(target.clone(), seed.clone(), iteratee.clone()),
                );
                let state_value_effect = self.allocator.create_signal(
                    SignalType::Custom(String::from(EVENT_TYPE_SCAN_STATE)),
                    self.allocator
                        .create_triple(target.clone(), seed.clone(), iteratee.clone()),
                );
                let reducer_label = format!("{} [reducer]", label);
                let source_effect = create_evaluate_effect(
                    label.clone(),
                    target,
                    QueryEvaluationMode::Standalone,
                    QueryInvalidationStrategy::Exact,
                    &self.factory,
                    &self.allocator,
                );
                let result_effect = create_evaluate_effect(
                    reducer_label,
                    self.factory.create_application_term(
                        iteratee,
                        self.allocator.create_pair(
                            self.factory.create_effect_term(state_value_effect.clone()),
                            self.factory.create_effect_term(source_value_effect.clone()),
                        ),
                    ),
                    QueryEvaluationMode::Standalone,
                    QueryInvalidationStrategy::Exact,
                    &self.factory,
                    &self.allocator,
                );
                let reducer_state = ScanHandlerReducerState {
                    metric_labels: vec![(String::from("label"), label)],
                    source_effect: source_effect.clone(),
                    source_value_effect,
                    source_value: None,
                    state_value_effect,
                    state_value: seed,
                    result_effect: result_effect.clone(),
                };
                gauge!(
                    self.metric_names.scan_effect_iteration_count,
                    0.0,
                    &reducer_state.metric_labels
                );
                gauge!(
                    self.metric_names.scan_effect_result_count,
                    0.0,
                    &reducer_state.metric_labels
                );
                gauge!(
                    self.metric_names.scan_effect_state_size,
                    0.0,
                    &reducer_state.metric_labels
                );
                entry.insert(reducer_state);
                Some((source_effect, result_effect))
            }
        }?;
        state
            .effect_mappings
            .insert(source_effect.id(), effect.id());
        state
            .effect_mappings
            .insert(result_effect.id(), effect.id());
        Some(
            EffectSubscribeAction {
                effect_type: String::from(EFFECT_TYPE_EVALUATE),
                effects: vec![source_effect, result_effect],
            }
            .into(),
        )
    }
    fn unsubscribe_scan_effect<TAction>(
        &self,
        state: &mut ScanHandlerState<T>,
        effect: &T::Signal<T>,
    ) -> Option<TAction>
    where
        TAction: Action + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        if let Entry::Occupied(entry) = state.effect_state.entry(effect.id()) {
            let reducer_state = entry.remove();
            gauge!(
                self.metric_names.scan_effect_iteration_count,
                0.0,
                &reducer_state.metric_labels
            );
            gauge!(
                self.metric_names.scan_effect_result_count,
                0.0,
                &reducer_state.metric_labels
            );
            gauge!(
                self.metric_names.scan_effect_state_size,
                0.0,
                &reducer_state.metric_labels
            );
            let ScanHandlerReducerState {
                metric_labels: _,
                source_effect,
                source_value_effect: _,
                source_value: _,
                state_value_effect: _,
                state_value: _,
                result_effect,
            } = reducer_state;
            state.effect_mappings.remove(&source_effect.id());
            state.effect_mappings.remove(&result_effect.id());
            Some(
                EffectUnsubscribeAction {
                    effect_type: String::from(EFFECT_TYPE_EVALUATE),
                    effects: vec![source_effect, result_effect],
                }
                .into(),
            )
        } else {
            None
        }
    }
}

struct ScanEffectArgs<T: Expression> {
    label: String,
    target: T,
    seed: T,
    iteratee: T,
}

fn parse_scan_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<ScanEffectArgs<T>, String> {
    let args = effect.args().as_deref();
    if args.len() != 4 {
        return Err(format!(
            "Invalid scan signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let name = remaining_args.next().unwrap();
    let target = remaining_args.next().unwrap();
    let seed = remaining_args.next().unwrap();
    let iteratee = remaining_args.next().unwrap();
    if let Some(name) = factory.match_string_term(name) {
        Ok(ScanEffectArgs {
            label: String::from(name.value().as_deref().as_str()),
            target: target.clone(),
            seed: seed.clone(),
            iteratee: iteratee.clone(),
        })
    } else {
        Err(format!(
            "Invalid scan signal arguments: Expected (String, <any>, <any>, <function:2>), received ({}, {}, {}, {})",
            name,
            target,
            seed,
            iteratee,
        ))
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
        allocator.create_unit_list(factory.create_string_term(allocator.create_string(message))),
    ))))
}
