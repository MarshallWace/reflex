// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap,
    },
    hash::Hasher,
    iter::once,
    marker::PhantomData,
};

use reflex::{
    core::{
        Expression, ExpressionFactory, HeapAllocator, Signal, SignalType, StateToken, StringValue,
    },
    hash::HashId,
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
    QueryInvalidationStrategy, StateUpdate,
};

pub const EFFECT_TYPE_SCAN: &'static str = "reflex::scan";

const HASH_NAMESPACE_SCAN_SOURCE: &'static str = "reflex::scan::source";
const HASH_NAMESPACE_SCAN_STATE: &'static str = "reflex::scan::result";

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
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> ScanHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(factory: TFactory, allocator: TAllocator) -> Self {
        Self {
            factory,
            allocator,
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
impl<T: Expression> ScanHandlerState<T> {
    fn subscribe<TAction>(
        &mut self,
        scan_effect: &Signal<T>,
        args: ScanEffectArgs<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<TAction>
    where
        TAction: Action + OutboundAction<EffectSubscribeAction<T>>,
    {
        let (source_effect, result_effect) = if let Entry::Vacant(entry) =
            self.effect_state.entry(scan_effect.id())
        {
            let ScanEffectArgs {
                name,
                target,
                seed,
                iteratee,
            } = args;
            let source_value_token =
                generate_hash(HASH_NAMESPACE_SCAN_SOURCE, [&target, &seed, &iteratee]);
            let state_value_token =
                generate_hash(HASH_NAMESPACE_SCAN_STATE, [&target, &seed, &iteratee]);
            let source_effect = create_evaluate_effect(
                format!("{} [input]", name),
                target,
                QueryEvaluationMode::Standalone,
                QueryInvalidationStrategy::Exact,
                factory,
                allocator,
            );
            let pending_value = create_pending_expression(factory, allocator);
            let result_effect = create_evaluate_effect(
                name,
                factory.create_application_term(
                    iteratee,
                    allocator.create_pair(
                        factory
                            .create_dynamic_variable_term(state_value_token, pending_value.clone()),
                        factory.create_dynamic_variable_term(source_value_token, pending_value),
                    ),
                ),
                QueryEvaluationMode::Standalone,
                QueryInvalidationStrategy::Exact,
                factory,
                allocator,
            );
            entry.insert(ScanHandlerReducerState {
                source_effect: source_effect.clone(),
                source_value_token,
                source_value: None,
                state_value_token,
                state_value: seed,
                result_effect: result_effect.clone(),
            });
            Some((source_effect, result_effect))
        } else {
            None
        }?;
        self.effect_mappings
            .insert(source_effect.id(), scan_effect.id());
        self.effect_mappings
            .insert(result_effect.id(), scan_effect.id());
        Some(
            EffectSubscribeAction {
                effect_type: String::from(EFFECT_TYPE_EVALUATE),
                effects: vec![source_effect, result_effect],
            }
            .into(),
        )
    }
    fn unsubscribe<TAction>(&mut self, scan_effect: &Signal<T>) -> Option<TAction>
    where
        TAction: Action + OutboundAction<EffectUnsubscribeAction<T>>,
    {
        if let Entry::Occupied(entry) = self.effect_state.entry(scan_effect.id()) {
            let ScanHandlerReducerState {
                source_effect,
                source_value_token: _,
                source_value: _,
                state_value_token: _,
                state_value: _,
                result_effect,
            } = entry.remove();
            self.effect_mappings.remove(&source_effect.id());
            self.effect_mappings.remove(&result_effect.id());
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

struct ScanHandlerReducerState<T: Expression> {
    source_effect: Signal<T>,
    source_value_token: StateToken,
    source_value: Option<T>,
    state_value_token: StateToken,
    state_value: T,
    result_effect: Signal<T>,
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
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_scan_effect_args(effect, &self.factory) {
                    Ok(args) => {
                        if let Some(action) =
                            state.subscribe(effect, args, &self.factory, &self.allocator)
                        {
                            Some((
                                (
                                    state_token,
                                    StateUpdate::Value(create_pending_expression(
                                        &self.factory,
                                        &self.allocator,
                                    )),
                                ),
                                Some(StateOperation::Send(current_pid, action)),
                            ))
                        } else {
                            None
                        }
                    }
                    Err(err) => Some((
                        (
                            state_token,
                            StateUpdate::Value(create_error_expression(
                                err,
                                &self.factory,
                                &self.allocator,
                            )),
                        ),
                        None,
                    )),
                }
            })
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
            if let Some(operation) = state.unsubscribe(effect) {
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
                    let (value, _) = match update {
                        StateUpdate::Value(value) => {
                            parse_evaluate_effect_result(value, &self.factory)
                        }
                        StateUpdate::Patch(operation) => {
                            let updated_value = operation.apply(
                                reducer_state.source_value.as_ref(),
                                &self.factory,
                                &self.allocator,
                            );
                            parse_evaluate_effect_result(&updated_value, &self.factory)
                        }
                    }?
                    .into_parts();
                    reducer_state.source_value.replace(value.clone());
                    // Assign values for both reducer arguments (this will trigger the reducer query to be re-evaluated)
                    Some([
                        (reducer_state.source_value_token, StateUpdate::Value(value)),
                        (
                            reducer_state.state_value_token,
                            StateUpdate::Value(reducer_state.state_value.clone()),
                        ),
                    ])
                } else if updated_state_token == reducer_state.result_effect.id() {
                    // The reducer has emitted a result, so emit a new result and reset the reducer to a pending state
                    // while we wait for the next input value to arrive
                    let (value, _) = match update {
                        StateUpdate::Value(value) => {
                            parse_evaluate_effect_result(value, &self.factory)
                        }
                        StateUpdate::Patch(operation) => {
                            let updated_value = operation.apply(
                                Some(&reducer_state.state_value),
                                &self.factory,
                                &self.allocator,
                            );
                            parse_evaluate_effect_result(&updated_value, &self.factory)
                        }
                    }?
                    .into_parts();
                    reducer_state.state_value = value.clone();
                    // Emit a result for the overall scan effect, resetting the reducer state to a pending value
                    // (this effectively blocks the reducer query from being prematurely re-evaluated with a stale state
                    // when the next source value arrives)
                    Some([
                        (
                            reducer_state.state_value_token,
                            StateUpdate::Value(create_pending_expression(
                                &self.factory,
                                &self.allocator,
                            )),
                        ),
                        (*scan_effect_id, StateUpdate::Value(value)),
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
}

struct ScanEffectArgs<T: Expression> {
    name: String,
    target: T,
    seed: T,
    iteratee: T,
}

fn parse_scan_effect_args<T: Expression>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<ScanEffectArgs<T>, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 4 {
        return Err(format!(
            "Invalid scan signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let name = args.next().unwrap();
    let target = args.next().unwrap();
    let seed = args.next().unwrap();
    let iteratee = args.next().unwrap();
    if let Some(name) = factory.match_string_term(name) {
        Ok(ScanEffectArgs {
            name: String::from(name.value.as_str()),
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

fn generate_hash<'a, T: std::hash::Hash + 'a>(
    namespace: &str,
    args: impl IntoIterator<Item = &'a T>,
) -> HashId {
    let mut hasher = DefaultHasher::new();
    hasher.write(namespace.as_bytes());
    for arg in args {
        arg.hash(&mut hasher);
    }
    hasher.finish()
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
        allocator.create_unit_list(factory.create_string_term(message.into())),
    ))))
}
