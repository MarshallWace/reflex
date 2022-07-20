// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
};

use reflex::core::{Expression, ExpressionFactory, HeapAllocator, Signal, SignalType, StateToken};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OutboundAction,
    StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::stdlib::{
    EFFECT_TYPE_VARIABLE_DECREMENT, EFFECT_TYPE_VARIABLE_GET, EFFECT_TYPE_VARIABLE_INCREMENT,
    EFFECT_TYPE_VARIABLE_SET,
};

pub trait VariableHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> VariableHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

pub struct VariableHandlerState<T: Expression> {
    subscriptions: HashMap<T, VariableState<T>>,
}
impl<T: Expression> Default for VariableHandlerState<T> {
    fn default() -> Self {
        Self {
            subscriptions: Default::default(),
        }
    }
}

struct VariableState<T: Expression> {
    value: Option<T>,
    effect_ids: HashSet<StateToken>,
}

#[derive(Clone)]
pub struct VariableHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> VariableHandler<T, TFactory, TAllocator>
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

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for VariableHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: VariableHandlerAction<T> + 'static,
{
    type State = VariableHandlerState<T>;
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
        } else {
            None
        }
        .unwrap_or_default();
        ActorTransition::new(state, actions)
    }
}
impl<T, TFactory, TAllocator> VariableHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        match action.effect_type.as_str() {
            EFFECT_TYPE_VARIABLE_GET => {
                self.handle_get_effect_subscribe(state, action, metadata, context)
            }
            EFFECT_TYPE_VARIABLE_SET => {
                self.handle_set_effect_subscribe(state, action, metadata, context)
            }
            EFFECT_TYPE_VARIABLE_INCREMENT => {
                self.handle_increment_effect_subscribe(state, action, metadata, context)
            }
            EFFECT_TYPE_VARIABLE_DECREMENT => {
                self.handle_decrement_effect_subscribe(state, action, metadata, context)
            }
            _ => None,
        }
    }
    fn handle_get_effect_subscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let updates = effects.iter().map(|effect| {
            match parse_get_effect_args(effect, &self.factory, &self.allocator) {
                Ok((state_token, initial_value)) => {
                    match state.subscriptions.entry(state_token.clone()) {
                        Entry::Vacant(entry) => {
                            entry.insert(VariableState {
                                value: None,
                                effect_ids: once(effect.id()).collect(),
                            });
                            (effect.id(), initial_value.clone())
                        }
                        Entry::Occupied(mut entry) => {
                            let value = {
                                let existing_value = entry.get().value.as_ref();
                                existing_value.unwrap_or(initial_value).clone()
                            };
                            entry.get_mut().effect_ids.insert(effect.id());
                            (effect.id(), value)
                        }
                    }
                }
                Err(err) => (
                    effect.id(),
                    create_error_expression(err, &self.factory, &self.allocator),
                ),
            }
        });
        Some(StateTransition::new(once(StateOperation::Send(
            context.pid(),
            EffectEmitAction {
                updates: updates.collect(),
            }
            .into(),
        ))))
    }
    fn handle_set_effect_subscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let current_pid = context.pid();
        let updates = effects
            .iter()
            .flat_map(|effect| {
                let entry = parse_set_effect_args(effect, &self.factory, &self.allocator);
                let (value, updates) = match entry {
                    Ok((state_token, value)) => {
                        let (value, updates) = match state.subscriptions.entry(state_token.clone())
                        {
                            Entry::Vacant(entry) => {
                                let updated_value = value.clone();
                                entry.insert(VariableState {
                                    value: Some(updated_value.clone()),
                                    effect_ids: Default::default(),
                                });
                                (updated_value, None)
                            }
                            Entry::Occupied(mut entry) => {
                                let updated_value = value.clone();
                                entry.get_mut().value = Some(updated_value.clone());
                                let updates = entry
                                    .get()
                                    .effect_ids
                                    .iter()
                                    .cloned()
                                    .map({
                                        let updated_value = updated_value.clone();
                                        move |state_token| (state_token, updated_value.clone())
                                    })
                                    .collect::<Vec<_>>();
                                (updated_value, Some(updates))
                            }
                        };
                        (value, updates)
                    }
                    Err(err) => (
                        create_error_expression(err, &self.factory, &self.allocator),
                        None,
                    ),
                };
                updates
                    .into_iter()
                    .flatten()
                    .chain(once((effect.id(), value)))
            })
            .collect();
        Some(StateTransition::new(once(StateOperation::Send(
            current_pid,
            EffectEmitAction { updates }.into(),
        ))))
    }
    fn handle_increment_effect_subscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let current_pid = context.pid();
        let updates = effects
            .iter()
            .flat_map(move |effect| {
                let entry = parse_increment_effect_args(effect, &self.factory, &self.allocator);
                let (value, actions) = match entry {
                    Ok(state_token) => {
                        let (value, actions) = match state.subscriptions.entry(state_token.clone())
                        {
                            Entry::Vacant(entry) => {
                                let updated_value =
                                    increment_variable(None, &self.factory, &self.allocator);
                                entry.insert(VariableState {
                                    value: Some(updated_value.clone()),
                                    effect_ids: Default::default(),
                                });
                                (updated_value, None)
                            }
                            Entry::Occupied(mut entry) => {
                                let updated_value = increment_variable(
                                    entry.get().value.as_ref(),
                                    &self.factory,
                                    &self.allocator,
                                );
                                entry.get_mut().value = Some(updated_value.clone());
                                let updates = entry
                                    .get()
                                    .effect_ids
                                    .iter()
                                    .cloned()
                                    .map(|state_token| (state_token, updated_value.clone()))
                                    .collect::<Vec<_>>();
                                (updated_value, Some(updates))
                            }
                        };
                        (value, actions)
                    }
                    Err(err) => (
                        create_error_expression(err, &self.factory, &self.allocator),
                        None,
                    ),
                };
                actions
                    .into_iter()
                    .flatten()
                    .chain(once((effect.id(), value)))
            })
            .collect();
        Some(StateTransition::new(once(StateOperation::Send(
            current_pid,
            EffectEmitAction { updates }.into(),
        ))))
    }
    fn handle_decrement_effect_subscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let current_pid = context.pid();
        let updates = effects
            .iter()
            .flat_map(|effect| {
                let entry = parse_decrement_effect_args(effect, &self.factory, &self.allocator);
                let (value, updates) = match entry {
                    Ok(state_token) => {
                        let (value, updates) = match state.subscriptions.entry(state_token.clone())
                        {
                            Entry::Vacant(entry) => {
                                let updated_value =
                                    decrement_variable(None, &self.factory, &self.allocator);
                                entry.insert(VariableState {
                                    value: Some(updated_value.clone()),
                                    effect_ids: Default::default(),
                                });
                                (updated_value, None)
                            }
                            Entry::Occupied(mut entry) => {
                                let updated_value = decrement_variable(
                                    entry.get().value.as_ref(),
                                    &self.factory,
                                    &self.allocator,
                                );
                                entry.get_mut().value = Some(updated_value.clone());
                                let updates = entry
                                    .get()
                                    .effect_ids
                                    .iter()
                                    .cloned()
                                    .map(|state_token| (state_token, updated_value.clone()))
                                    .collect::<Vec<_>>();
                                (updated_value, Some(updates))
                            }
                        };
                        (value, updates)
                    }
                    Err(err) => (
                        create_error_expression(err, &self.factory, &self.allocator),
                        None,
                    ),
                };
                updates
                    .into_iter()
                    .flatten()
                    .chain(once((effect.id(), value)))
            })
            .collect();
        Some(StateTransition::new(once(StateOperation::Send(
            current_pid,
            EffectEmitAction { updates }.into(),
        ))))
    }
    fn handle_effect_unsubscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects: _,
        } = action;
        match effect_type.as_str() {
            EFFECT_TYPE_VARIABLE_GET => {
                self.handle_get_effect_unsubscribe(state, action, metadata, context)
            }
            _ => None,
        }
    }
    fn handle_get_effect_unsubscribe<TAction>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EffectUnsubscribeAction {
            effect_type: _,
            effects,
        } = action;
        for effect in effects {
            if let Ok((state_token, _)) =
                parse_get_effect_args(effect, &self.factory, &self.allocator)
            {
                if let Entry::Occupied(mut entry) = state.subscriptions.entry(state_token.clone()) {
                    entry.get_mut().effect_ids.remove(&effect.id());
                    if entry.get().effect_ids.is_empty() {
                        entry.remove();
                    }
                };
            }
        }
        None
    }
}

fn increment_variable<T: Expression>(
    existing: Option<&T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    add_integer_value("increment", 1, existing, factory, allocator)
}

fn decrement_variable<T: Expression>(
    existing: Option<&T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    add_integer_value("decrement", -1, existing, factory, allocator)
}

fn add_integer_value<T: Expression>(
    operation_label: &'static str,
    delta: i32,
    existing: Option<&T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    match existing {
        None => factory.create_int_term(delta),
        Some(existing) => {
            let result = if let Some(term) = factory.match_int_term(existing) {
                Some(factory.create_int_term(term.value + delta))
            } else if let Some(term) = factory.match_float_term(existing) {
                Some(factory.create_float_term(term.value + (delta as f64)))
            } else if let Some(_) = factory.match_signal_term(existing) {
                Some(existing.clone())
            } else {
                None
            };
            match result {
                Some(result) => result,
                None => create_error_expression(
                    format!(
                        "Unable to {} non-numeric value: {}",
                        operation_label, existing
                    ),
                    factory,
                    allocator,
                ),
            }
        }
    }
}

fn parse_get_effect_args<'a, T: Expression>(
    effect: &'a Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<(&'a T, &'a T), String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable get signal: Expected 2 argument, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    let initial_value = args.next().unwrap();
    Ok((state_token, initial_value))
}

fn parse_set_effect_args<'a, T: Expression>(
    effect: &'a Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<(&'a T, &'a T), String> {
    let mut args = effect.args().into_iter();
    if args.len() != 3 {
        return Err(format!(
            "Invalid variable set signal: Expected 3 arguments, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    let value = args.next().unwrap();
    let _token = args.next().unwrap();
    Ok((state_token, value))
}

fn parse_increment_effect_args<'a, T: Expression>(
    effect: &'a Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<&'a T, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable increment signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    let _token = args.next().unwrap();
    Ok(state_token)
}

fn parse_decrement_effect_args<'a, T: Expression>(
    effect: &'a Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<&'a T, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable decrement signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let state_token = args.next().unwrap();
    let _token = args.next().unwrap();
    Ok(state_token)
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