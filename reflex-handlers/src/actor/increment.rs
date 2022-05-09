// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, marker::PhantomData};

use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, Signal, SignalType},
    hash::HashId,
    lang::ValueTerm,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OutboundAction, StateOperation,
    StateTransition,
};

use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, StateUpdate,
};

pub const EFFECT_TYPE_INCREMENT: &'static str = "reflex::increment";

pub trait IncrementHandlerAction<T: Expression>:
    Action + InboundAction<EffectSubscribeAction<T>> + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> IncrementHandlerAction<T> for TAction where
    Self: Action + InboundAction<EffectSubscribeAction<T>> + OutboundAction<EffectEmitAction<T>>
{
}

pub struct IncrementHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> IncrementHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    pub fn new(factory: TFactory, allocator: TAllocator) -> Self {
        Self {
            factory,
            allocator,
            _expression: Default::default(),
        }
    }
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for IncrementHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T> + Sync,
    TAllocator: AsyncHeapAllocator<T> + Sync,
    TAction: IncrementHandlerAction<T> + 'static,
{
    fn handle(
        &mut self,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction> {
        if let Some(action) = action.match_type() {
            self.handle_effect_subscribe(action, metadata, context)
        } else {
            StateTransition::new(None)
        }
    }
}
impl<T, TFactory, TAllocator> IncrementHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T> + Sync,
    TAllocator: AsyncHeapAllocator<T> + Sync,
{
    fn handle_effect_subscribe<TAction>(
        &mut self,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_INCREMENT {
            return StateTransition::new(None);
        }
        let current_pid = context.pid();
        let updates = effects.iter().flat_map(|effect| {
            let state_token = effect.id();
            let entry = parse_increment_effect_args(effect, &self.factory);
            let (update, result) = match entry {
                Ok(key) => (
                    Some((
                        key,
                        StateUpdate::patch({
                            let factory = self.factory.clone();
                            let allocator = self.allocator.clone();
                            move |existing| increment_value(existing, &factory, &allocator)
                        }),
                    )),
                    StateUpdate::Value(self.factory.create_dynamic_variable_term(
                        key,
                        create_pending_expression(&self.factory, &self.allocator),
                    )),
                ),
                Err(err) => (
                    None,
                    StateUpdate::Value(create_error_expression(
                        err,
                        &self.factory,
                        &self.allocator,
                    )),
                ),
            };
            update.into_iter().chain(once((state_token, result)))
        });
        StateTransition::new(Some(StateOperation::Send(
            current_pid,
            EffectEmitAction {
                updates: updates.collect(),
            }
            .into(),
        )))
    }
}

fn increment_value<T: Expression>(
    existing: Option<&T>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    match existing {
        None => factory.create_value_term(ValueTerm::Int(1)),
        Some(existing) => {
            let result = if let Some(value) = factory.match_value_term(existing) {
                if let Some(value) = value.match_int() {
                    Some(factory.create_value_term(ValueTerm::Int(value + 1)))
                } else if let Some(value) = value.match_float() {
                    Some(factory.create_value_term(ValueTerm::Float(value + 1.0)))
                } else {
                    None
                }
            } else if let Some(_) = factory.match_signal_term(existing) {
                Some(existing.clone())
            } else {
                None
            };
            match result {
                Some(result) => result,
                None => create_error_expression(
                    format!("Unable to increment non-numeric value: {}", existing),
                    factory,
                    allocator,
                ),
            }
        }
    }
}

fn parse_increment_effect_args<T: Expression>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<HashId, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid increment signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    // TODO: Allow arbitrary expressions as state tokens
    let state_token = parse_hash_arg(args.next().unwrap(), factory);
    let _token = args.next().unwrap();
    match state_token {
        Some(state_token) => Ok(state_token),
        _ => Err(format!(
            "Invalid increment signal arguments: {}",
            effect
                .args()
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
        )),
    }
}

fn parse_hash_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<HashId> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Hash(value)) => Some(*value),
        _ => None,
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