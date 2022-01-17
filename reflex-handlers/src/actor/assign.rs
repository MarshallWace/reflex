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
    StateUpdate,
};

pub const EFFECT_TYPE_ASSIGN: &'static str = "reflex::assign";

pub trait AssignHandlerAction<T: Expression>:
    Action + InboundAction<EffectSubscribeAction<T>> + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> AssignHandlerAction<T> for TAction where
    Self: Action + InboundAction<EffectSubscribeAction<T>> + OutboundAction<EffectEmitAction<T>>
{
}

pub struct AssignHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> AssignHandler<T, TFactory, TAllocator>
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

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for AssignHandler<T, TFactory, TAllocator>
where
    T: Expression + Send + 'static,
    TFactory: ExpressionFactory<T> + Clone + Send + 'static,
    TAllocator: HeapAllocator<T> + Clone + Send + 'static,
    TAction: AssignHandlerAction<T> + 'static,
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
impl<T, TFactory, TAllocator> AssignHandler<T, TFactory, TAllocator>
where
    T: Expression + Send + 'static,
    TFactory: ExpressionFactory<T> + Clone + Send + 'static,
    TAllocator: HeapAllocator<T> + Clone + Send + 'static,
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
        if effect_type.as_str() != EFFECT_TYPE_ASSIGN {
            return StateTransition::new(None);
        }
        let current_pid = context.pid();
        let updates = effects
            .iter()
            .flat_map(|effect| {
                let state_token = effect.id();
                let entry = parse_assign_effect_args(effect, &self.factory);
                let (update, result) = match entry {
                    Ok((key, value)) => (
                        Some((key, StateUpdate::Value(value.clone()))),
                        StateUpdate::Value(value),
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
            })
            .collect();
        StateTransition::new(Some(StateOperation::Send(
            current_pid,
            EffectEmitAction { updates }.into(),
        )))
    }
}

fn parse_assign_effect_args<T: Expression>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<(HashId, T), String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid assign signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    // TODO: Allow arbitrary expressions as state tokens
    let state_token = parse_hash_arg(args.next().unwrap(), factory);
    let value = args.next().unwrap();
    match state_token {
        Some(state_token) => Ok((state_token, value.clone())),
        _ => Err(format!(
            "Invalid assign signal arguments: {}",
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
