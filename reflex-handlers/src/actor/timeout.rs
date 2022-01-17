// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    time::Duration,
};

use futures::{stream, FutureExt, StreamExt};
use reflex::{
    core::{Expression, ExpressionFactory, HeapAllocator, Signal, SignalType, StateToken},
    lang::ValueTerm,
};
use reflex_dispatcher::{
    Action, Actor, HandlerContext, InboundAction, MessageData, OperationStream, OutboundAction,
    ProcessId, StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, StateUpdate,
};
use tokio::time::sleep;

pub const EFFECT_TYPE_TIMEOUT: &'static str = "reflex::timeout";

pub trait TimeoutHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> TimeoutHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

pub struct TimeoutHandler<T, TFactory, TAllocator>
where
    T: Expression,
    TFactory: ExpressionFactory<T>,
    TAllocator: HeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    state: TimeoutHandlerState,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
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

#[derive(Default)]
struct TimeoutHandlerState {
    tasks: HashMap<StateToken, ProcessId>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: TimeoutHandlerAction<T> + Send + 'static,
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
        } else {
            StateTransition::new(None)
        }
    }
}
impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
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
        TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
            return StateTransition::new(None);
        }
        let current_pid = context.pid();
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_timeout_effect_args(effect, &self.factory) {
                    Ok(duration) => match duration {
                        None => Some((
                            (
                                state_token,
                                StateUpdate::Value(self.factory.create_value_term(ValueTerm::Null)),
                            ),
                            None,
                        )),
                        Some(duration) => {
                            if let Entry::Vacant(entry) = self.state.tasks.entry(state_token) {
                                let (task_pid, task) = create_timeout_task(
                                    state_token,
                                    duration,
                                    &self.factory,
                                    context,
                                );
                                entry.insert(task_pid);
                                Some((
                                    (
                                        state_token,
                                        StateUpdate::Value(create_pending_expression(
                                            &self.factory,
                                            &self.allocator,
                                        )),
                                    ),
                                    Some(StateOperation::Task(task_pid, task)),
                                ))
                            } else {
                                None
                            }
                        }
                    },
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
        StateTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().flatten()),
        )
    }
    fn handle_effect_unsubscribe<TAction>(
        &mut self,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> StateTransition<TAction>
    where
        TAction: Action,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
            return StateTransition::new(None);
        }
        StateTransition::new(effects.iter().filter_map(|effect| {
            if let Entry::Occupied(entry) = self.state.tasks.entry(effect.id()) {
                let pid = entry.remove();
                Some(StateOperation::Kill(pid))
            } else {
                None
            }
        }))
    }
}

fn create_timeout_task<T: AsyncExpression, TAction>(
    state_token: StateToken,
    duration: f64,
    factory: &impl AsyncExpressionFactory<T>,
    context: &mut impl HandlerContext,
) -> (ProcessId, OperationStream<TAction>)
where
    TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
{
    let task_pid = context.generate_pid();
    let stream = sleep(Duration::from_millis(duration as u64))
        .into_stream()
        .map({
            let factory = factory.clone();
            let main_pid = context.pid();
            move |_| {
                StateOperation::Send(
                    main_pid,
                    EffectEmitAction {
                        updates: vec![(
                            state_token,
                            StateUpdate::Value(factory.create_value_term(ValueTerm::Null)),
                        )],
                    }
                    .into(),
                )
            }
        })
        .chain(stream::iter(once(StateOperation::Kill(task_pid))));
    (task_pid, OperationStream::new(Box::pin(stream)))
}

fn parse_timeout_effect_args<T: Expression>(
    effect: &Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<f64>, String> {
    let mut args = effect.args().into_iter();
    if args.len() != 2 {
        return Err(format!(
            "Invalid timeout signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let duration = parse_number_arg(args.next().unwrap(), factory);
    let _token = args.next().unwrap();
    match duration {
        Some(duration) if duration == 0.0 => Ok(None),
        Some(duration) => Ok(Some(duration.max(1.0))),
        _ => Err(format!(
            "Invalid timeout signal arguments: {}",
            effect
                .args()
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
        )),
    }
}

fn parse_number_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<f64> {
    match factory.match_value_term(value) {
        Some(ValueTerm::Int(value)) => Some(*value as f64),
        Some(ValueTerm::Float(value)) => Some(*value),
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
