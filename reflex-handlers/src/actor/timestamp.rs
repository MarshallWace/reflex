// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use futures::StreamExt;
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, FloatTermType, HeapAllocator,
    IntTermType, RefType, SignalType, StateToken,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction},
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};
use tokio::time::{interval_at, Instant};
use tokio_stream::wrappers::IntervalStream;

pub const EFFECT_TYPE_TIMESTAMP: &'static str = "reflex::timestamp";

pub trait TimestampHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> TimestampHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

#[derive(Clone)]
pub struct TimestampHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> TimestampHandler<T, TFactory, TAllocator>
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

#[derive(Default)]
pub struct TimestampHandlerState {
    tasks: HashMap<StateToken, ProcessId>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for TimestampHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: Action + Send + 'static + TimestampHandlerAction<T>,
{
    type State = TimestampHandlerState;
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
impl<T, TFactory, TAllocator> TimestampHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut TimestampHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMESTAMP {
            return None;
        }
        let current_pid = context.pid();
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_timestamp_effect_args(effect, &self.factory) {
                    Ok(duration) => {
                        if let Entry::Vacant(entry) = state.tasks.entry(state_token) {
                            let (task_pid, task) = create_timestamp_task(
                                state_token,
                                duration,
                                &self.factory,
                                context,
                            );
                            entry.insert(task_pid);
                            Some((
                                (
                                    state_token,
                                    self.factory.create_float_term(get_current_time()),
                                ),
                                Some(StateOperation::Task(task_pid, task)),
                            ))
                        } else {
                            None
                        }
                    }
                    Err(err) => Some((
                        (
                            state_token,
                            create_error_expression(err, &self.factory, &self.allocator),
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
        state: &mut TimestampHandlerState,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<StateTransition<TAction>>
    where
        TAction: Action,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMESTAMP {
            return None;
        }
        let unsubscribe_actions = effects.iter().filter_map(|effect| {
            if let Entry::Occupied(entry) = state.tasks.entry(effect.id()) {
                let pid = entry.remove();
                Some(StateOperation::Kill(pid))
            } else {
                None
            }
        });
        Some(StateTransition::new(unsubscribe_actions))
    }
}

fn create_timestamp_task<T: AsyncExpression, TAction>(
    state_token: StateToken,
    duration: f64,
    factory: &impl AsyncExpressionFactory<T>,
    context: &mut impl HandlerContext,
) -> (ProcessId, OperationStream<TAction>)
where
    TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
{
    let task_pid = context.generate_pid();
    let period = Duration::from_millis(duration as u64);
    let now = Instant::now();
    let first_update = now.checked_add(period).unwrap_or(now);
    let stream = IntervalStream::new(interval_at(first_update, period)).map({
        let factory = factory.clone();
        let main_pid = context.pid();
        move |_| {
            StateOperation::Send(
                main_pid,
                EffectEmitAction {
                    updates: vec![(state_token, factory.create_float_term(get_current_time()))],
                }
                .into(),
            )
        }
    });
    (task_pid, OperationStream::new(stream))
}

fn get_current_time() -> f64 {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs_f64();
    (timestamp * 1000.0).floor()
}

fn parse_timestamp_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<f64, String> {
    let args = effect.args().as_deref();
    if args.len() != 1 {
        return Err(format!(
            "Invalid timestamp signal: Expected 1 argument, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let interval = parse_number_arg(remaining_args.next().unwrap(), factory);
    match interval {
        Some(interval) if interval >= 1.0 => Ok(interval),
        _ => Err(format!(
            "Invalid timestamp signal arguments: {}",
            effect
                .args()
                .as_deref()
                .iter()
                .map(|item| item.as_deref())
                .map(|arg| format!("{}", arg))
                .collect::<Vec<_>>()
                .join(", "),
        )),
    }
}

fn parse_number_arg<T: Expression>(value: &T, factory: &impl ExpressionFactory<T>) -> Option<f64> {
    match factory.match_int_term(value) {
        Some(term) => Some(term.value() as f64),
        _ => match factory.match_float_term(value) {
            Some(term) => Some(term.value()),
            _ => None,
        },
    }
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
