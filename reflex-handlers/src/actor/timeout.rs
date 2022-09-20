// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    time::Duration,
};

use futures::{stream, FutureExt, StreamExt};
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

#[derive(Clone)]
pub struct TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
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
pub struct TimeoutHandlerState {
    tasks: HashMap<StateToken, ProcessId>,
}

impl<T, TFactory, TAllocator, TAction> Actor<TAction> for TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TAction: TimeoutHandlerAction<T> + Send + 'static,
{
    type State = TimeoutHandlerState;
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
impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut TimeoutHandlerState,
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
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
            return None;
        }
        let current_pid = context.pid();
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_timeout_effect_args(effect, &self.factory) {
                    Ok(duration) => match duration {
                        None => Some(((state_token, self.factory.create_nil_term()), None)),
                        Some(duration) => {
                            if let Entry::Vacant(entry) = state.tasks.entry(state_token) {
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
                                        create_pending_expression(&self.factory, &self.allocator),
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
        state: &mut TimeoutHandlerState,
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
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
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
                        updates: vec![(state_token, factory.create_nil_term())],
                    }
                    .into(),
                )
            }
        })
        .chain(stream::iter(once(StateOperation::Kill(task_pid))));
    (task_pid, OperationStream::new(Box::pin(stream)))
}

fn parse_timeout_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<f64>, String> {
    let args = effect.args().as_deref();
    if args.len() != 2 {
        return Err(format!(
            "Invalid timeout signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let duration = parse_number_arg(remaining_args.next().unwrap(), factory);
    let _token = remaining_args.next().unwrap();
    match duration {
        Some(duration) if duration == 0.0 => Ok(None),
        Some(duration) => Ok(Some(duration.max(1.0))),
        _ => Err(format!(
            "Invalid timeout signal arguments: {}",
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
