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

use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, FloatTermType, HeapAllocator,
    IntTermType, RefType, SignalType, StateToken, Uuid,
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
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::{
    action::timeout::TimeoutHandlerTimeoutAction,
    task::timeout::{TimeoutHandlerTask, TimeoutHandlerTaskFactory},
};

pub const EFFECT_TYPE_TIMEOUT: &'static str = "reflex::timeout";

#[derive(Named, Clone)]
pub struct TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    factory: TFactory,
    allocator: TAllocator,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    pub fn new(factory: TFactory, allocator: TAllocator, main_pid: ProcessId) -> Self {
        Self {
            factory,
            allocator,
            main_pid,
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct TimeoutHandlerState {
    active_operations: HashMap<StateToken, (Uuid, ProcessId)>,
    operation_effect_mappings: HashMap<Uuid, StateToken>,
}
impl TimeoutHandlerState {
    fn subscribe_timeout_task(
        &mut self,
        effect_id: StateToken,
        duration: Duration,
        context: &mut impl HandlerContext,
    ) -> Option<(ProcessId, TimeoutHandlerTaskFactory)> {
        let entry = match self.active_operations.entry(effect_id) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => Some(entry),
        }?;
        let operation_id = Uuid::new_v4();
        let (task_pid, task) = create_timeout_task(operation_id, duration, context);
        self.operation_effect_mappings
            .insert(operation_id, effect_id);
        entry.insert((operation_id, task_pid));
        Some((task_pid, task))
    }
    fn unsubscribe_timeout_task(&mut self, effect_id: StateToken) -> Option<ProcessId> {
        let (operation_id, task_pid) = self.active_operations.remove(&effect_id)?;
        let _ = self.operation_effect_mappings.remove(&operation_id)?;
        Some(task_pid)
    }
}

dispatcher!({
    pub enum TimeoutHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(TimeoutHandlerTimeoutAction),

        Outbox(EffectEmitAction<T>),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for TimeoutHandler<T, TFactory, TAllocator>
    where
        T: AsyncExpression,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask> + TimeoutHandlerTask,
    {
        type State = TimeoutHandlerState;
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
            action.effect_type.as_str() == EFFECT_TYPE_TIMEOUT
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
            action.effect_type.as_str() == EFFECT_TYPE_TIMEOUT
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

        fn accept(&self, _action: &TimeoutHandlerTimeoutAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &TimeoutHandlerTimeoutAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &TimeoutHandlerTimeoutAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_timeout_handler_timeout(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator> TimeoutHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut TimeoutHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask> + From<TimeoutHandlerTaskFactory>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
            return None;
        }
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_timeout_effect_args(effect, &self.factory) {
                    Ok(duration) => match duration {
                        None => Some(((state_token, self.factory.create_nil_term()), None)),
                        Some(duration) => {
                            match state.subscribe_timeout_task(effect.id(), duration, context) {
                                None => None,
                                Some((task_pid, task)) => {
                                    let initial_value =
                                        create_pending_expression(&self.factory, &self.allocator);
                                    Some((
                                        (state_token, initial_value),
                                        Some(SchedulerCommand::Task(task_pid, task.into())),
                                    ))
                                }
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
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_TIMEOUT.into(),
                        updates: initial_values,
                    }],
                }
                .into(),
            ))
        };
        Some(SchedulerTransition::new(
            initial_values_action
                .into_iter()
                .chain(tasks.into_iter().flatten()),
        ))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut TimeoutHandlerState,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_TIMEOUT {
            return None;
        }
        let active_pids = effects
            .iter()
            .filter_map(|effect| state.unsubscribe_timeout_task(effect.id()));
        Some(SchedulerTransition::new(
            active_pids.map(SchedulerCommand::Kill),
        ))
    }
    fn handle_timeout_handler_timeout<TAction, TTask>(
        &self,
        state: &mut TimeoutHandlerState,
        action: &TimeoutHandlerTimeoutAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let TimeoutHandlerTimeoutAction { operation_id } = action;
        let effect_id = state.operation_effect_mappings.get(operation_id).copied()?;
        let task_pid = state.unsubscribe_timeout_task(effect_id)?;
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(task_pid),
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_TIMEOUT.into(),
                        updates: vec![(effect_id, self.factory.create_nil_term())],
                    }],
                }
                .into(),
            ),
        ]))
    }
}

fn create_timeout_task(
    operation_id: Uuid,
    duration: Duration,
    context: &mut impl HandlerContext,
) -> (ProcessId, TimeoutHandlerTaskFactory) {
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = TimeoutHandlerTaskFactory {
        operation_id,
        duration,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

fn parse_timeout_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<Duration>, String> {
    let args = effect.args().as_deref();
    if args.len() != 2 {
        return Err(format!(
            "Invalid timeout signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let duration = parse_duration_millis_arg(remaining_args.next().unwrap(), factory);
    let _token = remaining_args.next().unwrap();
    match duration {
        Some(duration) if duration.as_millis() == 0 => Ok(None),
        Some(duration) => Ok(Some(duration.max(Duration::from_millis(1)))),
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

fn parse_duration_millis_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Duration> {
    match factory.match_int_term(value) {
        Some(term) => {
            let value = term.value();
            if value >= 0 {
                Some(Duration::from_millis(value as u64))
            } else {
                None
            }
        }
        _ => match factory.match_float_term(value) {
            Some(term) => {
                let value = term.value();
                if value >= 0.0 {
                    Some(Duration::from_millis(value.trunc() as u64))
                } else {
                    None
                }
            }
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
