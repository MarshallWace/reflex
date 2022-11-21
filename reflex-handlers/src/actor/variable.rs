// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::once,
    marker::PhantomData,
};

use reflex::{
    core::{
        ConditionType, Expression, ExpressionFactory, ExpressionListType, FloatTermType,
        HeapAllocator, IntTermType, RefType, SignalType, StateToken,
    },
    hash::HashId,
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

use crate::stdlib::{
    EFFECT_TYPE_VARIABLE_DECREMENT, EFFECT_TYPE_VARIABLE_GET, EFFECT_TYPE_VARIABLE_INCREMENT,
    EFFECT_TYPE_VARIABLE_SET,
};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
struct VariableKeyHash(HashId);
impl VariableKeyHash {
    fn new<T: Expression>(key: &T) -> Self {
        Self(key.id())
    }
}

pub struct VariableHandlerState<T: Expression> {
    subscriptions: HashMap<VariableKeyHash, VariableState<T>>,
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

#[derive(Named, Clone)]
pub struct VariableHandler<T, TFactory, TAllocator>
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
impl<T, TFactory, TAllocator> VariableHandler<T, TFactory, TAllocator>
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

dispatcher!({
    pub enum VariableHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),

        Outbox(EffectEmitAction<T>),
    }

    impl<T, TFactory, TAllocator, TAction, TTask> Dispatcher<TAction, TTask>
        for VariableHandler<T, TFactory, TAllocator>
    where
        T: AsyncExpression,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        type State = VariableHandlerState<T>;
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
            match action.effect_type.as_str() {
                EFFECT_TYPE_VARIABLE_GET
                | EFFECT_TYPE_VARIABLE_SET
                | EFFECT_TYPE_VARIABLE_INCREMENT
                | EFFECT_TYPE_VARIABLE_DECREMENT => true,
                _ => false,
            }
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
            match action.effect_type.as_str() {
                EFFECT_TYPE_VARIABLE_GET
                | EFFECT_TYPE_VARIABLE_SET
                | EFFECT_TYPE_VARIABLE_INCREMENT
                | EFFECT_TYPE_VARIABLE_DECREMENT => true,
                _ => false,
            }
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
    }
});

impl<T, TFactory, TAllocator> VariableHandler<T, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
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
    fn handle_get_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let updates = effects.iter().map(|effect| {
            match parse_get_effect_args(effect, &self.factory, &self.allocator) {
                Ok((state_token, initial_value)) => {
                    match state.subscriptions.entry(VariableKeyHash::new(state_token)) {
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_VARIABLE_GET.into(),
                    updates: updates.collect(),
                }],
            }
            .into(),
        ))))
    }
    fn handle_set_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let updates = effects
            .iter()
            .flat_map(|effect| {
                let entry = parse_set_effect_args(effect, &self.factory, &self.allocator);
                let (value, updates) = match entry {
                    Ok((state_token, value)) => {
                        let (value, updates) =
                            match state.subscriptions.entry(VariableKeyHash::new(state_token)) {
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_VARIABLE_SET.into(),
                    updates,
                }],
            }
            .into(),
        ))))
    }
    fn handle_increment_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let updates = effects
            .iter()
            .flat_map(move |effect| {
                let entry = parse_increment_effect_args(effect, &self.factory, &self.allocator);
                let (value, actions) = match entry {
                    Ok(state_token) => {
                        let (value, actions) =
                            match state.subscriptions.entry(VariableKeyHash::new(state_token)) {
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_VARIABLE_INCREMENT.into(),
                    updates,
                }],
            }
            .into(),
        ))))
    }
    fn handle_decrement_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectSubscribeAction {
            effect_type: _,
            effects,
        } = action;
        let updates = effects
            .iter()
            .flat_map(|effect| {
                let entry = parse_decrement_effect_args(effect, &self.factory, &self.allocator);
                let (value, updates) = match entry {
                    Ok(state_token) => {
                        let (value, updates) =
                            match state.subscriptions.entry(VariableKeyHash::new(state_token)) {
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
        Some(SchedulerTransition::new(once(SchedulerCommand::Send(
            self.main_pid,
            EffectEmitAction {
                effect_types: vec![EffectUpdateBatch {
                    effect_type: EFFECT_TYPE_VARIABLE_DECREMENT.into(),
                    updates,
                }],
            }
            .into(),
        ))))
    }
    fn handle_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
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
    fn handle_get_effect_unsubscribe<TAction, TTask>(
        &self,
        state: &mut VariableHandlerState<T>,
        action: &EffectUnsubscribeAction<T>,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
    {
        let EffectUnsubscribeAction {
            effect_type: _,
            effects,
        } = action;
        for effect in effects {
            if let Ok((state_token, _)) =
                parse_get_effect_args(effect, &self.factory, &self.allocator)
            {
                if let Entry::Occupied(mut entry) =
                    state.subscriptions.entry(VariableKeyHash::new(state_token))
                {
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
                Some(factory.create_int_term(term.value() + delta))
            } else if let Some(term) = factory.match_float_term(existing) {
                Some(factory.create_float_term(term.value() + (delta as f64)))
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
    effect: &'a T::Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<(&'a T, &'a T), String> {
    let args = effect.args().as_deref();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable get signal: Expected 2 argument, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let state_token = remaining_args.next().unwrap();
    let initial_value = remaining_args.next().unwrap();
    Ok((state_token, initial_value))
}

fn parse_set_effect_args<'a, T: Expression>(
    effect: &'a T::Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<(&'a T, &'a T), String> {
    let args = effect.args().as_deref();
    if args.len() != 3 {
        return Err(format!(
            "Invalid variable set signal: Expected 3 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let state_token = remaining_args.next().unwrap();
    let value = remaining_args.next().unwrap();
    let _token = remaining_args.next().unwrap();
    Ok((state_token, value))
}

fn parse_increment_effect_args<'a, T: Expression>(
    effect: &'a T::Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<&'a T, String> {
    let args = effect.args().as_deref();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable increment signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let state_token = remaining_args.next().unwrap();
    let _token = remaining_args.next().unwrap();
    Ok(state_token)
}

fn parse_decrement_effect_args<'a, T: Expression>(
    effect: &'a T::Signal<T>,
    _factory: &'a impl ExpressionFactory<T>,
    _allocator: &impl HeapAllocator<T>,
) -> Result<&'a T, String> {
    let args = effect.args().as_deref();
    if args.len() != 2 {
        return Err(format!(
            "Invalid variable decrement signal: Expected 2 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter().map(|item| item.as_deref());
    let state_token = remaining_args.next().unwrap();
    let _token = remaining_args.next().unwrap();
    Ok(state_token)
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
