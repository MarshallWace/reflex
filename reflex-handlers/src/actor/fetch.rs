// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    str::FromStr,
};

use bytes::Bytes;
use http::{header::HeaderName, HeaderValue};
use hyper::Body;
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator, ListTermType,
    RecordTermType, RefType, SignalType, StateToken, StringTermType, StringValue,
    StructPrototypeType, Uuid,
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
    action::fetch::{FetchHandlerConnectionErrorAction, FetchHandlerFetchCompleteAction},
    task::fetch::{create_fetch_error_message, FetchHandlerTask, FetchHandlerTaskFactory},
    utils::fetch::FetchRequest,
};

pub const EFFECT_TYPE_FETCH: &'static str = "reflex::fetch";

#[derive(Clone, Copy, Debug)]
pub struct FetchHandlerMetricNames {
    pub fetch_effect_total_request_count: &'static str,
    pub fetch_effect_active_request_count: &'static str,
}
impl FetchHandlerMetricNames {
    fn init(self) -> Self {
        describe_counter!(
            self.fetch_effect_total_request_count,
            Unit::Count,
            "Total Fetch effect request count"
        );
        describe_gauge!(
            self.fetch_effect_active_request_count,
            Unit::Count,
            "Active Fetch effect request count"
        );
        self
    }
}
impl Default for FetchHandlerMetricNames {
    fn default() -> Self {
        Self {
            fetch_effect_total_request_count: "fetch_effect_total_request_count",
            fetch_effect_active_request_count: "fetch_effect_active_request_count",
        }
    }
}

#[derive(Named, Clone)]
pub struct FetchHandler<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    client: hyper::Client<TConnect, Body>,
    factory: TFactory,
    allocator: TAllocator,
    metric_names: FetchHandlerMetricNames,
    main_pid: ProcessId,
    _expression: PhantomData<T>,
}
impl<T, TFactory, TAllocator, TConnect> FetchHandler<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    pub fn new(
        client: hyper::Client<TConnect, Body>,
        factory: TFactory,
        allocator: TAllocator,
        metric_names: FetchHandlerMetricNames,
        main_pid: ProcessId,
    ) -> Self {
        Self {
            factory,
            allocator,
            client,
            metric_names: metric_names.init(),
            main_pid,
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct FetchHandlerState {
    tasks: HashMap<StateToken, RequestState>,
    operation_effect_mappings: HashMap<Uuid, StateToken>,
}
impl FetchHandlerState {
    fn subscribe_fetch_task<TConnect>(
        &mut self,
        effect_id: StateToken,
        request: FetchRequest,
        client: &hyper::Client<TConnect, Body>,
        metric_names: &FetchHandlerMetricNames,
        context: &mut impl HandlerContext,
    ) -> Option<(ProcessId, FetchHandlerTaskFactory<TConnect>)>
    where
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    {
        let entry = match self.tasks.entry(effect_id) {
            Entry::Vacant(entry) => Some(entry),
            Entry::Occupied(_) => None,
        }?;
        let operation_id = Uuid::new_v4();
        // TODO: Allow configurable Fetch effect metric labels
        let metric_labels = [
            ("method", request.method.clone()),
            ("url", request.url.clone()),
        ];
        increment_counter!(
            metric_names.fetch_effect_total_request_count,
            &metric_labels
        );
        increment_gauge!(
            metric_names.fetch_effect_active_request_count,
            1.0,
            &metric_labels
        );
        let (task_pid, task) = create_fetch_task(operation_id, client.clone(), request, context);
        entry.insert(RequestState {
            operation_id,
            task_pid,
            metric_labels,
        });
        self.operation_effect_mappings
            .insert(operation_id, effect_id);
        Some((task_pid, task))
    }
    fn unsubscribe_fetch_task(
        &mut self,
        effect_id: StateToken,
        metric_names: &FetchHandlerMetricNames,
    ) -> Option<ProcessId> {
        let RequestState {
            operation_id,
            task_pid,
            metric_labels,
        } = self.tasks.remove(&effect_id)?;
        decrement_gauge!(
            metric_names.fetch_effect_active_request_count,
            1.0,
            &metric_labels
        );
        let _ = self.operation_effect_mappings.remove(&operation_id)?;
        Some(task_pid)
    }
}

struct RequestState {
    operation_id: Uuid,
    task_pid: ProcessId,
    metric_labels: [(&'static str, String); 2],
}

dispatcher!({
    pub enum FetchHandlerAction<T: Expression> {
        Inbox(EffectSubscribeAction<T>),
        Inbox(EffectUnsubscribeAction<T>),
        Inbox(FetchHandlerFetchCompleteAction),
        Inbox(FetchHandlerConnectionErrorAction),

        Outbox(EffectEmitAction<T>),
    }

    impl<T, TFactory, TAllocator, TConnect, TAction, TTask> Dispatcher<TAction, TTask>
        for FetchHandler<T, TFactory, TAllocator, TConnect>
    where
        T: AsyncExpression,
        TFactory: AsyncExpressionFactory<T>,
        TAllocator: AsyncHeapAllocator<T>,
        TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask> + FetchHandlerTask<TConnect>,
    {
        type State = FetchHandlerState;
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
            action.effect_type.as_str() == EFFECT_TYPE_FETCH
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
            action.effect_type.as_str() == EFFECT_TYPE_FETCH
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

        fn accept(&self, _action: &FetchHandlerFetchCompleteAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &FetchHandlerFetchCompleteAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &FetchHandlerFetchCompleteAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_fetch_handler_fetch_complete(state, action, metadata, context)
        }

        fn accept(&self, _action: &FetchHandlerConnectionErrorAction) -> bool {
            true
        }
        fn schedule(
            &self,
            _action: &FetchHandlerConnectionErrorAction,
            _state: &Self::State,
        ) -> Option<SchedulerMode> {
            Some(SchedulerMode::Async)
        }
        fn handle(
            &self,
            state: &mut Self::State,
            action: &FetchHandlerConnectionErrorAction,
            metadata: &MessageData,
            context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>> {
            self.handle_fetch_handler_connection_error(state, action, metadata, context)
        }
    }
});

impl<T, TFactory, TAllocator, TConnect> FetchHandler<T, TFactory, TAllocator, TConnect>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn handle_effect_subscribe<TAction, TTask>(
        &self,
        state: &mut FetchHandlerState,
        action: &EffectSubscribeAction<T>,
        _metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask> + From<FetchHandlerTaskFactory<TConnect>>,
    {
        let EffectSubscribeAction {
            effect_type,
            effects,
        } = action;
        if effect_type.as_str() != EFFECT_TYPE_FETCH {
            return None;
        }
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_fetch_effect_args(effect, &self.factory) {
                    Ok(request) => {
                        match state.subscribe_fetch_task(
                            effect.id(),
                            request,
                            &self.client,
                            &self.metric_names,
                            context,
                        ) {
                            None => None,
                            Some((task_pid, task)) => Some((
                                (
                                    state_token,
                                    create_pending_expression(&self.factory, &self.allocator),
                                ),
                                Some(SchedulerCommand::Task(task_pid, task.into())),
                            )),
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
            Some(SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_FETCH.into(),
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
        state: &mut FetchHandlerState,
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
        if effect_type.as_str() != EFFECT_TYPE_FETCH {
            return None;
        }
        let active_pids = effects
            .iter()
            .filter_map(|effect| state.unsubscribe_fetch_task(effect.id(), &self.metric_names));
        Some(SchedulerTransition::new(
            active_pids.map(SchedulerCommand::Kill),
        ))
    }
    fn handle_fetch_handler_fetch_complete<TAction, TTask>(
        &self,
        state: &mut FetchHandlerState,
        action: &FetchHandlerFetchCompleteAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let FetchHandlerFetchCompleteAction {
            operation_id,
            status_code,
            body,
            ..
        } = action;
        let effect_id = state.operation_effect_mappings.get(operation_id).cloned()?;
        let task_pid = state.unsubscribe_fetch_task(effect_id, &self.metric_names)?;
        let factory = &self.factory;
        let allocator = &self.allocator;
        let result = match String::from_utf8(body.into_iter().copied().collect()) {
            Ok(body) => factory.create_list_term(allocator.create_pair(
                factory.create_int_term(status_code.as_u16().into()),
                factory.create_string_term(allocator.create_string(body)),
            )),
            Err(err) => {
                create_error_expression(create_fetch_error_message(err), factory, allocator)
            }
        };
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(task_pid),
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_FETCH.into(),
                        updates: vec![(effect_id, result)],
                    }],
                }
                .into(),
            ),
        ]))
    }
    fn handle_fetch_handler_connection_error<TAction, TTask>(
        &self,
        state: &mut FetchHandlerState,
        action: &FetchHandlerConnectionErrorAction,
        _metadata: &MessageData,
        _context: &mut impl HandlerContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>
    where
        TAction: Action + From<EffectEmitAction<T>>,
        TTask: TaskFactory<TAction, TTask>,
    {
        let FetchHandlerConnectionErrorAction {
            operation_id,
            message,
            ..
        } = action;
        let effect_id = state.operation_effect_mappings.get(operation_id).cloned()?;
        let task_pid = state.unsubscribe_fetch_task(effect_id, &self.metric_names)?;
        let result = create_error_expression(message.clone(), &self.factory, &self.allocator);
        Some(SchedulerTransition::new([
            SchedulerCommand::Kill(task_pid),
            SchedulerCommand::Send(
                self.main_pid,
                EffectEmitAction {
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_FETCH.into(),
                        updates: vec![(effect_id, result)],
                    }],
                }
                .into(),
            ),
        ]))
    }
}

fn create_fetch_task<TConnect>(
    operation_id: Uuid,
    client: hyper::Client<TConnect, Body>,
    request: FetchRequest,
    context: &mut impl HandlerContext,
) -> (ProcessId, FetchHandlerTaskFactory<TConnect>)
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    let task_pid = context.generate_pid();
    let current_pid = context.pid();
    let task = FetchHandlerTaskFactory {
        operation_id,
        client,
        request,
        caller_pid: current_pid,
    };
    (task_pid, task)
}

fn parse_fetch_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<FetchRequest, String> {
    let payload = effect.payload().as_deref();
    let args = factory
        .match_list_term(payload)
        .map(|term| term.items().as_deref())
        .filter(|args| args.len() == 4)
        .ok_or_else(|| {
            format!(
                "Invalid fetch signal: Expected 4 arguments, received {}",
                payload
            )
        })?;
    let mut args = args.iter().map(|iter| iter.as_deref());
    let url = parse_string_arg(args.next().unwrap(), factory);
    let method = parse_string_arg(args.next().unwrap(), factory);
    let headers = parse_key_values_arg(args.next().unwrap(), factory);
    let body = parse_optional_string_arg(args.next().unwrap(), factory);
    match (method, url, headers, body) {
        (Some(method), Some(url), Some(headers), Some(body)) => {
            let headers = format_request_headers(headers)?;
            Ok(FetchRequest {
                method,
                url,
                headers,
                body: body.map(Bytes::from),
            })
        }
        _ => Err(format!("Invalid fetch signal arguments: {}", payload)),
    }
}

fn format_request_headers(
    headers: impl IntoIterator<Item = (impl Into<String>, impl Into<String>)>,
) -> Result<Vec<(HeaderName, HeaderValue)>, String> {
    headers
        .into_iter()
        .map(|(key, value)| {
            let key: String = key.into();
            let value: String = value.into();
            let key = HeaderName::from_str(key.as_str())
                .map_err(|_| format!("Invalid fetch header name: {}", key))?;
            let value = HeaderValue::from_str(value.as_str())
                .map_err(|_| format!("Invalid value for fetch header {}: {}", key, value))?;
            Ok((key, value))
        })
        .collect::<Result<Vec<_>, _>>()
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_string_term(value) {
        Some(term) => Some(String::from(term.value().as_deref().as_str())),
        _ => None,
    }
}

fn parse_optional_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<String>> {
    match factory.match_string_term(value) {
        Some(term) => Some(Some(String::from(term.value().as_deref().as_str()))),
        _ => match factory.match_nil_term(value) {
            Some(_) => Some(None),
            _ => None,
        },
    }
}

fn parse_key_values_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Vec<(String, String)>> {
    if let Some(value) = factory.match_record_term(value) {
        value
            .prototype()
            .as_deref()
            .keys()
            .as_deref()
            .iter()
            .map(|item| item.as_deref())
            .zip(value.values().as_deref().iter().map(|item| item.as_deref()))
            .map(|(key, value)| {
                match (
                    factory.match_string_term(key),
                    factory.match_string_term(value),
                ) {
                    (Some(key), Some(value)) => Some((
                        String::from(key.value().as_deref().as_str()),
                        String::from(value.value().as_deref().as_str()),
                    )),
                    _ => None,
                }
            })
            .collect::<Option<Vec<_>>>()
    } else {
        None
    }
}

fn create_pending_expression<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Pending,
        factory.create_nil_term(),
        factory.create_nil_term(),
    ))))
}

fn create_error_expression<T: Expression>(
    message: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        factory.create_string_term(allocator.create_string(message)),
        factory.create_nil_term(),
    ))))
}
