// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Jordan Hall <j.hall@mwam.com> https://github.com/j-hall-mwam
use std::{
    collections::{hash_map::Entry, HashMap},
    iter::once,
    marker::PhantomData,
    str::FromStr,
    sync::{Arc, Mutex},
};

use bytes::Bytes;
use futures::{stream, FutureExt, StreamExt};
use http::{header::HeaderName, HeaderValue, StatusCode};
use hyper::Body;
use metrics::{
    decrement_gauge, describe_counter, describe_gauge, increment_counter, increment_gauge, Unit,
};
use reflex::core::{
    ConditionType, Expression, ExpressionFactory, ExpressionListType, HeapAllocator,
    RecordTermType, RefType, SignalType, StateToken, StringTermType, StringValue,
    StructPrototypeType,
};
use reflex_dispatcher::{
    Action, Actor, ActorTransition, HandlerContext, InboundAction, MessageData, OperationStream,
    OutboundAction, ProcessId, StateOperation, StateTransition,
};
use reflex_runtime::{
    action::effect::{
        EffectEmitAction, EffectSubscribeAction, EffectUnsubscribeAction, EffectUpdateBatch,
    },
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator,
};

use crate::utils::fetch::{fetch, FetchError, FetchRequest};

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

pub trait FetchHandlerAction<T: Expression>:
    Action
    + InboundAction<EffectSubscribeAction<T>>
    + InboundAction<EffectUnsubscribeAction<T>>
    + OutboundAction<EffectEmitAction<T>>
{
}
impl<T: Expression, TAction> FetchHandlerAction<T> for TAction where
    Self: Action
        + InboundAction<EffectSubscribeAction<T>>
        + InboundAction<EffectUnsubscribeAction<T>>
        + OutboundAction<EffectEmitAction<T>>
{
}

#[derive(Clone)]
pub struct FetchHandler<T, TConnect, TFactory, TAllocator>
where
    T: AsyncExpression,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
{
    client: hyper::Client<TConnect, Body>,
    factory: TFactory,
    allocator: TAllocator,
    metric_names: FetchHandlerMetricNames,
    _expression: PhantomData<T>,
}
impl<T, TConnect, TFactory, TAllocator> FetchHandler<T, TConnect, TFactory, TAllocator>
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
    ) -> Self {
        Self {
            factory,
            allocator,
            client,
            metric_names: metric_names.init(),
            _expression: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct FetchHandlerState {
    tasks: HashMap<StateToken, RequestState>,
}

struct RequestState {
    task_pid: ProcessId,
    metric_labels: Arc<Mutex<Option<[(&'static str, String); 2]>>>,
}

impl<T, TConnect, TFactory, TAllocator, TAction> Actor<TAction>
    for FetchHandler<T, TConnect, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + Send + 'static + FetchHandlerAction<T>,
{
    type State = FetchHandlerState;
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
impl<T, TConnect, TFactory, TAllocator> FetchHandler<T, TConnect, TFactory, TAllocator>
where
    T: AsyncExpression,
    TFactory: AsyncExpressionFactory<T>,
    TAllocator: AsyncHeapAllocator<T>,
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{
    fn handle_effect_subscribe<TAction>(
        &self,
        state: &mut FetchHandlerState,
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
        if effect_type.as_str() != EFFECT_TYPE_FETCH {
            return None;
        }
        let current_pid = context.pid();
        let (initial_values, tasks): (Vec<_>, Vec<_>) = effects
            .iter()
            .filter_map(|effect| {
                let state_token = effect.id();
                match parse_fetch_effect_args(effect, &self.factory) {
                    Ok(request) => {
                        // TODO: Allow configurable Fetch effect metric labels
                        let metric_labels = [
                            ("method", request.method.clone()),
                            ("url", request.url.clone()),
                        ];
                        increment_counter!(
                            self.metric_names.fetch_effect_total_request_count,
                            &metric_labels
                        );
                        increment_gauge!(
                            self.metric_names.fetch_effect_active_request_count,
                            1.0,
                            &metric_labels
                        );
                        if let Entry::Vacant(entry) = state.tasks.entry(state_token) {
                            match create_fetch_task(
                                self.client.clone(),
                                state_token,
                                &request,
                                &self.factory,
                                &self.allocator,
                                context,
                                self.metric_names,
                                metric_labels,
                            ) {
                                Err(err) => Some(((state_token, err), None)),
                                Ok((request_state, task)) => {
                                    let task_pid = request_state.task_pid;
                                    entry.insert(request_state);
                                    Some((
                                        (
                                            state_token,
                                            create_pending_expression(
                                                &self.factory,
                                                &self.allocator,
                                            ),
                                        ),
                                        Some(StateOperation::Task(task_pid, task)),
                                    ))
                                }
                            }
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
                    effect_types: vec![EffectUpdateBatch {
                        effect_type: EFFECT_TYPE_FETCH.into(),
                        updates: initial_values,
                    }],
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
        state: &mut FetchHandlerState,
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
        if effect_type.as_str() != EFFECT_TYPE_FETCH {
            return None;
        }
        Some(StateTransition::new(effects.iter().filter_map(|effect| {
            if let Entry::Occupied(entry) = state.tasks.entry(effect.id()) {
                let RequestState {
                    task_pid,
                    metric_labels,
                } = entry.remove();
                if let Some(metric_labels) = metric_labels
                    .lock()
                    .ok()
                    .and_then(|mut metric_labels| metric_labels.take())
                {
                    decrement_gauge!(
                        self.metric_names.fetch_effect_active_request_count,
                        1.0,
                        &metric_labels
                    );
                }
                Some(StateOperation::Kill(task_pid))
            } else {
                None
            }
        })))
    }
}

fn create_fetch_task<T: Expression, TConnect, TAction>(
    client: hyper::Client<TConnect, Body>,
    state_token: StateToken,
    request: &FetchRequest,
    factory: &(impl ExpressionFactory<T> + Send + Clone + 'static),
    allocator: &(impl HeapAllocator<T> + Send + Clone + 'static),
    context: &mut impl HandlerContext,
    metric_names: FetchHandlerMetricNames,
    metric_labels: [(&'static str, String); 2],
) -> Result<(RequestState, OperationStream<TAction>), T>
where
    TConnect: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    TAction: Action + Send + 'static + OutboundAction<EffectEmitAction<T>>,
{
    let request = fetch(client, request)
        .map_err(|err| create_fetch_error_expression(err, factory, allocator))?;
    let task_pid = context.generate_pid();
    let shared_metric_labels = Arc::new(Mutex::new(Some(metric_labels)));
    let stream = request
        .map({
            let factory = factory.clone();
            let allocator = allocator.clone();
            let main_pid = context.pid();
            let metric_labels = shared_metric_labels.clone();
            move |result| {
                if let Some(metric_labels) = metric_labels
                    .lock()
                    .ok()
                    .and_then(|mut metric_labels| metric_labels.take())
                {
                    decrement_gauge!(
                        metric_names.fetch_effect_active_request_count,
                        1.0,
                        &metric_labels
                    );
                }
                let value = parse_fetch_result(result, &factory, &allocator);
                StateOperation::Send(
                    main_pid,
                    EffectEmitAction {
                        effect_types: vec![EffectUpdateBatch {
                            effect_type: EFFECT_TYPE_FETCH.into(),
                            updates: vec![(state_token, value)],
                        }],
                    }
                    .into(),
                )
            }
        })
        .into_stream()
        .chain(stream::iter(once(StateOperation::Kill(task_pid))));
    Ok((
        RequestState {
            task_pid,
            metric_labels: shared_metric_labels,
        },
        OperationStream::new(Box::pin(stream)),
    ))
}

fn parse_fetch_result<T: Expression>(
    result: Result<(StatusCode, Bytes), FetchError>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    match result {
        Ok((status, body)) => match String::from_utf8(body.into_iter().collect()) {
            Ok(body) => factory.create_list_term(allocator.create_pair(
                factory.create_int_term(status.as_u16().into()),
                factory.create_string_term(allocator.create_string(body)),
            )),
            Err(err) => create_fetch_error_expression(err, factory, allocator),
        },
        Err(err) => create_fetch_error_expression(err, factory, allocator),
    }
}

fn create_fetch_error_expression<T: Expression>(
    err: impl std::fmt::Display,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    create_error_expression(format!("Fetch error: {}", err), factory, allocator)
}

fn parse_fetch_effect_args<T: Expression>(
    effect: &T::Signal<T>,
    factory: &impl ExpressionFactory<T>,
) -> Result<FetchRequest, String> {
    let args = effect.args().as_deref();
    if args.len() != 4 {
        return Err(format!(
            "Invalid fetch signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let mut remaining_args = args.iter();
    let url = parse_string_arg(
        remaining_args.next().map(|iter| iter.as_deref()).unwrap(),
        factory,
    );
    let method = parse_string_arg(
        remaining_args.next().map(|iter| iter.as_deref()).unwrap(),
        factory,
    );
    let headers = parse_key_values_arg(
        remaining_args.next().map(|iter| iter.as_deref()).unwrap(),
        factory,
    );
    let body = parse_optional_string_arg(
        remaining_args.next().map(|iter| iter.as_deref()).unwrap(),
        factory,
    );
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
        _ => Err(format!(
            "Invalid fetch signal arguments: {}",
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
