// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use futures::{SinkExt, Stream, StreamExt};
use hyper::{
    header::{self, HeaderValue},
    upgrade::Upgraded,
    Body, Request, Response,
};
use hyper_tungstenite::{
    tungstenite::{error::ProtocolError, Message},
    WebSocketStream,
};
use reflex::{
    compiler::{Compile, CompilerOptions, InstructionPointer, Program},
    core::{Applicable, Expression, Reducible, Rewritable, StringValue},
    stdlib::Stdlib,
};
use reflex_graphql::{
    create_json_error_object, parse_graphql_operation, sanitize_signal_errors,
    stdlib::Stdlib as GraphQlStdlib,
    subscriptions::{
        deserialize_graphql_client_message, GraphQlSubscriptionClientMessage,
        GraphQlSubscriptionServerMessage, GraphQlSubscriptionStartMessage, SubscriptionId,
    },
    AsyncGraphQlQueryTransform, GraphQlOperationPayload,
};
use reflex_json::{json_array, json_object, sanitize, JsonMap, JsonValue};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, Runtime, StreamSubscription,
};
use std::{
    collections::HashMap,
    iter::once,
    sync::{Arc, Mutex},
};
use tokio::{sync::mpsc, task::JoinHandle};

use crate::{graphql::compile_graphql_query, GraphQlHttpQueryTransform, RequestHeaders};

pub(crate) async fn handle_graphql_ws_request<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    req: Request<Body>,
    runtime: Arc<Runtime<T>>,
    graph_root: Arc<(Program, InstructionPointer)>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> Result<Response<Body>, ProtocolError>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let headers = req.headers().clone();
    let (mut response, websocket) = hyper_tungstenite::upgrade(req, None)?;
    tokio::spawn({
        let factory = factory.clone();
        let allocator = allocator.clone();
        async move {
            match websocket.await {
                Err(error) => {
                    eprintln!("Websocket connection error: {}", error);
                }
                Ok(websocket) => {
                    match handle_websocket_connection(
                        headers,
                        websocket,
                        runtime,
                        &graph_root,
                        &factory,
                        &allocator,
                        &compiler_options,
                        transform,
                    )
                    .await
                    {
                        Ok(_) => {}
                        Err(error) => {
                            eprintln!("Websocket error: {}", error);
                        }
                    }
                }
            }
        }
    });
    response.headers_mut().insert(
        header::SEC_WEBSOCKET_PROTOCOL,
        HeaderValue::from_static("graphql-ws"),
    );
    Ok(response)
}

struct Subscription {
    store_id: reflex_runtime::SubscriptionId,
    operation: GraphQlOperationPayload,
    transform: Box<dyn AsyncGraphQlQueryTransform + Send + Sync + 'static>,
    diff: bool,
    task: JoinHandle<()>,
}
impl Subscription {
    fn unsubscribe(self) -> (GraphQlOperationPayload, Box<dyn AsyncGraphQlQueryTransform>) {
        self.task.abort();
        (self.operation, self.transform)
    }
}

async fn handle_websocket_connection<
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
>(
    headers: RequestHeaders,
    websocket: WebSocketStream<Upgraded>,
    runtime: Arc<Runtime<T>>,
    graph_root: &(Program, InstructionPointer),
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
    compiler_options: &CompilerOptions,
    transform: Arc<impl GraphQlHttpQueryTransform + Send + Sync + 'static>,
) -> Result<(), Box<dyn std::error::Error>>
where
    T::String: StringValue + Send + Sync,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let output_buffer_size = 32;
    let (messages_tx, mut messages_rx) =
        mpsc::channel::<GraphQlSubscriptionServerMessage>(output_buffer_size);
    let mut subscriptions = Mutex::new(HashMap::<SubscriptionId, Subscription>::new());
    let (mut websocket_output, mut websocket_input) = websocket.split();
    let output = tokio::spawn(async move {
        while let Some(message) = messages_rx.recv().await {
            match message.into_serialized() {
                Err(error) => {
                    let response = GraphQlSubscriptionServerMessage::ConnectionError(
                        create_json_error_object(error),
                    );
                    match response.into_serialized() {
                        Ok(message) => {
                            let _ = websocket_output.send(Message::Text(message)).await;
                        }
                        Err(_) => {}
                    }
                }
                Ok(message) => {
                    let _ = websocket_output.send(Message::Text(message)).await;
                }
            }
        }
    });
    let factory = factory.clone();
    let allocator = allocator.clone();
    let headers = Arc::new(headers);
    let connection_params = Mutex::new(None);
    while let Some(message) = websocket_input.next().await {
        let messages_tx = messages_tx.clone();
        let message = match message? {
            Message::Text(data) => deserialize_graphql_client_message(&data).map(Some),
            Message::Close(_) => Ok(Some(GraphQlSubscriptionClientMessage::ConnectionTerminate)),
            Message::Binary(_) => Err(String::from("Unsupported message encoding")),
            Message::Ping(_) | Message::Pong(_) => Ok(None),
        };
        match message {
            Ok(Some(message)) => match message {
                GraphQlSubscriptionClientMessage::ConnectionInit(message) => {
                    *connection_params.lock().unwrap() = message.into_payload();
                    let _ = messages_tx
                        .send(GraphQlSubscriptionServerMessage::ConnectionAck)
                        .await;
                }
                GraphQlSubscriptionClientMessage::Start(message) => {
                    let connection_params = connection_params.lock().unwrap().as_ref().cloned();
                    match transform.factory(&headers, connection_params.as_ref()) {
                        Err((_status, error)) => {
                            let _ = messages_tx
                                .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                    create_json_error_object(error),
                                ))
                                .await;
                        }
                        Ok(transform) => {
                            let subscription_id = message.subscription_id();
                            if subscriptions
                                .get_mut()
                                .unwrap()
                                .contains_key(subscription_id)
                            {
                                let _ = messages_tx
                                    .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                        create_json_error_object(format!(
                                            "Subscription ID already exists: {}",
                                            subscription_id
                                        )),
                                    ))
                                    .await;
                            } else {
                                let operation = message.payload();
                                let diff = is_diff_subscription(&message);
                                match subscribe_query(
                                    operation,
                                    &runtime,
                                    &graph_root,
                                    &transform,
                                    compiler_options,
                                    &factory,
                                    &allocator,
                                )
                                .await
                                {
                                    Err(error) => {
                                        let _ = messages_tx
                                            .send(GraphQlSubscriptionServerMessage::Error(
                                                subscription_id.clone(),
                                                create_json_error_object(error),
                                            ))
                                            .await;
                                    }
                                    Ok(store_subscription) => {
                                        let subscription_id = message.subscription_id();
                                        let subscription = register_subscription(
                                            subscription_id,
                                            operation.clone(),
                                            transform,
                                            store_subscription,
                                            diff,
                                            &messages_tx,
                                            &factory,
                                        );
                                        subscriptions
                                            .get_mut()
                                            .unwrap()
                                            .insert(subscription_id.clone(), subscription);
                                    }
                                }
                            }
                        }
                    }
                }
                GraphQlSubscriptionClientMessage::Stop(message) => {
                    let subscription_id = message.subscription_id();
                    match subscriptions.get_mut().unwrap().remove(subscription_id) {
                        None => {
                            let _ = messages_tx
                                .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                    create_json_error_object(format!(
                                        "Subscription ID not found: {}",
                                        subscription_id
                                    )),
                                ))
                                .await;
                        }
                        Some(subscription) => {
                            match runtime.unsubscribe(subscription.store_id).await {
                                Ok(result) => {
                                    let _ = subscription.unsubscribe();
                                    if result {
                                        let _ = messages_tx
                                            .send(GraphQlSubscriptionServerMessage::Complete(
                                                subscription_id.clone(),
                                            ))
                                            .await;
                                    } else {
                                        let _ = messages_tx
                                            .send(
                                                GraphQlSubscriptionServerMessage::ConnectionError(
                                                    create_json_error_object(format!(
                                                        "Subscription ID already unsubscribed: {}",
                                                        subscription_id
                                                    )),
                                                ),
                                            )
                                            .await;
                                    }
                                }
                                Err(error) => {
                                    subscriptions
                                        .get_mut()
                                        .unwrap()
                                        .insert(subscription_id.clone(), subscription);
                                    let _ = messages_tx
                                        .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                            create_json_error_object(format!(
                                                "Unsubscribe error: {}",
                                                error
                                            )),
                                        ))
                                        .await;
                                }
                            }
                        }
                    }
                }
                GraphQlSubscriptionClientMessage::Update(message) => {
                    let subscription_id = String::from(message.subscription_id());
                    let variable_updates = message.into_payload();
                    let has_updates = subscriptions
                        .get_mut()
                        .unwrap()
                        .get(&subscription_id)
                        .map(|subscription| {
                            variable_updates.iter().any(|(key, value)| {
                                subscription
                                    .operation
                                    .variable(key)
                                    .map(|existing_value| value != existing_value)
                                    .unwrap_or(true)
                            })
                        })
                        .unwrap_or(false);
                    if has_updates {
                        match subscriptions.get_mut().unwrap().remove(&subscription_id) {
                            None => {
                                let _ = messages_tx
                                    .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                        create_json_error_object(format!(
                                            "Subscription ID not found: {}",
                                            subscription_id
                                        )),
                                    ))
                                    .await;
                            }
                            Some(subscription) => match runtime
                                .unsubscribe(subscription.store_id)
                                .await
                            {
                                Ok(_) => {
                                    // TODO: inherit diff stream from existing query
                                    let diff = subscription.diff;
                                    let (operation, transform) = subscription.unsubscribe();
                                    let operation = {
                                        let (query, operation_name, variables, extensions) =
                                            operation.into_parts();
                                        let updated_variables =
                                            variables.into_iter().chain(variable_updates);
                                        GraphQlOperationPayload::new(
                                            query,
                                            operation_name,
                                            Some(updated_variables),
                                            Some(extensions),
                                        )
                                    };
                                    match subscribe_query(
                                        &operation,
                                        &runtime,
                                        &graph_root,
                                        &transform,
                                        compiler_options,
                                        &factory,
                                        &allocator,
                                    )
                                    .await
                                    {
                                        Err(error) => {
                                            let _ = messages_tx
                                                .send(GraphQlSubscriptionServerMessage::Error(
                                                    subscription_id,
                                                    create_json_error_object(error),
                                                ))
                                                .await;
                                        }
                                        Ok(store_subscription) => {
                                            let subscription = register_subscription(
                                                &subscription_id,
                                                operation,
                                                transform,
                                                store_subscription,
                                                diff,
                                                &messages_tx,
                                                &factory,
                                            );
                                            subscriptions
                                                .get_mut()
                                                .unwrap()
                                                .insert(subscription_id, subscription);
                                        }
                                    }
                                }
                                Err(error) => {
                                    subscriptions
                                        .get_mut()
                                        .unwrap()
                                        .insert(subscription_id, subscription);
                                    let _ = messages_tx
                                        .send(GraphQlSubscriptionServerMessage::ConnectionError(
                                            create_json_error_object(format!(
                                                "Update error: {}",
                                                error
                                            )),
                                        ))
                                        .await;
                                }
                            },
                        }
                    }
                }
                GraphQlSubscriptionClientMessage::ConnectionTerminate => {
                    let subscriptions = subscriptions
                        .get_mut()
                        .unwrap()
                        .drain()
                        .map(|(_, subscription)| subscription);
                    for subscription in subscriptions {
                        let _ = runtime.unsubscribe(subscription.store_id).await;
                        let _ = subscription.unsubscribe();
                    }
                }
            },
            Err(error) => {
                let _ = messages_tx.send(GraphQlSubscriptionServerMessage::ConnectionError(
                    create_json_error_object(format!("Invalid message: {}", error)),
                ));
            }
            Ok(None) => {}
        }
    }
    output.abort();
    Ok(())
}

async fn subscribe_query<'a, T: Expression>(
    operation: &GraphQlOperationPayload,
    runtime: &'a Runtime<T>,
    graph_root: &(Program, InstructionPointer),
    transform: &(impl AsyncGraphQlQueryTransform + Send + Sync + 'static),
    compiler_options: &CompilerOptions,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<StreamSubscription<'a, T, impl Stream<Item = T>>, String>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
    T::Builtin: From<Stdlib> + From<GraphQlStdlib>,
{
    let query = parse_graphql_operation(operation, factory, allocator, transform)?;
    let operation_id = uuid::Uuid::new_v4();
    let (program, entry_point) = compile_graphql_query(
        query,
        &operation_id,
        graph_root.clone(),
        compiler_options,
        factory,
        allocator,
    )
    .map_err(|err| format!("Compilation error: {}", err))?;
    let subscription_stream = runtime
        .subscribe(program, entry_point)
        .await
        .map_err(|err| format!("Subscription error: {}", err))?;
    Ok(subscription_stream)
}

fn register_subscription<T: Expression>(
    subscription_id: &SubscriptionId,
    operation: GraphQlOperationPayload,
    transform: impl AsyncGraphQlQueryTransform,
    store_subscription: StreamSubscription<
        T,
        impl Stream<Item = T> + Unpin + Send + Sync + 'static,
    >,
    diff: bool,
    messages: &mpsc::Sender<GraphQlSubscriptionServerMessage>,
    factory: &impl AsyncExpressionFactory<T>,
) -> Subscription
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
{
    let store_id = store_subscription.id();
    let mut results_stream = process_subscription_stream_results(
        subscription_id,
        diff,
        store_subscription.into_stream(),
        factory,
    );
    let task = tokio::spawn({
        let messages = messages.clone();
        async move {
            while let Some(result) = results_stream.next().await {
                let _ = messages.send(result).await;
            }
        }
    });
    Subscription {
        store_id,
        operation,
        transform: Box::new(transform),
        diff,
        task,
    }
}

fn process_subscription_stream_results<T: Expression>(
    subscription_id: &SubscriptionId,
    diff: bool,
    updates: impl Stream<Item = T>,
    factory: &impl AsyncExpressionFactory<T>,
) -> impl Stream<Item = GraphQlSubscriptionServerMessage>
where
    T: AsyncExpression + Rewritable<T> + Reducible<T> + Applicable<T> + Compile<T>,
{
    let results = updates.map({
        let factory = factory.clone();
        move |result| match factory.match_signal_term(&result) {
            Some(signal) => Err(sanitize_signal_errors(signal)),
            // TODO: avoid unnecessary sanitizing for non-diff streams
            None => match sanitize(&result) {
                Ok(result) => Ok(result),
                Err(error) => Err(vec![JsonValue::String(error)]),
            },
        }
    });
    if diff {
        create_diff_stream(results).left_stream()
    } else {
        results
            .map(|result| result.map(|value| (value, false)))
            .right_stream()
    }
    .map({
        let subscription_id = subscription_id.clone();
        move |result| format_subscription_result_message(&subscription_id, result)
    })
}

fn is_diff_subscription(message: &GraphQlSubscriptionStartMessage) -> bool {
    message
        .payload()
        .extension("diff")
        .map(|value| match value {
            JsonValue::Bool(value) => *value,
            _ => false,
        })
        .unwrap_or(false)
}

fn create_diff_stream(
    results: impl Stream<Item = Result<JsonValue, Vec<JsonValue>>>,
) -> impl Stream<Item = Result<(JsonValue, bool), Vec<JsonValue>>> {
    results
        .scan(None, |previous, value| {
            let result = match (&previous, &value) {
                (Some(previous_value), Ok(current_value)) => {
                    let diff = diff_results(previous_value, current_value);
                    *previous = value.ok();
                    Ok(diff.map(|value| (value, true)))
                }
                _ => {
                    *previous = value.as_ref().ok().cloned();
                    value.map(|value| Some((value, false)))
                }
            };
            Box::pin(async { Some(result) })
        })
        .filter_map(|result| {
            let result = match result {
                Ok(None) => None,
                Ok(Some(result)) => Some(Ok(result)),
                Err(error) => Some(Err(error)),
            };
            Box::pin(async { result })
        })
}

fn diff_results(previous: &JsonValue, current: &JsonValue) -> Option<JsonValue> {
    if current == previous {
        return None;
    }
    match previous {
        JsonValue::Object(previous) => match current {
            JsonValue::Object(current) => diff_objects(previous, current),
            _ => Some(current.clone()),
        },
        JsonValue::Array(previous) => match current {
            JsonValue::Array(current) => diff_lists(previous, current),
            _ => Some(current.clone()),
        },
        _ => Some(current.clone()),
    }
}

fn diff_objects(
    previous: &JsonMap<String, JsonValue>,
    current: &JsonMap<String, JsonValue>,
) -> Option<JsonValue> {
    let previous_entries = previous.iter().collect::<HashMap<_, _>>();
    let updates = json_object(current.iter().filter_map(|(key, current_value)| {
        previous_entries.get(key).and_then(|previous_value| {
            diff_results(previous_value, current_value).map(|value| (key.clone(), value))
        })
    }));
    if is_empty_json_object(&updates) {
        None
    } else {
        Some(updates)
    }
}

fn diff_lists(previous: &Vec<JsonValue>, current: &Vec<JsonValue>) -> Option<JsonValue> {
    let updates = current
        .iter()
        .zip(previous.iter())
        .map(|(current, previous)| diff_results(previous, current))
        .chain(
            current
                .iter()
                .skip(previous.len())
                .map(|item| Some(item.clone())),
        )
        .collect::<Vec<_>>();
    let updates = json_object(
        updates
            .into_iter()
            .enumerate()
            .filter_map(|(index, item)| item.map(|value| (index.to_string(), value))),
    );
    if is_empty_json_object(&updates) {
        None
    } else {
        Some(updates)
    }
}

fn is_empty_json_object(value: &JsonValue) -> bool {
    match value {
        JsonValue::Object(value) => value.is_empty(),
        _ => false,
    }
}

fn format_subscription_result_message(
    subscription_id: &SubscriptionId,
    result: Result<(JsonValue, bool), Vec<JsonValue>>,
) -> GraphQlSubscriptionServerMessage {
    match result {
        Ok((data, is_diff)) => {
            let payload = create_graphql_json_success_response(data);
            if is_diff {
                GraphQlSubscriptionServerMessage::Patch(subscription_id.clone(), payload)
            } else {
                GraphQlSubscriptionServerMessage::Data(subscription_id.clone(), payload)
            }
        }
        Err(errors) => {
            let payload = create_graphql_json_error_response(errors);
            GraphQlSubscriptionServerMessage::Data(subscription_id.clone(), payload)
        }
    }
}

fn create_graphql_json_success_response(value: JsonValue) -> JsonValue {
    json_object(once((String::from("data"), value)))
}

fn create_graphql_json_error_response(errors: impl IntoIterator<Item = JsonValue>) -> JsonValue {
    json_object(once((
        String::from("errors"),
        json_array(errors.into_iter().flat_map(parse_json_error_payload)),
    )))
}

fn parse_json_error_payload(payload: JsonValue) -> Vec<JsonValue> {
    match payload {
        JsonValue::Array(errors) => errors
            .into_iter()
            .flat_map(parse_json_error_payload)
            .collect(),
        JsonValue::Object(_) => vec![payload],
        payload => vec![json_object(once((String::from("message"), payload)))],
    }
}
