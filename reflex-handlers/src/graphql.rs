// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{
    iter::once,
    sync::{Arc, Mutex},
};

use futures_util::{
    future::{self, AbortHandle, Abortable},
    stream, FutureExt, StreamExt,
};
use reflex::{
    compiler::Compile,
    core::{
        Expression, ExpressionFactory, ExpressionList, HeapAllocator, Signal, SignalType,
        StateToken, StringValue,
    },
    lang::ValueTerm,
};
use reflex_graphql::GraphQlOperationPayload;
use reflex_json::JsonValue;
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect, SignalHandler,
    SignalHelpers, SignalResult, StateUpdate,
};

use crate::utils::{
    fetch,
    graphql::{create_json_error_object, WebSocketConnectionManager},
};

pub const SIGNAL_TYPE_GRAPHQL: &str = "reflex::graphql";

pub fn create_graphql_signal_handler<T>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl SignalHandler<T>
where
    T: AsyncExpression + Compile<T>,
    T::String: Send + Sync,
{
    let connections = WebSocketConnectionManager::default();
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], _helpers: &SignalHelpers<T>| {
        if signal_type != SIGNAL_TYPE_GRAPHQL {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_graphql_signal(
                        signal.id(),
                        signal.args(),
                        &connections,
                        &factory,
                        &allocator,
                    )
                })
                .collect(),
        )
    }
}

fn handle_graphql_signal<T: AsyncExpression>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    connections: &WebSocketConnectionManager,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    if args.len() != 6 {
        return Err(format!(
            "Invalid GraphQL signal: Expected 6 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap(), factory);
    let query = parse_string_arg(args.next().unwrap(), factory);
    let operation_name = parse_optional_string_arg(args.next().unwrap(), factory);
    let variables = parse_object_arg(args.next().unwrap(), factory)?;
    let extensions = parse_object_arg(args.next().unwrap(), factory)?;
    match (url, query, operation_name, variables, extensions) {
        (Some(url), Some(query), Some(operation_name), Some(variables), Some(extensions)) => {
            let operation = GraphQlOperationPayload::new(
                query.clone(),
                operation_name,
                Some(variables),
                Some(extensions),
            );
            if url.starts_with("ws") {
                handle_graphql_ws_operation(
                    signal_id,
                    &url,
                    operation,
                    connections,
                    factory,
                    allocator,
                )
            } else {
                handle_graphql_http_operation(signal_id, &url, operation, factory, allocator)
            }
        }
        _ => Err(String::from("Invalid GraphQL signal arguments")),
    }
}

fn handle_graphql_http_operation<T: AsyncExpression>(
    signal_id: StateToken,
    url: &str,
    operation: GraphQlOperationPayload,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    let url = String::from(url);
    let method = String::from("POST");
    let headers = once((
        String::from("Content-Type"),
        String::from("application/json"),
    ));
    let body = format!("{}", operation.into_json());
    Ok((
        create_pending_signal(factory, allocator),
        Some({
            let (task, dispose) = {
                let factory = factory.clone();
                let allocator = allocator.clone();
                let (abort_handle, abort_registration) = AbortHandle::new_pair();
                let task = async move {
                    eprintln!("[GraphQL] HTTP request: {} {}", method, url);
                    let response = fetch(&method, &url, headers, Some(body)).await;
                    let result = match response {
                        Ok((_status, data)) => {
                            let result = match reflex_json::deserialize(&data) {
                                Ok(result) => {
                                    parse_graphql_response_payload(result, &factory, &allocator)
                                }
                                Err(error) => Err(vec![create_json_error_object(error)]),
                            };
                            match result {
                                Ok(result) => {
                                    eprintln!(
                                        "[GraphQL] HTTP success response: {} {}",
                                        method, url
                                    );
                                    Ok(result)
                                }
                                Err(errors) => {
                                    eprintln!(
                                        "[GraphQL] HTTP error response: {} {}{}",
                                        method,
                                        url,
                                        errors
                                            .iter()
                                            .map(|error| format!("\n {}", error))
                                            .collect::<Vec<_>>()
                                            .join("")
                                    );
                                    Err(errors)
                                }
                            }
                        }
                        Err(error) => {
                            eprintln!(
                                "[GraphQL] HTTP network error: {} {}\n {}",
                                method, url, error
                            );
                            Err(vec![create_json_error_object(error)])
                        }
                    };
                    let result = match result {
                        Ok(result) => result,
                        Err(errors) => create_error_signal(errors, &factory, &allocator),
                    };
                    StateUpdate::value(signal_id, result)
                };
                let dispose = async move {
                    abort_handle.abort();
                };
                (Abortable::new(task, abort_registration), dispose)
            };
            RuntimeEffect::deferred(
                Box::pin({
                    let factory = factory.clone();
                    let allocator = allocator.clone();
                    async move {
                        let result = task.await;
                        match result {
                            Ok(result) => result,
                            Err(_) => StateUpdate::value(
                                signal_id,
                                create_pending_signal(&factory, &allocator),
                            ),
                        }
                    }
                }),
                dispose,
            )
        }),
    ))
}

fn handle_graphql_ws_operation<T: AsyncExpression>(
    signal_id: StateToken,
    url: &str,
    operation: GraphQlOperationPayload,
    connections: &WebSocketConnectionManager,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    let active_subscription_id = Arc::new(Mutex::new(None));
    let dispose = {
        let active_subscription_id = active_subscription_id.clone();
        let connections = connections.clone();
        async move {
            let active_subscription_id = active_subscription_id.lock().unwrap().take();
            if let Some(subscription_id) = active_subscription_id {
                let _ = connections.channel().unsubscribe(subscription_id).await;
            }
        }
    };
    Ok((
        factory.create_signal_term(allocator.create_signal_list(once(
            allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
        ))),
        Some(RuntimeEffect::stream(
            {
                let url = String::from(url);
                let factory = factory.clone();
                let allocator = allocator.clone();
                let active_subscription_id = active_subscription_id.clone();
                let connections = connections.clone();
                eprintln!("[GraphQL] WebSocket request: {}", url);
                Box::pin(
                    connections
                        .channel()
                        .subscribe(url.clone(), operation)
                        .into_stream()
                        .flat_map({
                            let connections = connections.clone();
                            move |connection| match connection {
                                Err(error) => {
                                    stream::iter(once(Err(vec![create_json_error_object(error)])))
                                        .left_stream()
                                }
                                Ok((subscription_id, results)) => {
                                    let existing_id = active_subscription_id
                                        .lock()
                                        .unwrap()
                                        .replace(subscription_id);
                                    let unsubscribe_existing = if let Some(existing_id) =
                                        existing_id
                                    {
                                        connections.channel().unsubscribe(existing_id).left_future()
                                    } else {
                                        future::ready(Ok(())).right_future()
                                    };
                                    unsubscribe_existing
                                        .map(move |result| match result {
                                            Err(error) => stream::iter(once(Err(vec![
                                                create_json_error_object(error),
                                            ])))
                                            .left_stream(),
                                            Ok(_) => results.right_stream(),
                                        })
                                        .into_stream()
                                        .flatten()
                                        .right_stream()
                                }
                            }
                        })
                        .map({
                            let url = url.clone();
                            let factory = factory.clone();
                            let allocator = allocator.clone();
                            move |result| {
                                let result = result.and_then(|payload| {
                                    parse_graphql_response_payload(payload, &factory, &allocator)
                                });
                                let value = match result {
                                    Err(errors) => {
                                        eprintln!(
                                            "[GraphQL] WebSocket error response: {}{}",
                                            url,
                                            errors
                                                .iter()
                                                .map(|error| format!("\n {}", error))
                                                .collect::<Vec<_>>()
                                                .join("")
                                        );
                                        create_error_signal(errors, &factory, &allocator)
                                    }
                                    Ok(result) => {
                                        eprintln!("[GraphQL] WebSocket success response: {}", url);
                                        result
                                    }
                                };
                                StateUpdate::value(signal_id, value)
                            }
                        }),
                )
            },
            dispose,
        )),
    ))
}

fn parse_graphql_response_payload<T: Expression>(
    result: JsonValue,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> Result<T, Vec<JsonValue>> {
    let result = match result {
        JsonValue::Object(value) => {
            let result = value
                .into_iter()
                .fold(Ok((None, None)), |results, (key, value)| {
                    let (data, errors) = results?;
                    match key.as_str() {
                        "data" => match &value {
                            JsonValue::Object(_) => {
                                match reflex_json::hydrate(value, factory, allocator) {
                                    Ok(data) => Ok((Some(data), errors)),
                                    Err(error) => Err(error),
                                }
                            }
                            _ => Ok((data, errors)),
                        },
                        "errors" => match value {
                            JsonValue::Array(errors) => Ok((data, Some(errors))),
                            _ => Ok((data, errors)),
                        },
                        _ => Ok((data, errors)),
                    }
                });
            match result {
                Err(error) => Some(Err(vec![create_json_error_object(error)])),
                Ok(result) => {
                    let (data, errors) = result;
                    match (data, errors) {
                        (_, Some(errors)) => Some(Err(errors)),
                        (Some(data), None) => Some(Ok(data)),
                        _ => None,
                    }
                }
            }
        }
        _ => None,
    };
    match result {
        Some(result) => result,
        None => Err(vec![create_json_error_object(String::from(
            "Invalid GraphQL response payload",
        ))]),
    }
}

fn create_pending_signal<T: Expression>(
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(
        allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
    )))
}

fn create_error_signal<T: Expression>(
    errors: impl IntoIterator<Item = JsonValue>,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(
        allocator.create_signal_list(errors.into_iter().map(|error| {
            allocator.create_signal(
                SignalType::Error,
                allocator.create_unit_list(match reflex_json::hydrate(error, factory, allocator) {
                    Ok(error) => error,
                    Err(error) => {
                        factory.create_value_term(ValueTerm::String(allocator.create_string(error)))
                    }
                }),
            )
        })),
    )
}

fn parse_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<String> {
    match factory.match_value_term(value) {
        Some(ValueTerm::String(value)) => Some(String::from(value.as_str())),
        _ => None,
    }
}

fn parse_optional_string_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Option<String>> {
    match factory.match_value_term(value) {
        Some(ValueTerm::String(value)) => Some(Some(String::from(value.as_str()))),
        Some(ValueTerm::Null) => Some(None),
        _ => None,
    }
}

fn parse_object_arg<T: Expression>(
    value: &T,
    factory: &impl ExpressionFactory<T>,
) -> Result<Option<impl IntoIterator<Item = (String, JsonValue)>>, String> {
    match factory.match_struct_term(value) {
        Some(value) => {
            let properties = value
                .entries()
                .into_iter()
                .map(|(key, value)| {
                    reflex_json::sanitize(value).map(|value| (String::from(key.as_str()), value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Some(properties))
        }
        _ => Ok(None),
    }
}
