// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, pin::Pin};

use futures_util::{FutureExt, StreamExt};
use reflex::{
    core::{
        Expression, ExpressionFactory, ExpressionList, HeapAllocator, Signal, SignalType,
        StateToken, StringValue,
    },
    lang::ValueTerm,
};
use reflex_graphql::{subscriptions::SubscriptionId, GraphQlOperationPayload};
use reflex_json::{hydrate, sanitize, JsonValue};
use reflex_runtime::{
    AsyncExpression, AsyncExpressionFactory, AsyncHeapAllocator, RuntimeEffect,
    SignalHandlerResult, SignalHelpers, SignalResult, StateUpdate,
};
use tokio_stream::Stream;
use uuid::Uuid;

use crate::utils::{
    create_json_error_object, create_websocket_connection, fetch, subscribe_websocket_operation,
};

pub fn graphql_execute_handler<T: AsyncExpression>(
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> impl Fn(&str, &[&Signal<T>], &SignalHelpers<T>) -> SignalHandlerResult<T> {
    let factory = factory.clone();
    let allocator = allocator.clone();
    move |signal_type: &str, signals: &[&Signal<T>], _helpers: &SignalHelpers<T>| {
        if signal_type != "reflex::graphql::execute" {
            return None;
        }
        Some(
            signals
                .iter()
                .map(|signal| {
                    handle_graphql_execute_signal(signal.id(), signal.args(), &factory, &allocator)
                })
                .collect(),
        )
    }
}

fn handle_graphql_execute_signal<T: AsyncExpression>(
    signal_id: StateToken,
    args: &ExpressionList<T>,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    if args.len() != 5 {
        return Err(format!(
            "Invalid GraphQL signal: Expected 5 arguments, received {}",
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
                handle_graphql_ws_operation(signal_id, &url, operation, factory, allocator)
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
        factory.create_signal_term(allocator.create_signal_list(once(
            allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
        ))),
        Some(RuntimeEffect::deferred({
            let factory = factory.clone();
            let allocator = allocator.clone();
            Box::pin(async move {
                let response = fetch(method, url, headers, Some(body)).await;
                let result = match response {
                    Ok((status, data)) => {
                        let result = match reflex_json::deserialize(&data) {
                            Ok(result) => {
                                parse_graphql_response_payload(result, &factory, &allocator)
                            }
                            Err(error) => Err(vec![create_json_error_object(error)]),
                        };
                        match result {
                            Ok(result) => Ok(result),
                            Err(errors) => Err(format_graphql_error(errors, Some(status))),
                        }
                    }
                    Err(error) => Err(error),
                };
                let result = match result {
                    Ok(result) => result,
                    Err(error) => create_error_signal(error, &factory, &allocator),
                };
                StateUpdate::value(signal_id, result)
            })
        })),
    ))
}

fn handle_graphql_ws_operation<T: AsyncExpression>(
    signal_id: StateToken,
    url: &str,
    operation: GraphQlOperationPayload,
    factory: &impl AsyncExpressionFactory<T>,
    allocator: &impl AsyncHeapAllocator<T>,
) -> Result<SignalResult<T>, String> {
    Ok((
        factory.create_signal_term(allocator.create_signal_list(once(
            allocator.create_signal(SignalType::Pending, allocator.create_empty_list()),
        ))),
        Some(RuntimeEffect::stream({
            let factory = factory.clone();
            let allocator = allocator.clone();
            Box::pin(
                create_graphql_subscription(String::from(url), operation)
                    .into_stream()
                    .flat_map(move |(_subscription_id, stream)| {
                        let factory = factory.clone();
                        let allocator = allocator.clone();
                        stream.map(move |message| {
                            let result = match message {
                                Err(error) => Err(error),
                                Ok(payload) => {
                                    parse_graphql_response_payload(payload, &factory, &allocator)
                                }
                            };
                            match result {
                                Ok(result) => result,
                                Err(errors) => create_error_signal(
                                    format_graphql_error(errors, None),
                                    &factory,
                                    &allocator,
                                ),
                            }
                        })
                    })
                    .map(move |value| StateUpdate::value(signal_id, value)),
            )
        })),
    ))
}

async fn create_graphql_subscription(
    url: String,
    operation: GraphQlOperationPayload,
) -> (
    SubscriptionId,
    Pin<Box<dyn Stream<Item = Result<JsonValue, Vec<JsonValue>>> + Send + Sync>>,
) {
    let result = match create_websocket_connection(&url).await {
        Err(error) => Err(error),
        Ok(socket) => {
            let subscription_id = Uuid::new_v4().to_hyphenated().to_string();
            match subscribe_websocket_operation(socket, subscription_id.clone(), operation).await {
                Err(error) => Err(error),
                Ok(stream) => Ok((subscription_id, stream)),
            }
        }
    };
    match result {
        Err(error) => (
            String::new(),
            Box::pin(tokio_stream::once(Err(vec![create_json_error_object(
                error,
            )]))),
        ),
        Ok((subscription_id, stream)) => (subscription_id, Box::pin(stream)),
    }
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
                            JsonValue::Object(_) => match hydrate(value, factory, allocator) {
                                Ok(data) => Ok((Some(data), errors)),
                                Err(error) => Err(error),
                            },
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

fn format_graphql_error(errors: Vec<JsonValue>, status: Option<u16>) -> String {
    format!(
        "GraphQL error{}: {}",
        match status {
            Some(status) => format!(" (HTTP status {})", status),
            None => String::new(),
        },
        errors
            .into_iter()
            .map(|err| format!("{}", err))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn create_error_signal<T: Expression>(
    error: String,
    factory: &impl ExpressionFactory<T>,
    allocator: &impl HeapAllocator<T>,
) -> T {
    factory.create_signal_term(allocator.create_signal_list(once(allocator.create_signal(
        SignalType::Error,
        allocator.create_unit_list(
            factory.create_value_term(ValueTerm::String(allocator.create_string(error))),
        ),
    ))))
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
                .map(|(key, value)| sanitize(value).map(|value| (String::from(key.as_str()), value)))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Some(properties))
        }
        _ => Ok(None),
    }
}
