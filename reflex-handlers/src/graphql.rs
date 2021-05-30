// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, pin::Pin};

use futures_util::{FutureExt, StreamExt};
use reflex::{
    core::{Expression, SerializedObjectTerm, SerializedTerm, Signal, SignalTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_graphql::{subscriptions::SubscriptionId, GraphQlOperationPayload};
use reflex_runtime::{RuntimeEffect, SignalResult};
use tokio_stream::Stream;
use uuid::Uuid;

use crate::utils::{create_websocket_connection, fetch, subscribe_websocket_operation};

pub fn handle_graphql_execute(args: &[SerializedTerm]) -> Result<SignalResult, String> {
    if args.len() != 4 {
        return Err(format!(
            "Invalid GraphQL signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap());
    let query = parse_string_arg(args.next().unwrap());
    let operation_name = parse_optional_string_arg(args.next().unwrap());
    let variables = parse_object_arg(args.next().unwrap());
    match (url, query, operation_name, variables) {
        (Some(url), Some(query), Some(operation_name), Some(variables)) => {
            let operation = GraphQlOperationPayload::new(query.clone(), operation_name, variables);
            if url.starts_with("ws") {
                handle_graphql_ws_operation(&url, operation)
            } else {
                handle_graphql_http_operation(&url, operation)
            }
        }
        _ => Err(String::from("Invalid GraphQL signal arguments")),
    }
}

fn handle_graphql_http_operation(
    url: &str,
    operation: GraphQlOperationPayload,
) -> Result<(Expression, Option<RuntimeEffect>), String> {
    let url = String::from(url);
    let method = String::from("POST");
    let headers = once((
        String::from("Content-Type"),
        String::from("application/json"),
    ));
    let body = operation.stringify()?;
    Ok((
        Expression::new(Term::Signal(SignalTerm::new(Signal::new(
            SignalType::Pending,
            Vec::new(),
        )))),
        Some(RuntimeEffect::Async(Box::pin(async move {
            let response = fetch(method, url, headers, Some(body)).await;
            let result = match response {
                Ok(data) => {
                    let result = match reflex_json::serialized(&data) {
                        Ok(result) => parse_graphql_response_payload(result),
                        _ => Err(None),
                    };
                    match result {
                        Ok(result) => Ok(result),
                        Err(errors) => Err(format_graphql_error(errors)),
                    }
                }
                Err(error) => Err(error),
            };
            match result {
                Ok(result) => result,
                Err(error) => create_error_result(error),
            }
        }))),
    ))
}

fn handle_graphql_ws_operation(
    url: &str,
    operation: GraphQlOperationPayload,
) -> Result<(Expression, Option<RuntimeEffect>), String> {
    Ok((
        Expression::new(Term::Signal(SignalTerm::new(Signal::new(
            SignalType::Pending,
            Vec::new(),
        )))),
        Some(RuntimeEffect::Stream(Box::pin(
            create_graphql_subscription(String::from(url), operation)
                .into_stream()
                .flat_map(|(_subscription_id, stream)| {
                    stream.map(|message| {
                        let result = match message {
                            Err(error) => Err(error),
                            Ok(payload) => match parse_graphql_response_payload(payload) {
                                Ok(result) => Ok(result),
                                Err(errors) => Err(format_graphql_error(errors)),
                            },
                        };
                        match result {
                            Ok(result) => result,
                            Err(error) => create_error_result(error),
                        }
                    })
                }),
        ))),
    ))
}

async fn create_graphql_subscription(
    url: String,
    operation: GraphQlOperationPayload,
) -> (
    SubscriptionId,
    Pin<Box<dyn Stream<Item = Result<SerializedTerm, String>> + Send + Sync>>,
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
        Err(error) => (String::new(), Box::pin(tokio_stream::once(Err(error)))),
        Ok((subscription_id, stream)) => (subscription_id, Box::pin(stream)),
    }
}

fn parse_graphql_response_payload(
    result: SerializedTerm,
) -> Result<Expression, Option<Vec<String>>> {
    match result {
        SerializedTerm::Object(value) => {
            let (data, errors) =
                value
                    .entries()
                    .iter()
                    .fold((None, None), |(data, errors), (key, value)| {
                        match key.as_str() {
                            "data" => match value {
                                SerializedTerm::Object(_) => (Some(value), errors),
                                _ => (data, errors),
                            },
                            "errors" => match value {
                                SerializedTerm::List(list) => {
                                    match parse_graphql_errors(list.items()) {
                                        Some(errors) => (data, Some(errors)),
                                        _ => (None, errors),
                                    }
                                }
                                _ => (None, errors),
                            },
                            _ => (data, errors),
                        }
                    });
            match (data, errors) {
                (_, Some(errors)) => Err(Some(errors)),
                (Some(data), None) => Ok(data.deserialize()),
                _ => Err(None),
            }
        }
        _ => Err(None),
    }
}

fn format_graphql_error(errors: Option<Vec<String>>) -> String {
    match errors {
        Some(errors) => format!("GraphQL error: {}", errors.join("\n")),
        None => format!("Invalid GraphQL response"),
    }
}

fn create_error_result(error: String) -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Error,
        vec![SerializedTerm::string(error)],
    ))))
}

fn parse_string_arg(value: &SerializedTerm) -> Option<String> {
    match value {
        SerializedTerm::Value(value) => match value {
            ValueTerm::String(value) => Some(value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn parse_optional_string_arg(value: &SerializedTerm) -> Option<Option<String>> {
    match value {
        SerializedTerm::Value(value) => match value {
            ValueTerm::String(value) => Some(Some(value.clone())),
            ValueTerm::Null => Some(None),
            _ => None,
        },
        _ => None,
    }
}

fn parse_object_arg(value: &SerializedTerm) -> Option<SerializedObjectTerm> {
    match value {
        SerializedTerm::Object(value) => Some(value.clone()),
        _ => None,
    }
}

fn parse_graphql_errors<'a>(
    items: impl IntoIterator<Item = &'a SerializedTerm>,
) -> Option<Vec<String>> {
    items
        .into_iter()
        .map(parse_graphql_error)
        .collect::<Option<Vec<_>>>()
}

fn parse_graphql_error(value: &SerializedTerm) -> Option<String> {
    match value {
        SerializedTerm::Object(value) => value.entries().iter().find_map(|(key, value)| match key
            .as_str()
        {
            "message" => match value {
                SerializedTerm::Value(ValueTerm::String(message)) => Some(message.clone()),
                _ => None,
            },
            _ => None,
        }),
        _ => None,
    }
}
