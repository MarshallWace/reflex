// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::once, pin::Pin};

use futures_util::{FutureExt, StreamExt};
use reflex::{
    core::{Expression, Signal, SignalTerm, StateToken, Term},
    serialize::{serialize, SerializedObjectTerm, SerializedTerm},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_graphql::{subscriptions::SubscriptionId, GraphQlOperationPayload};
use reflex_runtime::{
    RuntimeEffect, SignalHandlerResult, SignalHelpers, SignalResult, StateUpdate,
};
use tokio_stream::Stream;
use uuid::Uuid;

use crate::utils::{create_websocket_connection, fetch, subscribe_websocket_operation};

pub fn graphql_execute_handler(
    signal_type: &str,
    signals: &[&Signal],
    _helpers: &SignalHelpers,
) -> SignalHandlerResult {
    if signal_type != "reflex::graphql::execute" {
        return None;
    }
    Some(
        signals
            .iter()
            .map(|signal| handle_graphql_execute_signal(signal.id(), signal.args()))
            .collect(),
    )
}

fn handle_graphql_execute_signal(
    signal_id: StateToken,
    args: &[Expression],
) -> Result<SignalResult, String> {
    if args.len() != 5 {
        return Err(format!(
            "Invalid GraphQL signal: Expected 5 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap());
    let query = parse_string_arg(args.next().unwrap());
    let operation_name = parse_optional_string_arg(args.next().unwrap());
    let variables = parse_object_arg(args.next().unwrap());
    let extensions = parse_object_arg(args.next().unwrap());
    match (url, query, operation_name, variables, extensions) {
        (Some(url), Some(query), Some(operation_name), Some(variables), Some(extensions)) => {
            let operation =
                GraphQlOperationPayload::new(query.clone(), operation_name, variables, extensions);
            if url.starts_with("ws") {
                handle_graphql_ws_operation(signal_id, &url, operation)
            } else {
                handle_graphql_http_operation(signal_id, &url, operation)
            }
        }
        _ => Err(String::from("Invalid GraphQL signal arguments")),
    }
}

fn handle_graphql_http_operation(
    signal_id: StateToken,
    url: &str,
    operation: GraphQlOperationPayload,
) -> Result<SignalResult, String> {
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
        Some(RuntimeEffect::deferred(Box::pin(async move {
            let response = fetch(method, url, headers, Some(body)).await;
            let result = match response {
                Ok((status, data)) => {
                    let result = match reflex_json::serialized(&data) {
                        Ok(result) => parse_graphql_response_payload(result),
                        _ => Err(None),
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
                Err(error) => create_error_result(error),
            };
            StateUpdate::value(signal_id, result)
        }))),
    ))
}

fn handle_graphql_ws_operation(
    signal_id: StateToken,
    url: &str,
    operation: GraphQlOperationPayload,
) -> Result<SignalResult, String> {
    Ok((
        Expression::new(Term::Signal(SignalTerm::new(Signal::new(
            SignalType::Pending,
            Vec::new(),
        )))),
        Some(RuntimeEffect::stream(Box::pin(
            create_graphql_subscription(String::from(url), operation)
                .into_stream()
                .flat_map(|(_subscription_id, stream)| {
                    stream.map(|message| {
                        let result = match message {
                            Err(error) => Err(error),
                            Ok(payload) => match parse_graphql_response_payload(payload) {
                                Ok(result) => Ok(result),
                                Err(errors) => Err(format_graphql_error(errors, None)),
                            },
                        };
                        match result {
                            Ok(result) => result,
                            Err(error) => create_error_result(error),
                        }
                    })
                })
                .map(move |value| StateUpdate::value(signal_id, value)),
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

fn format_graphql_error(errors: Option<Vec<String>>, status: Option<u16>) -> String {
    match errors {
        Some(errors) => format!(
            "GraphQL error{}: {}",
            match status {
                Some(status) => format!(" (HTTP status {})", status),
                None => String::new(),
            },
            errors.join("\n")
        ),
        None => format!(
            "Invalid GraphQL response{}",
            match status {
                Some(status) => format!(" (HTTP status {})", status),
                None => String::new(),
            }
        ),
    }
}

fn create_error_result(error: String) -> Expression {
    Expression::new(Term::Signal(SignalTerm::new(Signal::new(
        SignalType::Error,
        vec![Expression::new(Term::Value(ValueTerm::String(error)))],
    ))))
}

fn parse_string_arg(value: &Expression) -> Option<String> {
    match value.value() {
        Term::Value(value) => match value {
            ValueTerm::String(value) => Some(value.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn parse_optional_string_arg(value: &Expression) -> Option<Option<String>> {
    match value.value() {
        Term::Value(value) => match value {
            ValueTerm::String(value) => Some(Some(value.clone())),
            ValueTerm::Null => Some(None),
            _ => None,
        },
        _ => None,
    }
}

fn parse_object_arg(value: &Expression) -> Option<SerializedObjectTerm> {
    match serialize(value.value()) {
        Ok(SerializedTerm::Object(value)) => Some(value),
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
