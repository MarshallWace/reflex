// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::iter::once;

use reflex::{
    core::{Expression, SerializedObjectTerm, SerializedTerm, Signal, SignalTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_json::stringify;
use reflex_runtime::{RuntimeEffect, SignalResult};

use crate::utils::fetch;

pub fn handle_graphql_execute(args: &[SerializedTerm]) -> Result<SignalResult, String> {
    if args.len() != 3 {
        return Err(format!(
            "Invalid GraphQL signal: Expected 3 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap());
    let query = parse_string_arg(args.next().unwrap());
    let variables = parse_object_arg(args.next().unwrap());
    match (url, query, variables) {
        (Some(url), Some(query), Some(variables)) => {
            let method = String::from("POST");
            let headers = once((
                String::from("Content-Type"),
                String::from("application/json"),
            ));
            let body = stringify(SerializedTerm::object(vec![
                (String::from("query"), SerializedTerm::string(query)),
                (String::from("variables"), SerializedTerm::Object(variables)),
            ]))?;
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
                                Ok(result) => match result {
                                    SerializedTerm::Object(value) => {
                                        let (data, errors) = value.entries().iter().fold(
                                            (None, None),
                                            |(data, errors), (key, value)| match key.as_str() {
                                                "data" => match value {
                                                    SerializedTerm::Object(_) => {
                                                        (Some(value), errors)
                                                    }
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
                                            },
                                        );
                                        match (data, errors) {
                                            (_, Some(errors)) => Err(Some(errors)),
                                            (Some(data), None) => Ok(data.deserialize()),
                                            _ => Err(None),
                                        }
                                    }
                                    _ => Err(None),
                                },
                                _ => Err(None),
                            };
                            match result {
                                Ok(result) => Ok(result),
                                Err(Some(errors)) => {
                                    Err(format!("GraphQL error: {}", errors.join("\n")))
                                }
                                Err(None) => Err(format!("Invalid GraphQL response")),
                            }
                        }
                        Err(error) => Err(error),
                    };
                    match result {
                        Ok(result) => result,
                        Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                            SignalType::Error,
                            vec![SerializedTerm::string(error)],
                        )))),
                    }
                }))),
            ))
        }
        _ => Err(String::from("Invalid GraphQL signal arguments")),
    }
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
