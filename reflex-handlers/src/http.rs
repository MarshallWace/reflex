// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex::{
    core::{Expression, SerializedTerm, Signal, SignalTerm, StructTerm, Term},
    stdlib::{signal::SignalType, value::ValueTerm},
};
use reflex_runtime::RuntimeEffect;

use crate::{utils::fetch, SignalResult};

pub fn handle_http_fetch(args: &[SerializedTerm]) -> Result<SignalResult, String> {
    if args.len() != 4 {
        return Err(format!(
            "Invalid fetch signal: Expected 4 arguments, received {}",
            args.len()
        ));
    }
    let mut args = args.into_iter();
    let url = parse_string_arg(args.next().unwrap());
    let method = parse_string_arg(args.next().unwrap());
    let headers = parse_key_values_arg(args.next().unwrap());
    let body = parse_optional_string_arg(args.next().unwrap());
    match (method, url, headers, body) {
        (Some(method), Some(url), Some(headers), Some(body)) => Ok((
            Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                SignalType::Pending,
                Vec::new(),
            )))),
            Some(RuntimeEffect::Async(Box::pin(async move {
                match fetch(method, url, headers, body).await {
                    Ok((status, data)) => Expression::new(Term::Struct(StructTerm::new(
                        None,
                        vec![
                            Expression::new(Term::Value(ValueTerm::Int(status as i32))),
                            Expression::new(Term::Value(ValueTerm::String(data))),
                        ],
                    ))),
                    Err(error) => Expression::new(Term::Signal(SignalTerm::new(Signal::new(
                        SignalType::Error,
                        vec![SerializedTerm::string(error)],
                    )))),
                }
            }))),
        )),
        _ => Err(String::from("Invalid fetch signal arguments")),
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

fn parse_key_values_arg(value: &SerializedTerm) -> Option<Vec<(String, String)>> {
    match value {
        SerializedTerm::Object(value) => value
            .entries()
            .iter()
            .map(|(key, value)| match value {
                SerializedTerm::Value(value) => match value {
                    ValueTerm::String(value) => Some((key.clone(), value.clone())),
                    _ => None,
                },
                _ => None,
            })
            .collect::<Option<Vec<_>>>(),
        _ => None,
    }
}
